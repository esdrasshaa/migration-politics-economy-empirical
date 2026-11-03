"""
Step 3 - Rolling-window Doc2Vec scoring (Section 3.3).

For each quarter t: PV-DBOW model trained on speeches in [t-40, t),
reference vectors = mean of dictionary-term vectors, speeches of quarter t
inferred and compared by cosine similarity.

Doc2Vec models do not depend on the dictionary, so both modes share
results/doc2vec_models2/... (existing models are loaded, not retrained).

Output: <results>/doc2vec_10Years_RollingWindow.csv   (same name as before)
        columns similarity_expansive / similarity_restrictive as before
"""
import multiprocessing
import time

import numpy as np
import pandas as pd
from gensim.models.doc2vec import Doc2Vec, TaggedDocument

from . import config as C
from . import utils as U


def train_or_load_doc2vec(subset, path):
    if path.exists():
        return Doc2Vec.load(str(path))
    docs = [TaggedDocument(str(t).split(), [i]) for i, t in enumerate(subset[C.TEXT_COL])]
    model = Doc2Vec(
        docs, dm=0, dbow_words=1, workers=max(1, multiprocessing.cpu_count() - 1),
        alpha=0.025, vector_size=C.DOC2VEC_PARAMS["vector_size"], min_count=1,
        epochs=C.DOC2VEC_PARAMS["epochs"], window=C.DOC2VEC_PARAMS["window"],
    )
    path.parent.mkdir(parents=True, exist_ok=True)
    model.save(str(path))
    return model


def reference_vector(terms, model):
    vocab = model.wv.key_to_index
    found, vectors = U.find_terms_vectors(terms, model, vocab)
    if not vectors:
        return None, 0
    return sum(vectors) / len(vectors), len(found)


def run(mode="global", rolling_dictionaries=None):
    """
    mode="global" : one dictionary (results/new_*_words.csv) for all quarters.
    mode="rolling": dictionary of the matching window (from step 2).
    """
    results_dir, _ = C.mode_dirs(mode)
    data = U.add_quarter_index(pd.read_csv(C.IMMIGRATION_SPEECHES, low_memory=False))
    data["similarity_expansive"] = np.nan
    data["similarity_restrictive"] = np.nan

    if mode == "global":
        g_dir, _ = C.mode_dirs("global")
        global_w = U.read_term_file(g_dir / "new_welcoming_words.csv", "welcoming_terms")
        global_r = U.read_term_file(g_dir / "new_restrictive_words.csv", "restrictive_terms")
    elif rolling_dictionaries is None:
        from .step2_dictionary import load_rolling_dictionaries
        rolling_dictionaries = load_rolling_dictionaries()

    coverage = []
    start = time.time()
    for train_q, scored_q in U.rolling_windows():
        path = C.DOC2VEC_MODELS_DIR / f"doc2vec_subset_{train_q[0]}_{train_q[-1]}"
        subset = data[data.index.isin(list(train_q))].reset_index(drop=True)
        model = train_or_load_doc2vec(subset, path)

        welcoming, restrictive = (
            (global_w, global_r) if mode == "global" else rolling_dictionaries[str(scored_q)]
        )
        v_w, n_w = reference_vector(welcoming, model)
        v_r, n_r = reference_vector(restrictive, model)
        coverage.append({"scored_quarter": str(scored_q), "terms_found_welcoming": n_w,
                         "terms_found_restrictive": n_r})

        mask = data.index == scored_q
        if mask.sum() == 0 or v_w is None or v_r is None:
            print(f"  {scored_q}: skipped (speeches={mask.sum()}, terms found {n_w}/{n_r})")
            continue

        doc_vecs = [model.infer_vector(str(t).split()) for t in data.loc[mask, C.TEXT_COL]]
        data.loc[mask, "similarity_expansive"] = [U.cosine(v, v_w) for v in doc_vecs]
        data.loc[mask, "similarity_restrictive"] = [U.cosine(v, v_r) for v in doc_vecs]
        print(f"  {scored_q}: {mask.sum():>4} speeches scored (terms found {n_w}/{n_r})")

    print(f"[step3/{mode}] Finished in {(time.time() - start) / 60:.1f} minutes")
    out = results_dir / "doc2vec_10Years_RollingWindow.csv"
    U.backup_if_exists(out)
    data.to_csv(out, index=False)
    pd.DataFrame(coverage).to_csv(results_dir / "dictionary_coverage_by_quarter.csv", index=False)
    return data
