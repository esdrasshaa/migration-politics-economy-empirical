"""
Step 2 - Dictionary construction with Word2Vec (Section 3.2).

Two modes, same file names:

global  (as in the thesis)
    One Word2Vec model on the whole immigration corpus.
    -> results/word2vec/word2vecModel
    -> output_dictionaries/welcoming_terms_preprocessed.csv, ..._scored.csv, ambiguous_terms.csv
    -> results/new_welcoming_words.csv, results/new_restrictive_words.csv

rolling (look-ahead free, like the Doc2Vec step)
    For every scored quarter t, Word2Vec, the TF-IDF filter and the PCA
    refinement only use speeches from [t-40, t). One dictionary per window.
    -> results_rolling/word2vec/word2vecModel_<start>_<end>
    -> output_dictionaries_rolling/windows/<start>_<end>/<same file names>
    -> results_rolling/dictionary_sizes_by_window.csv

FIX compared with the old notebook (cells 24-25):
    The PCA step selected `welcoming_words[i] for i in welcome_idx`, where
    welcome_idx indexed the *already filtered and shuffled* array. This
    returned the FIRST n words of an (arbitrarily ordered) list instead of
    the words above/below the PC2 threshold. Words are now selected with the
    same boolean mask as the points in the plot.
"""
from collections import defaultdict

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from gensim.models import Word2Vec
from sklearn.decomposition import PCA
from sklearn.feature_extraction.text import TfidfVectorizer

from . import config as C
from . import utils as U


# ======================================================================
# Building blocks (used by both modes)
# ======================================================================
def train_word2vec(tokens, path=None):
    if path is not None and path.exists():
        return Word2Vec.load(str(path))
    model = Word2Vec(sentences=tokens, workers=C.W2V_WORKERS, **C.W2V_PARAMS)
    if path is not None:
        path.parent.mkdir(parents=True, exist_ok=True)
        model.save(str(path))
    return model


def mine_candidates(seeds, model, topn=C.TOP_N):
    """Highest cosine similarity of each neighbour to any seed."""
    cands, missing = defaultdict(float), []
    for seed in seeds:
        if seed not in model.wv:
            missing.append(seed)
            continue
        for word, sim in model.wv.most_similar(seed, topn=topn):
            cands[word] = max(cands[word], sim)
    return dict(cands), missing


def drop_ambiguous(w_cands, r_cands):
    """Words found as neighbours of both seed lists are removed from both."""
    overlap = sorted(set(w_cands) & set(r_cands))
    ambiguous = [
        {"term": w, "sim_welcoming": round(w_cands[w], 4), "sim_restrictive": round(r_cands[w], 4)}
        for w in overlap
    ]
    for w in overlap:
        del w_cands[w], r_cands[w]
    return w_cands, r_cands, pd.DataFrame(ambiguous, columns=["term", "sim_welcoming", "sim_restrictive"])


def tfidf_vocabulary(tokens):
    docs = [" ".join(t) for t in tokens if t]
    tfidf = TfidfVectorizer(**C.TFIDF_PARAMS)
    tfidf.fit(docs)
    return set(tfidf.get_feature_names_out())


def apply_filters(cands, vocab):
    return {
        w: s for w, s in cands.items()
        if s >= C.MIN_SIM and w in vocab and len(w) >= C.MIN_TERM_LEN and w.isalpha()
    }


def pca_projection(model, welcoming_words, restrictive_words):
    """2D PCA of all in-vocabulary dictionary words (same procedure as the thesis)."""
    w_words = [w for w in welcoming_words if w in model.wv]
    r_words = [w for w in restrictive_words if w in model.wv]
    vectors = np.vstack([model.wv[w] for w in w_words] + [model.wv[w] for w in r_words])
    words = np.array(w_words + r_words)
    labels = np.array(["welcoming"] * len(w_words) + ["restrictive"] * len(r_words))

    np.random.seed(C.PCA_SHUFFLE_SEED)
    idx = np.random.permutation(len(vectors))
    vectors, words, labels = vectors[idx], words[idx], labels[idx]

    pca = PCA(n_components=2, random_state=C.PCA_RANDOM_STATE)
    xy = pca.fit_transform(vectors)
    return pd.DataFrame({"term": words, "label": labels, "pc1": xy[:, 0], "pc2": xy[:, 1]}), pca


def select_by_pc2(proj, mode):
    """
    global : fixed thresholds as in the thesis (PC2 >= 0.5 / PC2 < -0.48).
    rolling: orient PC2 (welcoming above restrictive), keep the same shares.
    Returns (welcoming_terms, restrictive_terms, proj_with_flag).
    """
    proj = proj.copy()
    w = proj["label"] == "welcoming"
    r = proj["label"] == "restrictive"

    if mode == "global":
        if proj.loc[w, "pc2"].mean() < proj.loc[r, "pc2"].mean():
            print("  WARNING: welcoming words lie BELOW restrictive words on PC2 - "
                  "check the sign before using the fixed thresholds.")
        keep_w = w & (proj["pc2"] >= C.WELCOMING_PC2_MIN)
        keep_r = r & (proj["pc2"] < C.RESTRICTIVE_PC2_MAX)
    else:
        if proj.loc[w, "pc2"].mean() < proj.loc[r, "pc2"].mean():
            proj["pc2"] = -proj["pc2"]          # sign of a PC is arbitrary
        thr_w = proj.loc[w, "pc2"].quantile(1 - C.WELCOMING_KEEP_SHARE)
        thr_r = proj.loc[r, "pc2"].quantile(C.RESTRICTIVE_KEEP_SHARE)
        keep_w = w & (proj["pc2"] >= thr_w)
        keep_r = r & (proj["pc2"] <= thr_r)

    proj["kept"] = keep_w | keep_r
    welcoming = sorted(proj.loc[keep_w, "term"])
    restrictive = sorted(proj.loc[keep_r, "term"])
    return welcoming, restrictive, proj


def plot_pca(proj, path, kept_only=False):
    p = proj[proj["kept"]] if kept_only else proj
    fig, ax = plt.subplots(figsize=(14, 8))
    for label, colour, name in [("welcoming", "green", "Welcoming"), ("restrictive", "red", "Restrictive")]:
        q = p[p["label"] == label]
        ax.scatter(q["pc1"], q["pc2"], color=colour, alpha=0.9, label=name)
    ax.set_title("Word2Vec Embeddings Projection (PCA)", fontsize=19)
    ax.set_xlabel("Principal Component 1", fontsize=19)
    ax.set_ylabel("Principal Component 2", fontsize=19)
    ax.legend(fontsize=19)
    ax.grid(True)
    fig.tight_layout()
    fig.savefig(path, dpi=200)
    plt.close(fig)


def build_dictionary(model, tokens, seed_w, seed_r, out_dict_dir, results_dir, mode,
                     exclusions, save_figures=True, verbose=True):
    """Full chain: expansion -> filters -> union with seeds -> PCA -> exclusions."""
    # 1) expansion of the topic-focused seed lists
    w_cands, miss_w = mine_candidates(C.WELCOMING_EXPANSION_SEEDS, model)
    r_cands, miss_r = mine_candidates(C.RESTRICTIVE_EXPANSION_SEEDS, model)
    w_cands, r_cands, ambiguous = drop_ambiguous(w_cands, r_cands)

    vocab = tfidf_vocabulary(tokens)
    w_final, r_final = apply_filters(w_cands, vocab), apply_filters(r_cands, vocab)

    def scored(d, col):
        return pd.DataFrame(sorted(d.items(), key=lambda x: -x[1]), columns=[col, "similarity_score"])

    df_w, df_r = scored(w_final, "welcoming_terms"), scored(r_final, "restrictive_terms")
    out_dict_dir.mkdir(parents=True, exist_ok=True)
    df_w[["welcoming_terms"]].to_csv(out_dict_dir / "welcoming_terms_preprocessed.csv", index=False)
    df_r[["restrictive_terms"]].to_csv(out_dict_dir / "restrictive_terms_preprocessed.csv", index=False)
    df_w.to_csv(out_dict_dir / "welcoming_terms_scored.csv", index=False)
    df_r.to_csv(out_dict_dir / "restrictive_terms_scored.csv", index=False)
    ambiguous.to_csv(out_dict_dir / "ambiguous_terms.csv", index=False)

    # 2) union with accumulated seed files, split multi-word entries
    welcoming_all = U.split_terms_to_words(list(seed_w) + list(df_w["welcoming_terms"]))
    restrictive_all = U.split_terms_to_words(list(seed_r) + list(df_r["restrictive_terms"]))

    # 3) PCA refinement
    proj, _ = pca_projection(model, welcoming_all, restrictive_all)
    welcoming, restrictive, proj = select_by_pc2(proj, mode)
    proj.to_csv(out_dict_dir / "pca_projection.csv", index=False)

    # 4) manual exclusions
    welcoming = [w for w in welcoming if w not in exclusions]
    restrictive = [w for w in restrictive if w not in exclusions]

    sizes = {
        "expansion_welcoming": len(w_final), "expansion_restrictive": len(r_final),
        "ambiguous_dropped": len(ambiguous),
        "before_pca_welcoming": int((proj["label"] == "welcoming").sum()),
        "before_pca_restrictive": int((proj["label"] == "restrictive").sum()),
        "after_pca_welcoming": int((proj["kept"] & (proj["label"] == "welcoming")).sum()),
        "after_pca_restrictive": int((proj["kept"] & (proj["label"] == "restrictive")).sum()),
        "final_welcoming": len(welcoming), "final_restrictive": len(restrictive),
    }
    if verbose:
        print(f"  Missing expansion seeds: {len(miss_w)} welcoming, {len(miss_r)} restrictive")
        for k, v in sizes.items():
            print(f"  {k:<26} {v}")
    if save_figures:
        plot_pca(proj, results_dir / "figures" / "fig02_pca_before_refinement.png")
        plot_pca(proj, results_dir / "figures" / "fig03_pca_after_refinement.png", kept_only=True)
    return welcoming, restrictive, sizes


def load_seed_files():
    seed_w = U.read_term_file(C.WELCOMING_SEED_FILE, "welcoming_terms", sep=C.SEED_FILE_SEP)
    seed_r = U.read_term_file(C.RESTRICTIVE_SEED_FILE, "restrictive_terms", sep=C.SEED_FILE_SEP)
    return seed_w, seed_r


# ======================================================================
# Mode 1: global (as in the thesis, PCA bug fixed)
# ======================================================================
def run_global(use_existing=True):
    """
    use_existing=True  : keep results/new_*_words.csv as they are (reproduces
                         the current thesis results; nothing is overwritten).
    use_existing=False : rebuild the dictionary with the corrected PCA step.
                         The old files are kept as new_*_words_previous.csv.
    """
    results_dir, out_dict_dir = C.mode_dirs("global")
    f_w = results_dir / "new_welcoming_words.csv"
    f_r = results_dir / "new_restrictive_words.csv"

    if use_existing and f_w.exists() and f_r.exists():
        print("[step2/global] Using existing dictionaries (new_welcoming_words.csv, new_restrictive_words.csv).")
        return

    print("[step2/global] Rebuilding dictionary on the whole corpus ...")
    df = pd.read_csv(C.IMMIGRATION_SPEECHES, low_memory=False)
    tokens = U.corpus_tokens(df)
    model = train_word2vec(tokens, results_dir / "word2vec" / "word2vecModel")
    print(f"  Vocabulary: {len(model.wv):,} words")

    seed_w, seed_r = load_seed_files()
    welcoming, restrictive, _ = build_dictionary(
        model, tokens, seed_w, seed_r, out_dict_dir, results_dir, "global", U.load_exclusions()
    )
    if not welcoming or not restrictive:
        raise RuntimeError("Rebuilt dictionary is empty for one side - nothing overwritten. "
                           "Check the PCA plot and thresholds in config.py.")
    U.backup_if_exists(f_w)
    U.backup_if_exists(f_r)
    U.write_term_file(welcoming, f_w, "welcoming_terms")
    U.write_term_file(restrictive, f_r, "restrictive_terms")
    print(f"  Saved {f_w.name} ({len(welcoming)}) and {f_r.name} ({len(restrictive)})")


# ======================================================================
# Mode 2: rolling (no look-ahead in the vocabulary)
# ======================================================================
def run_rolling():
    """
    Builds one dictionary per scored quarter t from speeches in [t-40, t).
    Word2Vec is refitted every W2V_REFIT_EVERY quarters; in between, the most
    recent model is reused. Because that model was trained on an earlier
    window, it never sees speeches from quarter t or later.
    Returns {scored_quarter (str): (welcoming, restrictive)}.
    """
    results_dir, out_dict_dir = C.mode_dirs("rolling")
    df = U.add_quarter_index(pd.read_csv(C.IMMIGRATION_SPEECHES, low_memory=False))
    seed_w, seed_r = load_seed_files()
    exclusions = U.load_exclusions()

    summary_path = results_dir / "dictionary_sizes_by_window.csv"
    previous = {}
    if summary_path.exists():
        previous = {r["scored_quarter"]: r for r in pd.read_csv(summary_path).to_dict("records")}

    dictionaries, rows = {}, []
    model, model_tokens, model_window, model_end = None, None, None, None

    for k, (train_q, scored_q) in enumerate(U.rolling_windows()):
        tag = f"{train_q[0]}_{train_q[-1]}"
        win_dir = out_dict_dir / "windows" / tag
        f_w, f_r = win_dir / "new_welcoming_words.csv", win_dir / "new_restrictive_words.csv"

        if f_w.exists() and f_r.exists():
            welcoming = U.read_term_file(f_w, "welcoming_terms")
            restrictive = U.read_term_file(f_r, "restrictive_terms")
            old = previous.get(str(scored_q), {})
            sizes = {key: val for key, val in old.items()
                     if k not in ("scored_quarter", "train_window", "word2vec_window")
                     and not key.startswith("jaccard")}
            sizes.update(final_welcoming=len(welcoming), final_restrictive=len(restrictive))
            rows.append({"scored_quarter": str(scored_q), "train_window": tag,
                         "word2vec_window": old.get("word2vec_window"), **sizes})
            dictionaries[str(scored_q)] = (welcoming, restrictive)
            print(f"[step2/rolling] {scored_q}: loaded dictionary of window {tag}")
            continue
        else:
            if model is None or k % C.W2V_REFIT_EVERY == 0:
                subset = df[df.index.isin(list(train_q))]
                model_tokens = U.corpus_tokens(subset)
                model = train_word2vec(model_tokens, results_dir / "word2vec" / f"word2vecModel_{tag}")
                model_window, model_end = tag, train_q[-1]
            # look-ahead check: the Word2Vec model must end before the scored quarter
            assert model_end < scored_q, f"look-ahead: model {model_window} used for {scored_q}"
            print(f"[step2/rolling] {scored_q}: Word2Vec window {model_window}, vocab {len(model.wv):,}")
            welcoming, restrictive, sizes = build_dictionary(
                model, model_tokens, seed_w, seed_r, win_dir, results_dir, "rolling",
                exclusions, save_figures=False, verbose=False,
            )
            U.write_term_file(welcoming, f_w, "welcoming_terms")
            U.write_term_file(restrictive, f_r, "restrictive_terms")

        dictionaries[str(scored_q)] = (welcoming, restrictive)
        rows.append({"scored_quarter": str(scored_q), "train_window": tag,
                     "word2vec_window": model_window, **sizes})

    summary = pd.DataFrame(rows)
    _add_overlap_with_global(summary, dictionaries)
    summary.to_csv(results_dir / "dictionary_sizes_by_window.csv", index=False)
    print(f"[step2/rolling] Saved dictionary_sizes_by_window.csv ({len(summary)} windows)")
    return dictionaries


def _add_overlap_with_global(summary, dictionaries):
    """Jaccard overlap of each window dictionary with the global one (if present)."""
    g_dir, _ = C.mode_dirs("global")
    f_w, f_r = g_dir / "new_welcoming_words.csv", g_dir / "new_restrictive_words.csv"
    if not (f_w.exists() and f_r.exists()):
        return
    gw = set(U.read_term_file(f_w, "welcoming_terms"))
    gr = set(U.read_term_file(f_r, "restrictive_terms"))

    def jac(a, b):
        return len(a & b) / max(1, len(a | b))

    summary["jaccard_welcoming_vs_global"] = [jac(set(dictionaries[q][0]), gw) for q in summary["scored_quarter"]]
    summary["jaccard_restrictive_vs_global"] = [jac(set(dictionaries[q][1]), gr) for q in summary["scored_quarter"]]


def load_rolling_dictionaries():
    """Read the per-window dictionaries written by run_rolling()."""
    _, out_dict_dir = C.mode_dirs("rolling")
    out = {}
    for train_q, scored_q in U.rolling_windows():
        win_dir = out_dict_dir / "windows" / f"{train_q[0]}_{train_q[-1]}"
        out[str(scored_q)] = (
            U.read_term_file(win_dir / "new_welcoming_words.csv", "welcoming_terms"),
            U.read_term_file(win_dir / "new_restrictive_words.csv", "restrictive_terms"),
        )
    return out
