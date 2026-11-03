"""Shared helpers: tokenisation, windows, term vectors, small IO helpers."""
import shutil
import pandas as pd
import numpy as np

from . import config as C


# ----------------------------------------------------------------------
# Tokens
# ----------------------------------------------------------------------
def text_to_tokens(text, min_len=C.MIN_TOKEN_LEN_TRAIN):
    """Whitespace split; keep alphabetic tokens of length >= min_len."""
    if pd.isna(text) or str(text).strip() == "":
        return []
    return [t for t in str(text).lower().split() if t.isalpha() and len(t) >= min_len]


def row_tokens(row):
    """Lemmatised text, falling back to the preprocessed text if (almost) empty."""
    tokens = text_to_tokens(row.get(C.TEXT_COL, ""))
    if len(tokens) < 3:
        tokens = text_to_tokens(row.get(C.FALLBACK_COL, ""))
    return tokens


def corpus_tokens(df):
    """Token lists of all speeches with at least MIN_TOKENS_PER_SPEECH tokens."""
    out = []
    for _, row in df.iterrows():
        toks = row_tokens(row)
        if len(toks) >= C.MIN_TOKENS_PER_SPEECH:
            out.append(toks)
    return out


# ----------------------------------------------------------------------
# Rolling windows (identical to the old Doc2Vec notebook)
# ----------------------------------------------------------------------
def rolling_windows():
    """
    Returns a list of (train_quarters, scored_quarter).
    train_quarters: 40 consecutive quarters [t-40, t); scored_quarter: t.
    First window 1980Q1-1989Q4 -> 1990Q1, last 2015Q2-2025Q1 -> 2025Q2.
    """
    periods = pd.period_range(C.FIRST_QUARTER, periods=C.N_QUARTERS, freq="Q")
    windows = []
    for i in range(C.FIRST_WINDOW_START, C.LAST_WINDOW_START):
        windows.append((periods[i:i + C.WINDOW_QUARTERS], periods[i + C.WINDOW_QUARTERS]))
    return windows


def add_quarter_index(df):
    df = df.copy()
    df.index = pd.PeriodIndex(pd.to_datetime(df[C.DATE_COL]), freq="Q")
    return df


# ----------------------------------------------------------------------
# Dictionary helpers
# ----------------------------------------------------------------------
def find_terms_vectors(terms_list, model, vocabulary, max_words=C.MAX_WORDS_PER_TERM):
    """
    Vectors of dictionary terms that exist in the model vocabulary.
    Multi-word terms (<= max_words) are summed; a term is used only if all
    of its words are in the vocabulary. Same logic as the original notebook.
    """
    vocabulary = set(vocabulary)
    terms, vectors = [], []
    for term in terms_list:
        words = str(term).split()
        if 1 <= len(words) <= max_words and all(w in vocabulary for w in words):
            vec = model.wv[words[0]].copy()
            for w in words[1:]:
                vec = vec + model.wv[w]
            terms.append(term)
            vectors.append(vec)
    return terms, vectors


def read_term_file(path, column=None, sep=","):
    """Read a one-column term file; tolerant to header / separator differences."""
    df = pd.read_csv(path, sep=sep, low_memory=False)
    if column is None or column not in df.columns:
        column = df.columns[0]
    return [str(t).strip() for t in df[column].dropna().tolist() if str(t).strip()]


def split_terms_to_words(terms):
    """Multi-word entries -> single words, deduplicated and sorted (deterministic)."""
    return sorted({w for term in terms for w in str(term).split()})


def load_exclusions():
    if not C.MANUAL_EXCLUSIONS_FILE.exists():
        return set()
    return set(read_term_file(C.MANUAL_EXCLUSIONS_FILE, "term"))


def write_term_file(terms, path, column):
    """One column with header (the Doc2Vec step reads it by column name)."""
    pd.DataFrame({column: list(terms)}).to_csv(path, index=False)


def backup_if_exists(path):
    """Keep the previous version of a result file as *_previous.csv (once)."""
    if path.exists():
        backup = path.with_name(path.stem + "_previous" + path.suffix)
        if not backup.exists():
            shutil.copy(path, backup)
            print(f"  Backup of previous file: {backup.name}")


def cosine(a, b):
    a, b = np.asarray(a), np.asarray(b)
    return float(a @ b / (np.linalg.norm(a) * np.linalg.norm(b) + 1e-12))
