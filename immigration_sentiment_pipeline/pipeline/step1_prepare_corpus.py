"""
Step 1 - Keyword filter (Section 3.2).

Input : ../data/all_bundestag_speeches_preprocessed.csv
Output: results/immigration_speeches_short.csv          (same name as before)
        results/figures/fig01_speeches_per_month.png
"""
import re
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

from . import config as C

_PATTERNS = [re.compile(rf"\b{k}") for k in C.MIGRATION_KEYWORDS]


def count_keyword_groups(text):
    if pd.isna(text):
        return 0
    text = str(text).lower()
    return sum(1 for p in _PATTERNS if p.search(text))


def _monthly_counts(df):
    s = pd.to_datetime(df[C.DATE_COL]).dt.to_period("M").value_counts().sort_index()
    return s.index.to_timestamp(), s.values


def plot_counts(data, df_imm, path):
    fig, axes = plt.subplots(2, 1, figsize=(12, 10))
    for ax, d, title in [
        (axes[0], data, "Number of Speeches per Month original data"),
        (axes[1], df_imm, "Number of Speeches per Month"),
    ]:
        x, y = _monthly_counts(d)
        ax.plot(x, y, color="#1f77b4", linewidth=1.2)
        ax.xaxis.set_major_locator(mdates.YearLocator(5))
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
        ax.set_title(title, fontsize=16)
        ax.set_xlabel("Year", fontsize=14)
        ax.set_ylabel("Number of Speeches", fontsize=14)
        ax.grid(True, linestyle=":", alpha=0.5)
    fig.tight_layout()
    fig.savefig(path, dpi=200)
    plt.close(fig)


def run(force=False):
    out = C.IMMIGRATION_SPEECHES
    out.parent.mkdir(parents=True, exist_ok=True)
    if out.exists() and not force:
        print(f"[step1] {out.name} exists - skipped (use force=True to rebuild).")
        return pd.read_csv(out, low_memory=False)

    print("[step1] Loading full corpus ...")
    data = pd.read_csv(C.DATA_PATH, sep=C.DATA_SEP, low_memory=False)
    data["migration_keyword_count"] = data[C.TEXT_COL].apply(count_keyword_groups)
    data["is_immigration"] = data["migration_keyword_count"] >= C.MIN_KEYWORD_GROUPS
    df_imm = data[data["is_immigration"]].copy()

    print(f"  Total speeches:       {len(data):,}")
    print(f"  Immigration speeches: {len(df_imm):,} ({len(df_imm) / len(data):.1%})")

    df_imm.to_csv(out)  # index kept, as in the original notebook
    (C.PROJECT_DIR / "results" / "figures").mkdir(parents=True, exist_ok=True)
    plot_counts(data, df_imm, C.PROJECT_DIR / "results" / "figures" / "fig01_speeches_per_month.png")
    return df_imm


if __name__ == "__main__":
    run()
