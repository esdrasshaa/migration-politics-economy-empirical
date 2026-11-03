"""
Step 5 - Validation of the sentiment measure (Sections 3.4 and 4.5).

a) Directional agreement with the hand-labelled speeches in
   results/filter_speeches.csv (built in the close-reading notebook).
b) Correlation of the monthly Doc2Vec series with the GPT-based series.

Output: <results>/validation_summary.csv
"""
import pandas as pd

from . import config as C
from .step4_monthly_series import speech_scores


def directional_agreement(mode):
    path = C.PROJECT_DIR / "results" / "filter_speeches.csv"
    if not path.exists():
        print("  filter_speeches.csv not found - skipped.")
        return []
    labels = pd.read_csv(path)
    labels["immigration_position"] = labels["immigration_position"].astype(str).str.strip().str.lower()
    data = speech_scores(mode)

    rows = []
    for label, expected in [("true", 1), ("false", -1)]:
        texts = labels.loc[labels["immigration_position"] == label, C.TEXT_COL]
        matched = data[data[C.TEXT_COL].isin(texts)]
        n = len(matched)
        correct = int((matched["sentiment"] * expected > 0).sum())
        rows.append({"check": f"hand-labelled {'welcoming' if expected == 1 else 'restrictive'}",
                     "n": n, "value": round(100 * correct / n, 2) if n else None, "unit": "% correct sign"})
    n_all = sum(r["n"] for r in rows)
    if n_all:
        correct_all = sum(r["value"] * r["n"] / 100 for r in rows if r["value"] is not None)
        rows.append({"check": "hand-labelled overall", "n": n_all,
                     "value": round(100 * correct_all / n_all, 2), "unit": "% correct sign"})
    return rows


def gpt_correlations(mode):
    results_dir, _ = C.mode_dirs(mode)
    d2v = pd.read_csv(results_dir / "immigration_sentiment_monthly.csv", parse_dates=["Date"]).set_index("Date")
    rows = []
    pairs = [("sentiment_gesamt", "gtp_overall_sentiment"),
             ("sentiment_government", "gpt_sentiment_government"),
             ("sentiment_opposition", "gpt_sentiment_opposition")]
    for name, path in [("GPT-4.1-nano", C.GPT41_MONTHLY), ("GPT-5-nano", C.GPT5_MONTHLY)]:
        if not path.exists():
            continue
        gpt = pd.read_csv(path)
        gpt["month"] = pd.to_datetime(gpt["month"])
        gpt = gpt.set_index("month")
        for d_col, g_col in pairs:
            if g_col not in gpt.columns:
                continue
            both = pd.concat([d2v[d_col], gpt[g_col]], axis=1, join="inner").dropna()
            for label, sub in [("1990-2025", both), ("2007-2025", both.loc["2007-05-01":])]:
                rows.append({"check": f"corr Doc2Vec {d_col} vs {name} ({label})", "n": len(sub),
                             "value": round(sub.iloc[:, 0].corr(sub.iloc[:, 1]), 3), "unit": "Pearson r"})
                smooth = sub.rolling(12, min_periods=6).mean().dropna()
                rows.append({"check": f"corr 12m-mean {d_col} vs {name} ({label})", "n": len(smooth),
                             "value": round(smooth.iloc[:, 0].corr(smooth.iloc[:, 1]), 3), "unit": "Pearson r"})
    return rows


def run(mode="global"):
    results_dir, _ = C.mode_dirs(mode)
    rows = directional_agreement(mode) + gpt_correlations(mode)
    summary = pd.DataFrame(rows, columns=["check", "n", "value", "unit"])
    summary.to_csv(results_dir / "validation_summary.csv", index=False)
    print(f"[step5/{mode}] Validation")
    print(summary.to_string(index=False))
    return summary
