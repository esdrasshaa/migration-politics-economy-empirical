"""
Step 4 - Monthly sentiment series and interpolation (Section 3.3).

Output (same names as before, in results/ or results_rolling/):
    immigration_sentiment_monthly.csv
    immigration_sentiment_monthly_doc2vec.csv
    immigration_sentiment_monthly_2006_2025_doc2vec.csv   (input of the BVAR)
New:
    empty_months_by_group.csv    (how many months were interpolated per series)
"""
import pandas as pd

from . import config as C
from . import utils as U

SERIES = [
    "sentiment_gesamt", "sentiment_government", "expansive_government",
    "restrictive_government", "sentiment_opposition", "expansive_opposition",
    "restrictive_opposition",
]


def speech_scores(mode):
    results_dir, _ = C.mode_dirs(mode)
    data = pd.read_csv(results_dir / "doc2vec_10Years_RollingWindow.csv", low_memory=False)
    data[C.DATE_COL] = pd.to_datetime(data[C.DATE_COL])
    for col in ["similarity_expansive", "similarity_restrictive"]:
        data[col] = pd.to_numeric(data[col], errors="coerce")
    data["sentiment"] = data["similarity_expansive"] - data["similarity_restrictive"]
    data = data.dropna(subset=["sentiment"])
    data["YearMonth"] = data[C.DATE_COL].dt.to_period("M")
    return data


def monthly_means(data):
    gov = data[data[C.GOV_COL] == 1].groupby("YearMonth")
    opp = data[data[C.GOV_COL] == 0].groupby("YearMonth")
    monthly = pd.concat(
        [
            data.groupby("YearMonth")["sentiment"].mean().rename("sentiment_gesamt"),
            gov["sentiment"].mean().rename("sentiment_government"),
            gov["similarity_expansive"].mean().rename("expansive_government"),
            gov["similarity_restrictive"].mean().rename("restrictive_government"),
            opp["sentiment"].mean().rename("sentiment_opposition"),
            opp["similarity_expansive"].mean().rename("expansive_opposition"),
            opp["similarity_restrictive"].mean().rename("restrictive_opposition"),
        ],
        axis=1,
    )
    counts = pd.concat(
        [
            data.groupby("YearMonth").size().rename("n_speeches_total"),
            data[data[C.GOV_COL] == 1].groupby("YearMonth").size().rename("n_speeches_government"),
            data[data[C.GOV_COL] == 0].groupby("YearMonth").size().rename("n_speeches_opposition"),
        ],
        axis=1,
    )
    return monthly, counts


def interpolate(monthly):
    """Linear interpolation inside the sample, nearest value at the edges (as before)."""
    full_index = pd.period_range(monthly.index.min(), monthly.index.max(), freq="M")
    complete = monthly.reindex(full_index)
    empty = complete[SERIES].isna().sum()
    for col in SERIES:
        complete[col] = complete[col].interpolate(method="linear", limit_area="inside")
    complete = complete.ffill().bfill()
    complete.index.name = "YearMonth"
    out = complete.reset_index()
    out.insert(0, "Date", out["YearMonth"].dt.to_timestamp())
    return out, empty


def run(mode="global"):
    results_dir, _ = C.mode_dirs(mode)
    data = speech_scores(mode)
    if data.empty:
        raise RuntimeError("No scored speeches - check the dictionary coverage file of step 3.")
    monthly, counts = monthly_means(data)
    final, empty = interpolate(monthly)

    print(f"[step4/{mode}] Months with any scored speech: {len(monthly)}; complete range: {len(final)}")
    report = pd.DataFrame({"series": empty.index, "interpolated_months": empty.values,
                           "total_months": len(final)})
    report["share"] = (report["interpolated_months"] / report["total_months"]).round(3)
    print(report.to_string(index=False))
    report.to_csv(results_dir / "empty_months_by_group.csv", index=False)

    final_cols = ["Date"] + SERIES
    for name in ["immigration_sentiment_monthly.csv", "immigration_sentiment_monthly_doc2vec.csv",
                 "immigration_sentiment_monthly_2006_2025_doc2vec.csv"]:
        U.backup_if_exists(results_dir / name)
    final[final_cols].to_csv(results_dir / "immigration_sentiment_monthly.csv", index=False)
    final[final_cols].to_csv(results_dir / "immigration_sentiment_monthly_doc2vec.csv", index=False)

    var = final[(final["Date"] >= C.VAR_SAMPLE_START) & (final["Date"] <= C.VAR_SAMPLE_END)]
    var[final_cols].to_csv(results_dir / "immigration_sentiment_monthly_2006_2025_doc2vec.csv", index=False)

    counts.index = counts.index.to_timestamp()
    counts.to_csv(results_dir / "speech_counts_monthly.csv")
    return final
