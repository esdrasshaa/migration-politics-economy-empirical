"""
Compare monthly sentiment series between
  - the previous global run   (results/immigration_sentiment_monthly_previous.csv, if present)
  - the current global run    (results/immigration_sentiment_monthly.csv)
  - the rolling run           (results_rolling/immigration_sentiment_monthly.csv)

Output: results_rolling/comparison_global_vs_rolling.csv and a figure.
"""
import matplotlib.pyplot as plt
import pandas as pd

from . import config as C

COLS = ["sentiment_gesamt", "sentiment_government", "sentiment_opposition"]


def _load(path):
    return pd.read_csv(path, parse_dates=["Date"]).set_index("Date") if path.exists() else None


def run():
    g_dir, _ = C.mode_dirs("global")
    r_dir, _ = C.mode_dirs("rolling")
    series = {
        "global_previous": _load(g_dir / "immigration_sentiment_monthly_previous.csv"),
        "global": _load(g_dir / "immigration_sentiment_monthly.csv"),
        "rolling": _load(r_dir / "immigration_sentiment_monthly.csv"),
    }
    series = {k: v for k, v in series.items() if v is not None}
    names = list(series)
    rows = []
    for i, a in enumerate(names):
        for b in names[i + 1:]:
            for col in COLS:
                both = pd.concat([series[a][col], series[b][col]], axis=1, join="inner").dropna()
                for label, sub in [("1990-2025", both), ("2007-2025", both.loc["2007-05-01":])]:
                    rows.append({"series": col, "pair": f"{a} vs {b}", "sample": label, "n": len(sub),
                                 "corr": round(sub.iloc[:, 0].corr(sub.iloc[:, 1]), 3),
                                 "corr_12m_mean": round(sub.rolling(12, min_periods=6).mean().dropna().corr().iloc[0, 1], 3)})
    out = pd.DataFrame(rows)
    out.to_csv(r_dir / "comparison_global_vs_rolling.csv", index=False)
    print(out.to_string(index=False))

    fig, axes = plt.subplots(3, 1, figsize=(14, 11), sharex=True)
    for ax, col in zip(axes, COLS):
        for name, df in series.items():
            df[col].rolling(12, min_periods=3).mean().plot(ax=ax, lw=2, label=name)
        ax.axhline(0, color="black", lw=0.8, ls="--")
        ax.set_title(f"{col} (12-month rolling mean)")
        ax.legend(loc="upper left")
    fig.tight_layout()
    fig.savefig(r_dir / "figures" / "comparison_global_vs_rolling.png", dpi=200)
    plt.close(fig)
    return out
