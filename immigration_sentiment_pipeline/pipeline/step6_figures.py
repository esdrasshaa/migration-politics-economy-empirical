"""
Step 6 - Thesis figures (saved instead of only shown).

<results>/figures/fig04_party_average.png
<results>/figures/fig06_overall.png, fig07_government.png, fig08_opposition.png
<results>/figures/fig09_gov_vs_opp_rolling.png
<results>/figures/fig34_party_small_multiples.png
"""
import math

import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from . import config as C
from .step4_monthly_series import speech_scores

PARTIES = ["Cabinet", "FDP", "CDU/CSU", "SPD", "GRÜNE", "PDS", "LINKE", "AfD", "BSW"]


def _save(fig, path):
    fig.tight_layout()
    fig.savefig(path, dpi=200)
    plt.close(fig)


def run(mode="global"):
    results_dir, _ = C.mode_dirs(mode)
    fig_dir = results_dir / "figures"
    data = speech_scores(mode)
    data = data[data[C.PARTY_COL].isin(PARTIES)]
    monthly = pd.read_csv(results_dir / "immigration_sentiment_monthly.csv", parse_dates=["Date"])

    # Figure 4: party averages
    mean = data.groupby(C.PARTY_COL)["sentiment"].mean().sort_values()
    fig, ax = plt.subplots(figsize=(10, 6))
    mean.plot(kind="barh", ax=ax, alpha=0.85,
              color=["tab:red" if v < 0 else "tab:green" for v in mean.values])
    ax.axvline(0, color="black", linestyle="--", linewidth=1.2)
    ax.set_title("Average Immigration Sentiment by Political Party (Doc2Vec)", fontsize=16)
    ax.set_xlabel("Mean Sentiment (Welcoming vs. Restrictive)", fontsize=14)
    ax.set_ylabel("Party", fontsize=14)
    _save(fig, fig_dir / "fig04_party_average.png")
    mean.to_csv(results_dir / "party_average_sentiment.csv")

    # Figures 6-8: monthly series
    for col, label, name in [("sentiment_gesamt", "Overall Parliament", "fig06_overall.png"),
                             ("sentiment_government", "Government", "fig07_government.png"),
                             ("sentiment_opposition", "Opposition", "fig08_opposition.png")]:
        fig, ax = plt.subplots(figsize=(12, 5))
        ax.plot(monthly["Date"], monthly[col], linewidth=1.5, color="#1f77b4", label=label)
        ax.axhline(0, color="gray", linestyle="--", linewidth=1.2)
        ax.set_title("Monthly Parliamentary Immigration Sentiment in Germany", fontsize=16)
        ax.set_xlabel("Time Period", fontsize=14)
        ax.set_ylabel("Sentiment Score (Welcoming vs. Restrictive)", fontsize=12)
        ax.legend(loc="upper left")
        ax.grid(True, linestyle=":", alpha=0.5)
        _save(fig, fig_dir / name)

    # Figure 9: 12-month rolling means
    m = monthly.set_index("Date")
    fig, ax = plt.subplots(figsize=(14, 6))
    m["sentiment_government"].rolling(12, min_periods=3).mean().plot(ax=ax, color="firebrick", lw=2.5, label="Government")
    m["sentiment_opposition"].rolling(12, min_periods=3).mean().plot(ax=ax, color="steelblue", lw=2.5, label="Opposition")
    m["sentiment_gesamt"].rolling(12, min_periods=3).mean().plot(ax=ax, color="gray", lw=1.8, ls=":", label="Overall")
    ax.axhline(0, color="black", linewidth=0.8, linestyle="--")
    ax.set_title("Immigration Sentiment Net Score: Government vs. Opposition (12-month rolling average)", fontsize=13)
    ax.set_xlabel("Year", fontsize=14)
    ax.set_ylabel("Net Sentiment Score", fontsize=14)
    ax.legend(loc="upper left")
    _save(fig, fig_dir / "fig09_gov_vs_opp_rolling.png")

    # Figure 34: party small multiples
    parties = sorted(data[C.PARTY_COL].dropna().unique())
    ncols = 2
    nrows = math.ceil(len(parties) / ncols)
    fig, axes = plt.subplots(nrows, ncols, figsize=(15, 3.4 * nrows), sharex=True, sharey=True, squeeze=False)
    palette = dict(zip(parties, plt.cm.tab10(np.linspace(0, 1, 10))[: len(parties)]))
    for ax, party in zip(axes.flatten(), parties):
        s = data[data[C.PARTY_COL] == party].set_index(C.DATE_COL)["sentiment"].resample("MS").mean()
        trend = s.rolling(12, min_periods=4, center=True).mean()
        ax.plot(s.index, s, color=palette[party], alpha=0.25, lw=0.8)
        ax.plot(trend.index, trend, color=palette[party], lw=2.5)
        ax.fill_between(trend.index, 0, trend, where=trend >= 0, color="tab:green", alpha=0.15, interpolate=True)
        ax.fill_between(trend.index, 0, trend, where=trend < 0, color="tab:red", alpha=0.15, interpolate=True)
        ax.axhline(0, color="black", lw=0.8, ls="--")
        ax.set_title(party, fontsize=14, loc="left", fontweight="bold")
        ax.xaxis.set_major_locator(mdates.YearLocator(5))
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
        ax.grid(alpha=0.25)
    for ax in axes.flatten()[len(parties):]:
        ax.set_visible(False)
    fig.supylabel("Sentiment (welcoming > 0 > restrictive)", fontsize=14)
    _save(fig, fig_dir / "fig34_party_small_multiples.png")
    print(f"[step6/{mode}] Figures saved to {fig_dir}")
