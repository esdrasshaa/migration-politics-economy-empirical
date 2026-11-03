# Immigration sentiment pipeline (Bundestag, Doc2Vec)

Reorganised version of the notebooks `text_prepared.ipynb`, `word2vec_dictionary.ipynb`,
`migration_sentiment_scoringDoc2Vec.ipynb` and `sentiment_plot.ipynb`.
All output file names are the same as before.

## Folder layout

Put `pipeline/`, `run_all.py` and `manual_exclusions.csv` into the folder where your
notebooks are. The pipeline uses the same relative paths as the notebooks:

```
<parent>/
  data/all_bundestag_speeches_preprocessed.csv
  dictionaries/welcoming_terms_preprocessed03.csv      (seed lists, sep=";")
  dictionaries/restrictive_terms_preprocessed03.csv
  <project>/                       <- your notebook folder
    run_all.py
    manual_exclusions.csv          <- words removed in manual cleaning
    pipeline/
    results/                       <- global mode (as before)
    output_dictionaries/
    results_rolling/               <- rolling mode (new, same file names)
    output_dictionaries_rolling/
```

## Steps

| Step | Module | Old notebook | Main output (unchanged name) |
|---|---|---|---|
| 1 Keyword filter | `step1_prepare_corpus.py` | text_prepared (cells 0-11) | `results/immigration_speeches_short.csv` |
| 2 Dictionary (Word2Vec, TF-IDF, PCA, exclusions) | `step2_dictionary.py` | word2vec_dictionary | `results/new_welcoming_words.csv`, `results/new_restrictive_words.csv`, `output_dictionaries/*` |
| 3 Rolling Doc2Vec scoring | `step3_doc2vec_scoring.py` | migration_sentiment_scoringDoc2Vec (cell 7) | `results/doc2vec_10Years_RollingWindow.csv` |
| 4 Monthly series + interpolation | `step4_monthly_series.py` | same notebook (cells 9, 33) | `results/immigration_sentiment_monthly.csv`, `..._doc2vec.csv`, `..._2006_2025_doc2vec.csv` |
| 5 Validation | `step5_validation.py` | same notebook (cell 41), sentiment_plot | `results/validation_summary.csv` (new) |
| 6 Figures | `step6_figures.py` | plotting cells | `results/figures/*.png` (new) |
| Compare | `compare_modes.py` | - | `results_rolling/comparison_global_vs_rolling.csv` |

The close-reading notebooks (`oratory_dictionary_building.ipynb`, second half of
`text_prepared.ipynb` -> `results/filter_speeches.csv`) are manual work and stay notebooks.

## How to run

```bash
python run_all.py --mode global                       # current dictionaries, nothing rebuilt
python run_all.py --mode global --rebuild-dictionary  # corrected PCA selection
python run_all.py --mode rolling                      # look-ahead-free dictionaries
python run_all.py --compare                           # correlations global vs rolling
```

Every file that would be overwritten is first copied once to `*_previous.csv`.
Doc2Vec models in `results/doc2vec_models2/...` are reused by both modes.

## The look-ahead-free ("rolling") dictionary

For each scored quarter t (1990Q1-2025Q2), Word2Vec, the TF-IDF vocabulary and the
PCA refinement only use speeches from [t-40, t), exactly like Doc2Vec. Word2Vec is
re-trained every `W2V_REFIT_EVERY` quarters (default 4 = yearly); in between the most
recent model is reused. That model was trained on an earlier window, so it never sees
quarter t or later (an assertion checks this). Set it to 1 to retrain every quarter.

PC2 is re-estimated in every window, so the absolute thresholds (0.5 / -0.48) are not
comparable across windows. The rolling mode orients PC2 (welcoming above) and keeps the
same shares of words that the thresholds kept in the thesis run (241/590, 288/718).

Remaining look-ahead: the seed words themselves come from close reading of speeches
up to 2025. This cannot be removed without redoing the close reading per period.

## Bug fixed: PCA selection (word2vec_dictionary.ipynb, cells 24-25)

```python
welcome_2d = welcome_2d[welcome_2d[:, 1] >= 0.5]            # filtered + shuffled
welcome_idx = np.where(welcome_2d[:, 1] > 0.5)[0]            # 0, 1, 2, ... n-1
selected_welcoming_words = [welcoming_words[i] for i in welcome_idx]
```

`welcome_idx` indexes the filtered, shuffled array, but is applied to the unfiltered,
unshuffled word list. The result is simply the first 241 (288) words of the list, not
the words above (below) the threshold. Because the list came from a Python `set`, its
order - and therefore the selection - also changed between sessions. Figure 3 in the
thesis shows the correct points; the exported word lists did not match it.

## Other changes

* All word lists are sorted -> deterministic results (for fully identical Word2Vec
  runs set `W2V_WORKERS = 1` and `PYTHONHASHSEED=0`).
* One Word2Vec model path (`results/word2vec/word2vecModel`); the old notebook saved
  to one path and loaded from another.
* `ambiguous_terms.csv` now stores both similarities instead of labelling every term
  "restrictive".
* `new_*_words.csv` are written with a header (`welcoming_terms`, `restrictive_terms`).
* Manual cleaning is applied from `manual_exclusions.csv` (add every word you removed).
* New diagnostics: `empty_months_by_group.csv` (interpolated months per series),
  `speech_counts_monthly.csv`, `dictionary_coverage_by_quarter.csv`,
  `dictionary_sizes_by_window.csv`, `validation_summary.csv`.
* Removed the unused first keyword filter (>= 3 groups, incl. "fachkraefte", "pflege").
