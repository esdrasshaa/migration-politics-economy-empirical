"""
Run the whole immigration-sentiment pipeline.

Examples (from the folder that contains this file):

  # 1) Reproduce the current thesis results (existing dictionaries, nothing rebuilt)
  python run_all.py --mode global

  # 2) Rebuild the global dictionary with the corrected PCA selection
  python run_all.py --mode global --rebuild-dictionary

  # 3) Look-ahead-free version: rolling Word2Vec dictionaries + rolling Doc2Vec
  python run_all.py --mode rolling

  # 4) Compare global and rolling series
  python run_all.py --compare

In a notebook:
  from pipeline import run_all
  run_all.main(["--mode", "rolling"])
"""
import argparse

from pipeline import (step1_prepare_corpus, step2_dictionary, step3_doc2vec_scoring,
                      step4_monthly_series, step5_validation, step6_figures, compare_modes)


def main(argv=None):
    ap = argparse.ArgumentParser()
    ap.add_argument("--mode", choices=["global", "rolling"], default="global")
    ap.add_argument("--rebuild-dictionary", action="store_true",
                    help="global mode: rebuild new_*_words.csv (old files kept as *_previous.csv)")
    ap.add_argument("--rebuild-corpus", action="store_true", help="redo the keyword filter")
    ap.add_argument("--compare", action="store_true", help="only compare global vs rolling series")
    args = ap.parse_args(argv)

    if args.compare:
        compare_modes.run()
        return

    step1_prepare_corpus.run(force=args.rebuild_corpus)

    if args.mode == "global":
        step2_dictionary.run_global(use_existing=not args.rebuild_dictionary)
        step3_doc2vec_scoring.run("global")
    else:
        dictionaries = step2_dictionary.run_rolling()
        step3_doc2vec_scoring.run("rolling", rolling_dictionaries=dictionaries)

    step4_monthly_series.run(args.mode)
    step5_validation.run(args.mode)
    step6_figures.run(args.mode)


if __name__ == "__main__":
    main()
