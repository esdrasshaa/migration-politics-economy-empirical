"""
Central configuration for the immigration-sentiment pipeline.

All paths are relative to PROJECT_DIR, i.e. the folder that contains this
`pipeline/` package (the same folder in which the original notebooks lived).
File names are identical to the ones used in the original notebooks.
"""
from pathlib import Path

# ----------------------------------------------------------------------
# Folders
# ----------------------------------------------------------------------
PROJECT_DIR = Path(__file__).resolve().parent.parent

DATA_PATH = PROJECT_DIR / ".." / "data" / "all_bundestag_speeches_preprocessed.csv"
DATA_SEP = ";"

# Seed lists from close reading (accumulated over the expansion rounds)
SEED_DICT_DIR = PROJECT_DIR / ".." / "dictionaries"
WELCOMING_SEED_FILE = SEED_DICT_DIR / "welcoming_terms_preprocessed03.csv"
RESTRICTIVE_SEED_FILE = SEED_DICT_DIR / "restrictive_terms_preprocessed03.csv"
SEED_FILE_SEP = ";"

# Terms removed during manual cleaning (one column "term"); applied in all modes
MANUAL_EXCLUSIONS_FILE = PROJECT_DIR / "manual_exclusions.csv"

# GPT-based monthly series (only used for the validation step)
GPT41_MONTHLY = PROJECT_DIR / "gpt-4.1-nano" / "DATA" / "GptSentimentMonthlyVargptNano.csv"
GPT5_MONTHLY = PROJECT_DIR / "Gpt-5-nano" / "DATA" / "GptSentimentMonthlyVarGpt5Nano.csv"


def mode_dirs(mode: str):
    """
    Output folders per mode. File names inside are the same in both modes,
    so results can be compared one-to-one.

    global  -> results/            output_dictionaries/           (as before)
    rolling -> results_rolling/    output_dictionaries_rolling/   (look-ahead free)
    """
    if mode == "global":
        results = PROJECT_DIR / "results"
        out_dict = PROJECT_DIR / "output_dictionaries"
    elif mode == "rolling":
        results = PROJECT_DIR / "results_rolling"
        out_dict = PROJECT_DIR / "output_dictionaries_rolling"
    else:
        raise ValueError("mode must be 'global' or 'rolling'")
    results.mkdir(parents=True, exist_ok=True)
    out_dict.mkdir(parents=True, exist_ok=True)
    (results / "figures").mkdir(exist_ok=True)
    return results, out_dict


# The keyword-filtered corpus is shared by both modes
IMMIGRATION_SPEECHES = PROJECT_DIR / "results" / "immigration_speeches_short.csv"

# Doc2Vec models do not depend on the dictionary -> shared by both modes
DOC2VEC_PARAMS = dict(window=5, epochs=50, vector_size=100)
DOC2VEC_MODELS_DIR = (
    PROJECT_DIR / "results" / "doc2vec_models2"
    / f"{DOC2VEC_PARAMS['window']}Window_{DOC2VEC_PARAMS['epochs']}Epochs_{DOC2VEC_PARAMS['vector_size']}Size"
)

# ----------------------------------------------------------------------
# Column names
# ----------------------------------------------------------------------
TEXT_COL = "text_preprocessed_lemmatized"
FALLBACK_COL = "text_preprocessed"
DATE_COL = "date"
PARTY_COL = "Party"
GOV_COL = "governing_Party"

# ----------------------------------------------------------------------
# Step 1: keyword filter (13 stems, Kostikova et al. 2024)
# ----------------------------------------------------------------------
MIGRATION_KEYWORDS = [
    "migration", "zuwander", "einwander", "asyl", "fluechtling", "gefluechtet",
    "schutzsuchend", "auslaend", "vertrieben", "migrant", "aussiedler",
    "ansiedler", "buergerkriegs",
]
MIN_KEYWORD_GROUPS = 1

# ----------------------------------------------------------------------
# Step 2: Word2Vec dictionary expansion
# ----------------------------------------------------------------------
W2V_PARAMS = dict(vector_size=100, window=7, min_count=10, epochs=50, sg=1, seed=42)
W2V_WORKERS = 4          # set to 1 (and PYTHONHASHSEED=0) for bit-identical runs
TOP_N = 20
MIN_SIM = 0.72
TFIDF_PARAMS = dict(max_features=80_000, min_df=2)
MIN_TOKEN_LEN_TRAIN = 3  # tokens used for training: alphabetic, len > 2
MIN_TERM_LEN = 4         # dictionary candidates: alphabetic, len > 3
MIN_TOKENS_PER_SPEECH = 5

# Topic-focused seed lists of the LAST expansion round (cell 3 of the old notebook).
# Their neighbours are added to the accumulated seed files above.
WELCOMING_EXPANSION_SEEDS = [
    "fachkraefteeinwanderungsgesetz", "fachkraeftezuwanderungsgesetz",
    "fachkraefteeinwanderung", "fachkraeftesicherung", "fachkraeftemangel",
    "fachkraeftemangels", "fachkraeftebedarf", "qualifiziert",
    "arbeitskraefteeinwanderung", "hochqualifizierte", "fachkraeften",
    "arbeitsvisa", "ausbilden", "sprachfoerderung", "integration",
    "einbuergerung", "staatsbuergerschaft", "spracherwerb",
    "integrationsmassnahmen",
]
RESTRICTIVE_EXPANSION_SEEDS = [
    "belastungsgrenzen", "belastbarkeit", "unterqualifizierter", "asyltourismus",
    "belasten", "ueberlastung", "ueberlasten", "lohndumping", "erschoepfen",
    "lasten", "ablehnen", "migrationsfolgekosten", "sozialleistungsmissbrauch",
    "wirtschaftsmigranten", "einwanderungsquote", "geringqualifiziert",
    "wohnungsmangel", "belastungsgrenze", "wirtschaftsfluechtlinge",
    "zusatzkosten", "asylstrom", "armutsfluechtlinge", "fluechtlingsstroms",
    "massenzustrom", "asylbewerberzustroms", "schwarzarbeit", "clanbildung",
    "irregulaere", "nichtqualifizierten",
]

# PCA refinement
PCA_SHUFFLE_SEED = 42      # same as old notebook -> same projection (Figure 2)
PCA_RANDOM_STATE = 12
# global mode: fixed thresholds on PC2, as in the thesis
WELCOMING_PC2_MIN = 0.5    # keep welcoming words with PC2 >= 0.5
RESTRICTIVE_PC2_MAX = -0.48  # keep restrictive words with PC2 < -0.48
# rolling mode: PC2 is re-estimated in every window, so absolute thresholds are
# not comparable. We orient PC2 (welcoming above) and keep the same SHARES of
# words that the global thresholds kept (241/590 and 288/718 in the thesis run).
WELCOMING_KEEP_SHARE = 241 / 590
RESTRICTIVE_KEEP_SHARE = 288 / 718

# Rolling Word2Vec: retrain every k quarters (1 = every quarter, like Doc2Vec).
# A model is only ever trained on speeches BEFORE the scored quarter, so any
# k keeps the design look-ahead free; k=4 (yearly) saves a lot of time.
W2V_REFIT_EVERY = 4

# ----------------------------------------------------------------------
# Step 3: rolling Doc2Vec scoring
# ----------------------------------------------------------------------
FIRST_QUARTER = "1960Q1"
N_QUARTERS = 262            # 1960Q1 ... 2025Q2
FIRST_WINDOW_START = 80     # periods[80] = 1980Q1
LAST_WINDOW_START = 222     # exclusive -> last scored quarter 2025Q2
WINDOW_QUARTERS = 40        # ten years
MAX_WORDS_PER_TERM = 5

# ----------------------------------------------------------------------
# Step 4: monthly aggregation
# ----------------------------------------------------------------------
VAR_SAMPLE_START = "2006-01-01"
VAR_SAMPLE_END = "2025-06-01"
