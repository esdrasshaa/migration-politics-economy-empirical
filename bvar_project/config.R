############################################################
# config.R  --  the ONLY place where choices are made
# Every window, lag length, variable, label and seed lives here.
# Scripts and functions never hard-code any of these.
############################################################

CFG <- list(
  
  seed = 0,
  
  paths = list(
    data    = "data",
    fig     = "outputs/figures",
    tab     = "outputs/tables"
  ),
  
  plot = list(w = 6.3, h = 8, pointsize = 13, cex = 1,
              cex_axis = 1.25, cex_legend = 1.3, legend_h = 0.3),
  
  # ---- estimation settings (shared by ALL models => comparable) ----
  bvar = list(
    p         = "bic",        # "bic" = selected per model; or a fixed integer (e.g. 2)
    p_max     = 4,            # largest lag length searched by BIC
    draws     = 1000,         # posterior draws kept
    hor       = 49,           # IRF / FEVD horizon
    ci        = c(68, 90),    # credible levels, from ONE set of draws
    max_tries = 500           # max rejections per draw (stability)
  ),

  # ---- raw files ----
  files = list(
    baseline_excel = "immigration_database.xlsx",
    baseline_sheet = "Monthly data (SA X13+)",
    new_macro      = "all_data_2006Q1_2025.csv",
    ba_unemp       = "statistik_lzr_20260818203025.csv",
    sentiment = c(
      doc2vec    = "immigration_sentiment_monthly_doc2vec.csv",
      gpt41nano  = "GptSentimentMonthlyVargptNano.csv",
      gpt5nano   = "GptSentimentMonthlyVarGpt5Nano.csv"
    )
  ),

  # ---- sentiment column names per source (handles the 'gtp' typo) ----
  sentiment_cols = list(
    doc2vec   = c(government = "sentiment_government",
                  opposition = "sentiment_opposition",
                  overall    = "sentiment_gesamt",
                  expansive_government   = "expansive_government",
                  restrictive_government = "restrictive_government",
                  expansive_opposition   = "expansive_opposition",
                  restrictive_opposition = "restrictive_opposition"),
    gpt41nano = c(government = "gpt_sentiment_government",
                  opposition = "gpt_sentiment_opposition",
                  overall    = "gtp_overall_sentiment"),
    gpt5nano  = c(government = "gpt_sentiment_government",
                  opposition = "gpt_sentiment_opposition",
                  overall    = "gtp_overall_sentiment")
  ),
  sentiment_date_col = c(doc2vec = "Date", gpt41nano = "month", gpt5nano = "month"),

  covid = list(start = "2020-03-01", end = "2021-10-01"),

  macro_labels = c("Arrivals", "Business Expectations", "Consumer Confidence",
                   "Industrial Production", "Unemployment"),
  macro_order  = c("arrivals", "busexp", "consconf", "indpro", "unrate")
)

############################################################
# MODEL SPECIFICATIONS
# One row = one model. Titles/labels are GENERATED from the
# actual estimation sample, so a title can never disagree
# with the data (old scripts said "2007-2019" for 2006-2019).
############################################################
make_spec <- function(id, panel, start, end, sentiment = "government",
                      source = "doc2vec", covid = FALSE, order = "macro_first",
                      p = CFG$bvar$p, shock = "sentiment") {
  list(id = id, panel = panel, start = as.Date(start), end = as.Date(end),
       sentiment = sentiment, source = source, covid = covid,
       order = order, p = p, shock = shock)
}

SPECS <- list(
  # --- replication of the original study (Excel panel) ---
  make_spec("base_gvt_2006_2019", "baseline", "2006-01-01", "2019-10-01", "government"),
  make_spec("base_opp_2006_2019", "baseline", "2006-01-01", "2019-10-01", "opposition"),

  # --- new data, same pre-2019 window (isolates the DATA effect) ---
  make_spec("new_gvt_2007_2019", "new", "2007-05-01", "2019-10-01", "government"),
  make_spec("new_opp_2007_2019", "new", "2007-05-01", "2019-10-01", "opposition"),

  # --- new data, full window with COVID dummy ---
  make_spec("new_gvt_2007_2025", "new", "2007-05-01", "2025-06-01", "government", covid = TRUE),
  make_spec("new_opp_2007_2025", "new", "2007-05-01", "2025-06-01", "opposition", covid = TRUE),
  make_spec("arr_gvt_2007_2025", "new", "2007-05-01", "2025-06-01", "government",
            covid = TRUE, shock = "arrivals"),
  make_spec("arr_opp_2007_2025", "new", "2007-05-01", "2025-06-01", "opposition",
            covid = TRUE, shock = "arrivals"),
  make_spec("unrate_gvt_2007_2025", "new", "2007-05-01", "2025-06-01", "government",
            covid = TRUE, shock = "unrate"),
  make_spec("unrate_opp_2007_2025", "new", "2007-05-01", "2025-06-01", "opposition",
            covid = TRUE, shock = "unrate"),

  # --- short pre-2015 window ---
  make_spec("new_gvt_2007_2014", "new", "2007-05-01", "2014-12-01", "government"),
  make_spec("new_opp_2007_2014", "new", "2007-05-01", "2014-12-01", "opposition"),

  # --- robustness: alternative sentiment measure (GPT-4.1-nano) ---
  # same windows as the doc2vec models above, so only the sentiment measure differs
  make_spec("rob_gvt_gpt41_2007_2014", "new", "2007-05-01", "2014-12-01", "government",
            source = "gpt41nano"),
  make_spec("rob_opp_gpt41_2007_2014", "new", "2007-05-01", "2014-12-01", "opposition",
            source = "gpt41nano"),
  make_spec("rob_gvt_gpt41_2007_2019", "new", "2007-05-01", "2019-10-01", "government",
            source = "gpt41nano"),
  make_spec("rob_opp_gpt41_2007_2019", "new", "2007-05-01", "2019-10-01", "opposition",
            source = "gpt41nano"),
  make_spec("rob_gvt_gpt41_2007_2025", "new", "2007-05-01", "2025-06-01", "government",
            source = "gpt41nano", covid = TRUE),
  make_spec("rob_opp_gpt41_2007_2025", "new", "2007-05-01", "2025-06-01", "opposition",
            source = "gpt41nano", covid = TRUE),
  make_spec("rob_arr_gvt_gpt41_2007_2025", "new", "2007-05-01", "2025-06-01", "government",
            source = "gpt41nano", covid = TRUE, shock = "arrivals"),
  make_spec("rob_arr_opp_gpt41_2007_2025", "new", "2007-05-01", "2025-06-01", "opposition",
            source = "gpt41nano", covid = TRUE, shock = "arrivals"),
  
  

  # --- robustness: Cholesky ordering (sentiment first) ---
  make_spec("rob_gvt_sentfirst", "new", "2007-05-01", "2025-06-01", "government",
            covid = TRUE, order = "sentiment_first"),
  make_spec("rob_opp_sentfirst", "new", "2007-05-01", "2025-06-01", "opposition",
            covid = TRUE, order = "sentiment_first")
)
names(SPECS) <- vapply(SPECS, `[[`, "", "id")

