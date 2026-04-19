# ============================================================
# Project: Truth Social Signals and Next-Day Market Returns
# Author: Sara Liu
# Description:
#   This script downloads GLD and SPY price data, constructs
#   posting-volume and text-based signals from Donald Trump
#   Truth Social posts, and backtests whether those signals
#   are associated with next-day returns.
#
# Notes:
#   - This is an exploratory backtest
#   - Signals generated on day t are applied to returns on day t+1
#   - No transaction costs, slippage, or out-of-sample validation
# ============================================================

# ---------------------------
# 1. Setup
# ---------------------------

rm(list = ls())

required_packages <- c(
  "tidyverse",
  "quantmod",
  "xts",
  "zoo",
  "tidytext",
  "lubridate"
)

invisible(lapply(required_packages, library, character.only = TRUE))

# Create output folders if they do not already exist
dir.create("output", showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2. Parameters
# ---------------------------

truth_data_url <- "https://ix.cnn.io/data/truth-social/truth_archive.csv"
analysis_start_date <- as.Date("2025-01-20")
trading_days_per_year <- 252
volume_z_threshold <- 1
risk_threshold <- 1
sentiment_threshold <- 5
rolling_window <- 20

risk_words <- c(
  "war", "attack", "crisis", "china", "tariff",
  "inflation", "fed", "rates", "debt",
  "oil", "iran", "trade", "conflict", "recession",
  "bomb", "russia", "lost", "loss", "law",
  "protest", "justice", "threat", "protect",
  "illegal", "violence"
)

deescalation_words <- c(
  "negotiation", "negotiate", "deal", "agreement",
  "stop", "stall", "postpone", "pause", "delay",
  "peace", "resolve", "resolution", "ease", "calm",
  "meet", "meeting", "talk", "win", "solved",
  "end", "solve"
)

# ---------------------------
# 3. Helper Functions
# ---------------------------

build_daily_returns <- function(price_xts) {
  tibble(
    date = as.Date(index(price_xts)),
    price = as.numeric(Ad(price_xts))
  ) %>%
    arrange(date) %>%
    mutate(
      daily_return = price / lag(price) - 1,
      next_day_return = lead(daily_return, 1)
    )
}

compute_rolling_z_score <- function(x, window = 20) {
  roll_mean <- zoo::rollmean(x, k = window, fill = NA, align = "right")
  roll_sd <- zoo::rollapply(x, width = window, FUN = sd, fill = NA, align = "right")
  
  tibble(
    roll_mean = roll_mean,
    roll_sd = roll_sd,
    z_score = (x - roll_mean) / roll_sd
  )
}

compute_annualized_sharpe <- function(x, scale = 252) {
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  
  if (is.na(sd_x) || sd_x == 0) {
    return(NA_real_)
  }
  
  (mean_x / sd_x) * sqrt(scale)
}

summarize_signal_split <- function(data, signal_col) {
  data %>%
    group_by(.data[[signal_col]]) %>%
    summarise(
      count = n(),
      avg_return = mean(next_day_return, na.rm = TRUE),
      sd_return = sd(next_day_return, na.rm = TRUE),
      .groups = "drop"
    )
}

summarize_strategy_vs_benchmark <- function(data, benchmark_col, strategy_cols) {
  benchmark <- data[[benchmark_col]]
  
  out <- tibble(
    metric = c("mean", "sd", "sharpe")
  )
  
  out[["Buy and Hold"]] <- c(
    mean(benchmark, na.rm = TRUE),
    sd(benchmark, na.rm = TRUE),
    compute_annualized_sharpe(benchmark, trading_days_per_year)
  )
  
  for (col_name in strategy_cols) {
    strategy <- data[[col_name]]
    
    out[[col_name]] <- c(
      mean(strategy, na.rm = TRUE),
      sd(strategy, na.rm = TRUE),
      compute_annualized_sharpe(strategy, trading_days_per_year)
    )
  }
  
  out
}

run_one_sample_ttest <- function(x, alternative = "greater") {
  test <- t.test(x, alternative = alternative)
  
  tibble(
    statistic = unname(test$statistic),
    p_value = test$p.value,
    conf_low = test$conf.int[1],
    conf_high = test$conf.int[2],
    estimate = unname(test$estimate),
    alternative = alternative
  )
}

run_two_group_ttest <- function(data, formula_obj) {
  test <- t.test(formula_obj, data = data)
  
  tibble(
    statistic = unname(test$statistic),
    p_value = test$p.value,
    conf_low = test$conf.int[1],
    conf_high = test$conf.int[2],
    estimate_group_1 = unname(test$estimate[1]),
    estimate_group_2 = unname(test$estimate[2])
  )
}

build_cumulative_return_plot_data <- function(data, date_col, series_map) {
  plot_data <- tibble(date = data[[date_col]])
  
  for (series_name in names(series_map)) {
    return_col <- series_map[[series_name]]
    plot_data[[series_name]] <- cumprod(1 + data[[return_col]])
  }
  
  plot_data %>%
    pivot_longer(
      cols = -date,
      names_to = "series",
      values_to = "cum_return"
    )
}

save_plot <- function(plot_object, file_name, width = 10, height = 6) {
  ggsave(
    filename = file.path("output/figures", file_name),
    plot = plot_object,
    width = width,
    height = height
  )
}

save_table <- function(data, file_name) {
  readr::write_csv(data, file.path("output/tables", file_name))
}

# ---------------------------
# 4. Download Market Data
# ---------------------------

getSymbols("GLD", src = "yahoo")
getSymbols("SPY", src = "yahoo")

gld_daily <- build_daily_returns(GLD)
spy_daily <- build_daily_returns(SPY)

# ---------------------------
# 5. Download and Clean Truth Social Data
# ---------------------------

truth_posts_raw <- readr::read_csv(truth_data_url, show_col_types = FALSE) %>%
  mutate(
    created_at = lubridate::ymd_hms(created_at),
    date = as.Date(created_at)
  )

truth_posts_clean <- truth_posts_raw %>%
  mutate(
    content = stringr::str_replace_all(content, "<.*?>", " "),
    content = stringr::str_to_lower(content),
    content = stringr::str_replace_all(content, "rt\\S+|http\\S+|www\\S+", " "),
    content = stringr::str_replace_all(content, "[^a-z\\s]", " "),
    content = stringr::str_squish(content)
  )

truth_words <- truth_posts_clean %>%
  select(date, content) %>%
  tidytext::unnest_tokens(word, content)

# ---------------------------
# 6. Build Daily Truth Social Features
# ---------------------------

truth_volume_daily <- truth_posts_clean %>%
  group_by(date) %>%
  summarise(
    post_count = n(),
    .groups = "drop"
  )

bing_lexicon <- tidytext::get_sentiments("bing")

truth_sentiment_daily <- truth_words %>%
  inner_join(bing_lexicon, by = "word") %>%
  mutate(
    sentiment_value = if_else(sentiment == "positive", 1, -1)
  ) %>%
  group_by(date) %>%
  summarise(
    sentiment_score = sum(sentiment_value, na.rm = TRUE),
    sentiment_avg = mean(sentiment_value, na.rm = TRUE),
    sentiment_words = n(),
    .groups = "drop"
  )

truth_risk_daily <- truth_words %>%
  mutate(
    risk_plus = if_else(word %in% risk_words, 1, 0),
    risk_minus = if_else(word %in% deescalation_words, 1, 0),
    net_risk_component = risk_plus - risk_minus,
    net_deescalation_component = risk_minus - risk_plus
  ) %>%
  group_by(date) %>%
  summarise(
    risk_score = sum(risk_plus, na.rm = TRUE),
    deescalation_score = sum(risk_minus, na.rm = TRUE),
    net_risk_score = sum(net_risk_component, na.rm = TRUE),
    net_deescalation_score = sum(net_deescalation_component, na.rm = TRUE),
    .groups = "drop"
  )

truth_daily <- truth_sentiment_daily %>%
  full_join(truth_risk_daily, by = "date") %>%
  full_join(truth_volume_daily, by = "date") %>%
  replace_na(list(
    sentiment_score = 0,
    sentiment_avg = 0,
    sentiment_words = 0,
    risk_score = 0,
    deescalation_score = 0,
    net_risk_score = 0,
    net_deescalation_score = 0,
    post_count = 0
  ))

# Save daily feature table
save_table(truth_daily, "truth_daily_features.csv")

# ---------------------------
# 7. GLD Volume Strategy
# ---------------------------

gld_volume_model <- gld_daily %>%
  left_join(truth_volume_daily, by = "date") %>%
  mutate(
    post_count = replace_na(post_count, 0)
  ) %>%
  bind_cols(compute_rolling_z_score(.$post_count, rolling_window)) %>%
  mutate(
    signal_volume = if_else(z_score > volume_z_threshold, 1, 0),
    strategy_volume_return = signal_volume * next_day_return
  ) %>%
  filter(!is.na(z_score), !is.na(next_day_return))

gld_volume_directional_summary <- summarize_signal_split(gld_volume_model, "signal_volume")
gld_volume_directional_ttest <- run_two_group_ttest(gld_volume_model, next_day_return ~ signal_volume)

gld_volume_backtest <- gld_volume_model %>%
  filter(date >= analysis_start_date)

gld_volume_summary <- summarize_strategy_vs_benchmark(
  data = gld_volume_backtest,
  benchmark_col = "next_day_return",
  strategy_cols = c("strategy_volume_return")
)

gld_volume_strategy_ttest <- run_one_sample_ttest(gld_volume_backtest$strategy_volume_return, "greater")

gld_volume_backtest <- gld_volume_backtest %>%
  mutate(
    excess_volume_return = strategy_volume_return - next_day_return
  )

gld_volume_excess_ttest <- run_one_sample_ttest(gld_volume_backtest$excess_volume_return, "greater")

gld_volume_plot_data <- build_cumulative_return_plot_data(
  data = gld_volume_backtest,
  date_col = "date",
  series_map = c(
    "Buy and Hold" = "next_day_return",
    "GLD Volume Strategy" = "strategy_volume_return"
  )
)

gld_volume_plot <- ggplot(gld_volume_plot_data, aes(x = date, y = cum_return, color = series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "GLD Volume Strategy vs Buy and Hold",
    x = "Date",
    y = "Growth of $1",
    color = NULL
  ) +
  theme_minimal()

save_plot(gld_volume_plot, "gld_volume_strategy.png")
save_table(gld_volume_directional_summary, "gld_volume_directional_summary.csv")
save_table(gld_volume_directional_ttest, "gld_volume_directional_ttest.csv")
save_table(gld_volume_summary, "gld_volume_summary.csv")
save_table(gld_volume_strategy_ttest, "gld_volume_strategy_ttest.csv")
save_table(gld_volume_excess_ttest, "gld_volume_excess_ttest.csv")

# ---------------------------
# 8. GLD Text-Based Strategies
# ---------------------------

gld_text_model <- gld_daily %>%
  left_join(truth_daily, by = "date") %>%
  replace_na(list(
    sentiment_score = 0,
    sentiment_avg = 0,
    sentiment_words = 0,
    risk_score = 0,
    deescalation_score = 0,
    net_risk_score = 0,
    net_deescalation_score = 0,
    post_count = 0
  )) %>%
  filter(!is.na(next_day_return))

gld_net_risk_directional_summary <- gld_text_model %>%
  mutate(signal_net_risk = if_else(net_risk_score > 0, 1, 0)) %>%
  summarize_signal_split("signal_net_risk")

gld_risk_directional_summary <- gld_text_model %>%
  mutate(signal_risk = if_else(risk_score > risk_threshold, 1, 0)) %>%
  summarize_signal_split("signal_risk")

gld_sentiment_directional_summary <- gld_text_model %>%
  mutate(signal_sentiment = if_else(sentiment_score > sentiment_threshold, 1, 0)) %>%
  summarize_signal_split("signal_sentiment")

gld_text_backtest <- gld_text_model %>%
  filter(date >= analysis_start_date) %>%
  mutate(
    signal_net_risk = if_else(net_risk_score > 0, 1, 0),
    strategy_net_risk_return = signal_net_risk * next_day_return,
    signal_risk = if_else(risk_score > risk_threshold, 1, 0),
    strategy_risk_return = signal_risk * next_day_return,
    signal_sentiment = if_else(sentiment_score > sentiment_threshold, 1, 0),
    strategy_sentiment_return = signal_sentiment * next_day_return
  )

gld_text_summary <- summarize_strategy_vs_benchmark(
  data = gld_text_backtest,
  benchmark_col = "next_day_return",
  strategy_cols = c(
    "strategy_net_risk_return",
    "strategy_risk_return",
    "strategy_sentiment_return"
  )
)

gld_net_risk_ttest <- run_one_sample_ttest(gld_text_backtest$strategy_net_risk_return, "greater")
gld_risk_ttest <- run_one_sample_ttest(gld_text_backtest$strategy_risk_return, "greater")
gld_sentiment_ttest <- run_one_sample_ttest(gld_text_backtest$strategy_sentiment_return, "greater")

gld_text_backtest <- gld_text_backtest %>%
  mutate(
    excess_net_risk_return = strategy_net_risk_return - next_day_return,
    excess_risk_return = strategy_risk_return - next_day_return,
    excess_sentiment_return = strategy_sentiment_return - next_day_return
  )

gld_excess_net_risk_ttest <- run_one_sample_ttest(gld_text_backtest$excess_net_risk_return, "greater")
gld_excess_risk_ttest <- run_one_sample_ttest(gld_text_backtest$excess_risk_return, "greater")
gld_excess_sentiment_ttest <- run_one_sample_ttest(gld_text_backtest$excess_sentiment_return, "greater")

gld_text_plot_data <- build_cumulative_return_plot_data(
  data = gld_text_backtest,
  date_col = "date",
  series_map = c(
    "Buy and Hold" = "next_day_return",
    "GLD Net Risk Strategy" = "strategy_net_risk_return",
    "GLD Risk Strategy" = "strategy_risk_return",
    "GLD Sentiment Strategy" = "strategy_sentiment_return"
  )
)

gld_text_plot <- ggplot(gld_text_plot_data, aes(x = date, y = cum_return, color = series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "GLD Text-Based Strategies vs Buy and Hold",
    x = "Date",
    y = "Growth of $1",
    color = NULL
  ) +
  theme_minimal()

save_plot(gld_text_plot, "gld_text_strategies.png")
save_table(gld_net_risk_directional_summary, "gld_net_risk_directional_summary.csv")
save_table(gld_risk_directional_summary, "gld_risk_directional_summary.csv")
save_table(gld_sentiment_directional_summary, "gld_sentiment_directional_summary.csv")
save_table(gld_text_summary, "gld_text_strategy_summary.csv")
save_table(gld_net_risk_ttest, "gld_net_risk_ttest.csv")
save_table(gld_risk_ttest, "gld_risk_ttest.csv")
save_table(gld_sentiment_ttest, "gld_sentiment_ttest.csv")
save_table(gld_excess_net_risk_ttest, "gld_excess_net_risk_ttest.csv")
save_table(gld_excess_risk_ttest, "gld_excess_risk_ttest.csv")
save_table(gld_excess_sentiment_ttest, "gld_excess_sentiment_ttest.csv")

# ---------------------------
# 9. SPY Volume Strategy
# ---------------------------

spy_volume_model <- spy_daily %>%
  left_join(truth_volume_daily, by = "date") %>%
  mutate(
    post_count = replace_na(post_count, 0)
  ) %>%
  bind_cols(compute_rolling_z_score(.$post_count, rolling_window)) %>%
  mutate(
    signal_volume = if_else(z_score > volume_z_threshold, 0, 1),
    strategy_volume_return = signal_volume * next_day_return
  ) %>%
  filter(!is.na(z_score), !is.na(next_day_return))

spy_volume_directional_summary <- summarize_signal_split(spy_volume_model, "signal_volume")
spy_volume_directional_ttest <- run_two_group_ttest(spy_volume_model, next_day_return ~ signal_volume)

spy_volume_backtest <- spy_volume_model %>%
  filter(date >= analysis_start_date)

spy_volume_summary <- summarize_strategy_vs_benchmark(
  data = spy_volume_backtest,
  benchmark_col = "next_day_return",
  strategy_cols = c("strategy_volume_return")
)

spy_volume_strategy_ttest <- run_one_sample_ttest(spy_volume_backtest$strategy_volume_return, "greater")

spy_volume_backtest <- spy_volume_backtest %>%
  mutate(
    excess_volume_return = strategy_volume_return - next_day_return
  )

spy_volume_excess_ttest <- run_one_sample_ttest(spy_volume_backtest$excess_volume_return, "greater")

spy_volume_plot_data <- build_cumulative_return_plot_data(
  data = spy_volume_backtest,
  date_col = "date",
  series_map = c(
    "Buy and Hold" = "next_day_return",
    "SPY Volume Strategy" = "strategy_volume_return"
  )
)

spy_volume_plot <- ggplot(spy_volume_plot_data, aes(x = date, y = cum_return, color = series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "SPY Volume Strategy vs Buy and Hold",
    x = "Date",
    y = "Growth of $1",
    color = NULL
  ) +
  theme_minimal()

save_plot(spy_volume_plot, "spy_volume_strategy.png")
save_table(spy_volume_directional_summary, "spy_volume_directional_summary.csv")
save_table(spy_volume_directional_ttest, "spy_volume_directional_ttest.csv")
save_table(spy_volume_summary, "spy_volume_summary.csv")
save_table(spy_volume_strategy_ttest, "spy_volume_strategy_ttest.csv")
save_table(spy_volume_excess_ttest, "spy_volume_excess_ttest.csv")

# ---------------------------
# 10. SPY Text-Based Strategies
# ---------------------------

spy_text_model <- spy_daily %>%
  left_join(truth_daily, by = "date") %>%
  replace_na(list(
    sentiment_score = 0,
    sentiment_avg = 0,
    sentiment_words = 0,
    risk_score = 0,
    deescalation_score = 0,
    net_risk_score = 0,
    net_deescalation_score = 0,
    post_count = 0
  )) %>%
  filter(!is.na(next_day_return))

spy_net_deescalation_directional_summary <- spy_text_model %>%
  mutate(signal_net_deescalation = if_else(net_deescalation_score > 1, 1, 0)) %>%
  summarize_signal_split("signal_net_deescalation")

spy_deescalation_directional_summary <- spy_text_model %>%
  mutate(signal_deescalation = if_else(deescalation_score > 1, 1, 0)) %>%
  summarize_signal_split("signal_deescalation")

spy_sentiment_directional_summary <- spy_text_model %>%
  mutate(signal_sentiment = if_else(sentiment_score > sentiment_threshold, 0, 1)) %>%
  summarize_signal_split("signal_sentiment")

spy_text_backtest <- spy_text_model %>%
  filter(date >= analysis_start_date) %>%
  mutate(
    signal_net_deescalation = if_else(net_deescalation_score > 0, 1, 0),
    strategy_net_deescalation_return = signal_net_deescalation * next_day_return,
    signal_deescalation = if_else(deescalation_score > 1, 1, 0),
    strategy_deescalation_return = signal_deescalation * next_day_return,
    signal_sentiment = if_else(sentiment_score > sentiment_threshold, 0, 1),
    strategy_sentiment_return = signal_sentiment * next_day_return
  )

spy_text_summary <- summarize_strategy_vs_benchmark(
  data = spy_text_backtest,
  benchmark_col = "next_day_return",
  strategy_cols = c(
    "strategy_net_deescalation_return",
    "strategy_deescalation_return",
    "strategy_sentiment_return"
  )
)

# One-sample t-tests:
# Is each strategy's average return greater than 0?
spy_net_deescalation_ttest <- run_one_sample_ttest(
  spy_text_backtest$strategy_net_deescalation_return,
  "greater"
)

spy_deescalation_ttest <- run_one_sample_ttest(
  spy_text_backtest$strategy_deescalation_return,
  "greater"
)

spy_sentiment_ttest <- run_one_sample_ttest(
  spy_text_backtest$strategy_sentiment_return,
  "greater"
)

# Strategy vs Buy-and-Hold:
# excess_return > 0 means the strategy beat buy-and-hold that day
spy_text_backtest <- spy_text_backtest %>%
  mutate(
    excess_net_deescalation_return = strategy_net_deescalation_return - next_day_return,
    excess_deescalation_return = strategy_deescalation_return - next_day_return,
    excess_sentiment_return = strategy_sentiment_return - next_day_return
  )

spy_excess_net_deescalation_ttest <- run_one_sample_ttest(
  spy_text_backtest$excess_net_deescalation_return,
  "greater"
)

spy_excess_deescalation_ttest <- run_one_sample_ttest(
  spy_text_backtest$excess_deescalation_return,
  "greater"
)

spy_excess_sentiment_ttest <- run_one_sample_ttest(
  spy_text_backtest$excess_sentiment_return,
  "greater"
)

spy_text_plot_data <- build_cumulative_return_plot_data(
  data = spy_text_backtest,
  date_col = "date",
  series_map = c(
    "Buy and Hold" = "next_day_return",
    "SPY Net Deescalation Strategy" = "strategy_net_deescalation_return",
    "SPY Deescalation Strategy" = "strategy_deescalation_return",
    "SPY Sentiment Strategy" = "strategy_sentiment_return"
  )
)

spy_text_plot <- ggplot(spy_text_plot_data, aes(x = date, y = cum_return, color = series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "SPY Text-Based Strategies vs Buy and Hold",
    x = "Date",
    y = "Growth of $1",
    color = NULL
  ) +
  theme_minimal()

save_plot(spy_text_plot, "spy_text_strategies.png")
save_table(spy_net_deescalation_directional_summary, "spy_net_deescalation_directional_summary.csv")
save_table(spy_deescalation_directional_summary, "spy_deescalation_directional_summary.csv")
save_table(spy_sentiment_directional_summary, "spy_sentiment_directional_summary.csv")
save_table(spy_text_summary, "spy_text_strategy_summary.csv")
save_table(spy_net_deescalation_ttest, "spy_net_deescalation_ttest.csv")
save_table(spy_deescalation_ttest, "spy_deescalation_ttest.csv")
save_table(spy_sentiment_ttest, "spy_sentiment_ttest.csv")
save_table(spy_excess_net_deescalation_ttest, "spy_excess_net_deescalation_ttest.csv")
save_table(spy_excess_deescalation_ttest, "spy_excess_deescalation_ttest.csv")
save_table(spy_excess_sentiment_ttest, "spy_excess_sentiment_ttest.csv")

# ---------------------------
# 11. Console Output
# ---------------------------

cat("\n====================\n")
cat("Analysis complete.\n")
cat("====================\n\n")

cat("Saved files:\n")
cat("- output/tables/\n")
cat("- output/figures/\n\n")

cat("Key summary tables created:\n")
cat("- truth_daily_features.csv\n")
cat("- gld_volume_summary.csv\n")
cat("- gld_text_strategy_summary.csv\n")
cat("- spy_volume_summary.csv\n")
cat("- spy_text_strategy_summary.csv\n\n")

