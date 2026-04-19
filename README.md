# Truth-Social-Stock-Signals
Truth Social-driven quant backtest in R testing whether posting volume, sentiment, risk, and de-escalation language predict next-day returns in GLD and SPY. Finds limited signal for gold, but stronger risk-adjusted results for equities, where several text-based strategies outperformed Buy and Hold.

The analysis focuses on two liquid ETFs:

- **GLD** — SPDR Gold Shares ETF  
- **SPY** — SPDR S&P 500 ETF

Rather than relying only on traditional price data, this project explores whether alternative data from Donald Trump’s Truth Social activity contains market-relevant information.

Signals tested:

- Posting Volume
- Sentiment Score
- Risk Word Frequency
- Net Risk Language
- De-escalation Language

Sample period:

**2025-01-20 to 2026-04-18**  
(Trump in-office period used for backtesting)

---

# Research Question

Can Truth Social activity generate useful trading signals?

More specifically:

- Does unusually high posting activity predict next-day returns?
- Does post language matter more than raw activity?
- Do defensive assets and equities react differently?
- Can any signals outperform Buy and Hold on a risk-adjusted basis?

---

# Data Sources

## Market Data
- Yahoo Finance price data via the `quantmod` package

## Truth Social Archive
- Historical Truth Social posts sourced from the public GitHub repository:

**stiles/trump-truth-social-archive**  
https://github.com/stiles/trump-truth-social-archive

This repository maintains an archive of Donald Trump’s Truth Social posts in CSV and JSON formats and was used as the primary alternative-data source for this project.

*Note: public data availability may change over time depending on updates to the archive.*

---

# Methodology

## Framework

Signals are built using information available on day *t* and applied to returns on day *t+1*.

Strategies are compared with Buy and Hold using:

- Average return
- Volatility
- Annualized Sharpe ratio
- Cumulative return paths
- Statistical testing

This helps distinguish meaningful signals from noisy backtest results.

---

# Signals Tested

## 1. Posting Volume

Measures whether daily post count is unusually high relative to recent history.

## 2. Sentiment Score

Uses positive and negative word counts from a sentiment lexicon.

## 3. Risk Word Strategy

Counts words associated with uncertainty or geopolitical stress.

Examples:

`war`, `attack`, `tariff`, `inflation`, `conflict`

## 4. De-escalation Strategy

Counts words associated with easing tension.

Examples:

`deal`, `peace`, `pause`, `resolution`, `solve`

## 5. Net Language Signals

Combines opposing word groups:

- Net Risk = Risk Words − De-escalation Words
- Net De-escalation = De-escalation Words − Risk Words

---

# Results

# GLD (Gold ETF)

## Overall Takeaway

Truth Social signals were **not stronger than Buy and Hold** on a risk-adjusted basis over the full sample.

### Findings

- Posting Volume underperformed Buy and Hold
- Sentiment Strategy had lower Sharpe than Buy and Hold
- Risk Word Strategy had lower Sharpe than Buy and Hold
- Net Risk Strategy had lower Sharpe than Buy and Hold

### Interesting Observation

Although full-period Sharpe ratios were weaker, both:

- Sentiment Strategy
- Risk Word Strategy

generated cumulative returns above Buy and Hold during parts of:

- 2025 Q1
- 2025 Q2
- 2025 Q3

### Interpretation

Truth Social language may contain short-lived tactical signals for gold during periods of uncertainty, but those signals were not persistent enough to outperform Buy and Hold across the full sample.

---

# SPY (S&P 500 ETF)

## Overall Takeaway

Truth Social signals were materially more useful for SPY than for GLD.

## Best Signals

### Posting Volume Strategy

- Sharpe: **0.911**
- Buy and Hold Sharpe: **0.867**

High posting activity often acted as a useful risk-off filter.

### Reversed Sentiment Strategy

(Positive sentiment interpreted contrarily / risk filter)

- Sharpe: **1.63**

This was one of the strongest signals in the project.

### De-escalation Word Strategy

- Sharpe: **1.10**

Language associated with easing tension aligned with stronger equity performance.

## Weak Signal

### Net De-escalation Strategy

(De-escalation − Risk)

This signal performed poorly and was not robust.

### Interpretation

For equities, market reaction appeared more sensitive to communication intensity and tone than for gold. Filtering exposure based on posting behavior improved risk-adjusted performance in several cases.

---

# Key Insights

## 1. Alternative Data Can Matter

Social media activity may contain usable market information.

## 2. Asset Reactions Differ

The same signal can behave differently across asset classes.

- Weak for GLD
- Stronger for SPY

## 3. Language Can Matter More Than Raw Volume

For SPY, sentiment and de-escalation signals outperformed simple posting counts.

## 4. Robustness Matters

Some strategies worked strongly, while others failed completely. Backtests should be judged across the full sample, not only short winning windows.

---

# Future Improvements

Potential next steps:

- Add transaction costs and slippage
- Out-of-sample validation
- Multi-factor combinations
- Regression / probability models
- More advanced NLP models
- Intraday event-time analysis
- Additional assets (rates, FX, sectors)

---

# Tech Stack

- R
- tidyverse
- quantmod
- xts
- zoo
- tidytext
- lubridate

---

# File Structure

```text
truth-social-market-backtest/
│
├── README.md
├── analysis/
│   └── truth_social_market_model.R
├── output/
│   ├── figures/
│   └── tables/
└── .gitignore
