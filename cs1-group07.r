# ==============================================================================
# SMM047 Probability and Mathematical Statistics (Subject CS1)
# Group Coursework 2025-26
# Group:        Group 07
# Authors (in alphabetical order):
#   - Abdulrahman Alolyan
#   - Benjamin Evans
#   - Amogh Sharma
#   - Nivetha Subbiah
# Professor:    Dr Russell Gerrard
# Institution:  Bayes Business School - City St George's, University of London
# Date:         TBC
# Description:  Term 1 group project for SMM047 Probability and Mathematical
# Statistics (50% of coursework grade - 15% of module grade). The R code below
# has been exported directly from an R Markdown (.rmd) file.  Hence the knitr
# settings.
# Dependencies:
#   - quantmod
#   - xts
#   - zoo
#   - ggplot2
#   - dplyr
#   - tidyr
#   - patchwork
#   - kableExtra
#   - e1071
#   - summarytools
#   - nortest
#   - boot
#   - clipr
#   - htmltools (only if knitting the rmd file to html)
# ==============================================================================


#----------------------- Initial setup (knitr settings) -----------------------#
dir.create("fig", showWarnings = FALSE)

# Defaults common to all outputs
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.align = "center",
  out.width = "100%",
  fig.path = "fig/",
  dpi = 300
)

# Output-specific settings
if (knitr::is_latex_output()) {
  knitr::opts_chunk$set(
    fig.width = 6,
    fig.height = 4,
    dev = "pdf",
    fig.pos = "ht",
    out.extra = ""
  )
} else {
  knitr::opts_chunk$set(
    fig.width = 6,
    fig.height = 4,
    dev = "svglite" # or "png"
  )
}

#----------------------------- Clean environment ------------------------------#
rm(list = ls()) # Remove all objects
graphics.off() # Close all graphical devices
cat("\014") # Clean console

#------------------- Load dependencies / external libraries -------------------#
library(quantmod)
library(xts) # for downloading
library(zoo) # for downloading

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(kableExtra)

library(e1071) # statistical tests
library(summarytools) # statistical tests (but not used other than demo)
library(nortest) # normality test

library(boot) # bootstrapping

# for custom functions
library(clipr) # for banner_comment function qol to annotate code



#---------------------------- Custom QOL functions ----------------------------#
#####################################
# function: banner comments (used to to section up code)
# Usage: banner_comment("Element 1: data cleaning") -> then ctrl + v (or cmd+v)
#####################################
banner_comment <- function(text, width = 80, border = "#", fill = "-") {
  txt <- paste0(" ", text, " ")
  inner_width <- width - 2 * nchar(border)
  banner_string <- ""

  if (inner_width <= nchar(txt)) {
    banner_string <- paste0(border, txt, border)
  } else {
    pad_total <- inner_width - nchar(txt)
    pad_left <- pad_total %/% 2
    pad_right <- pad_total - pad_left

    banner_string <- paste0(
      border,
      strrep(fill, pad_left),
      txt,
      strrep(fill, pad_right),
      border
    )
  }

  cat(banner_string, "\n")
  # copy banner to allow direct pasting (requires clipr)
  clipr::write_clip(banner_string)
  # avoid [1] when printing if want to manually copy
  invisible(banner_string)
}
#####################################
# function: format p-values for text
# Usage (in-line): `r format_p_vals(ad_test_result$p.value)`
# Usage (console): format_p_vals(ad_test_result$p.value)
#####################################
format_p_vals <- function(p) {
  if (length(p) != 1L || is.na(p)) {
    stop("Error! p must be a single non-missing value")
  }
  if (p > 1) {
    stop("Error! Value greater than 1")
  }
  if (p < 0) {
    stop("Error! Value less than 0")
  }

  if (p >= 0.01) {
    paste0("= ", formatC(p, format = "f", digits = 2))
  } else if (p >= 0.001) {
    paste0("= ", formatC(p, format = "f", digits = 3))
  } else {
    "< 0.001"
  }
}
#####################################
# function: format confidence intervals for tables & text
# Usage (in-line): `r format_interval(el2_ci_normal_95[1], el2_ci_normal_95[2])`
# Usage (console): format_interval(el2_ci_normal_95[1], el2_ci_normal_95[2])
#####################################
format_interval <- function(lower, upper, digits = 3) {
  paste0(
    "[",
    formatC(lower, format = "f", digits = digits), ", ",
    formatC(upper, format = "f", digits = digits),
    "]"
  )
}

#---------------------------- Download / Load data ----------------------------#
# Daily adjusted closing values for ^IXIC over the period 1 January 2018 to 31
# December 2024 were extracted using `getSymbols` from the `quantmod` package.
# These data were then used as the basis for our statistical evaluation in this
# report.
#
# Update 2025-11-21: included download for full IXIC data (used in appendix)
#
# Define inputs & date range
ticker <- "^IXIC"
study_from <- as.Date("2018-01-01")
study_to <- as.Date("2024-12-31")

# All data (for appendix etc.)
full_from <- as.Date("1971-01-01")
full_to <- study_to

# Clean ticker for filename (remove ^ and any non-alphanumerics)
ticker_clean <- gsub("[^A-Za-z0-9]", "", ticker)

make_fname <- function(prefix, from, to) {
  sprintf(
    "%s_%s_%sto%s.csv",
    ticker_clean,
    prefix,
    format(from, "%Y%m%d"),
    format(to, "%Y%m%d")
  )
}

load_from_csv <- function(path) {
  dat <- read.csv(path, stringsAsFactors = FALSE)
  if (!"Date" %in% names(dat)) {
    stop("csv file does not have expected 'Date' column.")
  }
  xts::xts(
    dat[, setdiff(names(dat), "Date"), drop = FALSE],
    order.by = as.Date(dat$Date)
  )
}

download_ixic <- function(ticker, from, to) {
  getSymbols(
    ticker,
    src         = "yahoo",
    from        = from,
    to          = to,
    auto.assign = FALSE
  )
}

# General "load or download + cache" helper
load_or_download_xts <- function(ticker, from, to, cache_file) {
  need_write <- FALSE

  if (file.exists(cache_file)) {
    dat <- tryCatch(
      load_from_csv(cache_file),
      error = function(e) {
        message("Cached file invalid, downloading fresh data: ", e$message)
        need_write <<- TRUE
        download_ixic(ticker, from, to)
      }
    )
  } else {
    need_write <- TRUE
    dat <- download_ixic(ticker, from, to)
  }

  if (need_write) {
    df <- data.frame(
      Date = index(dat),
      coredata(dat)
    )
    write.csv(df, cache_file, row.names = FALSE)
  }

  dat
}

# get full ixic data (1971-01-01 -> 2024-12-31)
fname_full <- make_fname("full", full_from, full_to)
IXIC_full <- load_or_download_xts(ticker, full_from, full_to, fname_full)

# get subset for our date range (2018-01-01 -> 2024-12-31)
IXIC <- IXIC_full[paste0(study_from, "/", study_to)]

# First we prepare our data, getting the adjusted close price, applying a log
# transform, calculating the log-return increment, and removing NaN values.

# get adjusted close price
IXIC_ad <- Ad(IXIC)
# log transform
y_n <- log(IXIC_ad)
# log-return increment (z_n) - get difference in log transformed data
z_n <- diff(y_n)
# remove NA values
z_n <- na.omit(z_n)

# BE note:
# @hull2021options Hull builds their model (the one we are given in the
#   assessment specs) on the assumption that the stock price follows a process
#   driven by dz, where z is a Wiener Process (BE note: this is another name for
#   Brownian Motion). By definition, a Wiener process has independent
#   increments. This means the movement in the next millisecond is completely
#   mathematically independent of the movement in the previous millisecond. Eq1
#   is the discrete-time snapshot of the continuous Wiener process Hull
#   describes. Because the underlying dz has "hard-coded" independence, the
#   daily returns inherit the assumption
#   or tldr; -> whole model is built on Brownian motion.
#
#   BE note to self 2: Geometric Brownian Motion assumes returns are independant
#   Sec 14.8 in Hull looks at Fractional Brownian motion which is different
#   ('non markov') which I think means it is dependent (idea that the market
#   has 'memory' whereas a markov frame of reference is that future price only
#   depends on current price, not path taken to get there*)
#   - Something to check during office hours (don't fully understand)


# # BE note: set eval to false on 19/11/2025 following group meeting
# # same length as IXIC_ad, first value NA
#
# df_initial_ixic <- tibble(
#   date = index(IXIC_ad),
#   price = as.numeric(IXIC_ad),
#   z_n = as.numeric(z_n)
# ) %>%
#   drop_na(z_n)
#
# date_range <- range(df_initial_ixic$date, na.rm = TRUE)
#
# p1_initial_vis <- ggplot(df_initial_ixic, aes(date, price)) +
#   theme_bw() +
#   geom_line(linewidth = 0.5) +
#   labs(
#     title = "NASDAQ Composite\n (Adjusted Close)",
#     x = "Date",
#     y = "Adjusted Close ($)"
#   )
#
# p2_initial_vis <- ggplot(df_initial_ixic, aes(date, z_n)) +
#   geom_line(linewidth = 0.5) +
#   theme_bw() +
#   labs(
#     title = expression(Log ~ Return ~ Increment ~ (z[n])),
#     x = "Date",
#     y = expression(z[n])
#   )
#
# # gridExtra::grid.arrange(p1, p2, ncol = 2)
# (p1_initial_vis | p2_initial_vis) &
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     plot.margin = margin(5.5, 5.5, 5.5, 5.5)
#   )

# trading_period_days <- 60
#
# # rolling mean return
# el1_rolling_return <- rollapply(
#   z_n,
#   width = trading_period_days,
#   FUN   = mean,
#   align = "right",
#   fill  = NA
# ) #* 252
#
# # rolling volatility
# el1_rolling_volatility <- rollapply(
#   z_n,
#   width = trading_period_days,
#   FUN   = sd,
#   align = "right",
#   fill  = NA
# ) #* sqrt(252)
#
# # combine into one xts and give sensible names
# el1_xts <- merge(el1_rolling_return, el1_rolling_volatility)
# colnames(el1_xts) <- c("rolling_return", "rolling_volatility")
#
# # convert xts → data frame with explicit date column
# el1_df <- fortify.zoo(el1_xts, name = "date")
# # columns are now: date, rolling_return, rolling_volatility
#
# # plot: 60-day annualised rolling mean log-return
# p1_rr <- ggplot(el1_df, aes(x = date, y = rolling_return)) +
#   geom_line(linewidth = 0.5) +
#   theme_bw() +
#   labs(
#     title = "60-day rolling mean log-return",
#     x = "Date",
#     y = expression(Annualised ~ mean ~ log ~ return)
#   )
#
# # plot: 60-day rolling volatility
# p2_vol <- ggplot(el1_df, aes(x = date, y = rolling_volatility)) +
#   geom_line(linewidth = 0.5) +
#   theme_bw() +
#   labs(
#     title = "60-day annualised rolling volatility",
#     x = "Date",
#     y = expression(Annualised ~ volatility)
#   )
#
# # combine with patchwork
# (p1_rr | p2_vol) &
#   theme(
#     plot.title  = element_text(hjust = 0.5),
#     plot.margin = margin(5.5, 5.5, 5.5, 5.5)
#   )

# ==============================================================================
#---------- Element 1: data cleaning and standard test of normality -----------#
# ==============================================================================

# outlier_periods_to_remove <- c(
#   "2020-03-01/2020-04-17",
#   "2021-01-10/2021-03-20",
#   "2022-06-01/2022-07-05"
# )
outlier_periods_to_remove <- c(
  "2020-02-24/2020-03-23"
)
# set all values from outlier period to NA
y_n_clean <- y_n
for (p in outlier_periods_to_remove) {
  y_n_clean[p] <- NA
}
# calculate log-return increments
z_n_clean <- diff(y_n_clean)
z_n_clean_pre_na_drop <- z_n_clean
# remove NA increments
z_n_clean <- na.omit(z_n_clean)

# outlier_df <- tibble(
#   id     = seq_along(outlier_periods_to_remove),
#   period = outlier_periods_to_remove
# ) %>%
#   tidyr::separate(period, into = c("start", "end"), sep = "/", convert = TRUE) %>%
#   mutate(
#     start = as.Date(start),
#     end   = as.Date(end),
#     mid   = start + floor(as.numeric(end - start) / 2),
#     label = as.character(id)
#   )
#
# df <- tibble(
#   date  = index(IXIC_ad),
#   price = as.numeric(IXIC_ad),
#   z_n   = as.numeric(z_n)
# ) %>%
#   drop_na(z_n)
#
# range_p1 <- range(df$price, na.rm = TRUE)
# range_p2 <- range(df$z_n,   na.rm = TRUE)
#
# label_y_p1 <- range_p1[2] - 0.08 * diff(range_p1)
# label_y_p2 <- range_p2[2] - 0.02 * diff(range_p2)
# # label_y_p1 <- label_y_p1*1.02
# # label_y_p2 <- label_y_p2*1.2
#
# base_theme <- theme_bw() +
#   theme(
#     plot.title  = element_text(hjust = 0.5),
#     plot.margin = margin(5.5, 5.5, 5.5, 5.5)
#   )
#
# p1 <- ggplot(df, aes(date, price)) +
#   geom_line() +
#   labs(
#     title = "NASDAQ Composite\n(Adjusted Close)",
#     x = "Date", y = "Adjusted Close ($)"
#   ) +
#   base_theme +
#   # vertical dotted lines
#   geom_vline(
#     data = outlier_df,
#     aes(xintercept = start),
#     linetype = "dotted"
#   ) +
#   geom_vline(
#     data = outlier_df,
#     aes(xintercept = end),
#     linetype = "dotted"
#   ) +
#   # circles
#   geom_point(
#     data = outlier_df,
#     aes(x = mid, y = label_y_p1),
#     inherit.aes = FALSE,
#     shape = 21,
#     fill  = "white",
#     color = "black",
#     size  = 6,
#     stroke = 0.6
#   ) +
#   # numbers
#   geom_text(
#     data = outlier_df,
#     aes(x = mid, y = label_y_p1, label = label),
#     inherit.aes = FALSE,
#     vjust = 0.35,
#     size = 3
#   ) #+
#   #scale_y_continuous(expand = expansion(mult = c(0.02, 0.02)))
#
# p2 <- ggplot(df, aes(date, z_n)) +
#   geom_line() +
#   labs(
#     title = expression(Log~Return~Increment~(z[n])),
#     x = "Date",
#     y = expression(z[n])
#   ) +
#   base_theme +
#   geom_vline(
#     data = outlier_df,
#     aes(xintercept = start),
#     linetype = "dotted"
#   ) +
#   geom_vline(
#     data = outlier_df,
#     aes(xintercept = end),
#     linetype = "dotted"
#   ) #+
#   # geom_point(
#   #   data = outlier_df,
#   #   aes(x = mid, y = label_y_p2),
#   #   inherit.aes = FALSE,
#   #   shape = 21,
#   #   fill  = "white",
#   #   color = "black",
#   #   size  = 6,
#   #   stroke = 0.6
#   # ) +
#   # geom_text(
#   #   data = outlier_df,
#   #   aes(x = mid, y = label_y_p2, label = label),
#   #   inherit.aes = FALSE,
#   #   vjust = 0.35,
#   #   size = 3
#   # ) #+
#   #scale_y_continuous(expand = expansion(mult = c(0.02, 0.02)))
#
# (p1 / p2)

# # We can plot a quick visualisation to compare pre & post outlier removal:
# df_raw <- tibble(
#   date = index(z_n),
#   z_n  = as.numeric(z_n)
# )
# df_clean <- tibble(
#   date = index(z_n_clean_pre_na_drop),
#   z_n  = as.numeric(z_n_clean_pre_na_drop)
# )
# ylim_all <- range(df_raw$z_n, df_clean$z_n, na.rm = TRUE)
#
#
# p_raw <- ggplot(df_raw, aes(date, z_n)) +
#   geom_line() +
#   theme_bw() +
#   scale_y_continuous(limits = ylim_all) +
#   labs(
#     title = "Log Return Increments\n(pre exclusion)",
#     x = "Date",
#     y = expression(z[n])
#   )
#
# p_clean <- ggplot(df_clean, aes(date, z_n)) +
#   geom_line() +
#   theme_bw() +
#   scale_y_continuous(limits = ylim_all) +
#   labs(
#     title = "Log Return Increments\n(post exclusion)",
#     x = "Date",
#     y = expression(z[n])
#   )
#
# (p_raw | p_clean) &
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     plot.margin = margin(5.5, 5.5, 5.5, 5.5)
#   )
#

#----------------------------- Element 1 - Table ------------------------------#
# Skewness and excess kurtosis of the cleaned log-returns $z_n$ were computed
# using the `e1071` package in R with type = 1 [@R-e1071]. This corresponds to
# the classical moment-based definitions $m_3/m_2^{3/2}$ and $m_4/m_2{^2} - 3$
# described by @joanes1998, and used in SMM047 2025-26
# [@gerrard_seg2_notes_questions]. These measures are widely used in textbooks
# and provide a direct description of the shape of the empirical distribution.
# Alternative implementations, such as the package `summarytools`, were
# considered. This includes applying a apply small-sample bias corrections
# (“type 3” estimators), which lead to slightly different values but are
# asymptotically equivalent. Given our relatively large sample size and
# descriptive aim, the type 1 definitions are appropriate and fully adequate.

# e1071::skewness(z_n_clean, type = 1)
z_n_clean_numeric <- as.numeric(z_n_clean)

z_n_mean <- mean(z_n_clean_numeric)
z_n_vari <- var(z_n_clean_numeric)
z_n_skew <- e1071::skewness(z_n_clean_numeric, type = 1)
z_n_kurt <- e1071::kurtosis(z_n_clean_numeric, type = 1)

desc_z_n_clean <- tibble(
  Statistic = c(
    "Sample Mean",
    "Sample Variance",
    "Skewness",
    "Excess Kurtosis"
  ),
  Value = c(
    z_n_mean,
    z_n_vari,
    z_n_skew,
    z_n_kurt
  )
) |>
  mutate(Value = sprintf("%.6f", Value))

#- Element 1 - (bonus) Table with type 2 & 3 estimators (not used in report) --#
# BE note: changed to include = FALSE on 19/11/2025
z_n_clean_numeric <- as.numeric(z_n_clean)

desc_ad <- tibble(
  Statistic = c(
    "Sample Mean",
    "Sample Variance",
    "Skewness (Type 1)",
    "Skewness (Type 2)",
    "Skewness (Type 3)",
    "Excess Kurtosis (Type 1)",
    "Excess Kurtosis (Type 2)",
    "Excess Kurtosis (Type 3)"
  ),
  Value = c(
    mean(z_n_clean_numeric),
    var(z_n_clean_numeric),
    e1071::skewness(z_n_clean_numeric, type = 1),
    e1071::skewness(z_n_clean_numeric, type = 2),
    e1071::skewness(z_n_clean_numeric, type = 3),
    # already excess kurtosis for type = 1
    e1071::kurtosis(z_n_clean_numeric, type = 1),
    e1071::kurtosis(z_n_clean_numeric, type = 2),
    e1071::kurtosis(z_n_clean_numeric, type = 3)
  )
) |>
  mutate(Value = sprintf("%.6f", Value))
kable(
  desc_ad,
  caption = "Additional descriptive statistics of cleaned log returns ($z_n$).",
  booktabs = TRUE,
  align = c("l", "r"),
  escape = FALSE
) |>
  kable_styling(full_width = FALSE, position = "center")

ad_test_result <- ad.test(z_n_clean)
ad_test_result

kable(
  desc_z_n_clean,
  # caption = "Descriptive statistics of cleaned log returns ($z_n$).",
  caption = paste0(
    "Descriptive statistics of cleaned Nasdaq log-returns (n = ",
    format(length(z_n_clean), big.mark = ","),
    "). Skewness and kurtosis were calculated using Type 1 estimators. Given the large sample size, differences compared to bias-corrected (Type 2 or 3) estimators are negligible."
  ),
  booktabs = TRUE,
  align = c("l", "r"),
  escape = FALSE
) |>
  kable_styling(
    full_width = FALSE,
    position = "center",
    latex_options = "hold_position"
  )

knitr::include_graphics("fig/timeline2.pdf")

#------------ Element 1 - Figure showing data pre & post-exclusion ------------#
trading_period_days <- 20

# trading period rolling log-return volatility (standard deviation)
el1_rolling_sd <- rollapply(
  z_n,
  width = trading_period_days,
  FUN   = sd,
  align = "right",
  fill  = NA
) # * sqrt(252)
# BE note: have not converted to annualised here - saving for section 3/4
# It's challenging to convey this within the text limits for element 1.  Opted
# to introduce later mathematically where we have a bit more space

# periods to remove
outlier_df <- tibble(
  id     = seq_along(outlier_periods_to_remove),
  period = outlier_periods_to_remove
) %>%
  tidyr::separate(period, into = c("start", "end"), sep = "/", convert = TRUE) %>%
  mutate(
    start = as.Date(start),
    end   = as.Date(end),
    mid   = start + floor(as.numeric(end - start) / 2),
    label = as.character(id)
  )
# outliner dotted lines for all plots
outlier_lines <- list(
  geom_vline(
    data = outlier_df,
    aes(xintercept = start),
    linetype = "dotted",
    color = "red" # Added color here
  ),
  geom_vline(
    data = outlier_df,
    aes(xintercept = end),
    linetype = "dotted",
    color = "red" # Added color here
  )
)
# testing using a shaded rectangle instead
outlier_shading <- geom_rect(
  data = outlier_df,
  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
  fill = "red",
  alpha = 0.2, # Makes it transparent
  inherit.aes = FALSE # Prevents conflicts with main plot aesthetics
)

# align price with z_n
IXIC_price_aligned <- IXIC_ad[index(z_n)]

# merge into one xts object (inner join on dates)
all_xts <- merge(
  IXIC_price_aligned,
  z_n,
  el1_rolling_sd
)
# join = "inner"

colnames(all_xts) <- c("price", "z_n", "rolling_sd")

# convert to tibble
df <- tibble(
  date       = index(all_xts),
  price      = as.numeric(all_xts$price),
  z_n        = as.numeric(all_xts$z_n),
  rolling_sd = as.numeric(all_xts$rolling_sd)
)

range_p1 <- range(df$price, na.rm = TRUE)
range_p2 <- range(df$z_n, na.rm = TRUE)

label_y_p1 <- range_p1[2] - 0.08 * diff(range_p1)
label_y_p2 <- range_p2[2] - 0.02 * diff(range_p2)

base_theme <- theme_bw() +
  theme(
    plot.title  = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)
  )

# initial visualisation p1: adjusted close with marked periods
initial_vis_p1 <- ggplot(df, aes(date, price)) +
  geom_line() +
  labs(
    title = "Nasdaq Composite (Adjusted Close)",
    x = NULL,
    y = "Adjusted close (USD)"
  ) +
  base_theme +
  outlier_shading +
  geom_point(
    data = outlier_df,
    aes(x = mid, y = label_y_p1),
    inherit.aes = FALSE,
    shape = 21,
    fill = "white",
    color = "black",
    size = 6,
    stroke = 0.6
  ) +
  geom_text(
    data = outlier_df,
    aes(x = mid, y = label_y_p1, label = label),
    inherit.aes = FALSE,
    vjust = 0.35,
    size = 3
  )

# initial visualisation p2: raw (pre-exclusion) daily log-return increments
initial_vis_p2 <- ggplot(df, aes(date, z_n)) +
  geom_line() +
  labs(
    title = "Daily Log-return Increments",
    x = NULL,
    y = expression(z[n])
  ) +
  base_theme +
  outlier_shading

# initial_vis p3: 20-day rolling log-return volatility (SD)
initial_vis_p3 <- ggplot(df, aes(date, rolling_sd)) +
  geom_line(na.rm = TRUE) +
  labs(
    title = paste0(
      "Nasdaq Composite Volatility (",
      trading_period_days,
      "-day rolling window)"
    ),
    x = NULL,
    y = bquote(s[n] ~ "(" * .(trading_period_days) * "-day rolling)")
  ) +
  base_theme +
  outlier_shading

# initial_vis_ p4: cleaned log-return series
df_clean_raw <- tibble(
  date = index(z_n_clean),
  z_n  = as.numeric(z_n_clean)
)

# Ensuring removed dates have z_n = NA, creating gap/gaps in the line
df_clean <- df %>%
  select(date) %>%
  left_join(df_clean_raw, by = "date")

# align y lim
ylim_all <- range(df$z_n, df_clean$z_n, na.rm = TRUE)

initial_vis_p4 <- ggplot(df_clean, aes(date, z_n)) +
  geom_line() +
  base_theme +
  scale_y_continuous(limits = ylim_all) +
  labs(
    title = "Daily Log-return Increments (Post Exclusion)",
    x = "Date",
    y = expression(z[n])
  )

# 4-panel figure
(initial_vis_p1 / initial_vis_p3 / initial_vis_p2 / initial_vis_p4)

# ==============================================================================
#------------ Element 2: investigation of normality by resampling -------------#
# ==============================================================================
# set random seed
set.seed(1234)

# set parameters from z_data (mean & st-dev)
z_mean <- mean(z_n_clean)
z_sd <- sd(z_n_clean)
N_reps <- 50000
n_sample_size <- 13

# Simulate 50,000 skewness values using sample size of 13
#   Generate 13 values normally distributed with (z_mean, z_sd)
#   Calculate skewness of these 13 values
#   Repeat process 50,000 times
#   Tells one how much noise one would expect given this small sample size (13)
skew_normal_sim <- replicate(
  N_reps, e1071::skewness(rnorm(n_sample_size, z_mean, z_sd), type = 1)
)

# bootstrapping skewness simulation
#   Pick 13 values from z_n_clean (with replacement)
#   Calculate skewness of these 13 values
#   Repeat process 50,000 times
#   Looking at skewness if we just take 13 random days of data
skew_bootstrap <- replicate(
  N_reps, e1071::skewness(sample(z_n_clean, n_sample_size, replace = TRUE), type = 1)
)

#------------ basic
skew_normal_sim_median <- median(skew_normal_sim)
skew_normal_bootstrap <- median(skew_bootstrap)
sd_normal_sim <- sd(skew_normal_sim)
sd_normal_boo <- sd(skew_bootstrap)
el2_original_skew <- e1071::skewness(z_n_clean, type = 1)
el2_bias <- mean(skew_bootstrap) - el2_original_skew

#------------ statistics/metrics
# calculating quantile based confidence interval
ci_95_skew_zn_boot_quantile <- quantile(skew_bootstrap, probs = c(0.025, 0.975))
ci_95_skew_normals_quantile <- quantile(skew_normal_sim, probs = c(0.025, 0.975))
#
# print(ci_95_skew_zn_boot_quantile)
# print(ci_95_skew_normals_quantile)


# Using open source: https://gitlab.com/scottkosty/bootstrap/-/blob/master/R/bcanon.R
#
# Takes a vector of observations x, the number of bootstrap samples to take,
# an estimator plus additional parameters for it, and confidence levels for
# the output intervals
"bcanon" <- function(x, nboot, theta, ..., alpha =
                       c(.025, .05, .1, .16, .84, .9, .95, .975)) {
  if (!all(alpha < 1) || !all(alpha > 0)) {
    stop("All elements of alpha must be in (0,1)")
  }

  # NB. these lines check that nboot > (1 / alpha) and because otherwise
  # you need more samples to get a somewhat useful confidence interval.
  alpha_sorted <- sort(alpha)
  if (nboot <= 1 / min(alpha_sorted[1], 1 - alpha_sorted[length(alpha_sorted)])) {
    warning("nboot is not large enough to estimate your chosen alpha.")
  }

  # unrelated to the actual bootstrapping
  call <- match.call()

  # compute theta(x) of the samples and resample the data nboot times
  n <- length(x)
  thetahat <- theta(x, ...)
  bootsam <- matrix(sample(x, size = n * nboot, replace = TRUE), nrow = nboot)

  # compute theta for each sample and compute the quartile of the fraction
  # below our original estimate under a normal distribution
  thetastar <- apply(bootsam, 1, theta, ...)
  z0 <- qnorm(sum(thetastar < thetahat) / nboot)

  # get a jackknife estimate for theta to compute the acceleration factor
  u <- rep(0, n)
  for (i in 1:n) {
    u[i] <- theta(x[-i], ...)
  }
  uu <- mean(u) - u
  acc <- sum(uu * uu * uu) / (6 * (sum(uu * uu))^1.5)

  # compute the actual distribution that we are taking the quantiles of to
  # create the confidence interval
  zalpha <- qnorm(alpha)

  tt <- pnorm(z0 + (z0 + zalpha) / (1 - acc * (z0 + zalpha)))

  confpoints <- quantile(x = thetastar, probs = tt, type = 1)

  # and now just some logic for outputting it
  names(confpoints) <- NULL
  confpoints <- cbind(alpha, confpoints)
  dimnames(confpoints)[[2]] <- c("alpha", "bca point")
  return(list(
    confpoints = confpoints,
    z0 = z0,
    acc = acc,
    u = u,
    call = call
  ))
}


#------------ BCa interval for skewness using bcanon
# bca_skew <- bcanon(
#   x     = z_n_clean,
#   nboot = N_reps,
#   alpha = c(.025,.975),
#   theta = e1071::skewness,
#   type  = 1
# )
#
# # calculating quantile based confidence interval
# print(ci_95_skew_zn_boot_quantile)
# print(ci_95_skew_normals_quantile)
#
# alpha and corresponding BCa points
# print(bca_skew$confpoints)

# quantile(skew_boot_full, c(0.025, 0.975))

#------------ 95% CIs for comparison ----------

# Normal-based parametric CI via simulation, n = 13
el2_ci_normal_95 <- quantile(skew_normal_sim, probs = c(0.025, 0.975))

# Bootstrap percentile CI from empirical data, n = 13
el2_ci_boot_95 <- quantile(skew_bootstrap, probs = c(0.025, 0.975))

# (bonus) bootstrap 9% CI for skewness using full data, n = ~1737
# using standard percentile
skew_boot_full <- replicate(
  N_reps,
  e1071::skewness(sample(z_n_clean, length(z_n_clean), replace = TRUE), type = 1)
)
el2_ci_boot_full_perc <- quantile(skew_boot_full, probs = c(0.025, 0.975))

# using bcanon BCa
bca_skew <- bcanon(
  x     = z_n_clean,
  nboot = N_reps,
  theta = e1071::skewness,
  type  = 1,
  alpha = c(0.025, 0.975)
)
el2_ci_boot_full_bca <- bca_skew$confpoints[, "bca point"]

ci_table <- data.frame(
  Dataset = c(
    "Small Sample (n=13)",
    "Small Sample (n=13)",
    "Full Data (n=1737)",
    "Full Data (n=1737)"
  ),
  Method = c(
    "Simulated Normal (Control)",
    "Bootstrap Percentile",
    "Bootstrap Percentile",
    "BCa Bootstrap (Robust)"
  ),
  Lower = c(
    el2_ci_normal_95[1],
    el2_ci_boot_95[1],
    el2_ci_boot_full_perc[1],
    el2_ci_boot_full_bca[1]
  ),
  Upper = c(
    el2_ci_normal_95[2],
    el2_ci_boot_95[2],
    el2_ci_boot_full_perc[2],
    el2_ci_boot_full_bca[2]
  )
)

ci_table_2_digits <- 2
ci_table_2 <- data.frame(
  Dataset = c(
    "Small Sample",
    "Small Sample",
    "Full Data",
    "Full Data"
  ),
  Method = c(
    "Simulated Normal",
    "Bootstrap Percentile",
    "Bootstrap Percentile",
    "Bootstrap BCa"
  ),
  N = c(
    n_sample_size,
    n_sample_size,
    length(z_n_clean),
    length(z_n_clean)
  ),
  `95 percent CI` = c(
    format_interval(el2_ci_normal_95[1], el2_ci_normal_95[2], ci_table_2_digits),
    format_interval(el2_ci_boot_95[1], el2_ci_boot_95[2], ci_table_2_digits),
    format_interval(el2_ci_boot_full_perc[1], el2_ci_boot_full_perc[2], ci_table_2_digits),
    format_interval(el2_ci_boot_full_bca[1], el2_ci_boot_full_bca[2], ci_table_2_digits)
  ),
  check.names = FALSE
)


# bootstrapping skewness simulation
skew_df <- data.frame(
  skewness = c(skew_normal_sim, skew_bootstrap),
  Source = factor(
    rep(c("Normal model", "Bootstrap from data"),
      each = N_reps
    )
  )
)

ggplot(skew_df, aes(x = skewness, fill = Source)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = expression("Sampling distributions of " * hat(gamma)[1] *
      " for n = 13"),
    x = expression(hat(gamma)[1]),
    y = "Density"
  )

# kable(
#   ci_table,
#   caption = "Comparison of 95% Confidence Intervals for skewness across sample sizes and methods. The ",
#   booktabs = TRUE,
#   align = c("l", rep("r", 3)),
#   digits = 3,
#   row.names = FALSE,
#   escape = FALSE
# ) |>
#   add_header_above(
#     c(" " = 2, "95 percent CI" = 2),
#     escape = FALSE
#   ) |>
#   kable_styling(full_width = FALSE, position = "center")

kable(
  ci_table_2,
  caption = "Comparison of 95 percent confidence intervals for skewness across sample sizes and methods. The simulated normal sample is a baseline control derived from a theoretical normal distribution with a small sampling size, the bootstrap percentile intervals are calculated directly from the empirical 2.5th and 97.5th quantiles, while the Bootstrap BCa interval employs bias correction and acceleration to adjust for the inherent asymmetry of the estimator, offering the most robust metric for the full dataset.",
  booktabs = TRUE,
  align = c("l", "l", "r", "l"),
  digits = 3,
  row.names = FALSE,
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    latex_options = "hold_position"
  )

# ==============================================================================
#----------------- Element 3: investigation of constant mean ------------------#
# ==============================================================================

# # ---------------- attempt 01: using flexible 6 month windows
# # split z data into 14 subsamples
# z_list <- split(z_n_clean, f = "months", k = 6)
#
# # drop values if all or most of 1 6M period have been removed
# # BE note: have just removed fully blank values, but need to tweak this
# z_list <- z_list[lengths(z_list) > 0]

# ---------------- attempt 02: new (using hard 6 month cut-offs)

# ensure we have dates
dates <- index(z_n_clean)

# start - first 6-month block that contains the first date
first_date <- min(dates)
fy <- as.integer(format(first_date, "%Y"))
fm <- as.integer(format(first_date, "%m"))
start0 <- if (fm <= 6) {
  as.Date(sprintf("%d-01-01", fy))
} else {
  as.Date(sprintf("%d-07-01", fy))
}

# end - first boundary after the last date
last_date <- max(dates)
ly <- as.integer(format(last_date, "%Y"))
lm <- as.integer(format(last_date, "%m"))
end_next <- if (lm <= 6) {
  as.Date(sprintf("%d-07-01", ly))
} else {
  as.Date(sprintf("%d-01-01", ly + 1))
}

# fixed 6-month breakpoints
breaks <- seq(start0, end_next, by = "6 months")

# nice labels "01 Jan 2018 - 30 Jun 2018" etc.
start_labels <- breaks[-length(breaks)]
end_labels <- breaks[-1] - 1

period_labels <- paste(
  format(start_labels, "%b %Y"),
  "-",
  format(end_labels, "%b %Y")
)

# assign each observation to a 6-month bin (right-open [start, end])
Period <- cut(
  dates,
  breaks = breaks,
  right = FALSE,
  labels = period_labels,
  include.lowest = TRUE
)

# create list of subsamples
z_list <- split(z_n_clean, Period)

# drop empty periods (where cleaning removed all data)
z_list <- z_list[lengths(z_list) > 0]

combined_df_el3 <- bind_rows(lapply(seq_along(z_list), function(i) {
  data.frame(
    Period  = factor(names(z_list)[i], levels = names(z_list)),
    z_value = as.numeric(z_list[[i]])
  )
}))

#---------- Element 4: check & count observations in 6-month periods ----------#
# Count observations in each 6-month period
days_per_period <- sapply(z_list, NROW)

period_labels <- names(days_per_period)

# split into start/end strings
start_dates <- sub(" -.*", "", period_labels)
end_dates <- sub(".*- ", "", period_labels)

days_table <- data.frame(
  `Start date` = start_dates,
  `End date` = end_dates,
  `Number of data entries` = as.integer(days_per_period),
  check.names = FALSE
)

kruskal_res <- kruskal.test(z_value ~ Period, data = combined_df_el3)

kruskal_df <- data.frame(
  Test = "Kruskal–Wallis rank sum test",
  Df = kruskal_res$parameter,
  Statistic = unname(kruskal_res$statistic),
  p.value = kruskal_res$p.value
)

kruskal_df$Statistic <- sprintf("%.3f", kruskal_df$Statistic)
kruskal_df$p.value <- formatC(kruskal_df$p.value, format = "fg", digits = 4)
# The interpretation of the Kruskal-Wallis test depends on the homogeneity of
# variance across groups. We test assumption of constant variance in
# \S\@ref(elementFou) so not safe (I think) to assume it here.
# We instead rely on the strict assumption that the distributions share
# identical shapes. The Kruskal-Wallis test should therefore be interpreted in
# its most general form
# -> assessing differences in mean ranks (stochastic dominance) rather than a
# whether the medians are equal
#
# Stochastic dominance:
# If Group A stochastically dominates Group B, it implies that if you picked
# one random number from A and one random number from B, the number from A
# is likely to be larger than B more than 50% of the time.
# tldr; because some boxplots are fat (high variance) and some are thin, we
# can't just talk about the median - instead want to say something like:
# "This group generally tends to produce higher numbers than that group"

# kable(
#   kruskal_df,
#   caption = "Kruskal–Wallis test outcome for differences in log-return distributions across six-month periods.",
#   booktabs = TRUE,
#   align = c("l", "r", "r", "r"),
#   row.names = FALSE
# ) |>
#   kable_styling(full_width = FALSE, position = "center")

ggplot(combined_df_el3, aes(x = Period, y = z_value)) +
  geom_boxplot() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Log-returns over six-month periods",
    x = "Six-month period",
    y = expression(z[n])
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

kable(
  days_table,
  col.names = c(
    "Start date",
    "End date",
    "Number of data points post cleaning"
  ),
  caption = "Number of log-return data points in each six-month period.",
  booktabs = TRUE,
  longtable = TRUE,
  linesep = c("", "\\addlinespace"),
  align = c("r", "r", "c")
) |>
  kable_styling(
    full_width = FALSE,
    position = "center",
    latex_options = c("repeat_header"),
    repeat_header_text = "\textit{(continued)}",
    repeat_header_method = "append",
  )

# ==============================================================================
#--------------- Element 4: investigation of constant variance ----------------#
# ==============================================================================

variances <- sapply(z_list, var, na.rm = TRUE)
min_var_loc <- which.min(variances)
max_var_loc <- which.max(variances)

# set up variance dataframe
var_df <- data.frame(
  Period   = factor(names(z_list), levels = names(z_list)),
  Variance = as.numeric(variances)
)
var_df_annual <- data.frame(
  Period   = factor(names(z_list), levels = names(z_list)),
  Variance = as.numeric(variances) * 252
)

# !changed to *252: extract the subsamples as numeric vectors & drop NA
min_var_sample <- na.omit(as.numeric(z_list[[min_var_loc]])) * 252
max_var_sample <- na.omit(as.numeric(z_list[[max_var_loc]])) * 252
min_var_sample <- na.omit(as.numeric(z_list[[min_var_loc]]))
max_var_sample <- na.omit(as.numeric(z_list[[max_var_loc]]))

# create bootstrap statistic function to get sample variance of resampled data
boot_var <- function(data, indices) {
  var(data[indices])
}

# run separate bootstrapping for both subsamples
boot_var_min <- boot(
  data      = min_var_sample,
  statistic = boot_var,
  R         = 5000
)

boot_var_max <- boot(
  data      = max_var_sample,
  statistic = boot_var,
  R         = 5000
)

# get CI using Bias-Correct and Accelerated (bca) interval
# (this adjusts for both bias and skewness in bootstrap distribution)
var_ci_min_95 <- boot.ci(boot_var_min, type = "bca", conf = 0.95)
var_ci_max_95 <- boot.ci(boot_var_max, type = "bca", conf = 0.95)
# 99% CI for interest
var_ci_min_99 <- boot.ci(boot_var_min, type = "bca", conf = 0.99)
var_ci_max_99 <- boot.ci(boot_var_max, type = "bca", conf = 0.99)

# Period labels for min/max variance subsamples
min_period <- names(z_list)[min_var_loc]
max_period <- names(z_list)[max_var_loc]

# Extract BCa 95% CI limits from boot.ci output
# (bca columns: conf, lower, upper, etc. We want the low & high limits.)
var_ci_min_bca <- var_ci_min_95$bca[4:5]
var_ci_max_bca <- var_ci_max_95$bca[4:5]

# getting end date
# start date of each block
start_dates <- as.Date(sapply(z_list, function(x) index(x)[1]))

# last observed date in each block
last_dates <- as.Date(sapply(z_list, function(x) index(x)[NROW(x)]))

# end date of each block:
# - for all but last: day before next block's start
# - for last: its own last date
end_dates <- c(start_dates[-1] - 1, last_dates[length(last_dates)])
period_labels <- paste(
  base::format(start_dates, "%b %Y"),
  "-",
  base::format(end_dates, "%b %Y")
)

var_df <- data.frame(
  Period   = factor(period_labels, levels = period_labels),
  Variance = as.numeric(variances)
)

min_period_label <- period_labels[min_var_loc]
max_period_label <- period_labels[max_var_loc]


# Daily variance per period, then annualise
variances_daily <- sapply(z_list, var, na.rm = TRUE)
variances_ann <- variances_daily * 252

min_var_loc <- which.min(variances_ann)
max_var_loc <- which.max(variances_ann)

# Period labels
start_dates <- as.Date(sapply(z_list, function(x) index(x)[1]))
last_dates <- as.Date(sapply(z_list, function(x) index(x)[NROW(x)]))
end_dates <- c(start_dates[-1] - 1, last_dates[length(last_dates)])

period_labels <- paste(
  base::format(start_dates, "%b %Y"), "-",
  base::format(end_dates, "%b %Y")
)

# min/max periods (still daily reforms)
min_var_sample <- na.omit(as.numeric(z_list[[min_var_loc]]))
max_var_sample <- na.omit(as.numeric(z_list[[max_var_loc]]))

# multiply bu 252
var_ann_stat <- function(x, idx) var(x[idx]) * 252

# iid bootstrap
boot_min <- boot(data = min_var_sample, statistic = var_ann_stat, R = 5000)
boot_max <- boot(data = max_var_sample, statistic = var_ann_stat, R = 5000)

# BCa 95% CIs
ci_min_95 <- boot.ci(boot_min, type = "bca", conf = 0.95)$bca[4:5]
ci_max_95 <- boot.ci(boot_max, type = "bca", conf = 0.95)$bca[4:5]

# annualised point estimates
s2_min_ann <- var(min_var_sample) * 252
s2_max_ann <- var(max_var_sample) * 252

min_period_label <- period_labels[min_var_loc]
max_period_label <- period_labels[max_var_loc]

# Bootstrapping confidence intervals for $\sigma^2$
# Create summary table
var_ci_table <- data.frame(
  Subsample = c("Min variance", "Max variance"),
  Period = c(min_period_label, max_period_label),
  s2 = c(
    var(min_var_sample),
    var(max_var_sample)
  ),
  CI_lower = c(
    var_ci_min_bca[1],
    var_ci_max_bca[1]
  ),
  CI_upper = c(
    var_ci_min_bca[2],
    var_ci_max_bca[2]
  )
)

fmt <- function(x) formatC(x, format = "f", digits = 6)

var_ci_table_disp <- var_ci_table %>%
  mutate(
    s2       = paste0("$", fmt(s2), "$"),
    CI_lower = paste0("$", fmt(CI_lower), "$"),
    CI_upper = paste0("$", fmt(CI_upper), "$")
  )

# kable(
#   var_ci_table,
#   col.names = c(
#     "Subsample",
#     "Six-month period",
#     "Sample variance",
#     "95\\% CI lower",
#     "95\\% CI upper"
#   ),
#   booktabs = TRUE,
#   align = c("l", "l", "r", "r", "r"),
#   escape = FALSE,
#   digits = 6,
#   caption = "Bootstrap bias-corrected and accelerated 95\\% confidence intervals for the sample variance $s^2$ in the six-month periods with minimum and maximum sample variance."
# ) %>%
#   kable_styling(full_width = FALSE, position = "center")

# kable(
#   var_ci_table,
#   caption = "Bootstrap BCa 95\\% confidence intervals for the variance $s^2$ in the six-month periods with the smallest and largest sample variance of log-returns.",
#   booktabs = TRUE,
#   align = c("l", "r", "r", "r"),
#   escape = FALSE
# ) |>
#   kable_styling(full_width = FALSE, position = "center")

var_ci_table <- data.frame(
  Subsample = c("Min variance", "Max variance"),
  Period    = c(min_period_label, max_period_label),
  s2        = c(s2_min_ann, s2_max_ann),
  CI_lower  = c(ci_min_95[1], ci_max_95[1]),
  CI_upper  = c(ci_min_95[2], ci_max_95[2])
)

fmt <- function(x) formatC(x, format = "f", digits = 6)

var_ci_table_disp <- var_ci_table |>
  dplyr::mutate(
    s2       = paste0("$", fmt(s2), "$"),
    CI_lower = paste0("$", fmt(CI_lower), "$"),
    CI_upper = paste0("$", fmt(CI_upper), "$")
  )

# kable(
#   var_ci_table,
#   col.names = c("Subsample", "Six-month period",
#                 "Annualised variance $s^2$ (×252)",
#                 "95\\% CI lower", "95\\% CI upper"),
#   booktabs = TRUE,
#   align = c("l","l","r","r","r"),
#   escape = FALSE,
#   digits = 3,
#   caption = "BCa 95\\% confidence intervals for annualised variance in the six-month periods with minimum and maximum variance."
# ) |>
#   kable_styling(full_width = FALSE, position = "center")

# replotting box plot from investigation of constant mean above new variance bar
# chart

# boxplot
p_box_el4 <- ggplot(combined_df_el3, aes(x = Period, y = z_value)) +
  geom_boxplot(width = 0.75, outlier.size = 0.7, linewidth = 0.3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Daily Nasdaq log-returns per six-month period from 2018-2024",
    x = NULL,
    y = expression(z[n])
  ) +
  theme(
    # panel.grid = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
  )

p_var_el4 <- ggplot(var_df_annual, aes(x = Period, y = Variance, fill = Period)) +
  # scale_fill_viridis_d() +
  geom_col(fill = "white", colour = "black", width = 0.75, linewidth = 0.3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Annualised Nasdaq log-return variance per six-month period from 2018-2024",
    x = "Six-month period",
    y = expression(Annualised ~ s[n]^2)
  ) +
  # scale_y_continuous(limits = c(0, 5e-04), expand = c(0,0)) +
  theme(
    # panel.grid = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
  )

# Stack vertically with aligned x-axis
(p_box_el4 / p_var_el4) +
  plot_layout(heights = c(2, 3))

final_table_df <- data.frame(
  Subsample = c("Min variance", "Max variance"),
  Period = c(min_period_label, max_period_label),
  s2_ann = c(s2_min_ann, s2_max_ann),
  ci = c(
    format_interval(ci_min_95[1], ci_min_95[2], 3),
    format_interval(ci_max_95[1], ci_max_95[2], 3)
  )
)

kable(
  final_table_df,
  col.names = c(
    "Subsample",
    "Six-month period",
    "Point Estimate",
    "95% CI"
  ),
  booktabs = TRUE,
  digits = 3,
  align = c("l", "l", "r", "c"),
  caption = "Comparison of annualised variance for the lowest and highest volatility periods. Ninety-five percent CIs were calculated using BCa bootstrapping (N=5,000)."
) %>%
  add_header_above(c(" " = 2, "Annualised Variance" = 2)) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    latex_options = "hold_position"
  )

# ==============================================================================
#------------------- Element 5: independence of increments --------------------#
# ==============================================================================

# Create a $z_{n-1}$ series
z_lag <- stats::lag(z_n_clean, k = 1)

# Align $z_n$ and $z_{n-1}$ pairs by merging the xts objects
# inner join to remove the NA value created by the lag
z_merged <- merge(z_n_clean, z_lag, join = "inner")
colnames(z_merged) <- c("z_n", "z_n_minus_1")

# Categorise by quartile
# calculate quartiles from whole z_n_clean dataset
q <- quantile(z_n_clean, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# create categorical variables
breaks <- c(-Inf, q, Inf)
labels <- c("Q1", "Q2", "Q3", "Q4")
z_n_cat <- cut(z_merged$z_n, breaks = breaks, labels = labels)
z_n_minus_1_cat <- cut(z_merged$z_n_minus_1, breaks = breaks, labels = labels)

# contingency
# rows $z_{n-1}$, columns $z_n$
con_table <- table(z_n_minus_1_cat, z_n_cat)

# statistical test (want Chi-squared, but need to see if cell frequencies are >5)
chisq_result <- chisq.test(con_table)
# chisq_result
# chisq_result$expected

# If any expected counts are low (R will often produce a warning),
# the $\chi^2$ p-value is unreliable may then want to use Fisher's Exact Test
if (any(chisq_result$expected < 5)) {
  fisher_result <- fisher.test(con_table)
  fisher_result
}

# Create dataframe for the definitions
quartile_def <- data.frame(
  Category = c("Q1", "Q2", "Q3", "Q4"),
  Range = c(
    "$z_n \\le Q_1$",
    "$Q_1 < z_n \\le Q_2$",
    "$Q_2 < z_n \\le Q_3$",
    "$z_n > Q_3$"
  )
)

kable(
  quartile_def,
  col.names = c("Category", "Mathematical Range"),
  align = c("c", "c", "l"),
  escape = FALSE,
  booktabs = TRUE,
  caption = "Classification of log-returns into discrete categories based on sample quartiles."
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    latex_options = "hold_position"
  )

# # get expected counts
# expected_matrix <- chisq_result$expected
#
# expected_df <- as.data.frame.matrix(expected_matrix)
# expected_df <- tibble::tibble(
#   `$z_{n-1}$ quartile` = rownames(expected_matrix),
#   expected_df
# )
#
# kable(
#   expected_df,
#   caption = "Expected cell frequencies under the null hypothesis of serial independence.",
#   booktabs = TRUE,
#   digits = 1,
#   align = c("l", rep("r", 4)),
#   escape = FALSE
# ) |>
#   add_header_above(
#     c(" " = 1, "$z_n$ quartile" = 4),
#     escape = FALSE
#   ) |>
#   kable_styling(
#     full_width = FALSE,
#     position = "center",
#     latex_options = "hold_position"
#     )

con_table_df <- as.data.frame.matrix(con_table)

con_table_df <- tibble::tibble(
  `$z_{n-1}$ quartile` = rownames(con_table),
  con_table_df
)

kable(
  con_table_df,
  caption = "Contingency table of quartile classifications for lagged log-returns $z_{n-1}$ (rows) and current log-returns $z_n$ (columns).",
  booktabs = TRUE,
  align = c("l", rep("r", 4)),
  escape = FALSE
) |>
  add_header_above(
    c(" " = 1, "$z_n$ quartile" = 4),
    escape = FALSE
  ) |>
  kable_styling(
    full_width = FALSE,
    position = "center",
    latex_options = "hold_position"
  )

# ==============================================================================
#------------------- Element 6: general upwardness of trend -------------------#
# ==============================================================================

num_positive <- sum(z_n_clean > 0)
total_days <- length(z_n_clean)

# test using binomial (could also approximate, but Russell normally says
# 'why approximate when you can calculate' so opted for that here)
binom_res <- binom.test(num_positive, total_days, p = 0.5, alternative = "greater")

# BE note: In element 5 we showed that returns are dependent but using a
# Binomial we assume they are independent.
# The below is using the normal approximation
# prop.test(num_positive, total_days, p = 0.5, correct = FALSE)
# This returns a p-value of ~2.28*10^-6


#------------------- Element 6: persistence of trends
# use run length encoding
# changing to using z_n_clean_numeric to avoid error
# 'rle(signs) : 'x' must be a vector of an atomic type'
signs <- sign(z_n_clean_numeric)
runs <- rle(signs)

# runs object contains $values (1 for positive, -1 for negative) and
# $lengths (the duration of each run)
pos_run_lengths <- runs$lengths[runs$values == 1]
neg_run_lengths <- runs$lengths[runs$values == -1]

mean_pos_run <- mean(pos_run_lengths)
mean_neg_run <- mean(neg_run_lengths)

# could use t-test, but unlikely these are normally distributed
# non-parametric alternative is Mann-Whitney test (/Wilcoxon Rank-Sum)
# -> want to compare distributions of two groups without assuming normality
# Onesided here I think

wilcox_res <- wilcox.test(pos_run_lengths, neg_run_lengths, alternative = "greater")

knitr::include_graphics("fig/appendix-dashboard-1.pdf")

knitr::include_graphics("fig/appendix-dashboard-2.pdf")

# ==============================================================================
#----------------------- Appendix: determining outliers -----------------------#
# ==============================================================================

# Concept: use full range of Nasdaq data (i.e., since the 1970s) then plot
# volatility and identify threshold to use to select outlier regions from our
# time period
threshold_annual_volitility <- 0.70

nasdaq_data_full <- data.frame(
  date = index(IXIC_full),
  price = as.numeric(Cl(IXIC_full))
) %>%
  arrange(date) %>%
  mutate(
    log_ret = c(NA, diff(log(price))),
    # Get annualised volatility (*252) for no. of trading days per year
    volitility_20d = runSD(log_ret, n = 20) * sqrt(252),
    volitility_60d = runSD(log_ret, n = 60) * sqrt(252)
  ) %>%
  filter(!is.na(volitility_20d)) %>%
  filter(!is.na(volitility_60d))

# Get percentiles for breaches of threshold defined above
vol_percentiles <- quantile(
  nasdaq_data_full$volitility_20d,
  probs = c(0.90, 0.95, 0.99, 0.995),
  na.rm = TRUE
)

# get dates where 20d vol > threshold
nasdaq_breaches_full <- nasdaq_data_full %>%
  filter(volitility_20d > threshold_annual_volitility) %>%
  select(date, volitility_20d)

# print("Major Crash Dates detected:")
# nasdaq_breaches_full

#----------- Appendix Figure: full nasdaq rolling 20day volatility ------------#
appendix_p1 <- ggplot(nasdaq_data_full, aes(x = date)) +
  geom_line(aes(y = volitility_20d, color = "20-Day"), size = 0.3) +
  # geom_line(aes(y = volitility_60d, color = "60-Day"), size = 0.6, alpha = 0.8) +
  geom_hline(yintercept = threshold_annual_volitility, linetype = "dashed", color = "black") +
  annotate(
    "text",
    x = as.Date("1975-01-01"),
    y = threshold_annual_volitility + 0.1,
    label = paste0("Threshold (", threshold_annual_volitility, ")"),
    size = 3
  ) +
  scale_color_manual(values = c("20-Day" = "black", "60-Day" = "blue")) +
  labs(
    title = paste0(
      "Nasdaq Composite Volatility (",
      trading_period_days,
      "-day rolling window)"
    ),
    x = "Date",
    y = bquote(s[n] ~ "(" * .(trading_period_days) * "-day rolling)")
  ) +
  # not visible when included so commented out
  # outlier_shading +
  ylim(0.01, 0.99) +
  theme_minimal() +
  theme(legend.position = "none")

# plot 2 (zoomed)
appendix_p2 <- ggplot(
  filter(nasdaq_data_full, date >= "2018-01-01" & date <= "2025-12-31"),
  aes(x = date)
) +
  geom_line(aes(y = volitility_20d, color = "20-Day"), size = 0.5) +
  # geom_line(aes(y = volitility_60d, color = "60-Day"), size = 0.8, alpha = 0.8) +
  # covid shading
  outlier_shading +
  # annotate(
  #   "text",
  #   x = as.Date("2020-03-01"), y = 0.9,
  #   label = "COVID Exclusion",
  #   color = "black", size = 3, hjust = 0
  # ) +
  scale_color_manual(values = c("20-Day" = "black", "60-Day" = "blue")) +
  labs(
    title = paste0(
      "Nasdaq Composite Volatility (",
      trading_period_days,
      "-day rolling window)"
    ),
    x = "Date",
    y = bquote(s[n] ~ "(" * .(trading_period_days) * "-day rolling)")
  ) +
  ylim(0.01, 0.99) +
  # theme(legend.position = "none") +
  theme_minimal()

(appendix_p1 / appendix_p2) &
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5),
    legend.position = "none"
  )

knitr::include_graphics("fig/appendix-COVID19-selection-4.pdf")
