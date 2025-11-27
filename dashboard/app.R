# Load necessary libraries
library(shiny)
library(tidyverse)
library(quantmod)
library(e1071)
library(boot)
library(patchwork)
library(DT)
library(bslib)

# Increase max upload size if needed, though we are fetching from web
options(shiny.maxRequestSize = 30 * 1024^2)

#------------------------------------------------------------------------------
# UI DEFINITION
#------------------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Financial Time-Series Statistical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Configuration"),

      # 1. Ticker Selection
      textInput("ticker", "Ticker Symbol (Yahoo Finance):", value = "^IXIC"),
      helpText("Examples: ^IXIC (Nasdaq), ^GSPC (S&P500), ^DJI, ^FTSE, ^HSI, ^GDAXI, ^STOXX50E, ^AORD"),

      # 2. Date Range
      dateRangeInput("date_range", "Study Period:",
        start = "2018-01-01",
        end = "2024-12-31"
      ),

      # 3. Outlier Handling (Dynamic)
      h5("Outlier Exclusion"),
      helpText("Define a period to exclude (e.g., COVID crash)."),
      checkboxInput("exclude_outliers", "Exclude Specific Period?", value = TRUE),
      conditionalPanel(
        condition = "input.exclude_outliers == true",
        dateInput("outlier_start", "Exclude From:", value = "2020-02-24"),
        dateInput("outlier_end", "Exclude To:", value = "2020-03-23")
      ),
      hr(),

      # 4. Bootstrap Settings
      h5("Simulation Settings"),
      numericInput("boot_reps", "Bootstrap Repetitions:", value = 1000, min = 100, max = 10000),
      helpText("Higher reps = more accuracy but slower."),
      hr(),
      actionButton("run_analysis", "Run Analysis", class = "btn-primary btn-lg", width = "100%")
    ),
    mainPanel(
      tabsetPanel(
        # TAB 1: Overview
        tabPanel(
          "1. Data & Cleaning",
          br(),
          uiOutput("dynamic_title"),
          plotOutput("plot_overview", height = "600px"),
          br(),
          h4("Descriptive Statistics (Post-Cleaning)"),
          tableOutput("table_descriptive")
        ),

        # TAB 2: Normality
        tabPanel(
          "2. Normality & Bootstrapping",
          br(),
          h4("Bootstrapped Skewness (Sample vs Full)"),
          p("Comparing small sample (n=13) behavior vs full dataset."),
          plotOutput("plot_bootstrap", height = "400px"),
          br(),
          h4("Confidence Intervals (BCa)"),
          tableOutput("table_ci")
        ),

        # TAB 3: Mean and Variance
        tabPanel(
          "3. Mean & Variance",
          br(),
          h4("6-Month Period Analysis"),
          p("Investigation of constant mean (Boxplots) and constant variance (Bar Chart)."),
          plotOutput("plot_stationarity", height = "700px"),
          br(),
          h4("Variance Comparison (Min vs Max Volatility)"),
          tableOutput("table_variance_comp")
        ),

        # TAB 4: Independence & Trend
        tabPanel(
          "4. Independence & Trend",
          br(),
          layout_column_wrap(
            width = 1 / 2,
            card(
              h5("Contingency Table (Quartiles)"),
              tableOutput("table_contingency")
            ),
            card(
              h5("Independence Tests"),
              verbatimTextOutput("text_independence")
            )
          ),
          br(),
          h4("Trend Analysis"),
          verbatimTextOutput("text_trend")
        )
      )
    )
  )
)

#------------------------------------------------------------------------------
# SERVER LOGIC
#------------------------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive: Fetch Data
  raw_data <- eventReactive(input$run_analysis, {
    req(input$ticker)

    withProgress(message = "Downloading Data...", {
      tryCatch(
        {
          # 1. Download
          df <- suppressWarnings(
            getSymbols(input$ticker,
              src = "yahoo",
              from = input$date_range[1],
              to = input$date_range[2],
              auto.assign = FALSE
            )
          )

          # 2. Clean
          df <- na.omit(df)

          # 3. CHECK: Did we lose all data? (e.g. if ticker exists but has no data)
          if (nrow(df) == 0) {
            return(NULL)
          }

          return(list(data = df, symbol = input$ticker))
        },
        error = function(e) {
          # Return NULL so we can handle it gracefully in the UI
          return(NULL)
        }
      )
    })
  })

  # Reactive: Clean Data (Calculate z_n and remove outliers)
  clean_data <- reactive({
    # We check if raw_data is available. If NULL, we return NULL too.
    input_pack <- raw_data()
    if (is.null(input_pack)) {
      return(NULL)
    }

    stock <- input_pack$data
    ticker_in <- input_pack$symbol

    # --- 1. Robust Dictionary (Added HSI and STOXX) ---
    stock_lookup <- c(
      "^IXIC" = "NASDAQ Composite",
      "^GSPC" = "S&P 500",
      "^DJI" = "Dow Jones Industrial Average",
      "DIA" = "SPDR Dow Jones ETF",
      "AAPL" = "Apple Inc.",
      "GOOG" = "Alphabet Inc.",
      "MSFT" = "Microsoft Corp.",
      "^FTSE" = "FTSE 100",
      "STOXX50E" = "EURO STOXX 50",
      "^STOXX50E" = "EURO STOXX 50", # adds variant
      "^GDAXI" = "DAX Performance Index",
      "^AORD" = "All Ordinaries",
      "^HSI" = "Hang Seng Index",
      "000001.SS" = "SSE Composite"
    )

    # --- 2. Name Fetching (Wrapped in tryCatch just in case) ---
    if (ticker_in %in% names(stock_lookup)) {
      stock_name <- stock_lookup[[ticker_in]]
    } else {
      stock_name <- tryCatch(
        {
          info <- getQuote(ticker_in, what = yahooQF("Name"))
          if (!is.data.frame(info)) {
            return(ticker_in)
          } # Safety check
          if (is.null(info$Name) || is.na(info$Name)) {
            return(ticker_in)
          }
          return(info$Name)
        },
        error = function(e) {
          return(ticker_in)
        }
      )
    }

    # 3. Process Data
    price_ad <- Ad(stock)
    y_n <- log(price_ad)

    if (input$exclude_outliers) {
      out_start <- as.Date(input$outlier_start)
      out_end <- as.Date(input$outlier_end)
      dates <- index(y_n)
      mask <- dates >= out_start & dates <= out_end
      y_n[mask] <- NA
    }

    z_n <- diff(y_n)

    list(
      price = price_ad,
      z_n_full = z_n,
      z_n_clean = na.omit(z_n),
      name = stock_name
    )
  })

  #----------------------------------------------------------------------------
  # ELEMENT 1: Overview Plots & Descriptives
  #----------------------------------------------------------------------------
  output$plot_overview <- renderPlot({
    # --- THE VALIDATION BLOCK ---
    # If clean_data() is NULL, this stops execution and shows the message
    validate(
      need(clean_data(), "Index not yet supported or data unavailable for this range.")
    )
    # ----------------------------

    data <- clean_data()
    # ... rest of your code ...
    df_price <- data.frame(date = index(data$price), price = as.numeric(data$price))
    df_zn <- data.frame(date = index(data$z_n_full), zn = as.numeric(data$z_n_full))

    p1 <- ggplot(df_price, aes(date, price)) +
      geom_line(na.rm = TRUE) +
      theme_bw() +
      labs(title = paste(data$name, "- Adjusted Close"), y = "Price ($)", x = NULL)

    p2 <- ggplot(df_zn, aes(date, zn)) +
      geom_line(na.rm = TRUE) +
      theme_bw() +
      labs(title = "Log-Return Increments (z_n)", y = expression(z[n]), x = "Date")

    if (input$exclude_outliers) {
      rect <- data.frame(
        xmin = input$outlier_start, xmax = input$outlier_end,
        ymin = -Inf, ymax = Inf
      )
      p1 <- p1 + geom_rect(
        data = rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "red", alpha = 0.2, inherit.aes = FALSE
      )
      p2 <- p2 + geom_rect(
        data = rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "red", alpha = 0.2, inherit.aes = FALSE
      )
    }
    p1 / p2
  })

  output$dynamic_title <- renderUI({
    # No validation needed - return nothing if data missing
    if (is.null(clean_data())) {
      return(NULL)
    }

    company_name <- clean_data()$name
    h4(paste0('Price History & Log Returns: "', company_name, '"'))
  })

  output$table_descriptive <- renderTable({
    validate(need(clean_data(), "Data unavailable."))

    z <- as.numeric(clean_data()$z_n_clean)
    tibble(
      Statistic = c("Mean", "Variance", "Skewness (Type 1)", "Excess Kurtosis (Type 1)", "N"),
      Value = c(
        sprintf("%.6f", mean(z)), sprintf("%.6f", var(z)),
        sprintf("%.6f", skewness(z, type = 1)), sprintf("%.6f", kurtosis(z, type = 1)),
        sprintf("%.0f", length(z))
      )
    )
  })

  #----------------------------------------------------------------------------
  # ELEMENT 2: Bootstrapping
  #----------------------------------------------------------------------------
  bootstrap_results <- reactive({
    req(clean_data())
    z <- as.numeric(clean_data()$z_n_clean)

    withProgress(message = "Running Bootstrap...", value = 0.5, {
      set.seed(1234)
      n_reps <- input$boot_reps
      n_sample <- 13

      # 1. Simulate Normal
      z_mean <- mean(z)
      z_sd <- sd(z)
      skew_normal <- replicate(n_reps, skewness(rnorm(n_sample, z_mean, z_sd), type = 1))

      # 2. Bootstrap Small Sample
      skew_boot <- replicate(n_reps, skewness(sample(z, n_sample, replace = TRUE), type = 1))

      # 3. Bootstrap Full Data
      skew_fun <- function(data, indices) {
        return(skewness(data[indices], type = 1))
      }

      boot_obj <- boot(data = z, statistic = skew_fun, R = n_reps)

      # Calculate BOTH BCa and Percentile
      # wrap in tryCatch because BCa can fail if N reps is too low
      boot_ci_all <- tryCatch(
        {
          boot.ci(boot_obj, type = c("bca", "perc"), conf = 0.95)
        },
        error = function(e) {
          NULL
        }
      )

      list(
        skew_normal = skew_normal,
        skew_boot = skew_boot,
        boot_ci = boot_ci_all # <--- Ensure this key name matches the table below
      )
    })
  })

  output$plot_bootstrap <- renderPlot({
    validate(need(clean_data(), "Data unavailable for bootstrapping."))
    req(bootstrap_results())

    res <- bootstrap_results()

    # 2. Prepare Data
    df <- data.frame(
      skewness = c(res$skew_normal, res$skew_boot),
      Source = factor(rep(c("Normal Model (n=13)", "Bootstrap from Data (n=13)"), each = input$boot_reps))
    )

    # 3. Plot
    ggplot(df, aes(x = skewness, fill = Source)) +
      geom_density(alpha = 0.4) +
      theme_bw() +
      labs(title = "Sampling distributions of Skewness (n=13)", x = "Skewness")
  })

  output$table_ci <- renderTable(
    {
      validate(need(clean_data(), "Data unavailable."))
      req(bootstrap_results())
      res <- bootstrap_results()

      # 1. Extract Normal & Small Sample (Quantiles)
      # unname() removes the "2.5%" labels so JSON doesn't complain
      norm_ci <- unname(quantile(res$skew_normal, probs = c(0.025, 0.975)))
      small_ci <- unname(quantile(res$skew_boot, probs = c(0.025, 0.975)))

      # 2. Extract Full Data (Robustly)
      # This helper prevents crashes if boot.ci failed or is missing components
      get_lims <- function(comp) {
        if (is.null(comp)) {
          return(c(NA, NA))
        }
        # boot.ci returns a matrix; indices 4 & 5 are the Lower/Upper bounds
        if (is.matrix(comp)) {
          return(comp[1, 4:5])
        }
        return(comp[4:5])
      }

      # Safely get limits (returns NAs if missing)
      full_perc <- get_lims(res$boot_ci$percent)
      full_bca <- get_lims(res$boot_ci$bca)

      tibble(
        Dataset = c("Simulated Normal (n=13)", "Bootstrap (n=13)", "Full Data", "Full Data"),
        Method = c("Quantile (Control)", "Percentile", "Percentile", "BCa (Robust)"),
        `Lower 95% CI` = c(norm_ci[1], small_ci[1], full_perc[1], full_bca[1]),
        `Upper 95% CI` = c(norm_ci[2], small_ci[2], full_perc[2], full_bca[2])
      )
    },
    digits = 4
  )

  #----------------------------------------------------------------------------
  # ELEMENT 3 & 4: Stationarity (6-Month Blocks)
  #----------------------------------------------------------------------------
  six_month_data <- reactive({
    req(clean_data())
    z_clean_xts <- clean_data()$z_n_clean

    # Create 6 month breaks
    dates <- index(z_clean_xts)

    # Logic from your script to define rigid 6 month blocks
    first_date <- min(dates)
    fy <- as.integer(format(first_date, "%Y"))
    fm <- as.integer(format(first_date, "%m"))
    start0 <- if (fm <= 6) as.Date(paste0(fy, "-01-01")) else as.Date(paste0(fy, "-07-01"))

    last_date <- max(dates)
    ly <- as.integer(format(last_date, "%Y"))
    end_next <- as.Date(paste0(ly + 1, "-07-01")) # Buffer

    breaks <- seq(start0, end_next, by = "6 months")

    # Create Labels
    start_labels <- breaks[-length(breaks)]
    end_labels <- breaks[-1] - 1
    period_labels <- paste(format(start_labels, "%b %Y"), "-", format(end_labels, "%b %Y"))

    # Cut
    Period <- cut(dates, breaks = breaks, right = FALSE, labels = period_labels[1:(length(breaks) - 1)], include.lowest = TRUE)

    # Split
    z_list <- split(z_clean_xts, Period)
    z_list <- z_list[lengths(z_list) > 0] # Remove empty

    return(z_list)
  })

  output$plot_stationarity <- renderPlot({
    validate(need(clean_data(), "Data unavailable for stationarity analysis."))
    req(six_month_data())
    z_list <- six_month_data()

    # Prepare Data for Boxplot
    combined_df <- bind_rows(lapply(seq_along(z_list), function(i) {
      data.frame(
        Period = factor(names(z_list)[i], levels = names(z_list)),
        z_value = as.numeric(z_list[[i]])
      )
    }))

    # Prepare Data for Variance Bar Chart
    variances <- sapply(z_list, var, na.rm = TRUE) * 252 # Annualized
    var_df <- data.frame(
      Period = factor(names(z_list), levels = names(z_list)),
      Variance = as.numeric(variances)
    )

    p1 <- ggplot(combined_df, aes(x = Period, y = z_value)) +
      geom_boxplot(outlier.size = 0.5) +
      theme_bw() +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
      labs(title = "Log-returns (Boxplots)", y = expression(z[n]))

    p2 <- ggplot(var_df, aes(x = Period, y = Variance)) +
      geom_col(fill = "steelblue") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Annualized Variance", y = "Variance")

    p1 / p2 + plot_layout(heights = c(2, 2))
  })

  output$table_variance_comp <- renderTable({
    validate(need(clean_data(), "Data unavailable."))
    req(six_month_data())
    z_list <- six_month_data()

    # 1. Calculate Annualized Variances for all periods
    variances <- sapply(z_list, var, na.rm = TRUE) * 252

    # 2. Identify Min and Max locations
    min_loc <- which.min(variances)
    max_loc <- which.max(variances)

    # 3. Extract the actual data for these periods
    min_data <- as.numeric(na.omit(z_list[[min_loc]]))
    max_data <- as.numeric(na.omit(z_list[[max_loc]]))

    # 4. Define Boot Function (Variance * 252)
    var_fun <- function(data, indices) {
      var(data[indices]) * 252
    }

    # 5. Helper to calculate CI (Robust BCa with fallback)
    get_ci <- function(data) {
      if (length(data) < 5) {
        return(c(NA, NA))
      } # Safety check for tiny samples

      boot_obj <- boot(data = data, statistic = var_fun, R = input$boot_reps)

      tryCatch(
        {
          # Try BCa first (Robust)
          boot.ci(boot_obj, type = "bca", conf = 0.95)$bca[4:5]
        },
        error = function(e) {
          # Fallback to Percentile if BCa fails (common in small subsamples)
          boot.ci(boot_obj, type = "perc", conf = 0.95)$percent[4:5]
        }
      )
    }

    # 6. Run Bootstrap (Wrapped in progress bar)
    withProgress(message = "Bootstrapping Variance CIs...", value = 0.5, {
      ci_min <- get_ci(min_data)
      incProgress(0.3)
      ci_max <- get_ci(max_data)
    })

    # 7. Build Table
    tibble(
      Type = c("Min Variance Period", "Max Variance Period"),
      Period = c(names(z_list)[min_loc], names(z_list)[max_loc]),
      `Annualized Variance` = sprintf("%.3f", c(variances[min_loc], variances[max_loc])),
      `Lower 95% CI` = sprintf("%.3f", c(ci_min[1], ci_max[1])),
      `Upper 95% CI` = sprintf("%.3f", c(ci_min[2], ci_max[2]))
    )
  })

  #----------------------------------------------------------------------------
  # ELEMENT 5 & 6: Independence
  #----------------------------------------------------------------------------
  independence_data <- reactive({
    req(clean_data())
    z <- clean_data()$z_n_clean

    # Lag
    z_lag <- stats::lag(z, k = 1)
    z_merged <- merge(z, z_lag, join = "inner")
    colnames(z_merged) <- c("z_n", "z_prev")

    # Quartiles
    q <- quantile(z, probs = c(0.25, 0.5, 0.75))
    breaks <- c(-Inf, q, Inf)
    labels <- c("Q1", "Q2", "Q3", "Q4")

    z_merged$cat_n <- cut(z_merged$z_n, breaks = breaks, labels = labels)
    z_merged$cat_prev <- cut(z_merged$z_prev, breaks = breaks, labels = labels)

    return(z_merged)
  })

  output$table_contingency <- renderTable(
    {
      validate(need(clean_data(), "Data unavailable."))
      req(independence_data())
      dat <- independence_data()
      tbl <- table(dat$cat_prev, dat$cat_n)
      as.data.frame.matrix(tbl, row.names = paste("Prev", rownames(tbl)))
    },
    rownames = TRUE
  )

  output$text_independence <- renderPrint({
    dat <- independence_data()
    tbl <- table(dat$cat_prev, dat$cat_n)
    print(chisq.test(tbl))
  })

  output$text_trend <- renderPrint({
    req(clean_data())
    z <- as.numeric(clean_data()$z_n_clean)

    # Binomial Test (Up vs Down)
    n_pos <- sum(z > 0)
    n_total <- length(z)
    binom <- binom.test(n_pos, n_total, p = 0.5, alternative = "greater")

    # Runs Test
    signs <- sign(z)
    runs <- rle(signs)
    pos_runs <- runs$lengths[runs$values == 1]
    neg_runs <- runs$lengths[runs$values == -1]
    wilcox <- wilcox.test(pos_runs, neg_runs, alternative = "greater")

    cat("--- Binomial Test (Proportion of positive days) ---\n")
    print(binom)
    cat("\n--- Mann-Whitney (Run Lengths: Positive vs Negative) ---\n")
    print(wilcox)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
