# ============================================================
# ONLINE RETAIL SHINY APP - DATA PREPARATION
# Author: Abdulrahman Jalilov
#
# Run this script ONCE locally before deploying the Shiny app.
# It reads the raw 541,909-row dataset, creates all pre-aggregated
# summary tables, and saves them as small .rds files.
#
# The Shiny app loads ONLY these summaries (<10MB total),
# keeping memory well under 1GB on Posit Cloud.
# ============================================================

library(tidyverse)
library(lubridate)
library(forecast)

library(readxl)
# With this:
RAW_PATH <- "C:/Users/aksta/Desktop/onlineretail/Online Retail.xlsx"
raw <- read_excel(RAW_PATH)

# ============================================================
# STEP 1: CLEAN & PREPARE
# ============================================================
cat("Cleaning data.../n")

df <- raw %>%
  rename_with(tolower) %>%
  rename_with(~gsub("//s+", "_", .)) %>%
  rename(
    invoice_no   = invoiceno,
    stock_code   = stockcode,
    invoice_date = invoicedate,
    unit_price   = unitprice,
    customer_id  = customerid
  ) %>%
  mutate(
    invoice_date = as.POSIXct(invoice_date, format = "%m/%d/%Y %H:%M"),
    invoice_date = if_else(is.na(invoice_date),
                           as.POSIXct(invoice_date, format = "%Y-%m-%d %H:%M:%S"),
                           invoice_date),
    revenue    = quantity * unit_price,
    date       = as.Date(invoice_date),
    month      = month(invoice_date),
    year       = year(invoice_date),
    month_year = format(invoice_date, "%b %Y"),
    weekday    = weekdays(invoice_date)
  )

# Clean business dataset: identified customers, positive qty & price
df_clean <- df %>%
  filter(!is.na(customer_id),
         quantity   > 0,
         unit_price > 0)

cat("Clean rows:", nrow(df_clean), "/n")

# ============================================================
# STEP 2: KPI SUMMARY
# ============================================================
cat("Building KPIs.../n")

kpi <- list(
  total_revenue       = round(sum(df_clean$revenue), 2),
  total_orders        = n_distinct(df_clean$invoice_no),
  unique_customers    = n_distinct(df_clean$customer_id),
  unique_products     = n_distinct(df_clean$stock_code),
  avg_order_value     = round(sum(df_clean$revenue) /
                                n_distinct(df_clean$invoice_no), 2),
  avg_customer_value  = round(sum(df_clean$revenue) /
                                n_distinct(df_clean$customer_id), 2),
  total_countries     = n_distinct(df_clean$country),
  return_value        = round(abs(sum(df$revenue[df$quantity < 0 &
                                                   df$unit_price > 0],
                                      na.rm = TRUE)), 2)
)

# ============================================================
# STEP 3: MONTHLY REVENUE
# ============================================================
cat("Building monthly revenue.../n")

monthly_revenue <- df_clean %>%
  group_by(year, month, month_year) %>%
  summarise(
    revenue             = round(sum(revenue), 2),
    orders              = n_distinct(invoice_no),
    customers           = n_distinct(customer_id),
    .groups             = "drop"
  ) %>%
  mutate(
    date               = as.Date(paste(year, month, "01", sep = "-")),
    revenue_per_order   = round(revenue / orders, 2),
    revenue_per_customer = round(revenue / customers, 2),
    month_label        = format(date, "%b %Y")
  ) %>%
  arrange(date)

# ============================================================
# STEP 4: RFM SEGMENTATION
# ============================================================
cat("Building RFM.../n")

snapshot_date <- as.Date("2011-12-10")

rfm_data <- df_clean %>%
  group_by(customer_id) %>%
  summarise(
    recency   = as.numeric(snapshot_date - as.Date(max(invoice_date))),
    frequency = n_distinct(invoice_no),
    monetary  = round(sum(revenue), 2),
    last_purchase = as.Date(max(invoice_date)),
    country   = first(country),
    .groups   = "drop"
  ) %>%
  mutate(
    segment = case_when(
      recency <= 30  & frequency >= 5  & monetary >= 1000 ~ "High Value",
      recency <= 90  & frequency >= 2                     ~ "Medium Value",
      TRUE                                                 ~ "Low Value"
    ),
    churn_risk = case_when(
      recency <= 30  ~ "Active",
      recency <= 60  ~ "At Risk",
      recency <= 180 ~ "Lapsing",
      TRUE           ~ "Churned"
    )
  )

segment_summary <- rfm_data %>%
  group_by(segment) %>%
  summarise(
    customers     = n(),
    avg_recency   = round(mean(recency), 1),
    avg_frequency = round(mean(frequency), 1),
    avg_monetary  = round(mean(monetary), 2),
    total_revenue = round(sum(monetary), 2),
    .groups       = "drop"
  ) %>%
  mutate(
    customer_share = round(customers / sum(customers) * 100, 1),
    revenue_share  = round(total_revenue / sum(total_revenue) * 100, 1)
  )

churn_summary <- rfm_data %>%
  group_by(churn_risk) %>%
  summarise(
    customers     = n(),
    avg_recency   = round(mean(recency), 1),
    avg_monetary  = round(mean(monetary), 2),
    total_revenue = round(sum(monetary), 2),
    .groups       = "drop"
  ) %>%
  mutate(
    customer_share = round(customers / sum(customers) * 100, 1),
    revenue_share  = round(total_revenue / sum(total_revenue) * 100, 1)
  )

# ============================================================
# STEP 5: PRODUCT INTELLIGENCE
# ============================================================
cat("Building product tables.../n")

product_data <- df_clean %>%
  group_by(stock_code, description) %>%
  summarise(
    revenue       = round(sum(revenue), 2),
    orders        = n_distinct(invoice_no),
    customers     = n_distinct(customer_id),
    total_qty     = sum(quantity),
    avg_unit_price = round(mean(unit_price), 2),
    .groups       = "drop"
  ) %>%
  arrange(desc(revenue))

product_segment <- df_clean %>%
  left_join(rfm_data %>% select(customer_id, segment), by = "customer_id") %>%
  filter(!is.na(segment)) %>%
  group_by(segment, stock_code, description) %>%
  summarise(
    revenue   = round(sum(revenue), 2),
    orders    = n_distinct(invoice_no),
    customers = n_distinct(customer_id),
    .groups   = "drop"
  ) %>%
  arrange(segment, desc(revenue)) %>%
  group_by(segment) %>%
  slice_head(n = 10) %>%
  ungroup()

# ============================================================
# STEP 6: COUNTRY STATS
# ============================================================
cat("Building country stats.../n")

country_stats <- df_clean %>%
  group_by(country) %>%
  summarise(
    revenue   = round(sum(revenue), 2),
    orders    = n_distinct(invoice_no),
    customers = n_distinct(customer_id),
    .groups   = "drop"
  ) %>%
  mutate(
    revenue_per_customer = round(revenue / customers, 2),
    orders_per_customer  = round(orders / customers, 2),
    avg_order_value      = round(revenue / orders, 2)
  ) %>%
  arrange(desc(revenue))

# ============================================================
# STEP 7: RETURNS ANALYSIS
# ============================================================
cat("Building returns data.../n")

returns_raw <- df %>%
  filter(quantity < 0, unit_price > 0)

returns_monthly <- returns_raw %>%
  group_by(year, month, month_year) %>%
  summarise(
    return_transactions = n(),
    unique_invoices     = n_distinct(invoice_no),
    total_neg_qty       = sum(quantity),
    return_value        = round(abs(sum(revenue)), 2),
    .groups             = "drop"
  ) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  arrange(date)

# ============================================================
# STEP 8: FORECASTING
# ============================================================
cat("Building forecasts.../n")

# Use only complete months: Jan-Nov 2011
monthly_ts_vals <- df_clean %>%
  filter(year == 2011, month <= 11) %>%
  group_by(month) %>%
  summarise(revenue = sum(revenue), .groups = "drop") %>%
  arrange(month) %>%
  pull(revenue)

ts_data <- ts(monthly_ts_vals, start = c(2011, 1), frequency = 12)

# Model A: Holt Damped ETS
ets_model    <- ets(ts_data, model = "AAN", damped = TRUE)
ets_fc       <- forecast(ets_model, h = 3)

# Model B: Polynomial Regression Degree 2
x_train   <- 1:11
poly_fit  <- lm(monthly_ts_vals ~ poly(x_train, 2))
x_pred    <- data.frame(x_train = 12:14)
poly_pred <- predict(poly_fit,
                     newdata  = data.frame(poly(12:14, 2,
                                                degree = 2,
                                                raw    = TRUE)),
                     interval = "prediction", level = 0.95)
# Use raw polynomial
poly_fit2 <- lm(monthly_ts_vals ~ x_train + I(x_train^2))
x_new     <- data.frame(x_train = 12:14)
poly_pred2 <- predict(poly_fit2, newdata = x_new,
                      interval = "prediction", level = 0.95)

forecast_data <- data.frame(
  month      = c("Dec 2011", "Jan 2012", "Feb 2012"),
  ets_point  = round(as.numeric(ets_fc$mean), 0),
  ets_lo80   = round(as.numeric(ets_fc$lower[, 1]), 0),
  ets_hi80   = round(as.numeric(ets_fc$upper[, 1]), 0),
  ets_lo95   = round(as.numeric(ets_fc$lower[, 2]), 0),
  ets_hi95   = round(as.numeric(ets_fc$upper[, 2]), 0),
  poly_point = round(poly_pred2[, 1], 0),
  poly_lo95  = round(poly_pred2[, 2], 0),
  poly_hi95  = round(poly_pred2[, 3], 0)
)

monthly_history <- data.frame(
  month_label = format(
    as.Date(paste("2011", 1:11, "01", sep = "-")), "%b %Y"),
  revenue     = round(monthly_ts_vals, 0)
)

# ============================================================
# STEP 9: SAVE ALL RDS FILES
# ============================================================
cat("Saving RDS files.../n")
dir.create("shiny_data", showWarnings = FALSE)

saveRDS(kpi,              "shiny_data/kpi.rds")
saveRDS(monthly_revenue,  "shiny_data/monthly_revenue.rds")
saveRDS(rfm_data,         "shiny_data/rfm_data.rds")
saveRDS(segment_summary,  "shiny_data/segment_summary.rds")
saveRDS(churn_summary,    "shiny_data/churn_summary.rds")
saveRDS(product_data,     "shiny_data/product_data.rds")
saveRDS(product_segment,  "shiny_data/product_segment.rds")
saveRDS(country_stats,    "shiny_data/country_stats.rds")
saveRDS(returns_monthly,  "shiny_data/returns_monthly.rds")
saveRDS(forecast_data,    "shiny_data/forecast_data.rds")
saveRDS(monthly_history,  "shiny_data/monthly_history.rds")

cat("/n=== Data Preparation Complete ===/n")
cat("Files saved to: shiny_data//n")
cat("Total files:    11 .rds files/n")
cat("Estimated total size: < 5MB/n")
cat("Memory used in Shiny app: < 50MB (well under 1GB)/n")

