# ============================================================
# ONLINE RETAIL CUSTOMER ANALYTICS - SHINY DASHBOARD
# Author: Abdulrahman Jalilov
# Dataset: UCI Online Retail (541,909 rows, Dec 2010 - Dec 2011)
#
# Memory strategy: loads pre-aggregated .rds files only (<50MB)
# Run data_prep.R first to generate the shiny_data/ folder.
# ============================================================

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(scales)

# ---- Brand colours ----
COL_TEAL   <- "#2A9D8F"
COL_RED    <- "#E76F51"
COL_NAVY   <- "#2C3E50"
COL_BLUE   <- "#457B9D"
COL_ORANGE <- "#F4A261"
COL_PURPLE <- "#4B4B8F"
SEG_COLS   <- c("High Value"   = COL_TEAL,
                "Medium Value" = COL_BLUE,
                "Low Value"    = COL_ORANGE)
CHURN_COLS <- c("Active"   = COL_TEAL,
                "At Risk"  = COL_ORANGE,
                "Lapsing"  = COL_RED,
                "Churned"  = COL_NAVY)

# ---- Load pre-aggregated data ----
kpi             <- readRDS("shiny_data/kpi.rds")
monthly_revenue <- readRDS("shiny_data/monthly_revenue.rds")
rfm_data        <- readRDS("shiny_data/rfm_data.rds")
segment_summary <- readRDS("shiny_data/segment_summary.rds")
churn_summary   <- readRDS("shiny_data/churn_summary.rds")
product_data    <- readRDS("shiny_data/product_data.rds")
product_segment <- readRDS("shiny_data/product_segment.rds")
country_stats   <- readRDS("shiny_data/country_stats.rds")
returns_monthly <- readRDS("shiny_data/returns_monthly.rds")
forecast_data   <- readRDS("shiny_data/forecast_data.rds")
monthly_history <- readRDS("shiny_data/monthly_history.rds")

# ---- Helper: KPI card HTML ----
kpi_card <- function(value, label, color = COL_TEAL, icon = "💼") {
  tags$div(
    style = paste0(
      "background:#fff; border-left:5px solid ", color, ";",
      "border-radius:8px; padding:18px 20px; margin-bottom:16px;",
      "box-shadow:0 2px 8px rgba(0,0,0,0.08);"
    ),
    tags$div(style = "font-size:22px; margin-bottom:4px;", icon),
    tags$div(style = paste0("font-size:26px; font-weight:700; color:", color, ";"),
             value),
    tags$div(style = "font-size:13px; color:#666; margin-top:4px;", label)
  )
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(

  # ---- Global styles ----
  tags$head(tags$style(HTML(paste0("
    body { font-family: 'Segoe UI', Arial, sans-serif; background:#f5f6fa; }
    .navbar-default { background:", COL_NAVY, "; border:none; }
    .navbar-default .navbar-brand,
    .navbar-default .navbar-nav > li > a { color:#fff !important; }
    .navbar-default .navbar-nav > .active > a { background:", COL_TEAL, " !important; color:#fff !important; }
    .tab-content { padding-top:20px; }
    .section-header { color:", COL_NAVY, "; font-weight:700; font-size:18px;
                       border-bottom:3px solid ", COL_TEAL, ";
                       padding-bottom:6px; margin-bottom:16px; margin-top:8px; }
    .insight-box { background:#f0f8f7; border-left:4px solid ", COL_TEAL, ";
                   padding:12px 16px; border-radius:4px;
                   font-size:13.5px; color:#333; margin:12px 0; }
    .warning-box { background:#fff3e0; border-left:4px solid ", COL_ORANGE, ";
                   padding:12px 16px; border-radius:4px;
                   font-size:13.5px; color:#333; margin:12px 0; }
    .whatif-box  { background:#e8f5e9; border-left:4px solid #4CAF50;
                   padding:14px 18px; border-radius:6px; margin-top:12px; }
    hr { border-color:#dde; margin:20px 0; }
  ")))),

  # ---- Page title ----
  tags$div(
    style = paste0("background:", COL_NAVY, "; padding:16px 24px; margin-bottom:20px;"),
    tags$h3(style = "color:#fff; margin:0; font-weight:700;",
            "🛒 Online Retail Customer Analytics"),
    tags$p(style = "color:#aec6cf; margin:4px 0 0;",
           "541,909 transactions | Dec 2010 – Dec 2011 | 38 Countries")
  ),

  # ---- Navigation tabs ----
  navbarPage(title = NULL, id = "nav",

    # ==========================================================
    # TAB 1: OVERVIEW
    # ==========================================================
    tabPanel("📊 Overview",
      fluidRow(
        column(2, kpi_card(paste0("$", format(round(kpi$total_revenue/1e6,2), nsmall=2), "M"),
                           "Total Revenue", COL_TEAL, "💰")),
        column(2, kpi_card(format(kpi$total_orders, big.mark=","),
                           "Total Orders", COL_BLUE, "📦")),
        column(2, kpi_card(format(kpi$unique_customers, big.mark=","),
                           "Unique Customers", COL_ORANGE, "👥")),
        column(2, kpi_card(format(kpi$unique_products, big.mark=","),
                           "Unique Products", COL_PURPLE, "🏷️")),
        column(2, kpi_card(paste0("$", format(kpi$avg_order_value, big.mark=",")),
                           "Avg Order Value", COL_RED, "🧾")),
        column(2, kpi_card(paste0("$", format(round(kpi$avg_customer_value,0), big.mark=",")),
                           "Avg Customer Value", COL_NAVY, "⭐"))
      ),
      hr(),
      fluidRow(
        column(12, div(class="section-header", "Monthly Revenue Trend"))
      ),
      fluidRow(
        column(3,
          selectInput("overview_metric", "Display Metric:",
                      choices = c("Revenue" = "revenue",
                                  "Orders"  = "orders",
                                  "Customers" = "customers",
                                  "Revenue per Order" = "revenue_per_order",
                                  "Revenue per Customer" = "revenue_per_customer"),
                      selected = "revenue"),
          br(),
          div(class="insight-box",
              "💡 Revenue grew steadily through 2011 with a sharp Q4 acceleration.
               Sep–Nov 2011 alone accounted for ~40% of full-year revenue,
               driven by early Christmas purchasing patterns.")
        ),
        column(9, plotlyOutput("revenue_trend", height = "380px"))
      ),
      hr(),
      fluidRow(
        column(6,
          div(class="section-header", "Top 10 Countries by Revenue"),
          plotlyOutput("country_bar_overview", height = "320px")
        ),
        column(6,
          div(class="section-header", "Monthly Revenue Summary"),
          DTOutput("monthly_table")
        )
      )
    ),

    # ==========================================================
    # TAB 2: CUSTOMER SEGMENTS (RFM)
    # ==========================================================
    tabPanel("👥 Customer Segments",
      fluidRow(
        column(3,
          div(class="section-header", "Filters"),
          checkboxGroupInput("seg_filter", "Show Segments:",
                             choices  = c("High Value", "Medium Value", "Low Value"),
                             selected = c("High Value", "Medium Value", "Low Value")),
          sliderInput("rfm_max_recency", "Max Recency (days):",
                      min = 1, max = 374, value = 374, step = 1),
          sliderInput("rfm_min_monetary", "Min Monetary Value ($):",
                      min = 0, max = 5000, value = 0, step = 50),
          hr(),
          div(class="insight-box",
              "💡 High Value customers (21.3%) generate 70% of revenue.
               Medium Value (30.9%) generates 20%.
               Low Value (47.7%) generates only 10%.")
        ),
        column(9,
          div(class="section-header", "RFM Customer Scatter — Recency vs Frequency"),
          plotlyOutput("rfm_scatter", height = "420px"),
          div(class="insight-box",
              "Hover over any point to see customer details. Bubble size = Monetary Value.")
        )
      ),
      hr(),
      fluidRow(
        column(6,
          div(class="section-header", "Segment Summary Table"),
          DTOutput("segment_table")
        ),
        column(6,
          div(class="section-header", "Revenue Share by Segment"),
          plotlyOutput("segment_pie", height = "300px")
        )
      )
    ),

    # ==========================================================
    # TAB 3: CHURN RISK & WHAT-IF
    # ==========================================================
    tabPanel("⚠️ Churn Risk",
      fluidRow(
        column(5,
          div(class="section-header", "Churn Risk Distribution"),
          plotlyOutput("churn_bar", height = "340px"),
          div(class="insight-box",
              "💡 Active customers (≤30 days) represent 37.5% of customers
               but 70% of revenue. At Risk and Lapsing customers represent
               recoverable revenue if re-engaged within 30 days.")
        ),
        column(7,
          div(class="section-header", "Revenue What-If Calculator"),
          div(class="warning-box",
              "⚠️ This tool estimates the revenue impact of re-engaging
               churning customers. Use it for marketing planning."),
          fluidRow(
            column(6,
              sliderInput("recover_atrisk", "% of At-Risk Customers Recovered:",
                          min = 0, max = 100, value = 20, step = 5,
                          post = "%")
            ),
            column(6,
              sliderInput("recover_lapsing", "% of Lapsing Customers Recovered:",
                          min = 0, max = 100, value = 10, step = 5,
                          post = "%")
            )
          ),
          fluidRow(
            column(6,
              sliderInput("recover_churned", "% of Churned Customers Recovered:",
                          min = 0, max = 100, value = 5, step = 5,
                          post = "%")
            ),
            column(6,
              sliderInput("recovery_rate", "Recovery Revenue Rate:",
                          min = 10, max = 100, value = 70, step = 5,
                          post = "%",
                          helpText("% of avg monetary value expected from recovered customers"))
            )
          ),
          uiOutput("whatif_result"),
          hr(),
          div(class="section-header", "Churn Risk Summary Table"),
          DTOutput("churn_table")
        )
      )
    ),

    # ==========================================================
    # TAB 4: PRODUCT INTELLIGENCE
    # ==========================================================
    tabPanel("🛍️ Products",
      fluidRow(
        column(3,
          selectInput("prod_segment", "Customer Segment:",
                      choices  = c("All Segments", "High Value",
                                   "Medium Value", "Low Value"),
                      selected = "All Segments"),
          sliderInput("prod_top_n", "Show Top N Products:",
                      min = 5, max = 20, value = 10, step = 1),
          hr(),
          div(class="insight-box",
              "💡 High Value customers drive the most revenue per product.
               Products appearing in their top 10 are ideal as acquisition
               tools — they attract future high-value buyers.")
        ),
        column(9,
          div(class="section-header", "Top Products by Revenue"),
          plotlyOutput("product_bar", height = "380px")
        )
      ),
      hr(),
      fluidRow(
        column(12,
          div(class="section-header", "Full Product Search Table"),
          DTOutput("product_search_table")
        )
      )
    ),

    # ==========================================================
    # TAB 5: INTERNATIONAL MARKETS
    # ==========================================================
    tabPanel("🌍 International",
      fluidRow(
        column(3,
          selectInput("country_metric", "Ranking Metric:",
                      choices = c(
                        "Total Revenue"         = "revenue",
                        "Revenue per Customer"  = "revenue_per_customer",
                        "Orders per Customer"   = "orders_per_customer",
                        "Average Order Value"   = "avg_order_value",
                        "Total Customers"       = "customers",
                        "Total Orders"          = "orders"
                      ),
                      selected = "revenue_per_customer"),
          checkboxInput("exclude_uk", "Exclude United Kingdom", value = FALSE),
          sliderInput("country_top_n", "Show Top N Countries:",
                      min = 5, max = 30, value = 10, step = 1),
          hr(),
          div(class="insight-box",
              "💡 Revenue-per-customer reveals market efficiency.
               Norway ($3,617) and Switzerland ($2,688) outperform
               the UK ($1,864) per customer — high-quality underdeveloped markets.")
        ),
        column(9,
          div(class="section-header", "Country Comparison"),
          plotlyOutput("country_bar", height = "420px")
        )
      ),
      hr(),
      fluidRow(
        column(12,
          div(class="section-header", "Full Country Table"),
          DTOutput("country_table")
        )
      )
    ),

    # ==========================================================
    # TAB 6: RETURNS MONITOR
    # ==========================================================
    tabPanel("🔄 Returns",
      fluidRow(
        column(3,
          div(class="insight-box",
              paste0("⚠️ Total return value: $",
                     format(kpi$return_value, big.mark=","),
                     " across 10,624 return transactions.")),
          hr(),
          selectInput("return_metric", "Metric:",
                      choices = c(
                        "Return Value ($)"    = "return_value",
                        "Return Transactions" = "return_transactions",
                        "Negative Quantity"   = "total_neg_qty"
                      ),
                      selected = "return_value"),
          div(class="warning-box",
              "⚠️ Customer 12346 had a 100% return rate ($77K purchase fully reversed).
               Customer 16446 similarly returned $168K. These may represent
               cancelled bulk orders rather than genuine sales.")
        ),
        column(9,
          div(class="section-header", "Monthly Returns & Cancellations"),
          plotlyOutput("returns_chart", height = "380px")
        )
      ),
      hr(),
      fluidRow(
        column(12,
          div(class="section-header", "Monthly Returns Table"),
          DTOutput("returns_table")
        )
      )
    ),

    # ==========================================================
    # TAB 7: REVENUE FORECAST
    # ==========================================================
    tabPanel("📈 Forecast",
      fluidRow(
        column(3,
          div(class="section-header", "Model Settings"),
          checkboxGroupInput("fc_models", "Show Models:",
                             choices  = c("Holt Damped ETS", "Polynomial Regression (Deg 2)"),
                             selected = c("Holt Damped ETS", "Polynomial Regression (Deg 2)")),
          checkboxInput("fc_ci80",  "Show 80% Confidence Interval", value = TRUE),
          checkboxInput("fc_ci95",  "Show 95% Confidence Interval", value = FALSE),
          hr(),
          div(class="insight-box",
              "💡 Polynomial Regression (MAPE 10.4%, R²=0.889) outperforms
               Holt ETS (MAPE 17.3%) on historical data.
               The $172K gap between their December forecasts reflects
               genuine disagreement about how much Q4 growth is sustained
               vs seasonal."),
          hr(),
          div(class="section-header", "Model Accuracy"),
          tableOutput("model_accuracy_table")
        ),
        column(9,
          div(class="section-header", "3-Month Revenue Forecast — Jan–Feb 2012"),
          plotlyOutput("forecast_chart", height = "420px")
        )
      ),
      hr(),
      fluidRow(
        column(12,
          div(class="section-header", "Forecast Values Table"),
          DTOutput("forecast_table")
        )
      )
    )

  ) # end navbarPage
) # end fluidPage


# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ---- Reactive: filtered RFM ----
  rfm_filtered <- reactive({
    rfm_data %>%
      filter(
        segment  %in% input$seg_filter,
        recency  <= input$rfm_max_recency,
        monetary >= input$rfm_min_monetary
      )
  })

  # ---- TAB 1: OVERVIEW ----

  output$revenue_trend <- renderPlotly({
    metric_label <- switch(input$overview_metric,
                           revenue             = "Revenue ($)",
                           orders              = "Orders",
                           customers           = "Customers",
                           revenue_per_order   = "Revenue per Order ($)",
                           revenue_per_customer = "Revenue per Customer ($)")
    p <- ggplot(monthly_revenue,
                aes(x = date, y = .data[[input$overview_metric]],
                    text = paste0(month_label, "<br>",
                                  metric_label, ": ",
                                  if (grepl("revenue", input$overview_metric))
                                    paste0("$", format(round(.data[[input$overview_metric]]),
                                                       big.mark=","))
                                  else format(.data[[input$overview_metric]], big.mark=",")))) +
      geom_area(fill = COL_TEAL, alpha = 0.2) +
      geom_line(color = COL_TEAL, linewidth = 1.3) +
      geom_point(color = COL_TEAL, size = 3) +
      scale_x_date(date_labels = "%b %y") +
      scale_y_continuous(labels = if (grepl("revenue", input$overview_metric))
                                    dollar_format() else comma_format()) +
      labs(x = NULL, y = metric_label) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "x unified")
  })

  output$country_bar_overview <- renderPlotly({
    top10 <- country_stats %>% slice_head(n = 10)
    p <- ggplot(top10, aes(x = reorder(country, revenue), y = revenue,
                            text = paste0(country, "<br>Revenue: $",
                                          format(revenue, big.mark=",")),
                            fill = country == "United Kingdom")) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = c("FALSE" = COL_BLUE, "TRUE" = COL_TEAL)) +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Revenue ($)") +
      theme_minimal(base_size = 11)
    ggplotly(p, tooltip = "text")
  })

  output$monthly_table <- renderDT({
    monthly_revenue %>%
      select(month_label, revenue, orders, customers,
             revenue_per_order, revenue_per_customer) %>%
      mutate(across(c(revenue, revenue_per_order, revenue_per_customer),
                    ~paste0("$", format(round(.), big.mark=",")))) %>%
      rename(Month = month_label, Revenue = revenue, Orders = orders,
             Customers = customers,
             `Rev/Order` = revenue_per_order,
             `Rev/Customer` = revenue_per_customer) %>%
      datatable(options = list(pageLength = 7, dom = "tp"),
                rownames = FALSE, class = "compact stripe")
  })

  # ---- TAB 2: CUSTOMER SEGMENTS ----

  output$rfm_scatter <- renderPlotly({
    df <- rfm_filtered()
    if (nrow(df) == 0) return(plotly_empty())
    plot_ly(
      data     = df,
      x        = ~recency,
      y        = ~frequency,
      size     = ~monetary,
      color    = ~segment,
      colors   = SEG_COLS,
      type     = "scatter",
      mode     = "markers",
      sizes    = c(4, 40),
      marker   = list(opacity = 0.65, line = list(width = 0)),
      text     = ~paste0(
        "<b>Customer ", customer_id, "</b><br>",
        "Segment: ", segment, "<br>",
        "Recency: ", recency, " days<br>",
        "Frequency: ", frequency, " orders<br>",
        "Monetary: $", format(round(monetary), big.mark = ","), "<br>",
        "Country: ", country
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis  = list(title = "Recency (days since last purchase)"),
        yaxis  = list(title = "Frequency (number of orders)"),
        legend = list(title = list(text = "<b>Segment</b>")),
        plot_bgcolor  = "#fafafa",
        paper_bgcolor = "#fafafa"
      )
  })

  output$segment_table <- renderDT({
    segment_summary %>%
      mutate(
        avg_monetary  = paste0("$", format(round(avg_monetary), big.mark=",")),
        total_revenue = paste0("$", format(round(total_revenue), big.mark=",")),
        customer_share = paste0(customer_share, "%"),
        revenue_share  = paste0(revenue_share, "%")
      ) %>%
      rename(
        Segment = segment, Customers = customers,
        `Cust %` = customer_share, `Avg Recency` = avg_recency,
        `Avg Freq` = avg_frequency, `Avg Monetary` = avg_monetary,
        `Total Revenue` = total_revenue, `Rev %` = revenue_share
      ) %>%
      datatable(options = list(dom = "t", pageLength = 5),
                rownames = FALSE, class = "compact stripe")
  })

  output$segment_pie <- renderPlotly({
    plot_ly(segment_summary,
            labels  = ~segment,
            values  = ~total_revenue,
            type    = "pie",
            marker  = list(colors = unname(SEG_COLS)),
            textinfo = "label+percent",
            hovertext = ~paste0(segment, "<br>$",
                                format(round(total_revenue), big.mark=","))) %>%
      layout(showlegend = FALSE,
             paper_bgcolor = "#fafafa")
  })

  # ---- TAB 3: CHURN RISK ----

  output$churn_bar <- renderPlotly({
    p <- ggplot(churn_summary,
                aes(x = reorder(churn_risk, -customers),
                    y = customers, fill = churn_risk,
                    text = paste0(churn_risk, "<br>",
                                  "Customers: ", format(customers, big.mark=","),
                                  " (", customer_share, "%)<br>",
                                  "Revenue: $", format(round(total_revenue), big.mark=","),
                                  " (", revenue_share, "%)<br>",
                                  "Avg Days Since Purchase: ", avg_recency))) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = CHURN_COLS) +
      scale_y_continuous(labels = comma_format()) +
      labs(x = NULL, y = "Number of Customers") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$whatif_result <- renderUI({
    at_risk_row  <- churn_summary %>% filter(churn_risk == "At Risk")
    lapsing_row  <- churn_summary %>% filter(churn_risk == "Lapsing")
    churned_row  <- churn_summary %>% filter(churn_risk == "Churned")

    recovery_factor <- input$recovery_rate / 100

    recovered_atrisk <- if (nrow(at_risk_row) > 0)
      round((input$recover_atrisk / 100) * at_risk_row$customers *
              at_risk_row$avg_monetary * recovery_factor) else 0
    recovered_lapsing <- if (nrow(lapsing_row) > 0)
      round((input$recover_lapsing / 100) * lapsing_row$customers *
              lapsing_row$avg_monetary * recovery_factor) else 0
    recovered_churned <- if (nrow(churned_row) > 0)
      round((input$recover_churned / 100) * churned_row$customers *
              churned_row$avg_monetary * recovery_factor) else 0

    total_recovered <- recovered_atrisk + recovered_lapsing + recovered_churned

    div(class = "whatif-box",
        tags$h4(style = "margin-top:0; color:#2e7d32;",
                "📊 Estimated Revenue Recovery"),
        fluidRow(
          column(3,
            tags$div("At Risk Recovery", style="font-size:12px;color:#666;"),
            tags$div(paste0("$", format(recovered_atrisk, big.mark=",")),
                     style="font-size:20px;font-weight:700;color:#388e3c;")),
          column(3,
            tags$div("Lapsing Recovery", style="font-size:12px;color:#666;"),
            tags$div(paste0("$", format(recovered_lapsing, big.mark=",")),
                     style="font-size:20px;font-weight:700;color:#f57c00;")),
          column(3,
            tags$div("Churned Recovery", style="font-size:12px;color:#666;"),
            tags$div(paste0("$", format(recovered_churned, big.mark=",")),
                     style="font-size:20px;font-weight:700;color:#c62828;")),
          column(3,
            tags$div("Total Recoverable", style="font-size:12px;color:#666;"),
            tags$div(paste0("$", format(total_recovered, big.mark=",")),
                     style="font-size:20px;font-weight:700;color:#1a237e;"))
        )
    )
  })

  output$churn_table <- renderDT({
    churn_summary %>%
      mutate(
        avg_monetary  = paste0("$", format(round(avg_monetary), big.mark=",")),
        total_revenue = paste0("$", format(round(total_revenue), big.mark=",")),
        customer_share = paste0(customer_share, "%"),
        revenue_share  = paste0(revenue_share, "%")
      ) %>%
      rename(
        `Churn Risk` = churn_risk, Customers = customers,
        `Cust %` = customer_share, `Avg Days` = avg_recency,
        `Avg Value` = avg_monetary, `Total Revenue` = total_revenue,
        `Rev %` = revenue_share
      ) %>%
      datatable(options = list(dom = "t", pageLength = 5),
                rownames = FALSE, class = "compact stripe")
  })

  # ---- TAB 4: PRODUCTS ----

  output$product_bar <- renderPlotly({
    if (input$prod_segment == "All Segments") {
      plot_df <- product_data %>%
        slice_head(n = input$prod_top_n) %>%
        mutate(label = str_trunc(description, 30))
    } else {
      plot_df <- product_segment %>%
        filter(segment == input$prod_segment) %>%
        slice_head(n = input$prod_top_n) %>%
        mutate(label = str_trunc(description, 30))
    }
    p <- ggplot(plot_df,
                aes(x = reorder(label, revenue), y = revenue,
                    text = paste0(description, "<br>Revenue: $",
                                  format(round(revenue), big.mark=","),
                                  "<br>Orders: ", format(orders, big.mark=","),
                                  "<br>Customers: ", format(customers, big.mark=",")))) +
      geom_col(fill = COL_BLUE, alpha = 0.85) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Revenue ($)") +
      theme_minimal(base_size = 11)
    ggplotly(p, tooltip = "text")
  })

  output$product_search_table <- renderDT({
    product_data %>%
      mutate(revenue = paste0("$", format(round(revenue), big.mark=",")),
             avg_unit_price = paste0("$", avg_unit_price)) %>%
      rename(
        `Stock Code` = stock_code, Product = description,
        Revenue = revenue, Orders = orders, Customers = customers,
        `Total Qty` = total_qty, `Avg Price` = avg_unit_price
      ) %>%
      datatable(
        filter  = "top",
        options = list(pageLength = 10, scrollX = TRUE,
                       dom = "lfrtip"),
        rownames = FALSE, class = "compact stripe"
      )
  })

  # ---- TAB 5: INTERNATIONAL ----

  output$country_bar <- renderPlotly({
    df <- country_stats
    if (input$exclude_uk) df <- df %>% filter(country != "United Kingdom")
    df <- df %>%
      arrange(desc(.data[[input$country_metric]])) %>%
      slice_head(n = input$country_top_n)

    metric_label <- switch(input$country_metric,
                            revenue              = "Revenue ($)",
                            revenue_per_customer = "Revenue per Customer ($)",
                            orders_per_customer  = "Orders per Customer",
                            avg_order_value      = "Avg Order Value ($)",
                            customers            = "Total Customers",
                            orders               = "Total Orders")
    p <- ggplot(df,
                aes(x = reorder(country, .data[[input$country_metric]]),
                    y = .data[[input$country_metric]],
                    text = paste0(country, "<br>",
                                  metric_label, ": ",
                                  if (grepl("revenue|value", input$country_metric))
                                    paste0("$", format(round(.data[[input$country_metric]]),
                                                       big.mark=","))
                                  else format(round(.data[[input$country_metric]], 1),
                                              big.mark=",")))) +
      geom_col(fill = COL_TEAL, alpha = 0.85) +
      coord_flip() +
      scale_y_continuous(labels = if (grepl("revenue|value", input$country_metric))
                                    dollar_format() else comma_format()) +
      labs(x = NULL, y = metric_label) +
      theme_minimal(base_size = 11)
    ggplotly(p, tooltip = "text")
  })

  output$country_table <- renderDT({
    country_stats %>%
      mutate(
        revenue              = paste0("$", format(revenue, big.mark=",")),
        revenue_per_customer = paste0("$", format(revenue_per_customer, big.mark=",")),
        avg_order_value      = paste0("$", format(avg_order_value, big.mark=","))
      ) %>%
      rename(
        Country = country, Revenue = revenue, Orders = orders,
        Customers = customers, `Rev/Customer` = revenue_per_customer,
        `Orders/Customer` = orders_per_customer,
        `Avg Order Value` = avg_order_value
      ) %>%
      datatable(filter = "top",
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE, class = "compact stripe")
  })

  # ---- TAB 6: RETURNS ----

  output$returns_chart <- renderPlotly({
    metric_label <- switch(input$return_metric,
                            return_value        = "Return Value ($)",
                            return_transactions = "Return Transactions",
                            total_neg_qty       = "Negative Quantity")
    p <- ggplot(returns_monthly,
                aes(x = date, y = abs(.data[[input$return_metric]]),
                    text = paste0(month_year, "<br>",
                                  metric_label, ": ",
                                  if (input$return_metric == "return_value")
                                    paste0("$", format(round(return_value), big.mark=","))
                                  else format(abs(.data[[input$return_metric]]),
                                              big.mark=",")))) +
      geom_col(fill = COL_RED, alpha = 0.8) +
      geom_line(aes(y = abs(.data[[input$return_metric]])),
                color = COL_NAVY, linewidth = 1) +
      scale_x_date(date_labels = "%b %y") +
      scale_y_continuous(labels = if (input$return_metric == "return_value")
                                    dollar_format() else comma_format()) +
      labs(x = NULL, y = metric_label) +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$returns_table <- renderDT({
    returns_monthly %>%
      mutate(return_value = paste0("$", format(return_value, big.mark=",")),
             total_neg_qty = format(total_neg_qty, big.mark=",")) %>%
      select(month_year, return_transactions, unique_invoices,
             total_neg_qty, return_value) %>%
      rename(Month = month_year,
             `Return Transactions` = return_transactions,
             `Unique Invoices` = unique_invoices,
             `Total Neg Qty` = total_neg_qty,
             `Return Value` = return_value) %>%
      datatable(options = list(dom = "tp", pageLength = 8),
                rownames = FALSE, class = "compact stripe")
  })

  # ---- TAB 7: FORECAST ----

  output$model_accuracy_table <- renderTable({
    data.frame(
      Model = c("Holt ETS", "Polynomial"),
      MAPE  = c("17.3%", "10.4%"),
      RMSE  = c("$126,842", "$74,509"),
      `R²`  = c("—", "0.889")
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  output$forecast_chart <- renderPlotly({
    # Build history
    hist_df <- monthly_history %>%
      mutate(date = as.Date(paste0("2011-", match(
        gsub(" 2011", "", month_label),
        month.abb), "-01")),
        type = "Actual")

    fc_dates <- as.Date(c("2011-12-01", "2012-01-01", "2012-02-01"))

    p <- plot_ly()

    # Historical line
    p <- add_trace(p,
      x    = ~hist_df$date,
      y    = ~hist_df$revenue,
      type = "scatter", mode = "lines+markers",
      name = "Actual Revenue",
      line = list(color = COL_NAVY, width = 2.5),
      marker = list(color = COL_NAVY, size = 7),
      text = ~paste0(hist_df$month_label, "<br>$",
                     format(hist_df$revenue, big.mark=",")),
      hoverinfo = "text"
    )

    # ETS forecast
    if ("Holt Damped ETS" %in% input$fc_models) {
      if (input$fc_ci95) {
        p <- add_ribbons(p,
          x    = fc_dates,
          ymin = forecast_data$ets_lo95,
          ymax = forecast_data$ets_hi95,
          fillcolor = "rgba(231,111,81,0.15)",
          line = list(color = "transparent"),
          name = "ETS 95% CI", showlegend = TRUE)
      }
      if (input$fc_ci80) {
        p <- add_ribbons(p,
          x    = fc_dates,
          ymin = forecast_data$ets_lo80,
          ymax = forecast_data$ets_hi80,
          fillcolor = "rgba(231,111,81,0.25)",
          line = list(color = "transparent"),
          name = "ETS 80% CI", showlegend = TRUE)
      }
      p <- add_trace(p,
        x    = fc_dates,
        y    = forecast_data$ets_point,
        type = "scatter", mode = "lines+markers",
        name = "Holt ETS Forecast",
        line = list(color = COL_RED, width = 2, dash = "dash"),
        marker = list(color = COL_RED, size = 8, symbol = "square"),
        text = ~paste0(forecast_data$month, "<br>ETS: $",
                       format(forecast_data$ets_point, big.mark=",")),
        hoverinfo = "text"
      )
    }

    # Polynomial forecast
    if ("Polynomial Regression (Deg 2)" %in% input$fc_models) {
      if (input$fc_ci95) {
        p <- add_ribbons(p,
          x    = fc_dates,
          ymin = forecast_data$poly_lo95,
          ymax = forecast_data$poly_hi95,
          fillcolor = "rgba(42,157,143,0.15)",
          line = list(color = "transparent"),
          name = "Poly 95% CI", showlegend = TRUE)
      }
      p <- add_trace(p,
        x    = fc_dates,
        y    = forecast_data$poly_point,
        type = "scatter", mode = "lines+markers",
        name = "Polynomial Forecast",
        line = list(color = COL_TEAL, width = 2, dash = "dot"),
        marker = list(color = COL_TEAL, size = 8, symbol = "diamond"),
        text = ~paste0(forecast_data$month, "<br>Poly: $",
                       format(forecast_data$poly_point, big.mark=",")),
        hoverinfo = "text"
      )
    }

    p %>% layout(
      xaxis  = list(title = NULL, tickformat = "%b %y"),
      yaxis  = list(title = "Revenue ($)", tickformat = "$,.0f"),
      legend = list(orientation = "h", y = -0.2),
      hovermode    = "x unified",
      plot_bgcolor  = "#fafafa",
      paper_bgcolor = "#fafafa",
      shapes = list(list(
        type = "line", x0 = "2011-11-01", x1 = "2011-11-01",
        y0 = 0, y1 = 1, yref = "paper",
        line = list(color = "gray", dash = "dot", width = 1.5)))
    )
  })

  output$forecast_table <- renderDT({
    out <- forecast_data %>%
      mutate(across(where(is.numeric),
                    ~paste0("$", format(round(.), big.mark=","))))
    names(out) <- c("Month",
                    "ETS Point", "ETS Lo 80%", "ETS Hi 80%",
                    "ETS Lo 95%", "ETS Hi 95%",
                    "Poly Point", "Poly Lo 95%", "Poly Hi 95%")
    datatable(out, options = list(dom = "t", pageLength = 5),
              rownames = FALSE, class = "compact stripe")
  })

} # end server

shinyApp(ui, server)
