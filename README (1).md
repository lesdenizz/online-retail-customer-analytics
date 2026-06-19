# Online Retail Customer Analytics

RFM segmentation, churn risk analysis, and revenue forecasting on 541,909 UK retail transactions, built with R and deployed as an interactive Shiny dashboard.

🔗 **[Live Dashboard](https://online-retail-analytics.share.connect.posit.cloud/)** *(update with your actual URL)*

---

## Business Problem

Online retailers collect large amounts of transaction data, but this data is only useful when converted into business insight. This project answers four questions:

1. Who are the most valuable customers, and how do we retain them?
2. Which customers are at risk of churning, and what is the revenue impact of losing them?
3. Which products and international markets offer the best growth opportunities?
4. What will revenue look like over the next quarter?

---

## Dataset

- **Source:** UCI Machine Learning Repository — Online Retail Dataset
- **Size:** 541,909 transactions | Dec 2010 – Dec 2011 | 38 countries
- **Customers:** 4,372 unique identified customers
- **Revenue:** $8.9M total revenue analyzed

---

## Methods

| Technique | Purpose |
|---|---|
| **RFM Segmentation** | Classified customers into High/Medium/Low Value based on Recency, Frequency, Monetary value |
| **Churn Risk Scoring** | 4-tier classification (Active/At Risk/Lapsing/Churned) based on recency |
| **Holt Damped ETS** | Time-series forecasting model (MAPE 17.3%) |
| **Polynomial Regression (Degree 2)** | Curve-fitting forecast model (MAPE 10.4%, R² = 0.889) |
| **Anomaly Detection** | Flagged extreme invoices, return-heavy customers, and data quality issues |

---

## Key Findings

- **70% of revenue comes from 21.3% of customers** (High Value segment) — classic 80/20 concentration
- **24.9% of transactions have no Customer ID** — a major data capture gap limiting analytics coverage
- Revenue grew steadily through 2011 with a **sharp Q4 acceleration** — Sep–Nov alone drove ~40% of annual revenue
- **Norway and Switzerland** show the highest revenue-per-customer — high-quality, underdeveloped international markets
- Two customers had **100% return rates**, flagging likely cancelled bulk orders requiring commercial review

---

## Repository Contents

| File | Description |
|---|---|
| `online_retail_complete.Rmd` | Full R Markdown analysis — data cleaning, RFM, churn, product-by-segment, international markets, forecasting, business recommendations |
| `app.R` | Interactive Shiny dashboard (7 tabs: Overview, Segments, Churn, Products, International, Returns, Forecast) |
| `data_prep.R` | Pre-aggregates the raw dataset into lightweight `.rds` files for the Shiny app |

---

## Dashboard Features

- **RFM Scatter Plot** — interactive, hover for individual customer details
- **Revenue Recovery Calculator** — what-if tool estimating revenue impact of churn-recovery campaigns
- **Country Comparison** — toggle between 6 different ranking metrics
- **Forecast Toggle** — compare Holt ETS vs Polynomial Regression with adjustable confidence intervals

---

## Tools

R · tidyverse · Shiny · plotly · forecast · DT · RFM Analysis · Time-Series Forecasting

---

## Author

Abdulrahman Jalilov — [LinkedIn](#) · [Portfolio](#)
