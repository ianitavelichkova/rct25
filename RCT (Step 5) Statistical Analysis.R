
library(broom)
library(dplyr)
library(gt)
library(glue)

# Step 5.1
# Statistical Analysis of Total Assets
# Preparation of the Data  
data_all <- orbis_panel_berlin %>%
  filter(!is.na(toas), !is.na(shfd), !is.na(postcode)) %>%
  filter(year >= 2005, year <= 2022) %>%          
  mutate(group10405 = ifelse(postcode == 10405, 1, 0))

data_toas <- data_all %>%
  mutate(
    year = as.factor(year),
    year = relevel(year, ref = "2021")) # year 2021 as Intercept

# Regression of log(toas) on group10405 and years 
reg_model_toas <- lm(log(toas) ~ group10405 + year, data = data_toas)
summary(reg_model_toas)
model_sum <- summary(reg_model_toas)

# Preparation of the Table
# Extraction of Summary Statistics
nobs <- length(reg_model_toas$fitted.values)
rse <- model_sum$sigma              # Residual standard error
df <- model_sum$df[2]               # Residual degrees of freedom
r2 <- model_sum$r.squared           # R-squared
r2_adj <- model_sum$adj.r.squared   # Adjusted R-squared
fstat <- model_sum$fstatistic[1]    # F-statistic
fdf1 <- model_sum$fstatistic[2]     # F df1
fdf2 <- model_sum$fstatistic[3]     # F df2
pval <- pf(fstat, fdf1, fdf2, lower.tail = FALSE) # F-test p-value

# Dynamic Source Note Text
source_note_text <- glue("
This table presents the results of the pooled ordinary least squares regression conducted to examine differences in average total assets between firms registered in Berlin under postcode 10405 and the overall Berlin firm population. The test is based on the following hypotheses: H₀ (Null Hypothesis): Registration under the Berlin postcode 10405 has no significant effect on the mean of the log-transformed total assets. H₁ (Alternative Hypothesis): Registration under the Berlin postcode 10405 has a signifi- cant effect on the mean of the log-transformed total assets. The dependent variable, log(toas), is the natural logarithm of total assets. The primary independent variable, group10405, is a binary indicator which takes the value of 1 if a firm is located in the 10405 postcode area and 0 otherwise. To control for year-specific effects, the model includes binary categorical variables for each year from 2005 to 2022, with 2021 serving as the reference year. The year2005 - year2022 dummies take the value of 1 if an observation corresponds to the respective reporting year and 0 otherwise. The Intercept reflects the expected value of log-transformed total assets in 2021 for a Berlin firm not registered in the 10405 postcode area. The underlying dataset consists of {format(nobs, big.mark = ',')} firm-year observations from the period 2005–2022. The data is sourced from the Orbis Database and cleaned to exclude entries with missing inforation on postal codes, total assets and shareholder funds. The model exhibits a residual standard error of {round(rse, 3)} on {format(df, big.mark = ',')} degrees of freedom. The overall explanatory power of the model is indicated by an R-squared of {round(r2, 4)} and an adjusted R-squared of {round(r2_adj, 4)}. The F-statistic of {round(fstat, 0)} (on {fdf1} and {format(fdf2, big.mark = ',')} degrees of freedom) is highly significant  \n(p {format.pval(pval, digits=3, eps=0.001)}).
")

# Extraction of Regression Results 
reg_table <- tidy(reg_model_toas, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(., 5))) %>%
  rename(
    Term = term,
    Estimate = estimate,
    `Standard Error` = std.error,
    `T-Statistic` = statistic,
    `P-Value` = p.value,
    `CI Lower Bound` = conf.low,
    `CI Upper Bound` = conf.high
  ) %>%
  mutate(
    Significance = case_when(
      `P-Value` < 0.001 ~ "***",
      `P-Value` < 0.01  ~ "**",
      `P-Value` < 0.05  ~ "*",
      TRUE              ~ ""
    ),
    `P-Value` = ifelse(`P-Value` < 2e-16, "<2e-16", formatC(`P-Value`, digits = 5, format = "f"))
  )


# Formatted Table with Dynamic Source Note
gt_table <- reg_table %>%
  gt() %>%
  tab_header(
    title = html("<b>Regression Results: Effect of Registration in Postcode 10405 on Berlin Firms’ Total Assets</b>"),
    subtitle = html(
      "<i>Model: log(toas) = β₀ + β₁ · group10405 + 
    <span style='display:inline-block; text-align:center; line-height:0.7'>
      <span style='font-size:10px'> 2022</span><br>&sum;<sub>t=2005</sub>
    </span>  
    β<sub>t</sub> · year<sub>t</sub></i>"
    )
  ) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", size = px(12)),
    locations = cells_title(groups = "subtitle")
  ) %>%
  cols_label(Significance = "Signif.") %>%
  cols_align(align = "center", columns = where(is.numeric)) %>%
  cols_align(align = "center", columns = Significance) %>%
  cols_align(align = "center", columns = `P-Value`) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center", font = "Times New Roman", size = px(12)),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", size = px(12)),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", size = px(14)),
    locations = cells_title(groups = "title")
  ) %>%
  tab_options(
    table.font.size = px(10),
    data_row.padding = px(2),
    table.border.top.style = "double",
    table.border.bottom.style = "double",
    table_body.hlines.style = "none",
    heading.border.bottom.style = "solid",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "none"
  ) %>%
  tab_footnote(
    footnote = "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001",
    locations = cells_column_labels(columns = Significance)
  ) %>%
  tab_source_note(
    source_note = source_note_text
  ) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", size = px(10)),
    locations = cells_source_notes()
  )

# Save the HTML-Output
gtsave(gt_table, "~/Desktop/toas_regression_results.html")

# Save the Table as PDF 
pagedown::chrome_print(
  input = "~/Desktop/toas_regression_results.html",
  output = "~/Desktop/toas_regression_results.pdf",
  options = list(paperWidth = 11.69, paperHeight = 8.27)
)




# Step 5.2
# Statistical Analysis of Equity Ratios
# Preparation of the Data  
data_all <- orbis_panel_berlin %>%
  filter(!is.na(toas), !is.na(shfd), !is.na(postcode)) %>%
  filter(year >= 2005, year <= 2022) %>%          
  mutate(group10405 = ifelse(postcode == 10405, 1, 0))

data_eqrat <- data_all %>%            # avoid division by zero
  filter(toas != 0) %>%               # when calculating equity ratio
  mutate(eqrat = shfd / toas)   
  
# Outlier Removal
# Remove outliers per year using IQR method
data_eqrat_clean <- data_eqrat %>%
  group_by(year) %>%
  mutate(
    Q1 = quantile(eqrat, 0.25, na.rm = TRUE),
    Q3 = quantile(eqrat, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,          # lower bound for outlier removal
    upper = Q3 + 1.5 * IQR           # upper bound for outlier removal
  ) %>%
  filter(eqrat >= lower, eqrat <= upper) %>%
  select(-Q1, -Q3, -IQR, -lower, -upper) %>%
  ungroup()

# Regression
data_eqrat <- data_eqrat_clean %>%
  mutate(
    year = as.factor(year),
    year = relevel(year, ref = "2021")
  )

reg_model_eqrat <- lm(eqrat ~ group10405 + year, data = data_eqrat)

summary(reg_model_eqrat)
model_stats <- glance(reg_model_eqrat)

# Preparation of the Table
# Extraction of Summary Statistics
nobs <- length(reg_model_eqrat$fitted.values)
rse <- model_sum$sigma              # Residual standard error
df <- model_sum$df[2]               # Residual degrees of freedom
r2 <- model_sum$r.squared           # R-squared
r2_adj <- model_sum$adj.r.squared   # Adjusted R-squared
fstat <- model_sum$fstatistic[1]    # F-statistic
fdf1 <- model_sum$fstatistic[2]     # F df1
fdf2 <- model_sum$fstatistic[3]     # F df2
pval <- pf(fstat, fdf1, fdf2, lower.tail = FALSE) # F-test p-value

# Dynamic Source Note Text
source_note_text <- glue("
This table presents the results of the pooled ordinary least squares regression conducted to examine differences in average equity ratios between firms registered in Berlin under postcode 10405 and the overall Berlin firm population. The test is based on the following hypotheses: H₀ (Null Hypothesis): Registration under the Berlin postcode 10405 has no significant effect on the equity ratio. H₁ (Alternative Hypothesis): Registration under the Berlin postcode 10405 has a significant effect on the equity ratio. The dependent variable, eqrat indicates the equity ratio defined as the book value of equity devided by total assets. The primary independent variable, group10405, is a binary indicator which takes the value of 1 if a firm is located in the 10405 postcode area and 0 otherwise. To control for year-specific effects, the model includes binary categorical variables for each year from 2005 to 2022, with 2021 serving as the reference year. The year2005 - year2022 dummy variables take the value of 1 if an obser- vation corresponds to the respective reporting year and 0 otherwise. The Intercept reflects the expected equity ratio in 2021 for a Berlin firm not registered in the 10405 postcode area. The underlying dataset consists of {format(nobs, big.mark = ',')} firm-year observations from the period 2005–2022. The data is sourced from the Orbis Database and cleaned to exclude outlying values as well as entries with missing information on postal codes, total assets and shareholder funds. The model exhibits a residual standard error of {round(rse, 3)} on {format(df, big.mark = ',')} degrees of freedom. The overall explanatory power of the model is indicated by an R-squared of {round(r2, 4)} and an adjusted R-squared of {round(r2_adj, 4)}. The F-statistic of {round(fstat, 0)} (on {fdf1} and {format(fdf2, big.mark = ',')} degrees of freedom) is highly significant  \n(p {format.pval(pval, digits=3, eps=0.001)}).
")

# Extraction of Regression Results 
reg_table <- tidy(reg_model_eqrat, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(., 5))) %>%
  rename(
    Term = term,
    Estimate = estimate,
    `Standard Error` = std.error,
    `T-Statistic` = statistic,
    `P-Value` = p.value,
    `CI Lower Bound` = conf.low,
    `CI Upper Bound` = conf.high
  ) %>%
  mutate(
    Significance = case_when(
      `P-Value` < 0.001 ~ "***",
      `P-Value` < 0.01  ~ "**",
      `P-Value` < 0.05  ~ "*",
      TRUE              ~ ""
    ),
    `P-Value` = ifelse(`P-Value` < 2e-16, "<2e-16", formatC(`P-Value`, digits = 5, format = "f"))
  )


# Formatted Table with Dynamic Source Note
gt_table <- reg_table %>%
  gt() %>%
  tab_header(
    title = html("<b>Regression Results: Effect of Registration in Postcode 10405 on Berlin Firms’ Equity Ratios</b>"),
    subtitle = html(
      "<i>Model: eqrat = β₀ + β₁ · group10405 + 
    <span style='display:inline-block; text-align:center; line-height:0.7'>
      <span style='font-size:10px'> 2022</span><br>&sum;<sub>t=2005</sub>
    </span>  
    β<sub>t</sub> · year<sub>t</sub></i>"
    )) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", size = px(12)),
    locations = cells_title(groups = "subtitle")
  ) %>%
  cols_label(Significance = "Signif.") %>%
  cols_align(align = "center", columns = where(is.numeric)) %>%
  cols_align(align = "center", columns = Significance) %>%
  cols_align(align = "center", columns = `P-Value`) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center", font = "Times New Roman", size = px(12)),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", size = px(12)),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", size = px(14)),
    locations = cells_title(groups = "title")
  ) %>%
  tab_options(
    table.font.size = px(10),
    data_row.padding = px(2),
    table.border.top.style = "double",
    table.border.bottom.style = "double",
    table_body.hlines.style = "none",
    heading.border.bottom.style = "solid",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "none"
  ) %>%
  tab_footnote(
    footnote = "Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001",
    locations = cells_column_labels(columns = Significance)
  ) %>%
  tab_source_note(
    source_note = source_note_text
  ) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", size = px(10)),
    locations = cells_source_notes()
  )

# Save the HTML-Output
gtsave(gt_table, "~/Desktop/eqrat_regression_results.html")

# Save the Table as PDF 
pagedown::chrome_print(
  input = "~/Desktop/eqrat_regression_results.html",
  output = "~/Desktop/eqrat_regression_results.pdf",
  options = list(paperWidth = 11.69, paperHeight = 8.27)
)





