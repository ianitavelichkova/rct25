getwd()
library(dplyr)
library(purrr)
library(broom)
library(tidyr)


# Step 3
orbis_panel_berlin <- readRDS("~/Desktop/R/orbis_panel_berlin.rds")
names(orbis_panel_berlin) 

spice_nerds <- subset(orbis_panel_berlin, name_internat == "SpiceNerds GmbH", select = c(name_internat, source, year, toas))
print(spice_nerds)

von_greifswald <- subset(orbis_panel_berlin, name_internat == "von Greifswald GmbH", select = c(name_internat, source,year, toas))
print(von_greifswald)

boetzow <- subset(orbis_panel_berlin, name_internat == "Boetzow Berlin GmbH & Co. KG",  select = c(name_internat, source, year, toas))
print(boetzow)

final_escape <- subset(orbis_panel_berlin, name_internat == "Final Escape 2.1 GmbH",  select = c(name_internat, source,year, toas))
print(final_escape)

final_escape_variations <- subset(orbis_panel_berlin, grepl("^Final Escape", name_internat), select = c(name_internat, source, year, toas))
print(final_escape_variations)


#Step 4
library(dplyr)
orbis_panel_berlin %>%
  filter(postcode == 10405, year== 2021, !is.na(toas), !is.na(shfd)) %>%
  arrange(desc(toas)) %>%
  slice(1)%>%
  select(name_internat, toas, shfd, turn, addr_native)



# Step 5
# Filter the Sample
data_all <- orbis_panel_berlin %>%
  filter(!is.na(toas), !is.na(shfd), !is.na(postcode))

na_counts <- sapply(orbis_panel_berlin[, c("postcode", "toas", "shfd")], function(x) sum(is.na(x)))
  na_counts

data_all %>%
  group_by(year) %>%
  summarise(unique_companies = n_distinct(name_internat)) %>%
  arrange(year) %>%
  print(n = Inf)

# Define Groups
# Postcode 10405
data_10405 <- data_all %>% 
  filter(postcode == 10405)

data_10405 %>%
  group_by(year) %>%
  summarise(unique_companies = n_distinct(name_internat)) %>%
  arrange(year) %>%
  print(n = Inf)

# All other Postcodes 
data_other <- data_all %>% 
  filter(postcode != 10405)

data_other %>%
  group_by(year) %>%
  summarise(unique_companies = n_distinct(name_internat)) %>%
  arrange(year) %>%
  print(n = Inf)

# Step 5.1: Analysis of Total Assets
#Define Total Assets
toas_10405 <- data_10405 %>%
  select(year, toas) %>%
  mutate(group = "postcode_10405")

toas_all <- data_other %>%
  select(year, toas) %>%
  mutate(group = "all_berlin")

# Combine datasets and apply log-transformation
combined <- bind_rows(toas_10405, toas_all) %>%
  filter(!is.na(toas), toas > 0) %>%      # log() requires positive values
  mutate(log_toas = log(toas))

# Create Discriptive Statistics
# For Untransformed Total Assets
summary_long <- combined %>%
  group_by(year, group) %>%
  summarise(
    mean = mean(toas),
    median = median(toas),                # to apply the log-transformation
    sd = sd(toas),                        # replace the variable
    q1 = quantile(toas, 0.25),            # toas with log(toas)
    q3 = quantile(toas, 0.75),
    iqr = IQR(toas),
    n = n(),
    .groups = "drop" )

summary_wide <- summary_long %>%
  pivot_wider(
    names_from = group,
    values_from = c(mean, median, sd, q1, q3, iqr, n),
    names_sep = "_" )

print(summary_wide)%>% print(n = Inf)
write_xlsx(summary_wide, path = "toas_summary.xlsx")

# The code used for the analysis of the statistically significant  
# differences in Total Assets between the groups
# is submitted separately 




# Step 5.2: Analysis of Equity Ratios
# Count entities with zero totas assets
count_zero_toas <- sum(orbis_panel_berlin$toas <= 0, na.rm = TRUE)
print(count_zero_toas)

# Filter and clean the data
eqrat_data <- data_all %>%            # avoid division by zero
  filter(toas != 0) %>%               # when calculating equity ratio
  mutate(eqrat = shfd / toas)         # eqrat = equity ratio

# Define two Groups
eqrat_10405 <- eqrat_data %>%
  filter(postcode == "10405") %>%
  select(year, eqrat) %>%
  mutate(group = "postcode_10405")

eqrat_all <- eqrat_data %>%
  filter(postcode != "10405") %>%
  select(year, eqrat) %>%
  mutate(group = "all_berlin")

combined <- bind_rows(eqrat_10405, eqrat_all) 

# Descriptive statistics for Equity Ratios
summary_long <- combined %>%
  group_by(year, group) %>%
  summarise(
    mean = mean(eqrat),
    median = median(eqrat),
    sd = sd(eqrat),
    q1 = quantile(eqrat, 0.25),
    q3 = quantile(eqrat, 0.75),
    iqr = IQR(eqrat),
    n = n(),
    .groups = "drop" )

summary_wide <- summary_long %>%
  pivot_wider(
    names_from = group,
    values_from = c(mean, median, sd, q1, q3, iqr, n),
    names_sep = "_" )

print(summary_wide)%>% print(n = Inf)
write_xlsx( summary_wide, path = "eqrat_summary.xlsx" )

# Outlier Removal
# Remove outliers per year using IQR method
eqrat_data_clean <- eqrat_data %>%
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

# Outlier Removal Summary
original_counts <- eqrat_data %>%
  count(year, name = "n_before")
clean_counts <- eqrat_data_no_outliers %>%
  count(year, name = "n_after")

removal_summary <- original_counts %>%
  left_join(clean_counts, by = "year") %>%
  mutate(removed = n_before - n_after)

print(removal_summary)%>% print(n = Inf)

# Re-Define two Groups
eqrat_10405 <- eqrat_data_clean %>%
  filter(postcode == "10405") %>%
  select(year, eqrat) %>%
  mutate(group = "postcode_10405")

eqrat_all <- eqrat_data_clean %>%
  filter(postcode != "10405") %>%
  select(year, eqrat) %>%
  mutate(group = "all_berlin")

combined <- bind_rows(eqrat_10405, eqrat_all) 

# Descriptive statistics for Equity Ratios 
# after the Outlier Removal
summary_long <- combined %>%
  group_by(year, group) %>%
  summarise(
    mean = mean(eqrat),
    median = median(eqrat),
    sd = sd(eqrat),
    q1 = quantile(eqrat, 0.25),
    q3 = quantile(eqrat, 0.75),
    iqr = IQR(eqrat),
    n = n(),
    .groups = "drop" )

summary_wide <- summary_long %>%
  pivot_wider(
    names_from = group,
    values_from = c(mean, median, sd, q1, q3, iqr, n),
    names_sep = "_" )

# View and save the result
print(summary_wide)%>% print(n = Inf)
write_xlsx( summary_wide, path = "clean_eqrat_summary.xlsx" )


# The code used for the analysis of the statistically significant  
# differences in Equity Ratios between the groups
# is submitted separately 
