
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)


# Loding figures setttings and functions
source("STURM_output/C00_plots.R")

ref <- "Counterfactual"
base_year <- 2015
run <- "standalone"
stp <- 5

# costs-benefits analysis parameters
lifetime_renovation <- 30 #30 # years
lifetime_heater <- 20 # years
social_discount <- 0.02 # % per year




scenarios <- c("EU" = "Counterfactual",
    "EU_carbon_tax_low" = "Carbon tax EUETS2",
    "EU_carbon_tax" = "Carbon tax EUETS",
    "EU_carbon_tax_social" = "Carbon tax social",
    "EU_carbon_tax_rebates" = "Carbon tax EUETS rebates",
    "EU_hp_subsidies" = "Heat pump subsidies",
    # "EU_hp_high_elasticity" = "Heat pump high elasticity",
    "EU_hp_learning" = "Heat pump learning",
    "EU_barriers_heater" = "Barriers heater",
    "EU_reno" = "Renovation wave",
    "EU_deep_reno" = "Deep renovation wave",
    "EU_barriers_renovation" = "Barriers renovation",
    "EU_realization_rate" = "Quality renovation"
    )

scenarios <- c(
  "EU" = "Counterfactual",
  "EU_carbon_tax_social" = "Carbon tax social",
  "EU_carbon_tax" = "Carbon tax EUETS"
)


path <- "STURM_data/input_csv/input_resid/macro/social_cost_carbon_20.csv"
social_cost_carbon <- read.csv(path) %>%
  rename(social_cost_carbon = value)

# reading data
save_dir <- paste("STURM_output", "figures", run, sep = "/")
if (!dir.exists(save_dir)) {
  dir.create(save_dir)
}

data <- data.frame(NULL)

for (scenario in names(scenarios)) {
  print(paste("Scenario:", scenario))
  file <- paste0("report_agg_STURM_", scenario, "_resid_region_bld_energy.csv")
  file <- paste("STURM_output/results", file, sep = "/")

  if (!file.exists(file)) {
    print("Check file name or directory!")
  }

  # Read output files
  temp <- read.csv(file) %>%
    mutate(scenario = scenario)
  
  data <- bind_rows(data, temp)
}
# Reconstuction flows
flows <- c("n_renovation", "cost_renovation_EUR", "sub_renovation_EUR",
  "n_replacement", "cost_heater_EUR", "sub_heater_EUR",
  "to_pay_renovation", "to_pay_heater", "taxes_revenues_EUR")

data <- data %>%
  mutate(scenario = scenarios[.data[["scenario"]]])

stp <- 5

data <- data %>%
 mutate(year = ifelse(variable %in% flows, year - stp, year),
        value = ifelse(variable %in% flows, value
         / stp, value))
data <- distinct(data)

# Output table
output_table(data, save_dir = save_dir, end_year = 2030, base_year = 2015)
format_temp <- output_table(data, save_dir = save_dir,
  end_year = 2050, base_year = 2015)

format_temp_eu <- format_temp %>%
  filter(Scenario != "Initial") %>%
  filter(`Member states` == "EU") %>%
  mutate(`Energy poverty (Percent)` = `Energy poverty (Percent)` * 100) %>%
  mutate(`Cost total (Billion EUR)` =
    `Cost renovation (Billion EUR)` + `Cost heater (Billion EUR)`) %>%
  mutate(`Cost total (Billion EUR per year)` =
    `Cost total (Billion EUR)` / 35) %>%
  mutate(Scenario = factor(Scenario), levels = unname(Scenario))

# Cost-benefits analysis
data_raw <- data

data <- filter(data,
    resolution == "all",
    variable %in% c("cost_renovation_EUR", "cost_heater_EUR",
      "cost_heat_EUR", "heat_tCO2", "thermal_comfort_EUR")) %>%
    filter(year > 2015) %>%
    select(-resolution)

wide_data <- pivot_wider(data,
                         id_cols = c("region_bld", "year", "scenario"),
                         names_from = variable,
                         values_from = value)

wide_data <- wide_data %>%
  left_join(social_cost_carbon, by = "year") %>%
  mutate(
    cost_renovation_EUR =
      ifelse(is.na(cost_renovation_EUR), 0, cost_renovation_EUR),
    cost_heater_EUR =
      ifelse(is.na(cost_heater_EUR), 0, cost_heater_EUR),
    annuities_renovation =
      social_discount / (1 - (1 + social_discount)^(-lifetime_renovation)),
    annuities_heater =
      social_discount / (1 - (1 + social_discount)^(-lifetime_heater)),
    cost_renovation_EUR_year = cost_renovation_EUR * annuities_renovation * stp,
    cost_heater_EUR_year = cost_heater_EUR * annuities_heater * stp,
    cost_emission_EUR = heat_tCO2 * social_cost_carbon * stp,
    cost_heat_EUR = cost_heat_EUR * stp,
    thermal_comfort_EUR = - thermal_comfort_EUR * stp
    )

wide_data <- wide_data %>%
  rowwise() %>%
  mutate(cost_renovation_EUR_cumsum =
    sum(wide_data$cost_renovation_EUR_year[wide_data$region_bld == region_bld &
        wide_data$scenario == scenario &
        wide_data$year <= year &
        wide_data$year >= (year - lifetime_renovation)], na.rm = TRUE)) %>%
  mutate(cost_heater_EUR_cumsum =
    sum(wide_data$cost_heater_EUR_year[wide_data$region_bld == region_bld &
        wide_data$scenario == scenario &
        wide_data$year <= year &
        wide_data$year > (year - lifetime_heater)], na.rm = TRUE)) %>%
  ungroup()

wide_data <- wide_data %>%
  mutate(running_cost = cost_renovation_EUR_cumsum + cost_heater_EUR_cumsum +
    cost_heat_EUR + cost_emission_EUR + thermal_comfort_EUR) %>%
  mutate(running_cost_private = running_cost - cost_emission_EUR)

write.csv(filter(wide_data, region_bld == "EU"), paste0(save_dir, "/cba_calculation_eu.csv"))

long_data <- wide_data %>%
  select("region_bld", "year", "scenario", "cost_renovation_EUR_cumsum",
    "cost_heater_EUR_cumsum", "cost_heater_EUR_cumsum",
    "cost_heat_EUR", "cost_emission_EUR", "thermal_comfort_EUR", "running_cost",
    "running_cost_private") %>%
  pivot_longer(cols = c("cost_renovation_EUR_cumsum",
    "cost_heater_EUR_cumsum", "cost_heater_EUR_cumsum",
    "cost_heat_EUR", "cost_emission_EUR", "thermal_comfort_EUR", "running_cost",
    "running_cost_private"),
               names_to = "variable",
               values_to = "value")

data_ref <- long_data %>%
  filter(scenario == ref) %>%
  rename(value_ref = value) %>%
  select(-scenario)

diff <- long_data %>%
  filter(scenario != ref) %>%
  left_join(data_ref,
    by = c("region_bld", "year", "variable")) %>%
  mutate(diff = (value - value_ref)) %>%
  select(-c(value, value_ref)) %>%
  rename(value = diff)

wide_data_diff <- pivot_wider(diff,
                         id_cols = c("region_bld", "year", "scenario"),
                         names_from = variable,
                         values_from = value) %>%
                  mutate(discount_factor = 1 / ((1 + social_discount)^(year - 2015)))

# households
pop <- data_raw %>%
  filter(variable == "stock_building", resolution == "all") %>%
  pivot_wider(id_cols = c("region_bld", "year", "scenario"),
              names_from = variable,
              values_from = value) %>%
  mutate(discount_factor = 1 / ((1 + social_discount)^(year - 2015))) %>%
  filter(year >= 2020) %>%
  group_by(scenario, region_bld) %>%
  summarize(
    stock_building_avg = sum(stock_building * discount_factor) / sum(discount_factor)
  ) %>%
  ungroup()

nb_years <- 30

# summary
summary <- wide_data_diff %>%
  group_by(scenario, region_bld) %>%
  summarize(
    cost_renovation_sum = sum(cost_renovation_EUR_cumsum * discount_factor),
    cost_heater_sum = sum(cost_heater_EUR_cumsum * discount_factor),
    cost_heat_sum = sum(cost_heat_EUR * discount_factor),
    cost_emission_sum = sum(cost_emission_EUR * discount_factor),
    thermal_comfort_sum = sum(thermal_comfort_EUR * discount_factor),
    running_cost = sum(running_cost * discount_factor),
    running_cost_private = sum(running_cost_private * discount_factor)
  ) %>%
  ungroup() %>%
  left_join(pop) %>%
  mutate(
    cost_renovation_sum = cost_renovation_sum / (stock_building_avg * nb_years),
    cost_heater_sum = cost_heater_sum/ (stock_building_avg * nb_years),
    cost_heat_sum = cost_heat_sum / (stock_building_avg * nb_years),
    cost_emission_sum = cost_emission_sum / (stock_building_avg * nb_years),
    thermal_comfort_sum = thermal_comfort_sum / (stock_building_avg * nb_years),
    running_cost = running_cost / (stock_building_avg * nb_years),
    running_cost_private = running_cost_private / (stock_building_avg * nb_years)
  ) %>%
  select(-stock_building_avg)


# Make table recap
list_variables <- c(
  "Space heating consumption (TWh)",
  "Space heating consumption electricity (TWh)",
  "Emission (MtCO2)",
  "Emission cumulated (GtCO2)",
  "Total cost (Billion EUR)",
  "Government expenditures (Billion EUR)"
  )

file <- paste0(save_dir, "/", run, "_2050_summary_eu.csv")
if (file.exists(file)) {
  # read file column as character



  summary_2050 <- read.csv(file, check.names = FALSE) %>%
    # select only rows who variables are in list_variables
    filter(variable %in% list_variables) %>%
    # remove column "Initial"
    select(-Initial) %>%
    mutate(across(-c(variable, all_of(ref)), ~ . - .data[[ref]])) %>%
    select(variable, all_of(ref), everything())


  # Calculate difference of each column with the column "Current policies"
  

  rename_list <- c(
  cost_renovation_sum = "Delta cost renovation (euro/hh/year)",
  cost_heater_sum = "Delta cost heating system (euro/hh/year)",
  cost_heat_sum = "Delta cost fuel (euro/hh/year)",
  thermal_comfort_sum = "Delta thermal comfort (euro/hh/year)",
  running_cost_private = "Delta total cost private (euro/hh/year)",
  cost_emission_sum = "Delta cost emission (euro/hh/year)",
  running_cost = "Delta total cost (euro/hh/year)"
  )


  temp <- summary %>%
    filter(region_bld == "EU") %>%
    select(-region_bld) %>%
    gather(variable, value, cost_renovation_sum, cost_heater_sum, cost_heat_sum,
    , thermal_comfort_sum, running_cost_private, cost_emission_sum, running_cost) %>%
    mutate(variable = rename_list[.data[["variable"]]]) %>%
    # put scneario in column
    pivot_wider(names_from = scenario, values_from = value) 

  
  test <- bind_rows(summary_2050, temp) %>%
      select(variable, all_of(ref), everything()) %>%
      # if na put nothing
      mutate(across(-variable, ~ ifelse(is.na(.), "", formatC(., format = "f", digits = 2)))) 

  write.csv(test, paste0(save_dir, "/results.csv"))

rename_list <- c(
  cost_renovation_sum = "Cost renovation",
  cost_heater_sum = "Cost heating system",
  cost_heat_sum = "Cost fuel",
  thermal_comfort_sum = "Thermal comfort",
  running_cost_private = "Total cost private",
  cost_emission_sum = "Cost emission",
  running_cost = "Total cost"
)

color_list <- c(
  "Cost renovation" = "#ee6a6b",
  "Cost heating system" = "#62c5c0",
  "Cost fuel" = "#00589d",
  "Cost emission" = "#fdbb40",
  "Thermal comfort" = "lightblue"
)

df <- summary %>%
  gather(variable, value, cost_renovation_sum, cost_heater_sum, cost_heat_sum,
    cost_emission_sum, thermal_comfort_sum, running_cost, running_cost_private) %>%
  mutate(variable = rename_list[.data[["variable"]]]) %>%
  filter(scenario != ref) %>%
  # mutate(scenario = scenarios[.data[["scenario"]]]) %>%
  mutate(value = - value)

total <- df %>%
  filter(variable == "Total cost") %>%
  rename(total_value = value) %>%
  select(-variable)

total_private <- df %>%
  filter(variable == "Total cost private") %>%
  rename(total_value_private = value) %>%
  select(-variable)

df <- df %>%
  filter(variable != "Total cost", variable != "Total cost private")

df <- df %>%
  left_join(total, by = c("scenario", "region_bld")) %>%
  left_join(total_private, by = c("scenario", "region_bld"))

presentation <- FALSE
legend <- TRUE

stacked_plots(df, subplot_column = "region_bld",
  save_path = paste0(save_dir, "/cba_countries.png"),
  color_list = color_list)

stacked_plots(filter(df, region_bld == "EU"),
  save_path = paste0(save_dir, "/cba_eu.png"),
  color_list = color_list, y_label_suffix = "EUR/(year.hh)",
  presentation = presentation, legend = legend)

stacked_plots(filter(df, region_bld == "EU"),
  save_path = paste0(save_dir, "/cba_eu_horizontal.png"),
  color_list = color_list, y_label_suffix = "EUR/(year.hh)",
  presentation = presentation, legend = legend, horizontal = TRUE)


}
