
library(rstudioapi)
library(tidyverse, quietly = TRUE)
library(readxl)
library(dplyr)

suppressPackageStartupMessages(library(tidyverse))

options(dplyr.width = Inf)
options(dplyr.summarise.inform = FALSE)
options(dplyr.show_progress = FALSE)

start_script_time <- Sys.time()

# Parameters
rnd <- 5 # rounding (number of decimals)
u_EJ_GWa <- 31.71
min_shr_fuel <- 0.01

param <- list(subsidies_renovation_type = "ad_valorem",
            objective_renovation = NULL,
            budget_constraint_insulation = NULL,
            premature_replacement = 3,
            anticipate_renovation = 3,
            sub_heater_type = "ad_valorem",
            budget_constraint_heater = NULL,
            recycling_rebates = "lump_sum",
            rate_dem_target = NULL,
            mandatory_switch = FALSE,
            inertia_wtp = 4.3,
            social_discount_rate = 0.03,
            heat_pump_floor_cost = FALSE,
            short_term_price_elasticity = -0.2,
            credit_constraint = 0.05,
            duration_remboursment = 10,
            nzeb = FALSE,
            tol = 1e-2)

source("./STURM_model/F10_scenario_run.R")

# paths
path_rcode <- paste(getwd(), "/STURM_model/", sep = "")
path_in <- paste(getwd(), "/STURM_data/", sep = "")
path_out <- paste(getwd(), "/STURM_output/results/", sep = "")
if (!dir.exists(path_out)) {
    dir.create(path_out)
}

# configuration file STURM
base_year <- 2015
end_year <- 2050
step_year <- 5

# configuration file STURM
region <- c("WEU", "EEU")
sector <- "resid"
file_inputs <- "input_list_resid_EU.csv"
file_scenarios <- "scenarios_EU.csv"
en_method <- "TABULA" # "VDD", "TABULA"

run <- "policies"
if (run == "test") {
    file_scenarios <- "scenarios_test_EU.csv"
    energy_efficiency <- "exogenous"
    runs <- c(
        "EU_no",
        "EU",
        "EU_double",
        "EU_double_emission_1p5c")
} else if (run == "full") {
    file_scenarios <- "scenarios_full_EU.csv"
    energy_efficiency <- "endogenous"
    runs <- c(
        "EU",
        "EU_constant",
        "EU_1p5c",
        "EU_ambitious_shell",
        "EU_ambitious_heat",
        "EU_carbon_tax_medium",
        "EU_carbon_tax_ambitious",
        "EU_carbon_tax",
        "EU_carbon_tax_policies"
        )
} else if (run == "policies") {
    file_scenarios <- "scenarios_EU.csv"
    energy_efficiency <- "endogenous"
    runs <- c("EU", "EU_mix", "EU_mix_ambitious", "EU_mix_ban")
    runs <- c(
        "EU_carbon_tax_heat_pump", "EU_carbon_tax_dual",
        "EU_carbon_tax", "EU_carbon_tax_rebates", "EU_carbon_tax_rebates_q1",
        "EU")
    runs <- c(
        "EU_reno", "EU_reno_lowincome", "EU_reno_performance",
        "EU_reno_all", "EU_reno_all_lowincome", "EU_reno_all_performance")
    runs <- c("EU_reno_endogenous")
    runs <- c("EU_reno_endogenous")
    runs <- c(
        "EU", "EU_carbon_tax"
    )
}

report <- list(var = c("energy"),
               type = c("STURM"),
               geo_level = "region_bld")

yrs <- seq(base_year, end_year, step_year)

for (run in runs) {

    if (energy_efficiency == "exogenous") {
        run <- paste(run, energy_efficiency, sep = "_")
    }

    sturm_scenarios <- run_scenario(
        run = run,
        scenario_name = run,
        sector = sector,
        path_in = path_in,
        path_rcode = path_rcode,
        path_out = path_out,
        file_inputs = file_inputs,
        file_scenarios = file_scenarios,
        geo_level_report = report$geo_level,
        yrs = yrs,
        report_var = report$var,
        report_type = report$type,
        region = region,
        energy_efficiency = energy_efficiency,
        en_method = en_method
    )

end_script_time <- Sys.time()
print(paste("Time to run script:",
    round(end_script_time - start_script_time, 0), "seconds."))
}