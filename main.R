
#library(rstudioapi)
library(tidyverse, quietly = TRUE)
library(readxl)
library(dplyr)
# Load the parallel package
library(parallel)
library(argparse)


suppressPackageStartupMessages(library(tidyverse))

options(dplyr.width = Inf)
options(dplyr.summarise.inform = FALSE)
options(dplyr.show_progress = FALSE)

start_script_time <- Sys.time()

# Create a parser object
parser <- ArgumentParser(description = "Script to set number of cores")

# Add argument for number of cores
parser$add_argument("-c", "--cores", type = "integer", default = NULL,
                    help = "Number of cores to use")

# Parse the arguments
args <- parser$parse_args()

# Set num_cores based on the argument or use the default value
if (is.null(args$cores)) {
  num_cores <- detectCores() - 2
} else {
  num_cores <- args$cores
}

# Print the number of cores to be used
print(paste("Number of cores to be used:", num_cores))

# Parameters
rnd <- 5 # rounding (number of decimals)
u_EJ_GWa <- 31.71
min_shr_fuel <- 0.01

param <- list(subsidies_renovation_type = "per_kWh",
            objective_renovation = NULL,
            objective_heat_pump = NULL,
            budget_constraint_insulation = NULL,
            premature_replacement = 3,
            anticipate_renovation = 5,
            sub_heater_type = "per_CO2",
            budget_constraint_heater = NULL,
            recycling_rebates = "lump_sum",
            rate_dem_target = NULL,
            mandatory_switch = FALSE,
            inertia_wtp = 4.3,
            social_discount_rate = 0.03,
            heat_pump_floor_cost = TRUE,
            short_term_price_elasticity = -0.2,
            credit_constraint = 0.05,
            duration_remboursment = 10,
            nzeb = FALSE,
            realization_rate_renovation = 1,
            elasticity_renovation = -1,
            elasticity_heat_pump = -1,
            remove_barriers_renovation = FALSE,
            remove_barriers_heater = FALSE,
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

file_scenarios <- "all_scenarios.csv"

# configuration file STURM
region <- c("WEU", "EEU")
sector <- "resid"
file_inputs <- "input_list_resid_EU.csv"
en_method <- "TABULA" # "VDD", "TABULA"
energy_efficiency <- "endogenous"
run <- "policies"

runs <- c("EU",
"EU_carbon_tax_low",
"EU_carbon_tax",
"EU_carbon_tax_social",
"EU_carbon_tax_rebates",
"EU_hp_subsidies",
# "EU_hp_high_elasticity",
"EU_hp_learning",
"EU_barriers_heater",
"EU_reno",
"EU_deep_reno",
"EU_barriers_renovation",
"EU_realization_rate")

# runs <- c("EU", "EU_carbon_tax", "EU_carbon_tax_social")

runs <- c("EU")

runs <- "all"


# read file_scenarios
if (runs == "all") {
    runs <- read.csv2(paste0(path_in, file_scenarios), sep = ',')$scenario_name
    # Create name_dir
    name_dir <- paste0(Sys.Date(), "_", format(Sys.time(), "%H%M%S"), "/")
    path_out <- paste(path_out, name_dir)
    if (!dir.exists(path_out)) {
        dir.create(path_out)
    }
}


parallel <- FALSE

report <- list(var = c("energy"),
               type = c("STURM"),
               geo_level = "region_bld")

yrs <- seq(base_year, end_year, step_year)

if (!parallel) {

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
} else {

    run_scenario_wrapper <- function(run) {
        tryCatch({
            result <- run_scenario(
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
            return(result)
        }, error = function(e) {
            message(sprintf("Error in run '%s': %s", run, e$message))
            return(NULL)  # Return NULL in case of error
        })
    }
    # Run in parallel
    sturm_scenarios <- mclapply(runs, run_scenario_wrapper, mc.cores = num_cores)

}

