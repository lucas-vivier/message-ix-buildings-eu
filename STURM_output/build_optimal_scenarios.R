library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(argparse)


# Create a parser object
parser <- ArgumentParser(description = "Script to set number of cores")

# Add argument for number of cores
parser$add_argument("-f", "--files", default = NULL,
                    help = "Display or not the figures")

parser$add_argument("-d", "--dir", default = NULL,
                    help = "Path to the directory containing the files")

# Parse the arguments
args <- parser$parse_args()
# args <- list(file = "max_scenario.csv", dir = "2024-06-16_184750")

file <- paste("STURM_data", args$file, sep = "/")
scenarios <- read.csv(file, header = TRUE)


dir <- paste("STURM_output/results", args$dir, sep = "/")

data <- data.frame()

for (scenario in unique(scenarios$scenario_name)) {

  region <- scenarios %>%
    filter(scenario_name == scenario) %>%
    select(region_bld) %>%
    unique()

  print(paste("Scenario:", scenario))
  file <- paste0("report_agg_", scenario, ".csv")
  file <- paste(dir, file, sep = "/")

  if (!file.exists(file)) {
    print("Check file name or directory!")
  }

  # Read output files
  temp <- read.csv(file) %>%
    select(c("region_bld", "year", "variable", "resolution", "value")) %>%
    filter(region_bld %in% region$region_bld) 
    #%>% mutate(scenario = scenario)

  data <- rbind(data, temp)
}

eu <- data %>%
  group_by(year, variable, resolution) %>%
  summarise(value = sum(value)) %>%
  mutate(region_bld = "EU")

data <- rbind(data, eu)

write.csv(data, paste(dir, paste0("report_agg_", args$file), sep = "/"), row.names = FALSE)