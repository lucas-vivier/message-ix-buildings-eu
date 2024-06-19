library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(argparse)


# Loding figures setttings and functions
source("STURM_output/C00_plots.R")

# Create a parser object
parser <- ArgumentParser(description = "Script to set number of cores")

# Add argument for number of cores
parser$add_argument("-f", "--files", default = NULL,
                    help = "Display or not the figures")

parser$add_argument("-d", "--dir", default = NULL,
                    help = "Path to the directory containing the files")

# Parse the arguments
args <- parser$parse_args()
#args <- list(path = "2024-06-14_094322", names_scenarios = "STURM_data/scenarios_renovation.csv", figures = TRUE, counterfactual="EU")


arg$file <- "max_scenario.csv"

