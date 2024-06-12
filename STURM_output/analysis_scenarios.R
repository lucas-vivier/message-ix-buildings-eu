library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)

source("STURM_output/C00_plots.R")
# Load data
dir <- "simulation"
save_dir <- paste("STURM_output", "figures", dir, sep = "/")
file <- paste(save_dir, "results.csv", sep = "/")
data <- read.csv(file,  check.names = FALSE, stringsAsFactors = FALSE, header = TRUE, row.names = NULL)
data <- data[, -1]

out_dir <- paste(save_dir, "standalone", sep = "/")
if (!dir.exists(out_dir)) {
    dir.create(out_dir)
}

scenarios <- c(
    "Counterfactual", "EU-ETS", "EU-ETS 2", "Social value of carbon",
    "Subsidies heat pumps", "Market-failures heater",
    "Learning-by-doing heat pumps",
    "Renovation wave", "Deep renovation wave",
    "Quality renovation", "Market-failures renovation")

# Select scenario by rows
subset <- data %>%
    filter(group %in% scenarios)

# Export as .csv
write.csv(subset, paste(out_dir, "results_standalone.csv", sep = "/"), row.names = FALSE)

#-----------------------------------
# Make heatmap of the results
cols <- c(
    "Space heating consumption (TWh)",
    "Space heating consumption electricity (TWh)",
    "Emission (MtCO2)", "Emission cumulated (GtCO2)",
    "Total cost (Billion EUR)",
    "Government expenditures (Billion EUR)",
    "Delta total cost private (euro/hh/year)",
    "Delta total cost (euro/hh/year)")

temp <- subset %>%
    select(all_of(c("group", cols))) %>%
    pivot_longer(cols = -group, names_to = "variable", values_to = "value") %>%
    #filter(group != "Counterfactual")
    # Replace NaN with 0
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    rename(scenario = group)


# Create a function to normalize colors per column
normalize_colors <- function(data) {
  data$value <- rescale(data$value, to = c(0, 1))
  return(data)
}

# Apply normalization by variable
temp <- temp %>%
  group_by(variable) %>%
  mutate(norm_value = rescale(value, to = c(0, 1)))

temp$variable <- factor(temp$variable, levels = cols)
# Reverse order compare to scenarios
temp$scenario <- factor(temp$scenario, levels = rev(scenarios))

# Create heatmap
p <- temp %>%
  ggplot(aes(x = variable, y = scenario)) +
  #geom_tile(aes(fill = norm_value), color = "white") +
  geom_tile(aes(fill = ifelse(scenario == "Counterfactual", NA, norm_value)), color = "white") +
  geom_tile(data = subset(temp, scenario == "Counterfactual"), aes(x = variable, y = scenario), fill = "grey", color = "white") +
  geom_text(aes(label = round(value, 1)), size=6) +
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0, na.value = "grey") +
  theme_minimal() +
    theme(axis.title.y = element_blank(),
      axis.title.x.top = element_blank(),
      axis.text.x.top = element_text(size = 15, angle = 90, face="bold"),
      axis.text.y = element_text(size = 15, face="bold"),
      legend.position = "none") +
  scale_x_discrete(position = "top")

# save plot
ggsave(filename = paste(out_dir, "heatmap.png", sep = "/"), plot = p,
    width = plot_settings[["width"]],
    height = plot_settings[["height"]],
    dpi = plot_settings[["dpi"]])


#-----------------------------------
# Make cost-benefits analysis
rename_list <- c(
  `Delta cost renovation (euro/hh/year)` = "Cost renovation",
  `Delta cost heating system (euro/hh/year)` = "Cost heating system",
  `Delta cost fuel (euro/hh/year)` = "Cost fuel",
  `Delta cost thermal comfort (euro/hh/year)` = "Cost thermal comfort",
  `Delta total cost private (euro/hh/year)` = "Total cost private",
  `Delta cost emission (euro/hh/year)` = "Cost emission",
  `Delta total cost (euro/hh/year)` = "Total cost"
)

color_list <- c(
  "Cost renovation" = "#ee6a6b",
  "Cost heating system" = "#62c5c0",
  "Cost fuel" = "#00589d",
  "Cost emission" = "#fdbb40",
  "Cost thermal comfort" = "lightblue"
)

df <- subset %>%
  rename(scenario = group) %>%
    # Select only the relevant columns based on the rename_list
  select(scenario, `Delta cost renovation (euro/hh/year)`,
          `Delta cost heating system (euro/hh/year)`, `Delta cost fuel (euro/hh/year)`,
          `Delta cost thermal comfort (euro/hh/year)`, `Delta total cost (euro/hh/year)`, 
          `Delta total cost private (euro/hh/year)`, `Delta cost emission (euro/hh/year)`) %>%
  gather(variable, value, `Delta cost renovation (euro/hh/year)`,
         `Delta cost heating system (euro/hh/year)`, `Delta cost fuel (euro/hh/year)`,
         `Delta cost thermal comfort (euro/hh/year)`, `Delta total cost (euro/hh/year)`, 
         `Delta total cost private (euro/hh/year)`, `Delta cost emission (euro/hh/year)`) %>%
  mutate(variable = rename_list[.data[["variable"]]]) %>%
  filter(scenario != "Counterfactual") %>%
  # mutate(scenario = scenarios[.data[["scenario"]]]) %>%
  mutate(value = value)

df$scenario  <- factor(df$scenario, levels = rev(scenarios))


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
  left_join(total) %>%
  left_join(total_private)

presentation <- FALSE
legend <- TRUE

stacked_plots(df,
save_path = paste(out_dir, "cba_eu.png", sep = "/"),
color_list = color_list, y_label_suffix = "", # EUR/(year.hh)
presentation = presentation, legend = legend)

stacked_plots(df,
save_path = paste(out_dir, "cba_eu_horizontal.png", sep = "/"),
color_list = color_list, y_label_suffix = "", #EUR/(year.hh)
presentation = presentation, legend = legend, horizontal = TRUE)

