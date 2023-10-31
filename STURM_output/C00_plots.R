# MESSAGEix-Buildings - color schemes

library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

source("STURM_output/C00_plots_settings.R")

plot_settings <- list(
  "size_axis" = 5, # axes
  "size_title" = 24, # axes and legend
  "size_text" = 24,
  "big_size_title" = 40, # axes and legend
  "big_size_text" = 40,
  "small_size_text" = 12,
  "small_size_title" = 18,
  "width" = 16, #width cm
  "height" = 1 * 16, #height cm
  "dpi" = 300, #DPI
  "font_family" = "Arial",
  "colors" = c(colors_efficiency, colors_fuels, colors_countries, colors_cost),
  "rename" = c(rename_eneff, rename_fuels, rename_countries, rename_hh, rename_cost),
  "order" = c(order_fuels, order_eneff, order_countries, order_hh, order_cost)
)


message_building_theme_presentation <- theme_minimal() +
    theme(text = element_text(size = plot_settings[["big_size_text"]],
                              family = plot_settings[["font_family"]]),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = plot_settings[["big_size_text"]]),
          axis.text.y = element_text(size = plot_settings[["big_size_text"]]),
          axis.title.y = element_blank(),
          axis.title = element_text(hjust = 0,
                                    size = plot_settings[["big_size_title"]]),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = plot_settings[["big_size_text"]]),
          legend.position = "right",
          # Top, Right, Bottom and Left margin
          plot.margin = margin(t = 0,
                      r = 0,
                      b = 0,
                      l = 0),
          axis.line.x =
            element_line(colour = "black", size = 1.5, linetype = "solid"),
          axis.line.y =
            element_line(colour = "black", size = 1.5, linetype = "solid")
        )

message_building_theme <- theme_minimal() +
    theme(text = element_text(size = plot_settings[["size_text"]],
                              family = plot_settings[["font_family"]]),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = plot_settings[["size_text"]]),
          axis.text.y = element_text(size = plot_settings[["size_text"]]),
          axis.title.y = element_blank(),
          axis.title = element_text(hjust = 0,
                                    size = plot_settings[["size_title"]]),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = plot_settings[["size_text"]]),
          legend.position = "right",
          # Top, Right, Bottom and Left margin
          plot.margin = margin(t = 0,
                      r = 0,
                      b = 0,
                      l = 0),
        axis.line.x =
          element_line(colour = "black", size = 1.5, linetype = "solid"),
        axis.line.y =
          element_line(colour = "black", size = 1.5, linetype = "solid")
        )

message_building_subplot_theme <- theme_minimal() +
    theme(text = element_text(size = plot_settings[["small_size_text"]],
                              family = plot_settings[["font_family"]]),
          plot.title = element_text(size = plot_settings[["small_size_title"]]),
          axis.title = element_text(hjust = 0,
                          size = plot_settings[["small_size_title"]]),
          axis.title.x = element_blank(),
          strip.text.x = element_text(size = plot_settings[["small_size_title"]]),
          axis.text.x = element_text(size = plot_settings[["small_size_text"]]),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = plot_settings[["small_size_text"]]),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = plot_settings[["small_size_text"]]),
          legend.position = "bottom",
          # Top, Right, Bottom and Left margin
          plot.margin = margin(t = 0,
                      r = 0,
                      b = 0,
                      l = 0)
          )


# Function to create Message-ix-Buidling Figures

plot_stacked_areas <- function(data,
                              x_column,
                              y_column,
                              fill_column,
                              subplot_column,
                              y_label = "",
                              save_path = NULL,
                              ncol = 4,
                              percent = TRUE,
                              vertical = NULL,
                              y_label_suffix = "",
                              presentation = FALSE,
                              legend = TRUE) {

  # Create the stacked area plot

  print(unique(data[[subplot_column]]))

  if (all(unique(data[[subplot_column]]) %in% names(plot_settings[["rename"]]))) {
    data <- data %>%
      mutate(!!subplot_column := plot_settings[["rename"]][.data[[subplot_column]]])
  }

  group <- c(x_column, fill_column, subplot_column)
  p <- data %>%
    mutate(!!fill_column := plot_settings[["rename"]][.data[[fill_column]]]) %>%
    group_by(across(all_of(group))) %>%
    summarise(total = sum(.data[[y_column]])) %>%
    ungroup()

  if (percent) {
    p <- p %>%
      group_by_at(c(x_column, subplot_column)) %>%
      mutate(total = total / sum(total)) %>%
      ungroup()
    scales <- NULL
  } else {
    scales <- "free_y"
  }
  p <- p %>%
    mutate(!!sym(fill_column) := factor(!!sym(fill_column),
      levels = rev(plot_settings[["order"]]))) %>%
    ggplot(aes(x = .data[[x_column]],
               y = total,
               fill = .data[[fill_column]])) +
    geom_area(position = "stack")

  if (!is.null(vertical)) {
    p <- p +
      geom_vline(xintercept = vertical, size = 1.5, color = "red")
  }
  p <- p +
    facet_wrap(subplot_column, ncol = ncol, scales = scales) +
    scale_fill_manual(values = plot_settings[["colors"]]) +
    labs(title = y_label,
         fill = str_replace_all(str_to_title(fill_column), "_", " "))
  
  if (length(unique(data[["region_bld"]])) > 1) {
    p <- p +
      message_building_subplot_theme

    } else {
      p <- p
      if (presentation) {
        print("ok")
        p <- p +
          message_building_theme_presentation +
           theme(strip.text = element_blank())
      } else {
        p <- p +
          message_building_theme
      }
    }
  
  p <- p +
    scale_x_continuous(breaks = c(min(data[[x_column]]), 2015, 2030, max(data[[x_column]])),
      labels = c(min(data[[x_column]]), 2015, 2030, max(data[[x_column]])))
  # Custom label function that combines comma and suffix
  custom_label <- function(x) {
    label_comma()(x) %>% paste0(" ", y_label_suffix)
  }

  if (percent) {
    p <- p +
      scale_y_continuous(breaks =  c(0, 0.5, 1),
        labels = function(x) paste0(format(100 * x, digits = 1), "%"))
  } else {
    p <- p +
      scale_y_continuous(labels = custom_label, expand = c(0, 0))
  }

  if (!legend) {
    p <- p + theme(legend.position = "none")
  }
  # Save the plot as PNG if save_path is specified
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = plot_settings[["width"]],
           height = plot_settings[["height"]],
           dpi = plot_settings[["dpi"]])
  }
  # Print the plot
  print(p)
}

plot_clustered_barplot <- function(df,
                                    x_column,
                                    fill_column,
                                    save_path = NULL,
                                    y_label_suffix = "",
                                    y_label = "",
                                    year_start = 2015,
                                    x_order = NULL,
                                    display_total = TRUE,
                                    angle_x_label = NULL) {

  custom_label <- function(x) {
    label_comma()(x) %>% paste0(" ", y_label_suffix)
  }

  if (!is.null(year_start)) {
    print(year_start)
    df <- df %>%
      filter(!(year == year_start & scenario != "Current policies"))
  }

  df <- df %>%
    mutate(!!fill_column := plot_settings[["rename"]][.data[[fill_column]]]) %>%
    mutate(!!sym(fill_column) := factor(!!sym(fill_column),
      levels = rev(plot_settings[["order"]])))

  if (!is.null(x_order)) {
    df <- df %>%
      mutate(!!sym(x_column) := factor(!!sym(x_column),
        levels = x_order))
  }

  p <- df %>%
    ggplot(aes(fill = .data[[fill_column]],
      y = value, x = factor(.data[[x_column]])),
    color = .data[[fill_column]]) +
    geom_bar(stat = "identity", position = "stack", width=0.6)
  if (display_total) {
    p <- p +
      geom_text(
        aes(label = paste0(round(after_stat(y), 0), " ", y_label_suffix),
          group = x_column),
        stat = 'summary', fun = sum, vjust = -1, fontface = "bold", size = 7
      )
  }

  p <- p +
    facet_grid(~year, scales = "free_x", space = "free") +
    labs(title = y_label) +
    scale_fill_manual(values =
      plot_settings[["colors"]], na.translate = FALSE) +
    scale_y_continuous(labels = custom_label) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = plot_settings[["size_text"]]),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = plot_settings[["small_size_text"]]),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = plot_settings[["size_text"]]),
          strip.text.x = element_text(size = plot_settings[["size_text"]]),
          plot.title = element_text(size = plot_settings[["size_text"]]),
          panel.background = element_rect(fill = "#efefef", color = "white"))
  
  if (!is.null(angle_x_label)) {
    # put text.x in bold
    p <- p +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = plot_settings[["size_text"]], face = "bold"))
  }

  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = plot_settings[["width"]],
            height = plot_settings[["height"]],
            dpi = plot_settings[["dpi"]])
  }
  # Print the plot
  print(p)
  
}

plot_multiple_lines <- function(df,
    x_column,
    y_column,
    line_column = NULL,
    type_column = NULL,
    group_column = NULL,
    ncol = 4,
    y_label = "",
    save_path = NULL,
    free_y = FALSE,
    colors_lines = NULL,
    legend = TRUE,
    y_label_suffix = "",
    line_types = NULL,
    presentation = FALSE,
    line_order = NULL,
    legend_title = FALSE) {
  
  if (is.null(type_column)) {
    type_column <- line_column
  }

  linewidth <- 2
  if (!is.null(group_column)) {
    df <- df %>%
      mutate(!!group_column := plot_settings[["rename"]][.data[[group_column]]]) %>%
      filter(!is.na(.data[[group_column]]))
    linewidth <- 1.5
  }

  if (!is.null(line_column)) {
    if (all(unique(df[[line_column]]) %in% names(plot_settings[["rename"]]))) {
      df <- df %>%
        mutate(!!line_column := plot_settings[["rename"]][.data[[line_column]]])
    }
  }

  if (!is.null(line_order)) {
    print(line_order)
    df <- df %>%
      mutate(!!sym(line_column) := factor(!!sym(line_column),
        levels = line_order))
  }

  if (!is.null(line_column)) {
    p <- ggplot(df, aes(x = .data[[x_column]], y = .data[[y_column]],
                color = .data[[line_column]], linetype = .data[[type_column]]))
    } else {
    p <- ggplot(df, aes(x = .data[[x_column]], y = .data[[y_column]]))
  }
    p <- p +
        geom_line(linewidth = linewidth) +
        expand_limits(y = 0)
  
  if (!is.null(colors_lines)) {
    p <- p + scale_color_manual(values = colors_lines)
  }
  if (!is.null(line_types)) {
    p <- p + scale_linetype_manual(values = line_types)
  }

  # Create subplot for each instance of group_column
  if (!is.null(group_column)) {
    if (free_y) {
        p <- p + facet_wrap(group_column, ncol = ncol, scales = "free_y")
      } else {
        p <- p + facet_wrap(group_column, ncol = ncol)
    }
    p <- p +
        # message_building_subplot_theme
        message_building_theme
  } else {
    if (presentation) {
      p <- p +
        message_building_theme_presentation
      y_label <- NULL
    } else {
      p <- p +
        message_building_theme
    }

  }
  if (!legend) {
    p <- p + theme(legend.position = "none", legend.title = element_text(size=10))
  }


  # Custom label function that combines comma and suffix
  custom_label <- function(x) {
    label_comma()(x) %>% paste0(" ", y_label_suffix)
  }

  p <- p +
      labs(title = y_label) +
      scale_x_continuous(
        breaks = c(min(df[[x_column]]), 2030, max(df[[x_column]])),
        labels = c(min(df[[x_column]]), 2030, max(df[[x_column]]))
        ) +
      scale_y_continuous(labels = custom_label) # expand = c(0, 0)

  if (legend_title) {
    print(str_replace_all(str_to_title(line_column), "_", " "))
    p <- p + labs(
      color = str_replace_all(str_to_title(line_column), "_", " "),
      linetype = str_replace_all(str_to_title(type_column), "_", " "))
  }

# Save the plot as PNG if save_path is specified
if (!is.null(save_path)) {
  ggsave(save_path, plot = p, width = plot_settings[["width"]],
          height = plot_settings[["height"]],
          dpi = plot_settings[["dpi"]])
}
  # Print the plot
  print(p)
}

scatter_plots <- function(df,
                          x_column,
                          y_column,
                          color_column = "Scenario",
                          colors_scenarios,
                          size_column = NULL,
                          save_path = NULL,
                          x_label_suffix = "",
                          y_label_suffix = "",
                          legend_suffix = "",
                          y_label = "",
                          x_label = "",
                          legend = FALSE,
                          presentation = FALSE) {
  
  p <- ggplot(df, aes(x = .data[[x_column]], y = .data[[y_column]],
    color = .data[[color_column]], size = .data[[size_column]])) +
    geom_point(alpha = 1) +
    expand_limits(y = 0, x = 0) +
    scale_color_manual(values = colors_scenarios) +
    scale_size(range = c(5, 10),
              breaks = c(
                min(df[[size_column]]),
                (min(df[[size_column]]) + max(df[[size_column]])) / 2,
                max(df[[size_column]])),
              labels = function(x) paste0(round(x, 0), legend_suffix)) +
    guides(color = guide_legend(override.aes = list(size = 5)))


  if (!presentation) {
      p <- p +
        message_building_theme  +
        theme(axis.title.x = element_text(size = 30, hjust = 0.5))

      size_axis <- plot_settings[["size_text"]]
  } else {
      p <- p +
        message_building_theme_presentation
      size_axis <- plot_settings[["big_size_text"]]
  }

  custom_label <- function(x, suffix) {
    label_comma()(x) %>% paste0(" ", suffix)
  }
  p <- p +
    labs(title = y_label, x = x_label) +
    scale_x_continuous(labels =
      function(x) custom_label(x, suffix = x_label_suffix)) +
    scale_y_continuous(labels =
      function(x) custom_label(x, suffix = y_label_suffix))


  if (!legend) {
    p <- p + theme(legend.position = "none")
  }
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = plot_settings[["width"]],
            height = plot_settings[["height"]],
            dpi = plot_settings[["dpi"]])
  }

  print(p)
}

stacked_plots <- function(data,
                          subplot_column = NULL,
                          save_path = NULL,
                          color_list = NULL,
                          y_label_suffix = "",
                          presentation = FALSE,
                          legend = TRUE) {

  if (!is.null(subplot_column)) {
    data <- data %>%
      mutate(!!subplot_column := plot_settings[["rename"]][.data[[subplot_column]]]) %>%
      filter(!is.na(.data[[subplot_column]]))

    size_text <- 3
    size_point <- 1
    nudge_y <- 0
    round <- 1
  } else {
    round <- 0
    if (presentation) {
      size_text <- 12
      size_point <- 5
      nudge_y <- 0.2

    } else {
      size_text <- 8
      size_point <- 3
      nudge_y <- 0.15
    }

    }
  p <- data %>%
    ggplot(aes(x = scenario, y = value, fill = variable)) +
    geom_bar(stat = "identity") +
    geom_point(aes(x = scenario, y = total_value),
      color = "black", size = size_point, , show.legend = FALSE) +
    geom_text(aes(x = scenario,
      label = paste(round(total_value, round)), y = total_value),
      vjust = -0.5, nudge_y = nudge_y, size = size_text)

  if (!is.null(color_list)) {
    p <- p + scale_fill_manual(values = color_list)
  }

  if (!is.null(subplot_column)) {
    p <- p +
      facet_wrap(subplot_column, ncol = 4, scales = "free_y") +
      message_building_subplot_theme +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))

  } else {
    if (presentation) {
      p <- p +
        geom_hline(yintercept = 0, color = "black", size = 1) +
        message_building_theme_presentation +
        theme(axis.line.y = element_blank(),
              axis.line.x = element_blank(),
              axis.text.x = element_blank()
                )
    } else {
      p <- p +
        message_building_theme +
        theme(axis.line.y = element_blank()) +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
    }

  }

  if (!legend) {
    p <- p + theme(legend.position = "none")
  }

  # Custom label function that combines comma and suffix
  custom_label <- function(x) {
    label_comma()(x) %>% paste0(" ", y_label_suffix)
  }
  p <- p +
    scale_y_continuous(labels = custom_label)

  print(p)
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = plot_settings[["width"]],
           height = plot_settings[["height"]],
           dpi = plot_settings[["dpi"]])
  }
}

cost_curve <- function(data, x_column, y_column, save_path, ncol = 4) {
  # Calculate cumulative values of energy_savings or n_units_fuel
  data <- data %>%
    group_by_at("region_bld") %>%
    arrange(!!sym(y_column)) %>%
    mutate(cumulative_x = cumsum(!!sym(x_column))) %>%
    mutate(cumulative_x = cumulative_x / sum(!!sym(x_column)))
  
  # Create the plot
  p <- ggplot(data, aes(x = cumulative_x, y = .data[[y_column]], group = !!sym("region_bld"))) +
    geom_line(size = 1.5) +
    geom_hline(yintercept = 0, color = "grey", size = 1) +
    labs(title = "Net Present Cost Curve of Shell Renovation",
         x = "Cumulative Number of Units of Fuel",
         y = "Net present cost") +
    message_building_subplot_theme +
    scale_y_continuous(labels =
      function(x) paste0(format(x, big.mark = ",", scientific = FALSE), "EUR")) +
    scale_x_continuous(breaks =  c(0, 0.5, 1),
        labels = function(x) paste0(format(100 * x, digits = 1), "%")) +
    facet_wrap(~ region_bld, ncol = ncol) 



  print(p)
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = plot_settings[["width"]],
           height = plot_settings[["height"]],
           dpi = plot_settings[["dpi"]])
  }
}


output_table <- function(data,
  save_dir,
  end_year,
  base_year = 2015
  ) {

  save_file <- paste(save_dir,
    paste0(run, "_", end_year, "_summary_countries.csv"), sep = "/")
  save_file_eu <- paste(save_dir,
    paste0(run, "_", end_year, "_summary_eu.csv"), sep = "/")

  variables <- c("heat_kWh", "heat_std_kWh", "heat_tCO2",
      "energy_poverty_thres", "stock_building",
        "heating_intensity")

  low_carbon <- c("biomass_solid", "heat_pump", "electricity", "district_heat")

  # initial data from variables
  ini <- data %>%
    filter(year == base_year) %>%
    filter(scenario == ref) %>%
    mutate(scenario = "Initial") %>%
    filter(resolution == "all") %>%
    filter(variable %in% variables) %>%
    select(-c("resolution", "year")) %>%
    pivot_wider(id_cols = c(region_bld, scenario),
      names_from = variable,
      values_from = value)

  # final data from variables
  table <- data %>%
    filter(year == end_year) %>%
    filter(resolution == "all") %>%
    filter(variable %in% variables) %>%
    select(-c("resolution", "year")) %>%
    pivot_wider(id_cols = c(region_bld, scenario),
      names_from = variable,
      values_from = value)

  table <- bind_rows(ini, table)

  # replacement heating system
  temp <- data %>%
    filter(year <= end_year) %>%
    filter(resolution == "all") %>%
    filter(variable == "n_replacement") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(replacement_heater = sum(value) * stp) %>%
    ungroup()
  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(replacement_heater =
      ifelse(is.na(replacement_heater), 0, replacement_heater))

  # cost renovation
  temp <- data %>%
    filter(year <= end_year) %>%
    filter(resolution == "all") %>%
    filter(variable == "cost_renovation_EUR") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(cost_renovation_EUR = sum(value) * stp) %>%
    ungroup()
  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(cost_renovation_EUR =
      ifelse(is.na(cost_renovation_EUR), 0, cost_renovation_EUR))

  # subsidies renovation
  temp <- data %>%
    filter(year <= end_year) %>%
    filter(resolution == "all") %>%
    filter(variable == "sub_renovation_EUR") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(sub_renovation_EUR = sum(value) * stp) %>%
    ungroup()
  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(sub_renovation_EUR =
      ifelse(is.na(sub_renovation_EUR), 0, sub_renovation_EUR))

  # cost heater
  temp <- data %>%
    filter(year <= end_year) %>%
    filter(resolution == "all") %>%
    filter(variable == "cost_heater_EUR") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(cost_heater_EUR = sum(value) * stp) %>%
    ungroup()
  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(cost_heater_EUR =
      ifelse(is.na(cost_heater_EUR), 0, cost_heater_EUR))

  # subsidies heater
  temp <- data %>%
    filter(year <= end_year) %>%
    filter(resolution == "all") %>%
    filter(variable == "sub_heater_EUR") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(sub_heater_EUR = sum(value) * stp) %>%
    ungroup()
  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(sub_heater_EUR =
      ifelse(is.na(sub_heater_EUR), 0, sub_heater_EUR))

  # renovated buildings
  temp <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == end_year) %>%
    filter(resolution %in% c("std", "adv")) %>%
    select(-c("resolution", "year")) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(renovated_building = sum(value)) %>%
    ungroup()

  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(renovated_building =
      ifelse(is.na(renovated_building), 0, renovated_building))

  # advanced buildings
  temp <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == end_year) %>%
    filter(resolution == "adv") %>%
    select(-c("resolution", "year")) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(advanced_renovated_building = sum(value)) %>%
    ungroup()

  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(advanced_renovated_building =
      ifelse(is.na(advanced_renovated_building), 0, advanced_renovated_building))

  # low-carbon buildings
  temp <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == end_year) %>%
    filter(resolution %in% low_carbon) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(low_carbon_building = sum(value)) %>%
    ungroup()

  # initial data
  ini <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == base_year) %>%
    filter(scenario == ref) %>%
    filter(resolution %in% low_carbon) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(low_carbon_building = sum(value)) %>%
    ungroup() %>%
    mutate(scenario = "Initial")
  temp <- bind_rows(ini, temp)

  table <- left_join(table, temp, by = c("region_bld", "scenario")) %>%
    mutate(renovated_building =
      ifelse(is.na(renovated_building), 0, renovated_building))

  # heat_pumps
  temp <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == end_year) %>%
    filter(resolution == "heat_pump") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_pump = sum(value)) %>%
    ungroup()

  # initial data
  ini <- data %>%
    filter(variable == "stock_building") %>%
    filter(year == base_year) %>%
    filter(scenario == ref) %>%
    filter(resolution == "heat_pump") %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_pump = sum(value)) %>%
    ungroup() %>%
    mutate(scenario = "Initial")
  temp <- bind_rows(ini, temp)

  table <- left_join(table, temp, by = c("region_bld", "scenario"))

  # consumption fossil-fuels
  temp <- data %>%
    filter(variable == "heat_kWh") %>%
    filter(year == end_year) %>%
    filter(resolution %in% c("oil", "coal", "gas")) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_kWh_fossil = sum(value)) %>%
    ungroup()

  # initial data
  ini <- data %>%
    filter(variable == "heat_kWh") %>%
    filter(year == base_year) %>%
    filter(scenario == ref) %>%
    filter(resolution %in% c("oil", "coal", "gas")) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_kWh_fossil = sum(value)) %>%
    ungroup() %>%
    mutate(scenario = "Initial")

  temp <- bind_rows(ini, temp)
  table <- left_join(table, temp, by = c("region_bld", "scenario"))

  # consumption electricity
  temp <- data %>%
    filter(variable == "heat_kWh") %>%
    filter(year == end_year) %>%
    filter(resolution %in% c("electricity", "heat_pump")) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_kWh_electricity = sum(value)) %>%
    ungroup()

  # initial data
  ini <- data %>%
    filter(variable == "heat_kWh") %>%
    filter(year == base_year) %>%
    filter(scenario == ref) %>%
    filter(resolution %in% c("electricity", "heat_pump")) %>%
    group_by_at(c("region_bld", "scenario")) %>%
    summarize(heat_kWh_electricity = sum(value)) %>%
    ungroup() %>%
    mutate(scenario = "Initial")

  temp <- bind_rows(ini, temp)
  table <- left_join(table, temp, by = c("region_bld", "scenario"))

  # formatting output
  format_temp <- table %>%
    arrange(region_bld, scenario) %>%
    mutate(
      heat_kWh_fossil = round(heat_kWh_fossil / 1e9, 2),
      heat_kWh_electricity = round(heat_kWh_electricity / 1e9, 2),
      heat_kWh = round(heat_kWh / 1e9, 2),
      heat_std_kWh = round(heat_std_kWh / 1e9, 2),
      heat_tCO2 = round(heat_tCO2 / 1e6, 2),
      stock_building = round(stock_building / 1e6, 2),
      heating_intensity = round(heating_intensity, 2),
      renovated_building = round(renovated_building / 1e6, 2),
      advanced_renovated_building =
        round(advanced_renovated_building / 1e6, 2),
      replacement_heater = round(replacement_heater / 1e6, 2),
      low_carbon_building = round(low_carbon_building / 1e6, 2),
      heat_pump = round(heat_pump / 1e6, 2),
      energy_poverty_thres =
        round(energy_poverty_thres / 1e6, 2),
      cost_renovation_EUR = round(cost_renovation_EUR / 1e9, 2),
      sub_renovation_EUR = round(sub_renovation_EUR / 1e9, 2),
      cost_heater_EUR = round(cost_heater_EUR / 1e9, 2),
      sub_heater_EUR = round(sub_heater_EUR / 1e9, 2)
        ) %>%
    mutate(region_bld = plot_settings[["rename"]][region_bld]) %>%
    select(c("region_bld", "scenario",
      "heat_kWh", "heat_std_kWh", "heat_tCO2",
      "heat_kWh_fossil", "heat_kWh_electricity",
      "heating_intensity", "stock_building",
      "low_carbon_building", "heat_pump",
      "renovated_building", "advanced_renovated_building",
      "replacement_heater", "energy_poverty_thres",
      "cost_renovation_EUR", "sub_renovation_EUR",
      "cost_heater_EUR", "sub_heater_EUR")) %>%
    mutate(low_carbon_building_rate = low_carbon_building / stock_building,
      renovated_building_rate = renovated_building / stock_building,
      energy_poverty_rate = energy_poverty_thres / stock_building,
      heat_kWh_fossil_rate = heat_kWh_fossil / heat_kWh,
      ) %>%
    rename(
      "Energy poverty (Million)" = energy_poverty_thres,
      "Energy poverty (Percent)" = energy_poverty_rate,
      "Renovated buildings (Million)" = renovated_building,
      "Renovated buildings advanced (Million)" = advanced_renovated_building,
      "Renovated buildings (Percent)" = renovated_building_rate,
      "Heat-pumps (Million)" = heat_pump,
      "Low carbon buildings (Million)" = low_carbon_building,
      "Low carbon buildings (Percent)" = low_carbon_building_rate,
      "Stock building (Million)" = stock_building,
      "Replacement heating system (Million)" = replacement_heater,
      "Heating intensity (Percent)" = heating_intensity,
      "Space heating consumption (TWh)" = heat_kWh,
      "Space heating consumption fossil (TWh)" = heat_kWh_fossil,
      "Share fossil-fuels (Percent)" = heat_kWh_fossil_rate,
      "Space heating consumption electricity (TWh)" = heat_kWh_electricity,
      "Space heating consumption standard (TWh)" = heat_std_kWh,
      "Emission (MtCO2)" = heat_tCO2,
      "Member states" = region_bld,
      "Scenario" = scenario,
      "Cost renovation (Billion EUR)" = cost_renovation_EUR,
      "Subsidies renovation (Billion EUR)" = sub_renovation_EUR,
      "Cost heater (Billion EUR)" = cost_heater_EUR,
      "Subsidies heater (Billion EUR)" = sub_heater_EUR
      )

  write.csv(format_temp, save_file, row.names = FALSE)

  format_temp_eu <- format_temp %>%
  filter(`Member states` == "EU") %>%
  select(-c("Member states"))

  consumption_ini <- format_temp_eu %>%
    filter(Scenario == "Initial") %>%
    pull(("Space heating consumption (TWh)"))

  format_temp_eu <- format_temp_eu %>%
    mutate(`Consumption saving (%)` =
      (consumption_ini - `Space heating consumption (TWh)`) / consumption_ini)

  emission_ini <- format_temp_eu %>%
    filter(Scenario == "Initial") %>%
    pull(("Emission (MtCO2)"))

  format_temp_eu <- format_temp_eu %>%
    mutate(`Emission saving (%)` =
      (emission_ini - `Emission (MtCO2)`) / emission_ini)

  format_temp_eu <- as.data.frame(t(format_temp_eu))
  colnames(format_temp_eu) <- format_temp_eu[1,]
  format_temp_eu <- format_temp_eu[-1,]

  format_temp_eu <- format_temp_eu %>%
    select(c("Initial", unname(scenarios)))

  select_order <- c(
    "Stock building (Million)",
    "Space heating consumption (TWh)",
    "Consumption saving (%)",
    "Space heating consumption fossil (TWh)",
    "Share fossil-fuels (Percent)",
    "Space heating consumption electricity (TWh)",
    "Heat-pumps (Million)",
    "Low carbon buildings (Percent)",
    "Renovated buildings (Million)",
    "Renovated buildings advanced (Million)",
    "Renovated buildings (Percent)",
    "Cost heater (Billion EUR)",
    "Subsidies heater (Billion EUR)",
    "Cost renovation (Billion EUR)",
    "Subsidies renovation (Billion EUR)",
    "Emission (MtCO2)",
    "Emission saving (%)",
    "Heating intensity (Percent)",
    "Energy poverty (Million)",
    "Energy poverty (Percent)"
  )

  format_temp_eu <- format_temp_eu[select_order, ]


  write.csv(format_temp_eu, save_file_eu)
  
  return(list(format_temp = format_temp, format_temp_eu = format_temp_eu))
}


budget_share_energy_plots <- function(data, years, sub_scenarios, ref, save_dir, angle_x_label = NULL, rename_income = NULL) {

    # STEP 1
  var <- c("to_pay_renovation", "to_pay_heater", "cost_energy", "stock_building")
  discount <- 0.05
  lifetime_loan <- 10
  lifetime_renovation <- 35 # years
  lifetime_heater <- 30 # years
  stp <- 5

  temp <- data %>%
    filter(variable %in% var) %>%
    filter(resolution %in% c("q1", "q2", "q3")) %>%
    rename(income = resolution)

  if (!is.null(rename_income)) {
    temp <- temp %>%
      mutate(income = rename_income[.data[["income"]]])
  }

  wide_data <- pivot_wider(
    temp,
    id_cols = c("region_bld", "year", "scenario", "income"),
    names_from = variable,
    values_from = value
    )

  wide_data <- wide_data %>%
    mutate(
      to_pay_renovation = ifelse(is.na(to_pay_renovation), 0, to_pay_renovation),
      to_pay_heater = ifelse(is.na(to_pay_heater), 0, to_pay_heater),
      cost_energy = ifelse(is.na(cost_energy), 0, cost_energy),
      to_pay_renovation = to_pay_renovation *
        discount / (1 - (1 + discount)^(-lifetime_loan)),
      to_pay_heater = to_pay_heater *
        discount / (1 - (1 + discount)^(-lifetime_loan))
          )

  wide_data <- wide_data %>%
    rowwise() %>%
    mutate(to_pay_renovation_cumsum =
      sum(wide_data$to_pay_renovation[wide_data$region_bld == region_bld &
          wide_data$scenario == scenario &
          wide_data$income == income &
          wide_data$year <= year &
          wide_data$year >= (year - lifetime_loan)], na.rm = TRUE)) %>%
    mutate(to_pay_heater_cumsum =
      sum(wide_data$to_pay_heater[wide_data$region_bld == region_bld &
          wide_data$scenario == scenario &
          wide_data$income == income &
          wide_data$year <= year &
          wide_data$year > (year - lifetime_loan)], na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(stock_building)) %>%
    mutate(total_cost_hh =
      to_pay_renovation_cumsum + to_pay_heater_cumsum +
      cost_energy,
      to_pay_renovation_cumsum = to_pay_renovation_cumsum / stock_building,
      to_pay_heater_cumsum = to_pay_heater_cumsum / stock_building,
      cost_energy = cost_energy / stock_building,
      total_cost_hh = total_cost_hh / stock_building)

  long_data <- wide_data %>%
    select(c("region_bld", "year", "scenario", "income",
      "to_pay_renovation_cumsum", "to_pay_heater_cumsum", "cost_energy", "total_cost_hh")) %>%
    pivot_longer(
      cols = c("to_pay_renovation_cumsum", "to_pay_heater_cumsum",
        "cost_energy", "total_cost_hh"),
      names_to = "variable",
      values_to = "value"
    )

  ## STEP 2


  parse_data <- long_data %>%
    filter(region_bld == "EU") %>%
    filter(year %in% years) %>%
    mutate(value = value) %>%
    filter(scenario %in% names(sub_scenarios)) %>%
    mutate(scenario = sub_scenarios[.data[["scenario"]]])

  data_ref <- parse_data %>%
    filter(scenario == ref) %>%
    rename(value_ref = value) %>%
    select(-scenario)

  diff <- parse_data %>%
    filter(variable != "total_cost_hh") %>%
    filter(scenario != ref) %>%
    left_join(data_ref, by = c("region_bld", "year", "variable", "income")) %>%
    mutate(diff = (value - value_ref)) %>%
    select(-c(value, value_ref)) %>%
    rename(value = diff)

  diff <- diff %>%
    rename(yr = year, year = income)

  plot_clustered_barplot(
    filter(diff, yr == 2030),
    "scenario",
    "variable",
    y_label = "Difference energy cost per household compared to current policies",
    y_label_suffix = "EUR",
    year_start = NULL,
    save_path = paste(save_dir, paste0(run, "_cost_energy_2030.png"), sep = "/"),
    display_total = TRUE,
    x_order = sub_scenarios,
    angle_x_label = angle_x_label)

  plot_clustered_barplot(
    filter(diff, yr == 2050),
    "scenario",
    "variable",
    y_label = "Difference energy cost per household compared to current policies",
    y_label_suffix = "EUR",
    year_start = NULL,
    save_path = paste(save_dir, paste0(run, "_cost_energy_2050.png"), sep = "/"),
    display_total = TRUE,
    x_order = sub_scenarios,
    angle_x_label = angle_x_label)
  }