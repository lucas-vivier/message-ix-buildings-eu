
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)

#' @param run: name of the scenario to run, e.g. "NAV_Dem-NPi-ref"
#' @param scenario_name: name of the scenario to run, e.g. "NAV_Dem-NPi-ref"
#' @param sector: sector to run, available: "com", "resid"
#' @param path_in: path to input data, e.g. "STURM_data/"
#' @param path_rcode: path to R code, e.g. "STURM_model/"
#' @param path_out: path to output data, e.g. "STURM_output/"
#' @param prices: prices to use, e.g. "input_prices_R12.csv"
#' @param file_inputs: file with input data, e.g. "input_list_resid.csv"
#' @param file_scenarios: file with scenarios, e.g. "scenarios_TEST.csv"
#' @param geo_level_report: level for reporting, e.g. "R12"
#' @param yrs: years to run, if NULL run all years, e.g. seq(2015, 2050, 5)
#' @param report_type: available reports,
#' available: "MESSAGE","STURM","IRP","NGFS","NAVIGATE"
#' @param report_var: available report variables,
#' available: "energy","material","vintage","dle"
#' @param region: region to run, if NULL run all regions
#' @return output data
run_scenario <- function(run,
                         scenario_name,
                         sector,
                         path_in,
                         path_rcode,
                         path_out,
                         file_inputs,
                         file_scenarios,
                         geo_level_report,
                         yrs,
                         report_type,
                         report_var,
                         region = NULL,
                         energy_efficiency = "endogenous",
                         en_method = "TABULA") {
  print(paste("Start scenario run: ", run))
  # Track time
  start_time <- Sys.time()


  # SOURCE MODEL FUNCTIONS
  print("Load functions")
  source(file.path(path_rcode, "B00_functions.R"))
  source(file.path(path_rcode, "F01_inputs.R"))
  source(file.path(path_rcode, "F03_energy_demand.R"))
  source(file.path(path_rcode, "F04_constr_decision.R"))
  source(file.path(path_rcode, "F05_renov_switch_decision.R"))
  source(file.path(path_rcode, "F05_endogenous_subsidies.R"))
  source(file.path(path_rcode, "F02_init_stock_dyn_fut.R"))
  source(file.path(path_rcode, "F06_stock_dyn_complete_rev.R"))
  source(file.path(path_rcode, "F07_formatting_output.R"))
  source(file.path(path_rcode, "F08_calibration.R"))

  if ("STURM" %in% report_type) {
    source(file.path(path_rcode, "R00_report_basic.R"))
  }
  if ("MESSAGE" %in% report_type) {
    source(file.path(path_rcode, "R01_report_MESSAGE.R"))
  }
  if ("IRP" %in% report_type) {
    source(file.path(path_rcode, "R02_report_IRP.R"))
  }
  if ("NGFS" %in% report_type) {
    source(file.path(path_rcode, "R03_report_NGFS.R"))
  }
  source(file.path(path_rcode, "R05_report_NAVIGATE.R"))
  print("Functions loaded!")

  # Read categories
  print("Load categories")
  path_in_csv <- paste0(path_in, "./input_csv/")
  cat <- read_categories(path_in_csv, sector, region)

  # Source input data
  print("Load data")
  temp <- fun_inputs_csv(path_in, file_inputs, file_scenarios, sector, run, param)
  d <- temp$d
  param <- temp$param

  
  # Multiple cost by cost_factor if cost_invest_heat do not have cost_factor
  if (!"region_bld" %in% names(d$cost_invest_heat)) {
    d$cost_invest_heat <- d$cost_invest_heat %>%
      cross_join(d$cost_factor) %>%
      mutate(cost_invest_heat = cost_invest_heat * cost_factor) %>%
      select(-cost_factor)
  }

  # Ensure that adv cost are higher than std
  temp <- d$cost_invest_ren_shell
  std_data <- temp %>% filter(eneff_f == "std")
  adv_data <- temp %>% filter(eneff_f == "adv")
  joined_data <- left_join(std_data, adv_data, by = "region_bld",
    suffix = c("_std", "_adv"))
  adjusted_data <- joined_data %>%
    rowwise() %>%
    mutate(cost_invest_ren_shell_adv =
      max(cost_invest_ren_shell_std, cost_invest_ren_shell_adv))
  final_data <- adjusted_data %>%
    select(region_bld, eneff_f_std = cost_invest_ren_shell_std, eneff_f_adv = cost_invest_ren_shell_adv) %>%
    pivot_longer(cols = -region_bld, names_to = "eneff_f", values_to = "cost_invest_ren_shell")
  final_data <- final_data %>%
    mutate(eneff_f = recode(eneff_f, "eneff_f_std" = "std", "eneff_f_adv" = "adv"))
  d$cost_invest_ren_shell <- final_data

  # Creating inertia data
  d$inertia <- d$cost_factor %>%
    mutate(cost_factor =
      cost_factor / cost_factor[region_bld == "C-WEU-FRA"]) %>%
    mutate(cost_factor = param$inertia_wtp * cost_factor) %>%
    rename(inertia = cost_factor)

  # Only selecting regions in geo_data that are in shr_fuel_heat_base
  cat$geo_data <- cat$geo_data %>%
    filter(region_bld %in% unique(d$shr_fuel_heat_base$region_bld))
  cat$regions <- unique(pull(cat$geo_data["region_bld"]))

  # Parsing share fuels data
  temp <- parse_share_fuels(d, cat)
  d <- temp$d
  cat <- temp$cat

  # Reading discount rate
  if (energy_efficiency == "endogenous") {
    if (!"tenr" %in% names(d$discount_rate)) {
      d$discount_rate <- crossing(d$discount_rate, cat$ct_hh_tenr) %>%
        rename(tenr = "cat$ct_hh_tenr") %>%
        mutate(discount_rate =
          ifelse(tenr == "own",
          filter(d$discount_rate, inc_cl == "q3",
          region_bld == region_bld)$discount_rate,  discount_rate))
    }
  }
  
  # Read energy prices
  print("Load energy prices")
  price_en <- read_energy_prices(d$energy_prices_ini,
    d$energy_prices_projections,
    cat$geo_data, yrs[[1]], yrs[[length(yrs)]], path_out = NULL)

  # Read emission factors
  print("Load emission factors")
  emission_factors <-
    read_emission_factors(d$emission_factors, d$emission_ini,
      cat$geo_data, yrs[[1]])

  # Calculate carbon price for electricity and district heating
  if (!is.null(d$carbon_market)) {
    d$carbon_market  <- d$carbon_market  %>%
      rowwise() %>%
      mutate(fuel = list(c("electricity", "district_heat"))) %>%
      unnest(fuel)

    carbon_market <- d$carbon_market %>%
      left_join(emission_factors, relationship =
        "many-to-many") %>%
      mutate(carbon_market = 3.6 * carbon_market * emission_factors / 1e6) %>%
      select(-c(emission_factors, region_gea))

    price_en <- price_en %>%
      left_join(carbon_market) %>%
      mutate(carbon_market = ifelse(is.na(carbon_market), 0, carbon_market)) %>%
      mutate(price_en = price_en + carbon_market) %>%
      select(-carbon_market)
  }

  # Read carbon tax for oil, gas and coal
  if (!is.null(d$carbon_tax)){
    d$carbon_tax  <- d$carbon_tax  %>%
      rowwise() %>%
      mutate(fuel = list(c("oil", "gas", "coal"))) %>%
      unnest(fuel)

    carbon_tax <- d$carbon_tax %>%
      left_join(emission_factors, relationship =
        "many-to-many") %>%
      mutate(carbon_tax = 3.6 * carbon_tax * emission_factors / 1e6) %>%
      select(-c(emission_factors, region_gea))
    price_en <- price_en %>%
      left_join(carbon_tax) %>%
      mutate(carbon_tax = ifelse(is.na(carbon_tax), 0, carbon_tax)) %>%
      mutate(price_en = price_en + carbon_tax) %>%
      select(-carbon_tax)
  }

  # Adding year column to start subsidies after calibration
  if (!"year" %in% names(d$sub_ren_shell)) {
    d$sub_ren_shell <- crossing(d$sub_ren_shell, yrs) %>%
      rename(year = "yrs") %>%
      mutate(sub_ren_shell = ifelse(year <= yrs[[2]], 0, sub_ren_shell))
  }
  if (!"year" %in% names(d$sub_heat)) {
    d$sub_heat <- crossing(d$sub_heat, yrs) %>%
      rename(year = "yrs") %>%
      mutate(sub_heat = ifelse(year <= yrs[[2]], 0, sub_heat))
  }

  print("Data loaded!")

  # Create matrix of all dimensions
  bld_cases_fuel <- expand.grid(
    region_bld = cat$regions,
    urt = cat$urts,
    inc_cl = cat$ct_hh_inc,
    stringsAsFactors = FALSE
    ) %>%
    left_join(cat$geo_data %>%
      select_at(c("region_bld", "region_gea"))) %>%
    left_join(cat$clim_zones,
      by = c("region_bld", "urt"),
      relationship = "many-to-many") %>%
    left_join(cat$ct_bld,
      relationship = "many-to-many") %>%
    left_join(cat$ct_eneff, by = "mat",
      relationship = "many-to-many") %>%
    inner_join(cat$ct_fuel, by = c("mat" = "mat"),
      relationship = "many-to-many") %>%
    merge(as.data.frame(cat$ct_hh_tenr)) %>%
    rename("tenr" = "cat$ct_hh_tenr")

  ### RESIDENTIAL SECTOR
  if (sector == "resid") {

    # Calculate energy consumption at dwelling level
    print("Calculate energy consumption at dwelling level")

    if (en_method == "TABULA") {

      en_sav_ren <- d$en_sav_ren %>%
          filter(year == yrs[[1]]) %>%
          select(-year)

      en_int_heat <- fun_space_heating_calculation(
        bld_cases_fuel,
        d$u_wall,
        d$u_roof,
        d$u_floor,
        d$u_windows,
        d$area_wall,
        d$area_roof,
        d$area_floor,
        d$area_windows,
        d$hdd,
        en_sav_ren,
      )
      d$en_int_heat <- en_int_heat
      # Add energy saving from envelope renovation
    }

    # Initialize housing stock (fun)
    print(paste("Initialize scenario run", sector))

    stock_aggr <- fun_stock_aggr(
      sector,
      yrs,
      cat$geo_data,
      bld_cases_fuel,
      d$pop,
      d$hh_size,
      d$floor_cap,
      cat$ct_eneff,
      cat$ct_fuel,
      d$shr_mat,
      d$shr_arch
    )
    print(paste("Initialize scenario run", sector, "- completed!"))

    # Initialize stock detailed ini
    temp <- fun_stock_det_ini(sector,
                           stock_aggr,
                           d$stock_arch_base,
                           cat$geo_data,
                           cat$ct_eneff,
                           cat$ct_fuel,
                           d$ct_heat,
                           d$shr_fuel_heat_base,
                           d$hh_tenure,
                           report_var)
    # Extract dataframes from list
    bld_det_ini <- temp$bld_det_i
    report <- temp$report
    print(paste("Initial building stock based on detailed data:",
      round(sum(bld_det_ini$n_units_fuel) / 1e6, 0), "million units."))
    rm(temp)



    print(paste("Calculate energy demand intensities - for space heating and cooling"))
    lst_en_i <- fun_en_sim(
      sector,
      yrs,
      1,
      bld_cases_fuel,
      d$en_int_heat,
      d$en_int_cool,
      d$days_cool,
      d$eff_cool,
      d$eff_heat,
      d$hours_heat,
      d$shr_floor_heat,
      d$hours_cool,
      d$shr_floor_cool,
      d$hours_fans,
      d$power_fans,
      d$shr_acc_cool,
      d$hh_size,
      d$floor_cap,
      price_en,
      d$income,
      en_method = en_method,
      path_out = path_out
    )
    # Extract dataframes from list
    en_m2_scen_heat <- lst_en_i$en_m2_scen_heat
    en_m2_scen_cool <- lst_en_i$en_m2_scen_cool
    en_hh_tot <- lst_en_i$en_hh_tot
    rm(lst_en_i)

    # Calibration of energy consumption at base year
    shr_en <- fun_calibration_consumption(bld_det_ini,
                                          en_hh_tot,
                                          d$hh_size,
                                          d$floor_cap,
                                          d$en_consumption,
                                          path_out)
    # shr_en <- mutate(shr_en, shr_en = 1)
    
    # Energy demand intensities - hot water
    print(paste("Calculate energy demand intensities for hot water"))
    en_hh_hw_scen <- fun_hw_resid(
      yrs, 1,
      bld_cases_fuel,
      d$hh_size,
      d$eff_hotwater,
      d$en_int_hotwater,
      d$en_int_heat
    )

    # Calibration energy poverty
    threshold_poverty <- fun_calibration_poverty(bld_det_ini,
                                                en_hh_tot,
                                                d$energy_poverty,
                                                path_out)


    report <- fun_format_output(1,
                    yrs,
                    stp,
                    sector,
                    run,
                    bld_det_ini,
                    bld_cases_fuel,
                    cat$ct_fuel,
                    d$shr_need_heat,
                    d$floor_cap,
                    d$hh_size,
                    d$pop,
                    report_var,
                    report,
                    en_m2_scen_heat,
                    en_m2_scen_cool,
                    en_hh_tot,
                    en_hh_hw_scen,
                    en_m2_hw_scen,
                    en_m2_others,
                    d$mat_int,
                    emission_factors,
                    d$emission_factors_embodied,
                    d$income,
                    threshold_poverty = threshold_poverty,
                    shr_en = shr_en)

    # Initial quantity to quantify learning
    techno_heat_ini <- bld_det_ini %>%
      group_by_at("fuel_heat") %>%
      summarize(n_i = sum(n_units_fuel)) %>%
      ungroup()

    # Loop over timesteps
    print(paste("Start scenario run", sector))
    parameters_renovation <- NULL
    parameters_heater <- NULL
    # Initialize carbon revenu in case of recycling
    carbon_revenue <- NULL
    renovation_ambition <- NULL
    # Initialize sub_report
    sub_report <- data.frame(value = numeric(),
      resolution = character(),
      variable = character(),
      type = character(),
      stringsAsFactors = FALSE)


    # Data store vintage of heating system by countries
    heater_vintage <- bld_det_ini %>%
      group_by(region_bld, fuel_heat) %>%
      summarize(n = sum(n_units_fuel)) %>%
      ungroup() %>%
      left_join(d$lifetime_heater) %>%
      select(-c("bld_age_min"))

    # Add all yrs with heater_vintage
    stp <- yrs[2] - yrs[1]
    heater_vintage <- heater_vintage %>%
      crossing(yrs[2:length(yrs)]) %>%
      rename("vintage" = "yrs[2:length(yrs)]") %>%
      mutate(n_vintage = ifelse(vintage <= yrs[1] + lifetime_heater,
        n * stp / lifetime_heater, 0)) %>%
      filter(n_vintage > 0) %>%
      select(c("region_bld", "fuel_heat", "vintage",
        "n_vintage", "lifetime_heater"))

    if (FALSE) {
      # Testing consistency of vintage stock
      t <- heater_vintage %>%
        group_by(region_bld, fuel_heat) %>%
        summarise(n_vintage = sum(n_vintage)) %>%
        ungroup()

      test <- bld_det_ini %>%
        group_by_at(c("region_bld", "fuel_heat")) %>%
        summarise(n_units_fuel = sum(n_units_fuel)) %>%
        ungroup() %>%
        left_join(t)
    }


    bld_det_i <- bld_det_ini
    for (i in 2:length(yrs)) {
      print(paste("Start scenario run", sector, "for year", yrs[i]))
      stp <- yrs[i] - yrs[i - 1]

      # Calculating revenue carbon tax
      if (!is.null(d$carbon_tax)) {
        carbon_revenue <- d$carbon_tax %>%
          filter(year == yrs[i]) %>%
          mutate(year = year - stp) %>%
          left_join(report$agg_result %>%
            filter(variable == "heat_tCO2") %>%
            rename(fuel = resolution)) %>%
          filter(region_bld %in% unique(cat$geo_data$region_bld)) %>%
          mutate(revenue = carbon_tax * value) %>%
          filter(!is.na(revenue))
        carbon_revenue <- sum(carbon_revenue$revenue) * stp / 1e9
      }

      # Learning rate
      learning_heat <- bld_det_i %>%
        group_by_at("fuel_heat") %>%
        summarize(n = sum(n_units_fuel)) %>%
        ungroup() %>%
        left_join(techno_heat_ini, by = "fuel_heat") %>%
        rename(fuel_heat_f = fuel_heat) %>%
        left_join(d$learning_rate_heat, by = "fuel_heat_f") %>%
        mutate(alpha = log(1 - learning_rate_heat) / log(2)) %>%
        mutate(learning_heat = (n / n_i)^(alpha)) %>%
        mutate(learning_heat = ifelse(learning_heat > 1, 1, learning_heat)) %>%
        select(c("fuel_heat_f", "learning_heat"))

      cost_invest_heat <- d$cost_invest_heat %>%
        left_join(learning_heat, by = "fuel_heat_f") %>%
        mutate(cost_invest_heat = cost_invest_heat * learning_heat) %>%
        select(-learning_heat)

      print("Calculate energy demand intensities for space heating and cooling")
      lst_en_i <- fun_en_sim(
        sector,
        yrs,
        i,
        bld_cases_fuel,
        d$en_int_heat,
        d$en_int_cool,
        d$days_cool,
        d$eff_cool,
        d$eff_heat,
        d$hours_heat,
        d$shr_floor_heat,
        d$hours_cool,
        d$shr_floor_cool,
        d$hours_fans,
        d$power_fans,
        d$shr_acc_cool,
        d$hh_size,
        d$floor_cap,
        price_en,
        d$income,
        en_method = en_method,
      )
      # Extract dataframes from list
      en_m2_scen_heat <- lst_en_i$en_m2_scen_heat
      en_m2_scen_cool <- lst_en_i$en_m2_scen_cool
      en_hh_tot <- lst_en_i$en_hh_tot
      rm(lst_en_i)

      # Energy demand intensities - hot water
      print("Calculate energy demand intensities for hot water")
      en_hh_hw_scen <- fun_hw_resid(
        yrs, i,
        bld_cases_fuel,
        d$hh_size,
        d$eff_hotwater,
        d$en_int_hotwater,
        d$en_int_heat
      )

      # Market share - new construction options
      print("Calculate market share for new construction")
      ms_new_i <- fun_ms_new_exogenous(
                    yrs,
                    i,
                    stock_aggr,
                    cat$ct_bld_age,
                    cat$ct_hh_inc,
                    cat$ct_fuel,
                    d$hh_tenure,
                    d$ms_shell_new_exo
      )
      try(if (nrow(ms_new_i) == 0)
        stop("Error in new construction calculation! Empty dataframe ms_new_i"))

      print(paste("1. Running stock turnover year:", yrs[i]))
      temp <- fun_stock_turnover_dyn(i, yrs, bld_cases_fuel, cat$ct_bld_age,
                                    stock_aggr, bld_det_i, d$prob_dem,
                                    path_out = path_out,
                                    rate_dem_target = param$rate_dem_target)

      bld_aggr_i <- temp$bld_aggr_i
      bld_det_i <- temp$bld_det_i
      demolition_heater <- temp$demolition_heater
      report_turnover <- temp$report

      # Updating vintage to consider demolition
      heater_vintage <- heater_vintage %>%
        left_join(demolition_heater, by = c("region_bld", "fuel_heat")) %>%
        group_by(region_bld, fuel_heat) %>%
        mutate(n_dem = n_dem * n_vintage / sum(n_vintage)) %>%
        ungroup() %>%
        mutate(n_vintage = n_vintage - n_dem) %>%
        select(-n_dem)

      print("2. Shell renovation decisions")
      new_det_i <- fun_stock_construction_dyn(bld_aggr_i,
                                              ms_new_i,
                                              cat$ct_fuel)

      if (energy_efficiency == "endogenous" && i == 2) {
        print("2.0 Calibration of renovation rate")
        if ("parameters_renovation" %in% names(d)) {
          parameters_renovation <- d$parameters_renovation %>%
            rename(constant = parameters_renovation)
        } else {
          parameters_renovation <- fun_calibration_ren_shell(yrs,
                          i,
                          bld_det_ini,
                          cat$ct_bld_age,
                          cat$ct_ren_eneff,
                          d$hh_size,
                          d$floor_cap,
                          d$cost_invest_ren_shell,
                          d$lifetime_ren,
                          en_hh_tot,
                          d$rate_shell_ren_exo,
                          d$ms_shell_ren_exo,
                          d$barriers_mfh,
                          d$barriers_rent,
                          stp,
                          path_out,
                          d$discount_rate,
                          param$social_discount_rate
                        )
        }
      }
      if (energy_efficiency == "endogenous") {
        print("2.1 Calculation of renovation rate")

        sub <- NULL
        if (!is.null(param$objective_type) && i >= 3) {
          if (param$objective_type == "deep_renovation" && i == 3) {
            print("Removing barriers for deep renovation")
            # Assigning constant value of "std" to "adv" parameters
            parameters_std <- filter(parameters_renovation,
              eneff_f == "std")
            parameters_adv <- mutate(parameters_std, "eneff_f" = "adv")
            parameters_std <- mutate(parameters_std, "constant" = NA)
            parameters_renovation <- bind_rows(parameters_std,
              parameters_adv)
          }

          for (region in unique(bld_det_i$region_bld)) {
            print(region)
            bld_det_i_region <- filter(bld_det_i, region_bld == region)

            temp <- fun_subsidies_insulation(i,
                        yrs,
                        stp,
                        param$objective_type,
                        param$budget_constraint_insulation,
                        carbon_revenue,
                        param$sub_ren_shell_type,
                        param$sub_ren_shell_household_target,
                        d,
                        cat,
                        param,
                        bld_det_i_region,
                        en_hh_tot,
                        parameters_renovation,
                        emission_factors,
                        param$anticipate_renovation,
                        region = region)

            sub_region <- temp$sub %>%
              mutate(region_bld = region)
            sub <- bind_rows(sub, sub_region)
            sub_report_region <- temp$sub_report %>%
              mutate(region_bld = region)
            sub_report <- bind_rows(sub_report, sub_report_region)
          }
        } else {
          sub <- filter(d$sub_ren_shell, year == yrs[i])
        }

        temp <- fun_ms_ren_shell_endogenous(yrs,
                          i,
                          bld_cases_fuel,
                          cat$ct_bld_age,
                          cat$ct_ren_eneff,
                          d$hh_size,
                          d$floor_cap,
                          d$cost_invest_ren_shell,
                          sub,
                          en_hh_tot,
                          d$lifetime_ren,
                          d$discount_rate,
                          param$social_discount_rate,
                          sub_ren_shell_type = param$sub_ren_shell_type,
                          parameters = parameters_renovation,
                          emission_factors = emission_factors,
                          anticipate_renovation = param$anticipate_renovation)
    
      } else {
        temp <- fun_ms_ren_shell_exogenous(
                      yrs,
                      i,
                      bld_cases_fuel,
                      cat$ct_bld_age,
                      d$rate_shell_ren_exo,
                      d$ms_shell_ren_exo)
      }
      # Extract dataframes from list
      ms_ren_i <- temp$ms_ren_i
      rate_ren_i <- temp$rate_ren_i
      anticipate <- temp$anticipate
      rm(temp)

      print("2.2 Integration of renovation in the existing stock")
      temp <- fun_stock_renovation_dyn(bld_det_i,
                                      rate_ren_i,
                                      ms_ren_i,
                                      stp,
                                      anticipate)
      bld_det_i <- temp$bld_det_i
      ren_det_i <- temp$ren_det_i
      rm(temp)
      bld_det_i <- bind_rows(bld_det_i, new_det_i)

      # Test consistency of stock turnover
      if (round(sum(bld_det_i$n_units_fuel) / 1e6, 1) !=
        round(sum(filter(stock_aggr, year == yrs[i])$n_units_aggr) / 1e6, 1)) {
        stop("Error in stock turnover calculation! 
          Sum of new and existing buildings is not equal to the total stock.")
      } else {
        print("Test stock turnover passed")
      }
      
      print("3. Switch heating system decisions")
      if (energy_efficiency == "endogenous" && i == 2) {
        print("3.0 Calibration of market shares switch fuel")
        if ("parameters_heater" %in% names(d)) {
          parameters_heater <- d$parameters_heater %>%
            rename(constant = parameters_heater)
          temp <- bld_cases_fuel %>%
            select(c("region_bld", "fuel_heat")) %>%
            distinct() %>%
            rename(fuel_heat_f = fuel_heat) %>%
            left_join(parameters_heater) %>%
            mutate(ct_fuel_excl_reg = ifelse(is.na(constant),
              1, 0)) %>%
            filter(ct_fuel_excl_reg == 1) %>%
            select(c("region_bld", "fuel_heat_f", "ct_fuel_excl_reg"))
          d$ct_fuel_excl_reg <- bind_rows(d$ct_fuel_excl_reg, temp) %>%
            distinct()
          
        } else {
          temp <- fun_calibration_switch_heat(yrs,
                    i,
                    bld_det_i,
                    cat$ct_bld_age,
                    d$ct_switch_heat,
                    d$ct_fuel_excl_reg,
                    d$cost_invest_heat,
                    en_hh_tot,
                    d$ms_switch_fuel_exo,
                    d$ct_heat,
                    d$ct_heat_new,
                    path_out,
                    d$discount_rate,
                    lifetime_heat = 20,
                    inertia = d$inertia,
                    emission_factors = emission_factors)
          parameters_heater <- temp$parameters_heater
          d$ct_fuel_excl_reg <- temp$ct_fuel_excl_reg
        }
      }

      if (energy_efficiency == "endogenous") {
        print("3.1 Calculate market share for fuel switches")

        budget <- NULL
        if (!is.null(param$budget_constraint_heater)) {
          if (param$budget_constraint_heater == "carbon_revenue") {
            budget <- carbon_revenue
            if (!is.null(budget)) {
              budget <- budget * param$share_recycling
            }
          } else if (budget_constraint_heater == "exogenous") {
            budget <- d$budget_constraint_heater
          } else {
            print("Cannot read budget_constraint_heater")
          }
        }

        sub <- NULL
        if (!is.null(budget)) {
          if (budget > 0) {
            temp <- fun_subsidies_heater(i,
                                        yrs,
                                        stp,
                                        param$sub_heater_type,
                                        budget,
                                        bld_det_i,
                                        ren_det_i,
                                        d,
                                        cat,
                                        param,
                                        en_hh_tot,
                                        cost_invest_heat,
                                        parameters_heater,
                                        lifetime_heater,
                                        emission_factors,
                                        param$premature_replacement,
                                        sub_report)
            sub <- temp$sub
            sub_report <- temp$sub_report
          }
        } else {
          sub <- filter(d$sub_heat, year == yrs[i])
        }
        temp <- fun_ms_switch_heat_endogenous(yrs,
                          i,
                          bld_det_i,
                          cat$ct_bld_age,
                          d$ct_switch_heat,
                          d$ct_fuel_excl_reg,
                          cost_invest_heat,
                          sub,
                          en_hh_tot,
                          d$ct_heat,
                          d$ct_heat_new,
                          d$discount_rate,
                          param$social_discount_rate,
                          lifetime_heat = 20,
                          inertia = d$inertia,
                          parameters = parameters_heater,
                          ban_fuel = d$ban_fuel,
                          ban_fuel_renovation = d$ban_fuel_renovation,
                          sub_heater_type = param$sub_heater_type,
                          emission_factors = emission_factors,
                          premature_replacement = param$premature_replacement)
          ms_sw_i <- temp$ms_i
          premature <- temp$premature

      } else {
        ms_sw_i <- fun_ms_fuel_sw_exogenous(yrs,
                          i,
                          bld_det_i,
                          cat$ct_bld_age,
                          d$ms_switch_fuel_exo)
      }
      print("3.2 Integration of switch fuel in the stock")
      # Check if there is at least one 1 in d$ban_fuel
      temp <- fun_stock_switch_fuel_dyn(
        bld_det_i,
        heater_vintage,
        ms_sw_i,
        cat$ct_fuel,
        stp,
        yrs[i],
        ren_det_i,
        mandatory_switch = param$mandatory_switch,
        premature = premature)
      bld_det_i <- temp$bld_det_i
      bld_det_i_sw <- temp$bld_det_i_sw

      # Updating vintage to consider new installation
      temp <- bld_det_i_sw %>%
        group_by(region_bld, fuel_heat) %>%
        summarize(n_vintage = sum(n_units_fuel)) %>%
        ungroup() %>%
        left_join(d$lifetime_heater) %>%
        mutate(vintage = yrs[i] + lifetime_heater) %>%
        select(c("region_bld", "fuel_heat", "vintage",
          "n_vintage", "lifetime_heater"))
      heater_vintage <- bind_rows(heater_vintage, temp) %>%
        filter(vintage > yrs[i]) %>%
        arrange(region_bld, fuel_heat, vintage)
      
      # Test consistency of stock turnover
      if (round(sum(bld_det_i$n_units_fuel) / 1e6, 0) !=
        round(sum(filter(stock_aggr, year == yrs[i])$n_units_aggr) / 1e6, 0)) {
        stop("Error in stock turnover calculation! 
          Sum of new and existing buildings is not equal to the total stock.")
      } else {
        print("Test stock turnover passed")
      }


      # Extract dataframes from list
      report <- fun_format_output(i,
                              yrs,
                              stp,
                              sector,
                              run,
                              bld_det_i,
                              bld_cases_fuel,
                              cat$ct_fuel,
                              d$shr_need_heat,
                              d$floor_cap,
                              d$hh_size,
                              d$pop,
                              report_var,
                              report,
                              en_m2_scen_heat,
                              en_m2_scen_cool,
                              en_hh_tot,
                              en_hh_hw_scen,
                              en_m2_hw_scen,
                              en_m2_others,
                              d$mat_int,
                              emission_factors,
                              d$emission_factors_embodied,
                              d$income,
                              threshold_poverty = threshold_poverty,
                              new_det_i = new_det_i,
                              ren_det_i = ren_det_i,
                              bld_det_i_sw = bld_det_i_sw,
                              shr_en = shr_en,
                              report_turnover = report_turnover)


      if (is.null(renovation_ambition)) {
        renovation_ambition <- report$agg_result %>%
          filter(region_bld == "EU", year == 2020, resolution == "all") %>%
          filter(variable %in%
            c("n_renovation", "stock_building")) %>%
          pivot_wider(id_cols = c(region_bld, year, resolution),
            names_from = variable,
            values_from = value) %>%
          mutate(renovation_rate = n_renovation / stock_building / stp) %>%
          pull(renovation_rate)
      }
    }
  }

  ### COMMERCIAL SECTOR
  if (sector == "comm") {
    # Initialize housing stock (fun)
    print(paste("Initialize scenario run", sector))

    lst_stock_init <- fun_stock_init_fut(
      sector, "stock",
      yrs,
      geo_data, "region_bld",
      bld_cases_eneff, bld_cases_fuel,
      pop_fut,
      hh_size, # used for residential
      floor_cap, # used for commercial
      ct_hh_inc,
      ct_eneff, ct_fuel,
      stock_arch_base,
      shr_mat, shr_arch, shr_fuel_heat_base, shr_distr_heat,
      # eff_cool_scen, eff_heat_scen,eff_hotwater_scen,
      # ren_en_sav_scen,
      # heat_hours_scen,cool_data_scen, heat_floor, shr_acc_cool,
      # en_m2_sim_r, price_en
      report_var
    )

    # Extract dataframes from list
    stock_aggr <- lst_stock_init$stock_aggr
    bld_det_age_i <- lst_stock_init$bld_det_age_i
    report <- lst_stock_init$report
    rm(lst_stock_init)

    # Loop over timesteps
    print(paste("Start scenario run", sector))

    for (i in 2:length(yrs)) {
      # Energy demand intensities
      lst_en_i <- fun_en_sim(sector,
        yrs, i,
        bld_cases_fuel,
        en_m2_sim_r,
        eff_cool_scen, eff_heat_scen,
        en_sav_ren,
        heat_hours_scen, heat_floor,
        cool_data_scen,
        shr_acc_cool,
        hh_size = NULL, # not used for commercial
        floor_cap,
        price_en = NULL # not used for commercial
      )

      # Extract dataframes from list
      en_m2_scen_heat <- lst_en_i$en_m2_scen_heat
      en_m2_scen_cool <- lst_en_i$en_m2_scen_cool
      ## Additional output not needed for commercial
      # en_hh_tot = lst_en_i$en_hh_tot #
      # en_hh_tot_ren_init = lst_en_i$en_hh_tot_ren_init
      # en_hh_tot_ren_fin = lst_en_i$en_hh_tot_ren_fin

      # Energy demand intensities - hot water
      en_m2_hw_scen <- fun_hw_comm(
        yrs, i,
        bld_cases_fuel,
        eff_hotwater_scen,
        en_m2_dhw
      )

      # Market share - new construction options
      ms_new_i <- fun_ms_new_target(
        yrs, i,
        bld_cases_eneff, bld_cases_fuel,
        ct_bld_age,
        shr_eneff_new, shr_fuel_new
      )

      # Market share - renovation options
      ms_ren_i <- fun_ms_ren_target(
        yrs, i,
        bld_cases_fuel, ct_bld_age,
        shr_eneff_ren, shr_fuel_ren
      )

      # Market share - fuel switches
      ms_sw_i <- fun_ms_fuel_sw(
        yrs, i,
        bld_cases_fuel, ct_bld_age,
        # ms_fuel_sw_target
        shr_fuel_sw
      )

      # Stock turnover
      lst_stock_i <- fun_stock_dyn(sector,
        yrs, i,
        run, ssp_r,
        "region_bld", "region_gea",
        bld_cases_fuel, bld_cases_eneff,
        ct_bld_age, ct_fuel,
        hh_size = NULL, # Not used for commercial
        floor_cap,
        stock_aggr, bld_det_age_i, # bld_det,
        # bld_eneff_age, # keep track of age
        bld_dyn_par,
        rate_ren_low, rate_ren_high, # ren_rate,
        rate_switch_fuel_heat,
        # ms_new, ms_ren,
        ms_new_i, ms_ren_i, rate_ren_i,
        ms_sw_i,
        # shr_acc_cool,
        shr_distr_heat, shr_need_heat,
        en_m2_scen_heat, en_m2_scen_cool,
        en_hh_hw_scen, en_m2_hw_scen, en_m2_others,
        # en_stock,
        mat_int,
        # mat_stock,
        report_var,
        report
      )

      # Extract dataframes from list
      report <- lst_stock_i$report
      stock_aggr <- lst_stock_i$stock_aggr
      bld_det_age_i <- lst_stock_i$bld_det_age_i

      rm(lst_stock_i)
    }
  }

  ### REPORTING ###

  ## MESSAGE report -  Aggregate results for reporting
  if ("MESSAGE" %in% report_type) {
    output <- fun_report_MESSAGE(sector, report_var, report,
      cat$geo_data, "region_bld", geo_level_report)
  }

  ## STURM basic report (results written as csv)
  if ("STURM" %in% report_type) {
    output <- fun_report_basic(report, report_var, cat$geo_data,
      "region_bld", geo_level_report, sector, scenario_name, path_out)

    write.csv(sub_report,
      paste0(path_out, "report_subsidies_", scenario_name, ".csv"))
  }

  ## Report results - IRP template (results written as csv)
  if ("IRP" %in% report_type) {
    output <- fun_report_IRP(report, report_var, cat$geo_data, "region_bld",
      geo_level_report, sector, scenario_name, yrs, path_out)
  }

  ## Report results - NGFS template (results written as csv)
  if ("NGFS" %in% report_type) {
    output <- fun_report_NGFS(report, report_var, geo_data, "region_bld",
      geo_level_report, sector, scenario_name, yrs, path_out)
  }

  ## Report results - NGFS template (results written as csv)
  if ("NAVIGATE" %in% report_type) {
    output <- fun_report_NAVIGATE(report, report_var,
      cat$geo_data, "region_bld",
      geo_level_report, sector, scenario_name, yrs, path_out, path_in,
      cat$ct_bld,cat$ct_ren_eneff, ren_en_sav_scen)
  }

  # Tracking time
  print("Scenario run completed!")
  print(paste("Time to run scenario:",
    round(Sys.time() - start_time, 0), "seconds."))
  rm(start_time)

  return(output)
}
