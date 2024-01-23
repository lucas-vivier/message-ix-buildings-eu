library(dplyr)


fun_subsidies_insulation <- function(i,
  yrs,
  stp,
  objective_type,
  budget_constraint_insulation,
  carbon_revenue,
  sub_ren_shell_type,
  sub_ren_shell_household_target,
  d,
  cat,
  param,
  bld_det_i,
  en_hh_tot,
  parameters_renovation,
  emission_factors,
  anticipate_renovation,
  region = NULL
  ) {

  print("Endogenous subsidies insulation")
  budget_renovation_function <- function(x, objective, objective_type,
                                  household_target,
                                  target = c("std", "adv")) {
      
      sub <- data.frame(sub_ren_shell = x, eneff_f = target,
        year = yrs[i]) %>%
        left_join(household_target, relationship = "many-to-many") %>%
        mutate(sub_ren_shell = sub_ren_shell * factor) %>%
        select(-c("factor"))

      temp <- fun_ms_ren_shell_endogenous(yrs,
            i,
            bld_det_i,
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
            sub_ren_shell_type = sub_ren_shell_type,
            parameters = parameters_renovation,
            emission_factors = emission_factors,
            anticipate_renovation = anticipate_renovation)

      anticipate <- temp$anticipate

      temp <- fun_stock_renovation_dyn(
        bld_det_i,
        temp$rate_ren_i,
        temp$ms_ren_i,
        stp,
        anticipate = anticipate,
        verbose = FALSE)
      
      ren_det_i <- temp$ren_det_i %>%
        mutate(subsidies = n_units_fuel * sub_ren_hh)

      sum <- sum(filter(bld_det_i, bld_age != "p5",
        eneff %in% c("avg"))$n_units_fuel_exst)

      if (objective_type == "budget") {
        rslt <- sum(ren_det_i$subsidies) * 1e3 / 1e9
      } else if (objective_type %in% c("renovation", "deep_renovation")) {
        rslt <- sum(ren_det_i$n_units_fuel) / sum / step_year
      }

      return(rslt - objective)
  }

  if (objective_type == "budget") {
    objective <- NULL
    if (!is.null(budget_constraint_insulation)) {
      if (budget_constraint_insulation == "carbon_revenue") {
        objective <- carbon_revenue
      } else if (budget_constraint_insulation == "exogenous") {
        objective <- d$budget_constraint_insulation
      } else {
        print("Cannot read budget_constraint_insulation")
      }
    }
  } else if (objective_type %in% "renovation") {
    if (!is.null(region)) {
      objective <- filter(d$objectives_renovation, region_bld == region)
    } else {
      objective <- d$objectives_renovation
    }

    objective <- filter(objective,
      year == yrs[i]) %>% pull(objectives_renovation)
    
    target <- c("std", "adv")
  } else if (objective_type == "deep_renovation") {
    if (!is.null(region)) {
      objective <- filter(d$objectives_renovation, region_bld == region)
        } else {
      objective <- d$objectives_renovation
    }

    objective <- filter(objective,
      year == yrs[i]) %>% pull(objectives_renovation)
    target <- c("adv")
  } else {
    print("Cannot read objective_type")
  }


  if (sub_ren_shell_type == "ad_valorem") {
    interval <- c(0, 0.9)
  } else if (sub_ren_shell_type == "per_CO2") {
    interval <- c(0, 2000)
  } else if (sub_ren_shell_type == "per_kWh") {
    interval <- c(0.01, 0.2)
  } else {
    print("No subsidies type for renovation")
  }

  household_target <- data.frame(inc_cl = c("q1", "q2", "q3"),
    factor = 1, year = yrs[i])
  if (sub_ren_shell_household_target == "low_income") {
    print("low_income")
    household_target <- data.frame(inc_cl = c("q1", "q2", "q3"),
      factor = c(1, 0.7, 0.5), year = rep(yrs[i], 3))
  }


  if (budget_renovation_function(interval[2], objective, objective_type, household_target, target) <= 0) {
    val <- interval[2]
  } else if (budget_renovation_function(interval[1], objective, objective_type, household_target, target) >= 0) {
    val <- interval[1]
  } else {
    root <- uniroot(budget_renovation_function,
      interval = interval, objective = objective,
      objective_type = objective_type, target = target,
      household_target = household_target,
      tol = 0.01)
    val <- root$root
  }



  if (sub_ren_shell_type == "ad_valorem") {
    print(paste0("Subsidies renovation shell: ", round(val * 100, 3), "%"))
  } else if (sub_ren_shell_type == "per_kWh") {
    print(paste0("Subsidies renovation shell: ", round(val, 3), "EUR/kWh"))
  } else if (sub_ren_shell_type == "per_CO2") {
    print(paste0("Subsidies renovation shell: ", round(val, 0), "EUR/tCO2"))
  } else {
    print("No subsidies type for renovation")
  }

  sub <- data.frame(sub_ren_shell = val,
    eneff_f = target,
    year = yrs[i]) %>%
    left_join(household_target, relationship = "many-to-many") %>%
    mutate(sub_ren_shell = sub_ren_shell * factor) %>%
    select(-c("factor"))
      
  sub_report <- sub %>%
    mutate(resolution = paste0(eneff_f, "-", inc_cl)) %>%
    select(-c(eneff_f, inc_cl)) %>%
    rename(value = sub_ren_shell) %>%
    mutate(variable = "insulation",
      type = sub_ren_shell_type)
    
  return(list(sub = sub, sub_report = sub_report))

}

fun_subsidies_heater <- function(i,
  yrs,
  stp,
  sub_heater_type,
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
  premature_replacement,
  sub_report
  ) {

  print("Endogenous heater subsidies to reach budget constraint")

  budget_switch_function <- function(x, budget) {
    # print(x)
    sub <- data.frame(sub_heat = x,
                      fuel_heat_f = "heat_pump",
                      fuel_heat = c("oil", "gas", "coal"),
                      year = yrs[i])

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
                      sub_heater_type = sub_heater_type,
                      emission_factors = emission_factors,
                      premature_replacement = premature_replacement)
    ms_sw_i <- temp$ms_i
    premature <- temp$premature

    temp <- fun_stock_switch_fuel_dyn(bld_det_i,
                              lifetime_heater,
                              ms_sw_i,
                              cat$ct_fuel,
                              stp,
                              yrs[i],
                              ren_det_i,
                              mandatory_switch = FALSE,
                              premature = premature,
                              verbose = FALSE)
    bld_det_i_sw <- temp$bld_det_i_sw %>%
      mutate(subsidies = n_units_fuel * sub_heat_hh) %>%
      mutate(cost = n_units_fuel * cost_invest_heat)
    
    number_hp <- sum(filter(bld_det_i_sw,
      fuel_heat == "heat_pump")$n_units_fuel, na.rm = TRUE) / 1e3
    rslt <- sum(bld_det_i_sw$subsidies, na.rm = TRUE) / 1e9

    return(rslt - budget)
  }

  root <- uniroot(budget_switch_function,
    interval = c(0, 0.9), budget = budget, tol = 0.01)

  if (sub_heater_type == "ad_valorem") {
    print(paste0("Subsidies heater: ",
    round(root$root * 100, 0), "%"))

  } else if (sub_heater_type == "per_CO2") {
    print(paste0("Subsidies heater: ",
    round(root$root, 0), "EUR/tCO2"))
  } else {
    print("No subsidies type for renovation")
  }

  sub <- data.frame(sub_heat = root$root,
                    fuel_heat_f = "heat_pump",
                    year = yrs[i])
  temp <- sub %>%
    rename(resolution = fuel_heat_f,
      value = sub_heat) %>%
    mutate(variable = "heater",
      type = sub_heater_type)
  sub_report <- bind_rows(sub_report, temp)

  return(list(sub = sub, sub_report = sub_report))
}