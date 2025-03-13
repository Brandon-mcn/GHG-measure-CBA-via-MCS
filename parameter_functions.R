### CO2e Emission Factor Library Function

EFL_co2e <- function(GWP){
  GWP_full <- read_excel("openEFL.xlsx", sheet = "GWPs") # read openEFL tables
  EFL <- read_excel("openEFL.xlsx", sheet = "EFL")
  
  gwp_key <- c("ghg",GWP) # Create the table of global warming potentials
  GWPs <- GWP_full[, gwp_key, with = FALSE]
  colnames(GWPs)[2] <- "gwp"
  co2gwp <- GWPs$gwp[GWPs$ghg == "co2"]
  ch4gwp <- GWPs$gwp[GWPs$ghg == "ch4"]
  n2ogwp <- GWPs$gwp[GWPs$ghg == "n2o"]
  
  EFL1 <- merge.data.table(EFL, GWPs, sort = FALSE, all.x = TRUE) # Consolidate the emission factor library into CO2e values
  EFL1 <- as.data.table(EFL1)
  EFL1[, gwps_ar := GWP]
  EFL1[, kgco2e_perunit := kg_ghg_perunit*gwp]
  EFL1[, ghg := ifelse(ghg %in% c("co2", "ch4", "n2o"), ghg, "other_ghgs")]
  EFL_CO2e <- dcast(EFL1, ef_source +
                      ef_publishdate +
                      ef_activeyear +
                      service_type +
                      unit +
                      emission_category +
                      service_subcategory1 +
                      service_subcategory2 +
                      emission_scope +
                      country +
                      subregion +
                      gwps_ar ~ ghg,
                    value.var = "kgco2e_perunit",
                    fun.aggregate = sum)
  EFL_CO2e[, kgco2e_perunit := as.numeric(co2 + ch4 + n2o + other_ghgs)]
  EFL_CO2e[, co2_gwp := co2gwp]
  EFL_CO2e[, ch4_gwp := ch4gwp]
  EFL_CO2e[, n2o_gwp := n2ogwp]
  EFL_CO2e[, co2 := co2 / co2gwp]
  EFL_CO2e[, ch4 := ch4 / ch4gwp]
  EFL_CO2e[, n2o := n2o / n2ogwp]
  setnames(EFL_CO2e, "co2", "co2_kgperunit")
  setnames(EFL_CO2e, "ch4", "ch4_kgperunit")
  setnames(EFL_CO2e, "n2o", "n2o_kgperunit")
  setnames(EFL_CO2e, "other_ghgs", "otherghgs_kgco2eperunit")
  EFL_CO2e[, ef_activeyear := as.numeric(ef_activeyear)]
  EFL_CO2e[is.na(EFL_CO2e)] <- ""
  setcolorder(EFL_CO2e, c("ef_source",
                          "ef_publishdate",
                          "ef_activeyear",
                          "service_type",
                          "unit",
                          "emission_category",
                          "service_subcategory1", 
                          "service_subcategory2", 
                          "emission_scope",
                          "country",
                          "subregion",
                          "gwps_ar",
                          "co2_gwp",
                          "ch4_gwp",
                          "n2o_gwp",
                          "co2_kgperunit", 
                          "ch4_kgperunit",
                          "n2o_kgperunit", 
                          "otherghgs_kgco2eperunit",
                          "kgco2e_perunit"))
  assign("EFL", EFL_CO2e, envir = .GlobalEnv)
}

### Normal Distribution Function

normal_PDF <- function(param_name, mean, sd, units) {
  mcs_out <- data.frame(matrix(ncol = scenario_years, nrow = run_count)) # Initialize an empty dataframe
  for (i in 1:run_count) {
    value <- rnorm(1, mean, sd) # Loop through each row, generate a single random value from normal distribution
    mcs_out[i, ] <- rep(value, scenario_years) # Fill the entire row with the same value
  }
  mcs_out <- as.data.frame(mcs_out)
  colnames(mcs_out) <- output_headers # Assign column names
  mcs_out <- mcs_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(param_name, mcs_out, envir = .GlobalEnv) # Assign the generated dataframe to the specified name in the global environment
}

### continuous uniform parameter 1 - Select random value and assign it to all scenario years in one MCS trial

continuous_uniform1 <- function(param_name, minvalue, maxvalue, units){
  mcs_out <- data.frame(matrix(ncol = scenario_years, nrow = run_count)) # Initialize an empty dataframe
  for (i in 1:run_count) {
    random_value <- runif(1, min = minvalue, max = maxvalue)
    param_sim <- rep(random_value, times = scenario_years)
    mcs_out[i, ] <- param_sim
  }
  mcs_out <- as.data.frame(mcs_out)
  colnames(mcs_out) <- output_headers # Assign column names
  mcs_out <- mcs_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(param_name, mcs_out, envir = .GlobalEnv) # Assign the generated dataframe to the specified name in the global environment
}

### continuous uniform parameter 2 - randomly assign a value to each scenario year 

continuous_uniform2 <- function(param_name, minvalue, maxvalue, units){
  mcs_out <- data.frame(matrix(ncol = scenario_years, nrow = run_count)) # Initialize an empty dataframe
  for (i in 1:run_count) {
    param_sim <- runif(scenario_years, min = minvalue, max = maxvalue)
    mcs_out[i, ] <- param_sim
  }
  mcs_out <- as.data.frame(mcs_out)
  colnames(mcs_out) <- output_headers # Assign column names
  mcs_out <- mcs_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(param_name, mcs_out, envir = .GlobalEnv) # Assign the generated dataframe to the specified name in the global environment
}

### discrete uniform parameter 1 - Select random value and assign it to all scenario years in one MCS trial

discrete_uniform1 <- function(param_name, minvalue, maxvalue, units){
  mcs_out <- data.frame(matrix(ncol = scenario_years, nrow = run_count)) # Initialize an empty dataframe
  for (i in 1:run_count) {
    random_value <- sample(minvalue:maxvalue, size = 1)
    param_sim <- rep(random_value, times = scenario_years)
    mcs_out[i, ] <- param_sim
  }
  mcs_out <- as.data.frame(mcs_out)
  colnames(mcs_out) <- output_headers # Assign column names
  mcs_out <- mcs_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(param_name, mcs_out, envir = .GlobalEnv) # Assign the generated dataframe to the specified name in the global environment
}

### discrete uniform parameter 2 - randomly assign a value to each scenario year 

discrete_uniform2 <- function(param_name, minvalue, maxvalue, units){
  mcs_out <- data.frame(matrix(ncol = scenario_years, nrow = run_count)) # Initialize an empty dataframe
  for (i in 1:run_count) {
    param_sim <- sample(minvalue:maxvalue, size = scenario_years)
    mcs_out[i, ] <- param_sim
  }
  mcs_out <- as.data.frame(mcs_out)
  colnames(mcs_out) <- output_headers # Assign column names
  mcs_out <- mcs_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(param_name, mcs_out, envir = .GlobalEnv) # Assign the generated dataframe to the specified name in the global environment
}

### discrete uniform parameter 3 - select random value and assign it for a specific number of scenario years, then default to a secondary value

discrete_uniform3 <- function(param_name, minvalue, maxvalue, nyears, value2 = 0, units){
  mcs_out <- data.frame(matrix(ncol = scenario_years, nrow = run_count)) # Initialize an empty dataframe
  for (i in 1:run_count) {
    random_value <- sample(minvalue:maxvalue, size = 1)
    param_sim <- c(rep(random_value, times = nyears),rep(value2, times = scenario_years - nyears))
    mcs_out[i, ] <- param_sim
  }
  mcs_out <- as.data.frame(mcs_out)
  colnames(mcs_out) <- output_headers # Assign column names
  mcs_out <- mcs_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(param_name, mcs_out, envir = .GlobalEnv) # Assign the generated dataframe to the specified name in the global environment
}

### Static value parameter

static_value <- function(param_name, value, units){
  mcs_out <- data.frame(matrix(value, nrow = run_count, ncol = scenario_years))
  colnames(mcs_out) <- output_headers # Assign column names
  mcs_out <- mcs_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(param_name, mcs_out, envir = .GlobalEnv) # Assign the generated dataframe to the specified name in the global environment
}

### Scenario selection formula

scenario_MCS <- function(param_name, sheet_name, units){
  df <- read_excel("scenarios.xlsx", sheet = sheet_name)
  colfilter <- c("scenario_prob", output_headers)
  df_scenarios <- df[, colfilter]
  mcs_out <- data.frame(matrix(ncol = scenario_years + 1, nrow = run_count))
  for (i in 1:run_count) {
    scenario_selection <- df_scenarios[sample(nrow(df_scenarios), size = 1, prob = df_scenarios$scenario_prob), ]
    mcs_out[i, ] <- scenario_selection
  }
  colnames(mcs_out) <- colfilter
  mcs_out <- mcs_out %>% select(-scenario_prob)
  mcs_out <- mcs_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(param_name, mcs_out, envir = .GlobalEnv)
}

### Activity data calculation 1 - Reference scenario is a baseline, intervention is a % reduction from the baseline

calc_ad1 <- function(ref_name, intv_name, boundary_df, ref_pdf, ref_params, intv_pdf, intv_params, units){
  #initiate empty dataframe 
  ref_out <- matrix(0, nrow = run_count, ncol = scenario_years)
  intv_out <- matrix(0, nrow = run_count, ncol = scenario_years)
  
  for (i in 1:run_count) {
    for (j in 1:scenario_years) {
      n <- boundary_df[i, (j+1)] # Because the boundary data has a leading unit column, j is shifted over by 1
      
      # Simulate reference and intervention scenarios for each intervention instance
      ref_sim <- do.call(ref_pdf, c(list(n), ref_params)) # Simulate activity data for each instance
      intv_factors <- do.call(intv_pdf, c(list(n), intv_params)) # Simulate the activity reduction factor for each instance
      intv_savings <- 1-intv_factors # inverse of intv_factor
      intv_sim <- ref_sim * intv_savings # calculate intervention activity data for each instance
      
      # Sum the total activity data 
      ref_sum <- sum(ref_sim) # sum of reference energy consumption for all homes
      intv_sum <- sum(intv_sim) # sum of intervention energy consumption for all homes
      
      # Store in respective matrices
      ref_out[i, j] <- ref_sum
      intv_out[i, j] <- intv_sum
    }
  }
  
  ref_out <- as.data.frame(ref_out)
  colnames(ref_out) <- output_headers
  ref_out <- ref_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(ref_name, ref_out, envir = .GlobalEnv)
  
  intv_out <- as.data.frame(intv_out)
  colnames(intv_out) <- output_headers
  intv_out <- intv_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(intv_name, intv_out, envir = .GlobalEnv)
}

### Random assignment of intervention instances to project type

assign_projecttype <- function(intervention_sim){
  # Initialize empty matrices for each energy efficiency project type
  ashp_bound <- matrix(0, nrow = run_count, ncol = scenario_years)
  weatherize_bound <- matrix(0, nrow = run_count, ncol = scenario_years)
  hpwh_bound <- matrix(0, nrow = run_count, ncol = scenario_years)
  
  # Loop through each cell in the dataframe
  for (i in 1:run_count) {
    for (j in 1:scenario_years) {
      value <- intervention_sim[i, (j+1)]  # Get the original value, because the boundary data has a leading unit column, j is shifted over by 1
      
      # Generate random assignments for A, B, and C
      assignments <- sample(c("A", "W", "H"), value, replace = TRUE)
      
      # Count how many went to A, B, and C
      count_A <- sum(assignments == "A")
      count_W <- sum(assignments == "W")
      count_H <- sum(assignments == "H")
      
      # Store in respective matrices
      ashp_bound[i, j] <- count_A
      weatherize_bound[i, j] <- count_W
      hpwh_bound[i, j] <- count_H
    }
  }
  
  # Convert matrices to dataframes, set column headers, and assign to the global environment
  ashp_bound <- as.data.frame(ashp_bound)
  colnames(ashp_bound) <- output_headers
  ashp_bound <- ashp_bound %>%
    mutate(unit = intervention_sim[1,1]) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign("ashp_boundary_mgnl", ashp_bound, envir = .GlobalEnv)
  
  weatherize_bound <- as.data.frame(weatherize_bound)
  colnames(weatherize_bound) <- output_headers
  weatherize_bound <- weatherize_bound %>%
    mutate(unit = intervention_sim[1,1]) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign("weatherize_boundary_mgnl", weatherize_bound, envir = .GlobalEnv)
  
  hpwh_bound <- as.data.frame(hpwh_bound)
  colnames(hpwh_bound) <- output_headers
  hpwh_bound <- hpwh_bound %>%
    mutate(unit = intervention_sim[1,1]) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign("hpwh_boundary_mgnl", hpwh_bound, envir = .GlobalEnv)
  
}

### Cumulative sum calculation 1 - Basic cumulative sum, preserving the units column

cumul_sum1 <- function(param_name, df){
  values <- df[, -1]
  df_cumsum <- as.data.frame(t(apply(values, 1, cumsum)))
  df_cumsum <- df_cumsum %>%
    mutate(unit = df[1,1]) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(param_name, df_cumsum, envir = .GlobalEnv)
}

### Cumulative sum calculation 2 - Custom cumulative sum based on intervention lifetime

cumul_sum2 <- function(param_name, df, intv_life = intv_lifetime){
  df_trim <- df[, -1]
  df_result <- df[, -1]
  for (i in 1:run_count) {  # Loop through each row
    for (j in 2:scenario_years) {  # Start from column 2
      if (j <= intv_life) {
        df_result[i, j] <- sum(df_trim[i, 1:j])  # Regular cumulative sum
      } else {
        df_result[i, j] <- sum(df_trim[i, (j-(intv_life-1)):j])  # Rolling sum of the active intervention instances
      }
    }
  }
  df_result <- df_result %>%
    mutate(unit = df[1,1]) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(param_name, df_result, envir = .GlobalEnv)
}

### Capex calculation

calculate_capex <- function(ref_name, intv_name, ref_bound, intv_bound, capex_min, capex_max, units){
  #initiate empty dataframe 
  ref_out <- matrix(0, nrow = run_count, ncol = scenario_years)
  intv_out <- matrix(0, nrow = run_count, ncol = scenario_years)
  
  for (i in 1:run_count) {
    for (j in 1:scenario_years) {
      nr <- ref_bound[i, (j+1)] # Because the boundary data has a leading unit column, j is shifted over by 1
      ni <- intv_bound[i, (j+1)]
      
      # Calculate total capex costs for the reference and intervention scenarios
      capex_refsim <- runif(nr, min = capex_min, max = capex_max) # Simulate capex for each home
      capex_intvsim <- runif(ni, min = capex_min, max = capex_max) # Simulate capex for each home

      # Sum total capex
      ref_sum <- sum(capex_refsim) # sum of reference capex for all homes
      intv_sum <- sum(capex_intvsim) # sum of intervention capex for all homes
      
      # Store in respective matrices
      ref_out[i, j] <- ref_sum
      intv_out[i, j] <- intv_sum
    }
  }
  
  ref_out <- as.data.frame(ref_out) * -1
  colnames(ref_out) <- output_headers
  ref_out <- ref_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(ref_name, ref_out, envir = .GlobalEnv)
  
  intv_out <- as.data.frame(intv_out) * -1
  colnames(intv_out) <- output_headers
  intv_out <- intv_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(intv_name, intv_out, envir = .GlobalEnv)
}

### Opex calculation

calculate_opex <- function(ref_name, intv_name, ref_ad, intv_ad, opex, units){
  #initiate empty dataframe 
  ref_out <- matrix(0, nrow = run_count, ncol = scenario_years)
  intv_out <- matrix(0, nrow = run_count, ncol = scenario_years)
  
  for (i in 1:run_count) {
    for (j in 1:scenario_years) {
      nr <- ref_ad[i, (j+1)] # Because the activity data has a leading unit column, j is shifted over by 1
      ni <- intv_ad[i, (j+1)]
      
      # Calculate total opex
      ref_sum <- nr * opex
      intv_sum <- ni * opex
      
      # Store in respective matrices
      ref_out[i, j] <- ref_sum
      intv_out[i, j] <- intv_sum
    }
  }
  
  ref_out <- as.data.frame(ref_out) * -1
  colnames(ref_out) <- output_headers
  ref_out <- ref_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(ref_name, ref_out, envir = .GlobalEnv)
  
  intv_out <- as.data.frame(intv_out) * -1
  colnames(intv_out) <- output_headers
  intv_out <- intv_out %>%
    mutate(unit = units) %>%  # Add "unit" column with user 'units' input as value
    select(unit, everything())  # Move "unit" to the first position
  assign(intv_name, intv_out, envir = .GlobalEnv)
}

### GHG Conversion function using a static emission factor

ghg_conversion1 <- function(ref_name,
                            intv_name,
                            ref_ad,
                            intv_ad,
                            fltr.ef_activeyear,
                            fltr.service_type, 
                            fltr.emission_category, 
                            fltr.emission_scope, 
                            fltr.service_subcategory1 = "", 
                            fltr.service_subcategory2 = "", 
                            fltr.country = "global", 
                            fltr.subregion = "") {
  fltr.unit <- ref_ad[1,1]
  EFL <- as.data.table(EFL)
  EF_row <- EFL[ef_activeyear == fltr.ef_activeyear &
                service_type == fltr.service_type &
                emission_category == fltr.emission_category &
                emission_scope == fltr.emission_scope &
                service_subcategory1 == fltr.service_subcategory1 &
                service_subcategory2 == fltr.service_subcategory2 &
                country == fltr.country &
                subregion == fltr.subregion &
                unit == fltr.unit]
  EF_value <- EF_row$kgco2e_perunit
  EF_df <- data.frame(matrix(EF_value, nrow = run_count, ncol = scenario_years))
  
  ref_ghg <- EF_df * ref_ad[, -1]
  colnames(ref_ghg) <- output_headers
  ref_ghg <- ref_ghg %>%
    mutate(unit = "kgco2e") %>% 
    select(unit, everything())  
  assign(ref_name, ref_ghg, envir = .GlobalEnv)
  
  intv_ghg <- EF_df * intv_ad[, -1]
  colnames(intv_ghg) <- output_headers
  intv_ghg <- intv_ghg %>%
    mutate(unit = "kgco2e") %>% 
    select(unit, everything())  
  assign(intv_name, intv_ghg, envir = .GlobalEnv)
}

### GHG Conversion function with EF simulation from MCS 

ghg_conversion2 <- function(ref_name, intv_name, ef_sim, ref_ad, intv_ad){
  ref_ghg <- ef_sim[, -1] * ref_ad[, -1]
  colnames(ref_ghg) <- output_headers
  ref_ghg <- ref_ghg %>%
    mutate(unit = "kgco2e") %>% 
    select(unit, everything())  
  assign(ref_name, ref_ghg, envir = .GlobalEnv)
  
  intv_ghg <- ef_sim[, -1] * intv_ad[, -1]
  colnames(intv_ghg) <- output_headers
  intv_ghg <- intv_ghg %>%
    mutate(unit = "kgco2e") %>% 
    select(unit, everything())  
  assign(intv_name, intv_ghg, envir = .GlobalEnv)
}

### Spend-based GHG Conversion function with EF simulation from MCS (because costs are negative, this takes the inverse of the ad input)

ghg_conversion3 <- function(ref_name, intv_name, ef_sim, ref_ad, intv_ad){
  ref_ghg <- ef_sim[, -1] * ref_ad[, -1] * -1
  colnames(ref_ghg) <- output_headers
  ref_ghg <- ref_ghg %>%
    mutate(unit = "kgco2e") %>% 
    select(unit, everything())  
  assign(ref_name, ref_ghg, envir = .GlobalEnv)
  
  intv_ghg <- ef_sim[, -1] * intv_ad[, -1] * -1
  colnames(intv_ghg) <- output_headers
  intv_ghg <- intv_ghg %>%
    mutate(unit = "kgco2e") %>% 
    select(unit, everything())  
  assign(intv_name, intv_ghg, envir = .GlobalEnv)
}

### Calculate CBA indicators (NPV, SROI, LCCA) at different discount rates

cba_discountrange <- function(dr_min, dr_max, scenario_lifetime = scenario_years){
  # Set discount rate variables
  drmin_loop = dr_min*10
  drmax_loop = dr_max*10
  dr_count = dr_max*10 - dr_min*10 + 1
  
  # NPV
  npv_private_dr <- data.frame(matrix(NA, nrow = run_count, ncol = dr_count)) # make an empty matrix to populate NPV for each discount rate
  npv_econ_dr <- data.frame(matrix(NA, nrow = run_count, ncol = dr_count))
  npv_social_dr <- data.frame(matrix(NA, nrow = run_count, ncol = dr_count))
  colnames(npv_private_dr) <- seq(dr_min, dr_max, by = 0.1) # column headers for each discount rate, sequenced by 1/10 percent
  colnames(npv_econ_dr) <- seq(dr_min, dr_max, by = 0.1)
  colnames(npv_social_dr) <- seq(dr_min, dr_max, by = 0.1)
  
  for (i in drmin_loop:drmax_loop) { # the loop repeats for each discount rate in the provided range
    drvalues <- numeric(scenario_years) # an empty vector 'drvalues' is created to house the loop outputs
    drvalues[1] <- 1        
    for (n in 2:scenario_years) {
      drvalues[n] <- (1+(i/1000))^(n-1) # drvalues is populated with the discount rate conversion factor for each year
    }
    drvalues_df <- data.frame(t(drvalues)) # make a dataframe out of the discount rate loop output
    colnames(drvalues_df) <- output_headers # make the column headers years
    discount_df <- drvalues_df[rep(1, run_count),] # the discount rate vector is replicated into rows equal to the MCS run count
    
    annual_npv_private <- cashflow_private/discount_df # the total annual cash flow is converted into NPV
    annual_npv_econ <- cashflow_econ/discount_df
    annual_npv_social <- annual_npv_econ + coben_value_trim # cobenefit value is adjusted at different discount rate. Right now, dr = 0 is used
    
    npv_private_cumsum <- as.data.frame(t(apply(annual_npv_private, 1, cumsum))) # net present value is summed cumulatively for each year
    npv_econ_cumsum <- as.data.frame(t(apply(annual_npv_econ, 1, cumsum)))
    npv_social_cumsum <- as.data.frame(t(apply(annual_npv_social, 1, cumsum)))
    
    npv_private <- npv_private_cumsum[scenario_lifetime] # the total NPV is selected based on the scenario lifetime
    npv_econ <- npv_econ_cumsum[scenario_lifetime]
    npv_social <- npv_social_cumsum[scenario_lifetime]
    
    col_entry <- (i - ((dr_min*10) - 1))  # identify column for storing data
    
    npv_private_dr[, col_entry] <- npv_private #bind the NPV values for each MCS row for the given discount rate
    npv_econ_dr[, col_entry] <- npv_econ #bind the NPV values for each MCS row for the given discount rate
    npv_social_dr[, col_entry] <- npv_social #bind the NPV values for each MCS row for the given discount rate
  }
  assign("npv_private_dr", npv_private_dr, envir = .GlobalEnv)
  assign("npv_econ_dr", npv_econ_dr, envir = .GlobalEnv)
  assign("npv_social_dr", npv_social_dr, envir = .GlobalEnv)
  
  # SROI
  sroi_private_dr <- data.frame(matrix(NA, nrow = run_count, ncol = dr_count)) # make an empty matrix to populate sroi for each discount rate
  sroi_econ_dr <- data.frame(matrix(NA, nrow = run_count, ncol = dr_count))
  sroi_social_dr <- data.frame(matrix(NA, nrow = run_count, ncol = dr_count))
  colnames(sroi_private_dr) <- seq(dr_min, dr_max, by = 0.1) # column headers for each discount rate, sequenced by 1/10 percent
  colnames(sroi_econ_dr) <- seq(dr_min, dr_max, by = 0.1)
  colnames(sroi_social_dr) <- seq(dr_min, dr_max, by = 0.1)
  
  for (i in drmin_loop:drmax_loop) { # the loop repeats for each discount rate in the provided range
    drvalues <- numeric(scenario_years) # an empty vector 'drvalues' is created to house the loop outputs
    drvalues[1] <- 1        
    for (n in 2:scenario_years) {
      drvalues[n] <- (1+(i/1000))^(n-1) # drvalues is populated with the discount rate conversion factor for each year
    }
    drvalues_df <- data.frame(t(drvalues)) # make a dataframe out of the discount rate loop output
    colnames(drvalues_df) <- output_headers # make the column headers years
    discount_df <- drvalues_df[rep(1, run_count),] # the discount rate vector is replicated into rows equal to the MCS run count
    
    annual_beni_private <- cashflow_opex/discount_df # opex is treated as the flow of economic benefits in this case
    annual_beni_econ <- cashflow_opex/discount_df
    annual_beni_social <- annual_beni_econ + coben_value_trim # cobenefit value is adjusted at different discount rate. Right now, dr = 0 is used
    
    annual_capex_private <- (cashflow_investment*-1)/discount_df # capex is converted to a positive number and adjusted for the discount rate
    annual_capex_econ <- (cashflow_investment*-1)/discount_df 
    annual_capex_social <- (cashflow_investment*-1)/discount_df 
    
    beni_private_cumsum <- as.data.frame(t(apply(annual_beni_private, 1, cumsum))) # annual benefits is summed cumulatively for each year
    beni_econ_cumsum <- as.data.frame(t(apply(annual_beni_econ, 1, cumsum)))
    beni_social_cumsum <- as.data.frame(t(apply(annual_beni_social, 1, cumsum)))
    
    capex_private_cumsum <- as.data.frame(t(apply(annual_capex_private, 1, cumsum))) # annual capex is summed cumulatively for each year
    capex_econ_cumsum <- as.data.frame(t(apply(annual_capex_econ, 1, cumsum)))
    capex_social_cumsum <- as.data.frame(t(apply(annual_capex_social, 1, cumsum)))
    
    beni_private <- beni_private_cumsum[scenario_lifetime] # the total benefits is selected based on the scenario lifetime
    beni_econ <- beni_econ_cumsum[scenario_lifetime]
    beni_social <- beni_social_cumsum[scenario_lifetime]
    
    capex_private <- capex_private_cumsum[scenario_lifetime] # the total capex is selected based on the scenario lifetime
    capex_econ <- capex_econ_cumsum[scenario_lifetime]
    capex_social <- capex_social_cumsum[scenario_lifetime]
    
    sroi_private <- beni_private / capex_private
    sroi_econ <- beni_econ / capex_econ
    sroi_social <- beni_social / capex_social
    
    col_entry <- (i - ((dr_min*10) - 1))  # identify column for storing data
    
    sroi_private_dr[, col_entry] <- sroi_private #bind the sroi values for each MCS row for the given discount rate
    sroi_econ_dr[, col_entry] <- sroi_econ #bind the sroi values for each MCS row for the given discount rate
    sroi_social_dr[, col_entry] <- sroi_social #bind the sroi values for each MCS row for the given discount rate
  }
  assign("sroi_private_dr", sroi_private_dr, envir = .GlobalEnv)
  assign("sroi_econ_dr", sroi_econ_dr, envir = .GlobalEnv)
  assign("sroi_social_dr", sroi_social_dr, envir = .GlobalEnv)
  
  # LCCA
  ghg_cumsum_annual <- as.data.frame(t(apply(ghg_mitigation[, -1], 1, cumsum)))
  ghg_cumsum <- ghg_cumsum_annual[scenario_lifetime]
  ghg_dr <- data.frame(matrix(rep(ghg_cumsum[,1], dr_count), ncol = dr_count))
  
  lcca_private_dr <- (npv_private_dr * -1)/ghg_dr
  lcca_econ_dr <- (npv_econ_dr * -1)/ghg_dr
  lcca_social_dr <- (npv_social_dr * -1)/ghg_dr
  
  assign("lcca_private_dr", lcca_private_dr, envir = .GlobalEnv)
  assign("lcca_econ_dr", lcca_econ_dr, envir = .GlobalEnv)
  assign("lcca_social_dr", lcca_social_dr, envir = .GlobalEnv)
}

### Calculate GHG mitigation median, ub, and lb 

ghg_mcs_stats <- function(lower_bound = 0.25, upper_bound = .75){
  ghg_stats <- data.frame(
    year = character(),
    ghg_lb = numeric(),
    ghg_median = numeric(),
    ghg_ub = numeric(),
    stringsAsFactors = FALSE)
  
  ghg_cumsum <- as.data.frame(t(apply(ghg_mitigation[, -1], 1, cumsum)))
  loop_count = ncol(ghg_cumsum)
  
  for (i in 1:loop_count) {
    # Extract the values for each year
    g_values <- ghg_cumsum[,i]
    
    # Calculate statistics
    g_median <- median(g_values)        
    g_lower_bound <- quantile(g_values, probs = lower_bound)
    g_upper_bound <- quantile(g_values, probs = upper_bound)
    
    # Append results to the results dataframe
    ghg_stats <- rbind(
      ghg_stats,
      data.frame(
        year = colnames(ghg_cumsum)[i],
        ghg_lb = g_lower_bound,
        ghg_median = g_median,
        ghg_ub = g_upper_bound))
  }
    
    rownames(ghg_stats) <- NULL
    assign("ghg_stats", ghg_stats, envir = .GlobalEnv)
}

### Calculate Monte Carlo Simulation Statistics (lower bound, median, and upper bound)

cba_mcs_stats <- function(lower_bound = 0.25, upper_bound = .75){
  # NPV MCS stats 
  npv_stats <- data.frame(
    dr = character(),
    prv_lb = numeric(),
    prv_median = numeric(),
    prv_ub = numeric(),
    econ_lb = numeric(),
    econ_median = numeric(),
    econ_ub = numeric(),
    soc_ub = numeric(),
    soc_median = numeric(),
    soc_lb = numeric(),
    stringsAsFactors = FALSE
  )
  loop_count = ncol(npv_econ_dr)
  
  for (i in 1:loop_count) {
    # Extract the values for each discount rate
    p_values <- npv_private_dr[,i]
    e_values <- npv_econ_dr[,i]
    s_values <- npv_social_dr[,i]
    
    # Calculate statistics
    p_median <- median(p_values)        
    p_lower_bound <- quantile(p_values, probs = lower_bound)
    p_upper_bound <- quantile(p_values, probs = upper_bound)
    
    e_median <- median(e_values)        
    e_lower_bound <- quantile(e_values, probs = lower_bound)
    e_upper_bound <- quantile(e_values, probs = upper_bound)
    
    s_median <- median(s_values)        
    s_lower_bound <- quantile(s_values, probs = lower_bound)
    s_upper_bound <- quantile(s_values, probs = upper_bound)
    
    # Append results to the results dataframe
    npv_stats <- rbind(
      npv_stats,
      data.frame(
        dr = colnames(npv_econ_dr)[i],
        prv_lb = p_lower_bound,
        prv_median = p_median,
        prv_ub = p_upper_bound,
        econ_lb = e_lower_bound,
        econ_median = e_median,
        econ_ub = e_upper_bound,
        soc_lb = s_lower_bound,
        soc_median = s_median,
        soc_ub = s_upper_bound
      )
    )
  }
  rownames(npv_stats) <- NULL
  assign("npv_stats", npv_stats, envir = .GlobalEnv)
  
  # SROI MCS stats 
  sroi_stats <- data.frame(
    dr = character(),
    prv_lb = numeric(),
    prv_median = numeric(),
    prv_ub = numeric(),
    econ_lb = numeric(),
    econ_median = numeric(),
    econ_ub = numeric(),
    soc_ub = numeric(),
    soc_median = numeric(),
    soc_lb = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:loop_count) {
    # Extract the values for each discount rate
    p_values <- sroi_private_dr[,i]
    e_values <- sroi_econ_dr[,i]
    s_values <- sroi_social_dr[,i]
    
    # Calculate statistics
    p_median <- median(p_values)        
    p_lower_bound <- quantile(p_values, probs = lower_bound)
    p_upper_bound <- quantile(p_values, probs = upper_bound)
    
    e_median <- median(e_values)        
    e_lower_bound <- quantile(e_values, probs = lower_bound)
    e_upper_bound <- quantile(e_values, probs = upper_bound)
    
    s_median <- median(s_values)        
    s_lower_bound <- quantile(s_values, probs = lower_bound)
    s_upper_bound <- quantile(s_values, probs = upper_bound)
    
    # Append results to the results dataframe
    sroi_stats <- rbind(
      sroi_stats,
      data.frame(
        dr = colnames(npv_econ_dr)[i],
        prv_lb = p_lower_bound,
        prv_median = p_median,
        prv_ub = p_upper_bound,
        econ_lb = e_lower_bound,
        econ_median = e_median,
        econ_ub = e_upper_bound,
        soc_lb = s_lower_bound,
        soc_median = s_median,
        soc_ub = s_upper_bound
      )
    )
  }
  rownames(sroi_stats) <- NULL
  assign("sroi_stats", sroi_stats, envir = .GlobalEnv)
  
  # LCCA MCS stats 
  lcca_stats <- data.frame(
    dr = character(),
    prv_lb = numeric(),
    prv_median = numeric(),
    prv_ub = numeric(),
    econ_lb = numeric(),
    econ_median = numeric(),
    econ_ub = numeric(),
    soc_ub = numeric(),
    soc_median = numeric(),
    soc_lb = numeric(),
    stringsAsFactors = FALSE
  )
 
  for (i in 1:loop_count) {
    # Extract the values for each discount rate
    p_values <- lcca_private_dr[,i]
    e_values <- lcca_econ_dr[,i]
    s_values <- lcca_social_dr[,i]
    
    # Calculate statistics
    p_median <- median(p_values)        
    p_lower_bound <- quantile(p_values, probs = lower_bound)
    p_upper_bound <- quantile(p_values, probs = upper_bound)
    
    e_median <- median(e_values)        
    e_lower_bound <- quantile(e_values, probs = lower_bound)
    e_upper_bound <- quantile(e_values, probs = upper_bound)
    
    s_median <- median(s_values)        
    s_lower_bound <- quantile(s_values, probs = lower_bound)
    s_upper_bound <- quantile(s_values, probs = upper_bound)
    
    # Append results to the results dataframe
    lcca_stats <- rbind(
      lcca_stats,
      data.frame(
        dr = colnames(npv_econ_dr)[i],
        prv_lb = p_lower_bound,
        prv_median = p_median,
        prv_ub = p_upper_bound,
        econ_lb = e_lower_bound,
        econ_median = e_median,
        econ_ub = e_upper_bound,
        soc_lb = s_lower_bound,
        soc_median = s_median,
        soc_ub = s_upper_bound
      )
    )
  }
  rownames(lcca_stats) <- NULL
  assign("lcca_stats", lcca_stats, envir = .GlobalEnv)
}


