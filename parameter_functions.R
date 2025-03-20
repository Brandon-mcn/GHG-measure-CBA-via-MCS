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

cont_uniform1 <- function(param_name, minvalue, maxvalue, units){
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
    mutate(unit = units) %>%  
    select(unit, everything())
  assign(param_name, mcs_out, envir = .GlobalEnv)
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

### Calc reference data 

calc_ref <- function(ref_name, boundary_df, ref_pdf, ref_params, units){
  
  # Initiate empty dataframe 
  sim_out <- matrix(0, nrow = run_count, ncol = scenario_years)
  
  # Simulate activity data
  for (i in 1:run_count) {
      ref_sim <- do.call(ref_pdf, c(list(1), ref_params))
      param_sim <- rep(ref_sim, times = scenario_years)
      sim_out[i,] <- param_sim
    }
  
  # Calculate annual reference data 
  ref_out <- sim_out * boundary_df[, -1]
  
  # Prep and create output
  ref_out <- as.data.frame(ref_out)
  colnames(ref_out) <- output_headers
  ref_out <- ref_out %>%
    mutate(unit = units) %>%  
    select(unit, everything())
  assign(ref_name, ref_out, envir = .GlobalEnv)
}

### Calc intv data

calc_intv <- function(intv_name, ref_data, intv_factor, units){
  savings <- 1 - intv_factor[, -1]
  intv_out <- ref_data[, -1] * savings
  intv_out <- as.data.frame(intv_out)
  colnames(intv_out) <- output_headers
  intv_out <- intv_out %>%
    mutate(unit = units) %>%  
    select(unit, everything())  
  assign(intv_name, intv_out, envir = .GlobalEnv)
}

### Modify dataframes

df_mod <- function(param_name, df1, df2, operation, units) {
  if (!all(dim(df1) == dim(df2))) {
    stop("Both data frames must have the same dimensions.")
  }
  
  valid_operations <- c("+", "-", "*", "/")
  if (!(operation %in% valid_operations)) {
    stop("Invalid operation. Choose from '+', '-', '*', or '/'.")
  }
  # Preform operation
  result <- as.data.frame(mapply(function(x, y) eval(parse(text = paste0("x", operation, "y"))), df1[,-1], df2[,-1]))
  
  # Prep and create output
  colnames(result) <- output_headers
  result <- result %>%
    mutate(unit = units) %>%  
    select(unit, everything())  
  assign(param_name, result, envir = .GlobalEnv)
}

df_sum <- function(param_name, ...) {
  dfs <- list(...)
  
  # Check that all data frames have the same dimensions
  dims <- lapply(dfs, dim)
  if (!all(sapply(dims, function(x) identical(x, dims[[1]])))) {
    stop("All data frames must have the same dimensions.")
  }
  
  # Extract the first column (assuming it's the units column)
  units_col <- dfs[[1]][, 1, drop = FALSE]  # Keep it as a data frame
  
  # Extract the numeric columns
  numeric_dfs <- lapply(dfs, function(df) df[, -1, drop = FALSE])
  
  # Sum element-wise across all numeric data frames
  summed_numeric <- Reduce(`+`, numeric_dfs)
  
  # Combine the units column with the summed numeric columns
  result <- cbind(units_col, summed_numeric)
  
  # Create output
  assign(param_name, result, envir = .GlobalEnv)
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

### Calculate MCS stats based on input, annual output

mcs_stats_ann <- function(param_name, df, lower_bound_out = .05, lower_bound_in = .25, upper_bound_in = .75, upper_bound_out = .95){
  stats_out <- data.frame(
    year = numeric(),
    lb1 = numeric(),
    lb2 = numeric(),
    median = numeric(),
    ub2 = numeric(),
    ub1 = numeric(),
    stringsAsFactors = FALSE)
  
  df1 <- df[, -1] # Remove leading unit column
  loop_count = ncol(df1)
  
  for (i in 1:loop_count) {
    # Extract the values for each year
    l_values <- df1[,i]
    
    # Calculate statistics
    l_median <- median(l_values)        
    l_lower_bound1 <- quantile(l_values, probs = lower_bound_out)
    l_lower_bound2 <- quantile(l_values, probs = lower_bound_in)
    l_upper_bound2 <- quantile(l_values, probs = upper_bound_in)
    l_upper_bound1 <- quantile(l_values, probs = upper_bound_out)
    
    # Append results to the results dataframe
    stats_out <- rbind(
      stats_out,
      data.frame(
        year = colnames(df1)[i],
        lb1 = l_lower_bound1,
        lb2 = l_lower_bound2,
        median = l_median,
        ub2 = l_upper_bound2,
        ub1 = l_upper_bound1))
  }
  
  rownames(stats_out) <- NULL
  assign(param_name, stats_out, envir = .GlobalEnv)
}


### Calculate MCS stats based on input, cumulative output

mcs_stats_cumul <- function(param_name, df, lower_bound_out = .05, lower_bound_in = .25, upper_bound_in = .75, upper_bound_out = .95){
  stats_out <- data.frame(
    year = numeric(),
    lb1 = numeric(),
    lb2 = numeric(),
    median = numeric(),
    ub2 = numeric(),
    ub1 = numeric(),
    stringsAsFactors = FALSE)
  
  cumsum <- as.data.frame(t(apply(df[, -1], 1, cumsum)))
  loop_count = ncol(cumsum)
  
  for (i in 1:loop_count) {
    # Extract the values for each year
    l_values <- cumsum[,i]
    
    # Calculate statistics
    l_median <- median(l_values)        
    l_lower_bound1 <- quantile(l_values, probs = lower_bound_out)
    l_lower_bound2 <- quantile(l_values, probs = lower_bound_in)
    l_upper_bound2 <- quantile(l_values, probs = upper_bound_in)
    l_upper_bound1 <- quantile(l_values, probs = upper_bound_out)
    
    # Append results to the results dataframe
    stats_out <- rbind(
      stats_out,
      data.frame(
        year = colnames(cumsum)[i],
        lb1 = l_lower_bound1,
        lb2 = l_lower_bound2,
        median = l_median,
        ub2 = l_upper_bound2,
        ub1 = l_upper_bound1))
  }
  
  rownames(stats_out) <- NULL
  assign(param_name, stats_out, envir = .GlobalEnv)
}

### Apply discount rate to MCS dataframe

quick_discount <- function(param_name, dr_name, df, dr, scenario_lifetime = scenario_years){
  drvalues <- numeric(scenario_years) # an empty vector 'drvalues' is created to house the loop outputs
  drvalues[1] <- 1          
  for (n in 2:scenario_years) {
    drvalues[n] <- (1+(dr/100))^(n-1) # drvalues is populated with the discount rate conversion factor for each year
  }
  drvalues_df <- data.frame(t(drvalues)) # make a dataframe out of the discount rate loop output
  discount_df <- drvalues_df[rep(1, run_count),] # the discount rate vector is replicated into rows equal to the MCS run count
  npv <- df[, -1]/discount_df # the total annual cash flow is converted into NPV
  colnames(npv) <- output_headers # make the column headers years
  output <- cbind(df[,1], npv)
  colnames(output)[1] <- "unit"
  assign(param_name, output, envir = .GlobalEnv)
  assign("scc_dr_out", discount_df, envir = .GlobalEnv)
}


### Calculate CBA indicators (NPV, SROI, LCCA) at different discount rates

cba_discountrange <- function(dr_min, dr_max, annual_coben, scenario_lifetime = scenario_years){
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
    
    annual_npv_private <- cashflow_private[, -1]/discount_df # the total annual cash flow is converted into NPV
    annual_npv_econ <- cashflow_econ[, -1]/discount_df
    annual_npv_social <- annual_npv_econ + annual_coben[, -1] # cobenefit value is adjusted at different discount rate. Right now, dr = 0 is used
    
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
    
    annual_beni_private <- cashflow_opex[, -1]/discount_df # opex is treated as the flow of economic benefits in this case
    annual_beni_econ <- cashflow_opex[, -1]/discount_df
    annual_beni_social <- annual_beni_econ + annual_coben[, -1] # cobenefit value is adjusted at different discount rate. Right now, dr = 0 is used
    
    annual_capex_private <- (cashflow_investment[, -1]*-1)/discount_df # capex is converted to a positive number and adjusted for the discount rate
    annual_capex_econ <- (cashflow_investment[, -1]*-1)/discount_df 
    annual_capex_social <- (cashflow_investment[, -1]*-1)/discount_df 
    
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

### Calculate Monte Carlo Simulation Statistics for CBA (lower bound, median, and upper bound)

mcs_cba_stats <- function(lower_bound_out = .05, lower_bound_in = .25, upper_bound_in = .75, upper_bound_out = .95){
  # NPV MCS stats 
  npv_stats <- data.frame(
    dr = character(),
    prv_lb1 = numeric(),
    prv_lb2 = numeric(),
    prv_median = numeric(),
    prv_ub2 = numeric(),
    prv_ub1 = numeric(),
    econ_lb1 = numeric(),
    econ_lb2 = numeric(),
    econ_median = numeric(),
    econ_ub2 = numeric(),
    econ_ub1 = numeric(),
    soc_lb1 = numeric(),
    soc_lb2 = numeric(),
    soc_median = numeric(),
    soc_ub2 = numeric(),
    soc_ub1 = numeric(),
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
    p_lb1 <- quantile(p_values, probs = lower_bound_out)
    p_lb2 <- quantile(p_values, probs = lower_bound_in)
    p_ub1 <- quantile(p_values, probs = upper_bound_out)
    p_ub2 <- quantile(p_values, probs = upper_bound_in)
    
    e_median <- median(e_values)        
    e_lb1 <- quantile(e_values, probs = lower_bound_out)
    e_lb2 <- quantile(e_values, probs = lower_bound_in)
    e_ub1 <- quantile(e_values, probs = upper_bound_out)
    e_ub2 <- quantile(e_values, probs = upper_bound_in)
    
    s_median <- median(s_values)        
    s_lb1 <- quantile(s_values, probs = lower_bound_out)
    s_lb2 <- quantile(s_values, probs = lower_bound_in)
    s_ub1 <- quantile(s_values, probs = upper_bound_out)
    s_ub2 <- quantile(s_values, probs = upper_bound_in)
    
    # Append results to the results dataframe
    npv_stats <- rbind(
      npv_stats,
      data.frame(
        dr = colnames(npv_econ_dr)[i],
        prv_lb1 = p_lb1,
        prv_lb2 = p_lb2,
        prv_median = p_median,
        prv_ub2 = p_ub2,
        prv_ub1 = p_ub1,
        econ_lb1 = e_lb1,
        econ_lb2 = e_lb2,
        econ_median = e_median,
        econ_ub2 = e_ub2,
        econ_ub1 = e_ub1,
        soc_lb1 = s_lb1,
        soc_lb2 = s_lb2,
        soc_median = s_median,
        soc_ub2 = s_ub2,
        soc_ub1 = s_ub1
      )
    )
  }
  rownames(npv_stats) <- NULL
  assign("npv_stats", npv_stats, envir = .GlobalEnv)
  
  # SROI MCS stats 
  sroi_stats <- data.frame(
    dr = character(),
    prv_lb1 = numeric(),
    prv_lb2 = numeric(),
    prv_median = numeric(),
    prv_ub2 = numeric(),
    prv_ub1 = numeric(),
    econ_lb1 = numeric(),
    econ_lb2 = numeric(),
    econ_median = numeric(),
    econ_ub2 = numeric(),
    econ_ub1 = numeric(),
    soc_lb1 = numeric(),
    soc_lb2 = numeric(),
    soc_median = numeric(),
    soc_ub2 = numeric(),
    soc_ub1 = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:loop_count) {
    # Extract the values for each discount rate
    p_values <- sroi_private_dr[,i]
    e_values <- sroi_econ_dr[,i]
    s_values <- sroi_social_dr[,i]
    
    # Calculate statistics
    p_median <- median(p_values)        
    p_lb1 <- quantile(p_values, probs = lower_bound_out)
    p_lb2 <- quantile(p_values, probs = lower_bound_in)
    p_ub1 <- quantile(p_values, probs = upper_bound_out)
    p_ub2 <- quantile(p_values, probs = upper_bound_in)
    
    e_median <- median(e_values)        
    e_lb1 <- quantile(e_values, probs = lower_bound_out)
    e_lb2 <- quantile(e_values, probs = lower_bound_in)
    e_ub1 <- quantile(e_values, probs = upper_bound_out)
    e_ub2 <- quantile(e_values, probs = upper_bound_in)
    
    s_median <- median(s_values)        
    s_lb1 <- quantile(s_values, probs = lower_bound_out)
    s_lb2 <- quantile(s_values, probs = lower_bound_in)
    s_ub1 <- quantile(s_values, probs = upper_bound_out)
    s_ub2 <- quantile(s_values, probs = upper_bound_in)
    
    # Append results to the results dataframe
    sroi_stats <- rbind(
      sroi_stats,
      data.frame(
        dr = colnames(sroi_econ_dr)[i],
        prv_lb1 = p_lb1,
        prv_lb2 = p_lb2,
        prv_median = p_median,
        prv_ub2 = p_ub2,
        prv_ub1 = p_ub1,
        econ_lb1 = e_lb1,
        econ_lb2 = e_lb2,
        econ_median = e_median,
        econ_ub2 = e_ub2,
        econ_ub1 = e_ub1,
        soc_lb1 = s_lb1,
        soc_lb2 = s_lb2,
        soc_median = s_median,
        soc_ub2 = s_ub2,
        soc_ub1 = s_ub1
      )
    )
  }
  rownames(sroi_stats) <- NULL
  assign("sroi_stats", sroi_stats, envir = .GlobalEnv)
  
  # LCCA MCS stats 
  lcca_stats <- data.frame(
    dr = character(),
    prv_lb1 = numeric(),
    prv_lb2 = numeric(),
    prv_median = numeric(),
    prv_ub2 = numeric(),
    prv_ub1 = numeric(),
    econ_lb1 = numeric(),
    econ_lb2 = numeric(),
    econ_median = numeric(),
    econ_ub2 = numeric(),
    econ_ub1 = numeric(),
    soc_lb1 = numeric(),
    soc_lb2 = numeric(),
    soc_median = numeric(),
    soc_ub2 = numeric(),
    soc_ub1 = numeric(),
    stringsAsFactors = FALSE
  )
 
  for (i in 1:loop_count) {
    # Extract the values for each discount rate
    p_values <- lcca_private_dr[,i]
    e_values <- lcca_econ_dr[,i]
    s_values <- lcca_social_dr[,i]
    
    # Calculate statistics
    p_median <- median(p_values)        
    p_lb1 <- quantile(p_values, probs = lower_bound_out)
    p_lb2 <- quantile(p_values, probs = lower_bound_in)
    p_ub1 <- quantile(p_values, probs = upper_bound_out)
    p_ub2 <- quantile(p_values, probs = upper_bound_in)
    
    e_median <- median(e_values)        
    e_lb1 <- quantile(e_values, probs = lower_bound_out)
    e_lb2 <- quantile(e_values, probs = lower_bound_in)
    e_ub1 <- quantile(e_values, probs = upper_bound_out)
    e_ub2 <- quantile(e_values, probs = upper_bound_in)
    
    s_median <- median(s_values)        
    s_lb1 <- quantile(s_values, probs = lower_bound_out)
    s_lb2 <- quantile(s_values, probs = lower_bound_in)
    s_ub1 <- quantile(s_values, probs = upper_bound_out)
    s_ub2 <- quantile(s_values, probs = upper_bound_in)
    
    # Append results to the results dataframe
    lcca_stats <- rbind(
      lcca_stats,
      data.frame(
        dr = colnames(lcca_econ_dr)[i],
        prv_lb1 = p_lb1,
        prv_lb2 = p_lb2,
        prv_median = p_median,
        prv_ub2 = p_ub2,
        prv_ub1 = p_ub1,
        econ_lb1 = e_lb1,
        econ_lb2 = e_lb2,
        econ_median = e_median,
        econ_ub2 = e_ub2,
        econ_ub1 = e_ub1,
        soc_lb1 = s_lb1,
        soc_lb2 = s_lb2,
        soc_median = s_median,
        soc_ub2 = s_ub2,
        soc_ub1 = s_ub1
      )
    )
  }
  rownames(lcca_stats) <- NULL
  assign("lcca_stats", lcca_stats, envir = .GlobalEnv)
}

### CBA Chart - economic value

cba_econ_plot <- function(plot_name, df, c, ylab, xlab, panel){
  plot <- ggplot(df, aes(x = dr, y = econ_median)) +
    geom_line(aes(group = 1), color = c, size = .5) +   
    geom_ribbon(aes(ymin = econ_lb1, ymax = econ_ub1, group = 1), fill = c, alpha = 0.25) + 
    geom_ribbon(aes(ymin = econ_lb2, ymax = econ_ub2, group = 1), fill = c, alpha = 0.4) +  
    labs(title = panel,
         x = xlab,
         y = ylab) +
    scale_y_continuous(labels = comma, breaks = scales::breaks_extended(n = 8)) +
    scale_x_continuous(
      breaks = seq(dr_lb, dr_ub, by = 1),
      labels = seq(dr_lb, dr_ub, by = 1)) +
    theme_minimal() +
    theme(text = element_text(size = 8, color = "black"),
          plot.title = element_text(face = "bold", size = 12, color = "black"),
          axis.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 8, color = "black"),
          axis.title.x = element_text(margin = margin(t = 5)),  
          axis.title.y = element_text(margin = margin(r = 5)),
          legend.text = element_text(size = 8, color = "black"),
          axis.ticks = element_line(color = "black", size = .5),
          axis.line = element_line(color = "black", size = .5),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          aspect.ratio = 1)
  
  assign(plot_name, plot, envir = .GlobalEnv)
}

### CBA Chart - social value

cba_social_plot <- function(plot_name, df, c, ylab, xlab, panel){
  plot <- ggplot(df, aes(x = dr, y = soc_median)) +
    geom_line(aes(group = 1), color = c, size = .5) +   
    geom_ribbon(aes(ymin = soc_lb1, ymax = soc_ub1, group = 1), fill = c, alpha = 0.25) + 
    geom_ribbon(aes(ymin = soc_lb2, ymax = soc_ub2, group = 1), fill = c, alpha = 0.4) +  
    labs(title = panel,
         x = xlab,
         y = ylab) +
    scale_y_continuous(labels = comma, breaks = scales::breaks_extended(n = 8)) +
    scale_x_continuous(
      breaks = seq(dr_lb, dr_ub, by = 1),
      labels = seq(dr_lb, dr_ub, by = 1)) +
    theme_minimal() +
    theme(text = element_text(size = 8, color = "black"),
          plot.title = element_text(face = "bold", size = 12, color = "black"),
          axis.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 8, color = "black"),
          axis.title.x = element_text(margin = margin(t = 5)),  
          axis.title.y = element_text(margin = margin(r = 5)),
          legend.text = element_text(size = 8, color = "black"),
          axis.ticks = element_line(color = "black", size = .5),
          axis.line = element_line(color = "black", size = .5),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          aspect.ratio = 1)
  
  assign(plot_name, plot, envir = .GlobalEnv)
}

