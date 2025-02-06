### CO2e Emission Factor Library Function

EFL.co2e <- function(GWP){
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

normal.PDF <- function(param_name, mean, sd) {
  mcs_out <- data.frame(matrix(ncol = year_count, nrow = run_count)) # Initialize an empty dataframe
  colnames(mcs_out) <- output_headers # Assign column names
  for (i in 1:run_count) {
    value <- rnorm(1, mean, sd) # Loop through each row, generate a single random value from normal distribution
    mcs_out[i, ] <- rep(value, year_count) # Fill the entire row with the same value
  }
  assign(param_name, mcs_out, envir = .GlobalEnv) # Assign the generated dataframe to the specified name in the global environment
}

### Random value parameter

random.value <- function(param_name, minvalue, maxvalue, lifetime, value2 = 0){
  mcs_out <- data.frame(matrix(ncol = year_count, nrow = run_count)) # Initialize an empty dataframe
  for (i in 1:run_count) {
    random_value <- sample(minvalue:maxvalue, size = 1)
    param_sim <- c(rep(random_value, times = lifetime),rep(value2, times = year_count - lifetime))
    mcs_out[i, ] <- param_sim
  }
  colnames(mcs_out) <- output_headers
  assign(param_name, mcs_out, envir = .GlobalEnv)
}

### Static value parameter

static.value <- function(param_name, value){
  mcs_out <- data.frame(matrix(value, nrow = run_count, ncol = year_count))
  colnames(mcs_out) <- output_headers
  assign(param_name, mcs_out, envir = .GlobalEnv)
}

### Scenario selection formula

scenario.MCS <- function(param_name){
  df <- read_excel("p1_scenarios.xlsx", sheet = param_name)
  colfilter <- c("scenario_prob", output_headers)
  df_scenarios <- df[, colfilter]
  mcs_out <- data.frame(matrix(ncol = year_count + 1, nrow = run_count))
  for (i in 1:run_count) {
    scenario_selection <- df_scenarios[sample(nrow(df_scenarios), size = 1, prob = df_scenarios$scenario_prob), ]
    mcs_out[i, ] <- scenario_selection
  }
  colnames(mcs_out) <- colfilter
  mcs_out <- mcs_out %>% select(-scenario_prob)
  assign(param_name, mcs_out, envir = .GlobalEnv)
}

### Activity data calculation - does both ref and intv scenarios for a given parameter

calculate.AD <- function(ref_name, intv_name, boundary_data, ad_factor, intv_factor, unit){
  ref <- boundary_data * ad_factor
  ref$unit <- unit
  ref <- ref[, c("unit", setdiff(names(ref), "unit"))]
  assign(ref_name, ref, envir = .GlobalEnv)
  intv <- boundary_data * ad_factor * (1 - intv_factor)
  intv$unit <- unit
  intv <- intv[, c("unit", setdiff(names(intv), "unit"))]
  assign(intv_name, intv, envir = .GlobalEnv)
}

### GHG Conversion function using a static emission factor

ghg_conversion1 <- function(ref_name,
                            intv_name,
                            ref_AD,
                            intv_AD,
                            fltr.ef_activeyear,
                            fltr.service_type, 
                            fltr.emission_category, 
                            fltr.emission_scope, 
                            fltr.service_subcategory1 = "", 
                            fltr.service_subcategory2 = "", 
                            fltr.country = "global", 
                            fltr.subregion = "") {
  fltr.unit <- ref_AD[1,1]
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
  EF_df <- data.frame(matrix(EF_value, nrow = run_count, ncol = year_count))
  colnames(EF_df) <- output_headers
  ghg1 <- EF_df * ref_AD[, -1]
  ghg1$unit <- "kgco2e"
  ghg1 <- ghg1[, c("unit", setdiff(names(ghg1), "unit"))]
  assign(ref_name, ghg1, envir = .GlobalEnv)
  ghg2 <- EF_df * intv_AD[, -1]
  ghg2$unit <- "kgco2e"
  ghg2 <- ghg2[, c("unit", setdiff(names(ghg2), "unit"))]
  assign(intv_name, ghg2, envir = .GlobalEnv)
}

### GHG Conversion function with EFs from MCS 

ghg_conversion2 <- function(ref_name, intv_name, EFsim_out, ref_AD, intv_AD){
  ghg1 <- EFsim_out * ref_AD[, -1]
  ghg1$unit <- "kgco2e"
  ghg1 <- ghg1[, c("unit", setdiff(names(ghg1), "unit"))]
  assign(ref_name, ghg1, envir = .GlobalEnv)
  ghg2 <- EFsim_out * intv_AD[, -1]
  ghg2$unit <- "kgco2e"
  ghg2 <- ghg2[, c("unit", setdiff(names(ghg2), "unit"))]
  assign(intv_name, ghg2, envir = .GlobalEnv)
}

### Calculate CBA indicators (NPV, LCCA) at different discount rates

cba_discountrange <- function(dr_min, dr_max, scenario_lifetime = year_count){
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
    drvalues <- numeric(year_count) # an empty vector 'drvalues' is created to house the loop outputs
    drvalues[1] <- 1        
    for (n in 2:year_count) {
      drvalues[n] <- (1+(i/1000))^(n-1) # drvalues is populated with the discount rate conversion factor for each year
    }
    drvalues_df <- data.frame(t(drvalues)) # make a dataframe out of the discount rate loop output
    colnames(drvalues_df) <- output_headers # make the column headers years
    discount_df <- drvalues_df[rep(1, run_count),] # the discount rate vector is replicated into rows equal to the MCS run count
    
    annual_npv_private <- cashflow_private/discount_df # the total annual cash flow is converted into NPV
    annual_npv_econ <- cashflow_econ/discount_df
    annual_npv_social <- annual_npv_econ + total_coben_value # cobenefit value is adjusted at different discount rate. Right now, dr = 0 is used
    
    npv_private_cumsum <- as.data.frame(t(apply(annual_npv_private, 1, cumsum))) # net present value is summed cumulatively for each year
    npv_econ_cumsum <- as.data.frame(t(apply(annual_npv_econ, 1, cumsum)))
    npv_social_cumsum <- as.data.frame(t(apply(annual_npv_social, 1, cumsum)))
    
    npv_private <- npv_private_cumsum[scenario_lifetime] # the total NPV is selected based on the scenario lifetime
    npv_econ <- npv_econ_cumsum[scenario_lifetime]
    npv_social <- npv_social_cumsum[scenario_lifetime]
    
    col_rename <- i/10 # rename column with the discount rate
    colnames(npv_private) <- col_rename
    colnames(npv_econ) <- col_rename
    colnames(npv_social) <- col_rename
    
    npv_private_dr[,i+1] <- npv_private #bind the NPV values for each MCS row for the given discount rate
    npv_econ_dr[,i+1] <- npv_econ #bind the NPV values for each MCS row for the given discount rate
    npv_social_dr[,i+1] <- npv_social #bind the NPV values for each MCS row for the given discount rate
  }
  assign("npv_private_dr", npv_private_dr, envir = .GlobalEnv)
  assign("npv_econ_dr", npv_econ_dr, envir = .GlobalEnv)
  assign("npv_social_dr", npv_social_dr, envir = .GlobalEnv)
  
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

### Calculate Monte Carlo Simulation Statistics (lower bound, median, and upper bound)

mcs_stats <- function(lower_bound = 0.25, upper_bound = .75){
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
    sco_lb = numeric(),
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
        sco_ub = s_upper_bound
      )
    )
  }
  rownames(npv_stats) <- NULL
  assign("npv_stats", npv_stats, envir = .GlobalEnv)
  
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
    sco_lb = numeric(),
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
        sco_ub = s_upper_bound
      )
    )
  }
  rownames(lcca_stats) <- NULL
  assign("lcca_stats", lcca_stats, envir = .GlobalEnv)
}
