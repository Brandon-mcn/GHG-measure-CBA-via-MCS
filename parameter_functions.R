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
