### Normal Distribution Function

normalPDF <- function(output_name, mean, sd) {
  # Initialize an empty dataframe
  df <- data.frame(matrix(ncol = year_count, nrow = run_count))
  
  # Assign column names
  colnames(df) <- as.character(scenario_start:scenario_end)
  
  # Loop through each row
  for (i in 1:run_count) {
    # Generate a single random value from normal distribution
    value <- rnorm(1, mean, sd)
    
    # Fill the entire row with the same value
    df[i, ] <- rep(value, year_count)
  }
  
  # Assign the generated dataframe to the specified name in the global environment
  assign(output_name, df, envir = .GlobalEnv)
}

### CO2e Emission Factor Library Function

EFLco2e <- function(GWP){
  GWP_full <- read_excel("openEFL.xlsx", sheet = "GWPs")
  EFL <- read_excel("openEFL.xlsx", sheet = "EFL")
  
  # Create the table of global warming potentials
  gwp_key <- c("ghg",GWP)
  GWPs <- GWP_full[, gwp_key, with = FALSE]
  colnames(GWPs)[2] <- "gwp"
  co2gwp <- GWPs$gwp[GWPs$ghg == "co2"]
  ch4gwp <- GWPs$gwp[GWPs$ghg == "ch4"]
  n2ogwp <- GWPs$gwp[GWPs$ghg == "n2o"]
  
  # Consolidate the emission factor library into CO2e values
  EFL1 <- merge.data.table(EFL, GWPs, sort = FALSE, all.x = TRUE)
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
  setnames(EFL_CO2e, "ef_activeyear", "year")
  EFL_CO2e[, year := as.numeric(year)]
  EFL_CO2e[is.na(EFL_CO2e)] <- ""
  setcolorder(EFL_CO2e, c("ef_source",
                          "ef_publishdate",
                          "year",
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

### GHG Conversion function using a static emission factor

staticEF <- function(factor_name,
                     ef_activeyear, 
                     service_type, 
                     unit, 
                     emission_category, 
                     emission_scope, 
                     service_subcategory1 = "", 
                     service_subcategory2 = "", 
                     country = "global", 
                     subregion = "", ) {
  
  
}
