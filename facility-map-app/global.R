
####### constants #######
years <- c(seq(1995, 2020))
API_url_base <- Sys.getenv("API_url_base")
API_KEY <- Sys.getenv("API_KEY")
measure_units <- data.frame("emiss_type" = c("so2Mass","so2Rate","noxMass",
                                             "noxRate","co2Mass","co2Rate",
                                             "grossLoad", "steamLoad", "heatInput"),
                            "measure_unit" = c("(short tons)","(lbs/mmBtu)",
                                               "(short tons)","(lbs/mmBtu)",
                                               "(short tons)","(short tons/mmBtu)",
                                               "(MWh)","(1000 lb/hr)","(mmBtu)"))

program_names <- data.frame("prg_code" = c("ARP", "CSSO2G1", "CSSO2G2", "CSNOX",
                                           "MATS", "RGGI", "TXSO2", "CAIRSO2",
                                           "CSOSG1", "CSOSG2", "CSOSG3",
                                           "SIPNOX", "CSNOXOS", "CAIROS",
                                           "NBP", "OTC"),
                            "prg_name" = c("Acid Rain Program (ARP)", 
                                           "Cross-State Air Pollution SO2 Annual Group 1 Program (CSSO2G1)",
                                           "Cross-State Air Pollution SO2 Annual Group 2 Program (CSSO2G2)",
                                           "Cross-State Air Pollution NOx Annual Program (CSNOX)",
                                           "Mercury and Air Toxics Standards (MATS)",
                                           "Regional Greenhouse Gas Initiative (RGGI)",
                                           "Texas SO2 Trading Program (TXSO2)",
                                           "Clean Air Interstate Rule SO2 (CAIRSO2) (ended 2014)",
                                           "Cross-State Air Pollution Rule NOx Ozone Season Group 1 Program (CSOSG1)",
                                           "Cross-State Air Pollution Rule NOx Ozone Season Group 2 Program (CSOSG2)",
                                           "Cross-State Air Pollution Rule NOx Ozone Season Group 3 Program (CSOSG3)",
                                           "SIP NOx Program (SIPNOX)",
                                           "Cross-State Air Pollution NOx Ozone Season Program (CSNOXOS) (ended 2016)",
                                           "Clean Air Interstate Rule Ozone Season (CAIROS) (ended 2014)",
                                           "NOx Budget Program (NBP) (ended 2008)",
                                           "Ozone Transport Commission NOx Budget Program (OTC) (ended 2002)"
                            ))

####### functions #######
# annual emissions by facility ID data API call
get_annual_fac_emissions_data <- function(years, facilityId){
  url <- paste0(API_url_base,"/emissions-mgmt/apportioned/annual?api_key=",API_KEY)
  res = GET(url, query = list(facilityId=facilityId,
                              year=c(paste0(years, collapse = '|')),
                              page="1",
                              perPage="1000"))
  year_emiss_data <- fromJSON(rawToChar(res$content))
}

# emissions percent change by pollutant for a facility
get_emissions_percent_df <- function(years, facilityId){
  year_emiss_data <- get_annual_fac_emissions_data(years, facilityId)
  year_fac_emiss_data <- aggregate(cbind(so2Mass,co2Mass,noxMass) 
                                   ~ stateCode + year + 
                                     facilityName + facilityId, 
                                   data = year_emiss_data, 
                                   FUN = sum, na.rm = TRUE)
  first_3_years <- year_fac_emiss_data[1:3,]
  so2maxrow <- first_3_years[which.max(first_3_years$so2Mass),]
  noxmaxrow <- first_3_years[which.max(first_3_years$noxMass),]
  co2maxrow <- first_3_years[which.max(first_3_years$co2Mass),]
  current_emiss <- year_fac_emiss_data[which(year_fac_emiss_data$year == year_for_ploting),]
  
  so2_percent_diff <- ifelse(so2maxrow$so2Mass > current_emiss$so2Mass, 
         percentage_decrease(so2maxrow$so2Mass,current_emiss$so2Mass), 
         percentage_increase(so2maxrow$so2Mass,current_emiss$so2Mass))
}

percentage_decrease <- function(start_val,final_val){
  percent <- (start_val-final_val)/start_val*100
}
percentage_increase <- function(start_val,final_val){
  percent <- (final_val-start_val)/start_val*100
}

# API call to get all unit attribute data given years
get_unit_att_data <- function(year){
  url <- paste0(API_url_base,"/facilities-mgmt/facilities/attributes?api_key=",API_KEY)
  res = GET(url, query = list(year=year,
                              page="1",
                              perPage="20000"))
  full_data <- fromJSON(rawToChar(res$content))
  full_data
}

# API call to get allowance compliance info for a facility
get_allow_comp_data <- function(year, facilityId){
  url <- paste0(API_url_base,"/account-mgmt/allowance-compliance?api_key=",API_KEY)
  res = GET(url, query = list(facilityId=facilityId,
                              year=year,
                              page="1",
                              perPage="1000"))
  data <- fromJSON(rawToChar(res$content))
  
  data
}

# API call to get allowance holdings info for a facility
get_allow_holding_data <- function(facilityId){
  url <- paste0(API_url_base,"/account-mgmt/allowance-holdings?api_key=",API_KEY)
  res = GET(url, query = list(facilityId=facilityId,
                              page="1",
                              perPage="10000"))
  data <- fromJSON(rawToChar(res$content))
  holding_data <- aggregate(data$totalBlock,
                 by=list(programCodeInfo=data$programCodeInfo), FUN=sum)
  holding_data
}


# get only facilities lat and long 
get_facilities_lat_long <- function(data){
  fac_data <- data %>% select(facilityId, facilityName,
                              longitude, latitude)
  fac_data <- unique(fac_data)
}

# higher level facility data
get_facility_att_data <- function(data){
  fac_data <- data %>% select(stateCode, county, facilityId, 
                              facilityName, year, programCodeInfo,
                              primaryFuelInfo, secondaryFuelInfo,
                              longitude, latitude)
  fac_data <- unique(fac_data)
}

# get all programs for a given year (get non acronym)
get_programs <- function(data, year){
  prg_data <- data %>% select(year, programCodeInfo)
  prg_data <- prg_data[prg_data$year == year,]
  prg_list <- unique(prg_data$programCodeInfo)
  prg_list[sapply(prg_list, length) != 0] <- strsplit(unlist(prg_list), ", ")
  prg_df <- data.frame(na.omit(unique(unlist(prg_list))))
  colnames(prg_df) <- c('prg_code')
  prg_code_names <- merge(program_names, prg_df, by="prg_code")
}

# get all fuel types for a given year
get_fuel_types <- function(data, year){
  fuel_data <- data %>% select(year, primaryFuelInfo, secondaryFuelInfo)
  fuel_data <- fuel_data[fuel_data$year == year,]
  prim_fuel_list <- unique(fuel_data$primaryFuelInfo)
  prim_fuel_list[sapply(prim_fuel_list, length) != 0] <- c(strsplit(unlist(prim_fuel_list), ", "))
  prim_fuel_list <- na.omit(unique(unlist(prim_fuel_list)))
  #print(unique(as.list(prim_fuel_list)))
  sec_fuel_list <- unique(fuel_data$secondaryFuelInfo)
  sec_fuel_list[sapply(sec_fuel_list, length) != 0] <- strsplit(unlist(sec_fuel_list), ", ")
  sec_fuel_list <- na.omit(unique(unlist(sec_fuel_list)))
  fuel_list <- unique(unlist(c(prim_fuel_list, sec_fuel_list)))
  
}

# calls of functions for global variables
year_for_ploting <- years[length(years)]
data <- get_unit_att_data(year_for_ploting)
fuel_list <- get_fuel_types(data, year_for_ploting)
fac_lat_long <- get_facilities_lat_long(data)
fac_data <- get_facility_att_data(data)

