
####### constants #######
years <- c(1980, 1985, 1990, seq(1995, 2020))
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

# API call to get all unit attribute data given years
get_unit_att_data <- function(years){
  url <- paste0(API_url_base,"/facilities-mgmt/facilities/attributes?api_key=",API_KEY)
  full_data <- data.frame()
  for(year in years){
    res = GET(url, query = list(year=year))
    data <- fromJSON(rawToChar(res$content))
    full_data <- rbind(full_data,data)
  }
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

# calls of functions for global variables
year_for_ploting <- years[length(years)]
data <- get_unit_att_data(year_for_ploting)
fac_lat_long <- get_facilities_lat_long(data)
fac_data <- get_facility_att_data(data)

