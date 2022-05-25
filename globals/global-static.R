
# API info
apiUrlBase <- Sys.getenv("API_url_base")
apiKEY <- Sys.getenv("API_KEY")

# table to convert column name to appropriate lables for UI
mulitLabelConversion <- data.frame(columnName=c("programDescription", 
                                                "programCode", 
                                                "stateName", 
                                                "unitTypeGroupDescription", 
                                                "fuelGroupDescription", 
                                                "controlEquipParamDescription", 
                                                "year"), 
                                   label=c("Select up to 5 Regulatory Programs",
                                           "Select up to 5 Regulatory Programs",
                                           "Select up to 5 States (required)",
                                           "Select up to 5 Unit Types",
                                           "Select up to 5 Fuel Types",
                                           "Select up to 5 Control Technologies",
                                           "Select a Range of Years"))

singleLabelConversion <- data.frame(columnName=c("programDescription", 
                                                 "programCodeInfo", 
                                                 "programCode", 
                                                 "stateName", 
                                                 "facilityName",
                                                 "year",
                                                 "assuranceFlag"), 
                                    label=c("Select a Regulatory Program (required)",
                                            "Select a Regulatory Program (required)",
                                            "Select a Regulatory Program (required)",
                                            "Select a State (required)",
                                            "Select a Facility (required)",
                                            "Select a Year (required)",
                                            "Show Budgets with Assurance Levels? (required)"),
                                    placeholder=c("--select program--",
                                                  "--select program--",
                                                  "--select program--",
                                                  "--select state--",
                                                  "--select facility--",
                                                  "--select year--",
                                                  "--select y/n--")
                                    )

facilityMapLabelConversion <- data.frame(columnName=c("programCode", 
                                                      "programDescription",
                                                      "facilityName", 
                                                      "stateName", 
                                                      "countyName"), 
                                    label=c("Select a Regulatory Program",
                                            "Select a Regulatory Program",
                                            "Select a Facility",
                                            "Select a State",
                                            "Select a County"),
                                    placeholder=c("--select program--",
                                                  "--select program--",
                                                  "--select facility--",
                                                  "--select state--",
                                                  "--select county--"))

tableLabelConversion <- data.frame(columnName=c("programCode", 
                                                "programDescription", 
                                                "stateName", 
                                                "unitTypeGroupDescription", 
                                                "fuelGroupDescription", 
                                                "controlEquipParamDescription", 
                                                "year",
                                                "allocated",
                                                "totalAllowancesDeducted",
                                                "carriedOver",
                                                "so2Mass",
                                                "noxMass",
                                                "allocations",
                                                "variabilityLimit",
                                                "assuranceLevel",
                                                "allowEmissRatio",
                                                "excessAllow",
                                                "percentExcess"), 
                                   label=c("Program Code", 
                                           "Regulatory Program",
                                           "State Name",
                                           "Unit Types",
                                           "Fuel Types",
                                           "Control Technologies",
                                           "Year",
                                           "Allowances Allocated", 
                                           "Allowances Deducted", 
                                           "Allowances Banked",
                                           "SO2 Mass",
                                           "NOx Mass",
                                           "Allowances Allocated",
                                           "Variability Limit",
                                           "Assurance Level",
                                           "Allowance to Emissions Ratio",
                                           "Excess Allowances",
                                           "Percent Excess"))


####### CSAPR Budgets #######
state_budgets <- read.csv("./globals/csapr-state-assurance-levels.csv")


# drop these states in shapefiles
dropStates <- c("American Samoa", "American Samoa", 
                "Commonwealth of the Northern Mariana Islands",
                "Guam", "United States Virgin Islands")

####### County Search Data #######
get_county_search_data <- function(shapefilepath){
  
  USA <- st_read(dsn = shapefilepath)
  
  countyDF <- USA[,c("STATEFP", "COUNTYNS","NAME","geometry"),]
  
  names(countyDF) <- tolower(names(countyDF))
  names(countyDF)[names(countyDF)=="name"] <- c("countyName")
  
  county_sf <- st_as_sf(countyDF)
  
  FIPS <- read.csv("./globals/stateFIPS.csv")
  
  FIPS$FIPS <- formatC(FIPS$FIPS, width = 2, format = "d", flag = "0", big.mark = "-")
  
  countyState <- merge(county_sf, FIPS, by.x="statefp", by.y="FIPS",all.x=TRUE)
  
  countyState <- countyState[!(countyState$stateName %in% dropStates),]
  countyState
  
}

countyState <- get_county_search_data("./globals/cb_2018_us_county_5m.shp")

####### State Search Data #######
get_state_search_data <- function(shapefilepath){
  USA <- st_read(dsn = shapefilepath)
  
  stateDF <- USA[,c("NAME","geometry"),]
  
  names(stateDF) <- tolower(names(stateDF))
  names(stateDF)[names(stateDF)=="name"] <- c("stateName")
  
  state_sf <- st_as_sf(stateDF)
  
  state_sf <- state_sf[order(state_sf$stateName),]
  
  state_sf <- state_sf[!(state_sf$stateName %in% dropStates),]
  state_sf
}

state_sf <- get_state_search_data("./globals/cb_2018_us_state_5m.shp")


## global functions

get_annual_emiss_data <- function(emissionYears, programs=NULL, 
                                  unitType=NULL, unitFuelType=NULL, 
                                  states=NULL, facilities=NULL){
  
  url <- paste0(apiUrlBase,"/emissions-mgmt/apportioned/annual/stream?api_key=",apiKEY)
  query <- list(year=(paste0(emissionYears, collapse = '|')))
  
  if (!is.null(programs)){query <- append(query, list(programCodeInfo = (paste0(programs, collapse = '|'))))}
  if (!is.null(unitType)){query <- append(query, list(unitType = (paste0(unitType, collapse = '|'))))}
  if (!is.null(unitFuelType)){query <- append(query, list(unitFuelType = (paste0(fuelType, collapse = '|'))))}
  if (!is.null(states)){query <- append(query, list(stateCode = (paste0(states, collapse = '|'))))}
  if (!is.null(facilities)){query <- append(query, list(facilityId = (paste0(facilities, collapse = '|'))))}
  
  res = GET(url, query = query)
  
  if (length(res$content) > 2){
    annualEmissData <- fromJSON(rawToChar(res$content))
  }
  else(return(NULL))
  annualEmissData
}

get_ozone_emiss_data <- function(emissionYears, programs=NULL, 
                                  unitType=NULL, unitFuelType=NULL, 
                                  states=NULL, facilities=NULL){
  
  url <- paste0(apiUrlBase,"/emissions-mgmt/apportioned/ozone/stream?api_key=",apiKEY)
  query <- list(year=(paste0(emissionYears, collapse = '|')))
  
  if (!is.null(programs)){query <- append(query, list(programCodeInfo = (paste0(programs, collapse = '|'))))}
  if (!is.null(unitType)){query <- append(query, list(unitType = (paste0(unitType, collapse = '|'))))}
  if (!is.null(unitFuelType)){query <- append(query, list(unitFuelType = (paste0(fuelType, collapse = '|'))))}
  if (!is.null(states)){query <- append(query, list(stateCode = (paste0(states, collapse = '|'))))}
  if (!is.null(facilities)){query <- append(query, list(facilityId = (paste0(facilities, collapse = '|'))))}
  
  res = GET(url, query = query)
  
  if (length(res$content) > 2){
    annualEmissData <- fromJSON(rawToChar(res$content))
  }
  else(return(NULL))
  annualEmissData
}

get_facility_data <- function(years){
  
  url <- paste0(apiUrlBase,"/facilities-mgmt/facilities/attributes/stream?api_key=",apiKEY)
  query <- list(year=(paste0(years, collapse = '|')))
  
  res = GET(url, query = query)
  
  if (length(res$content) > 2){
    yearFacilityData <- fromJSON(rawToChar(res$content))
  }
  else{retun(NULL)}
  yearFacilityData
}

#facilityData <- get_facility_data(1995,get_latest_valid_vear(facilitiesUrl))

# API calls to get compliance data
# format queryList - list(stateCode = paste0(c("AL"), collapse = '|'),programCodeInfo = paste0(c("ARP"), collapse = '|'))
# where states is a c() vector of elements
get_allow_comp_data <- function(complianceYears, programs=NULL, 
                                states=NULL, facilities=NULL){
  
  url <- paste0(apiUrlBase,"/account-mgmt/allowance-compliance/stream?api_key=",apiKEY)
  query <- list(year=(paste0(complianceYears, collapse = '|')))
  
  if (!is.null(programs)){query <- append(query, list(programCodeInfo = (paste0(programs, collapse = '|'))))}
  if (!is.null(states)){query <- append(query, list(stateCode = (paste0(states, collapse = '|'))))}
  if (!is.null(facilities)){query <- append(query, list(facilityId = (paste0(facilities, collapse = '|'))))}
  
  res = GET(url, query = query)
  
  if (length(res$content) > 2){
    yearComplianceData <- fromJSON(rawToChar(res$content))
  }
  else{return(NULL)}
  yearComplianceData
}

# API call to get allowance holdings info for a facility
get_allow_holding_data <- function(facilityId){
  
  url <- paste0(apiUrlBase,"/account-mgmt/allowance-holdings/stream?api_key=",apiKEY)
  query <- list(facilityId=facilityId)
  
  res = GET(url, query = query)
  
  if (length(res$content) > 2){
    holdingData <- fromJSON(rawToChar(res$content))
  }
  else{return(NULL)}
  aggregate(totalBlock ~ programCodeInfo, 
            data = holdingData[c("programCodeInfo","totalBlock")], sum)
}

# API call to get transaction data by a facility
get_transaction_data <- function(facilityId, transactionBeginDate,
                                 transactionEndDate){
  
  url <- paste0(apiUrlBase,"/account-mgmt/allowance-transactions/stream?api_key=",apiKEY)
  query <- list(transactionBeginDate=transactionBeginDate,
                transactionEndDate=transactionEndDate,
                transactionType="Private Transfer",
                facilityId=facilityId)
  
  res = GET(url, query = query)
  
  if (length(res$content) > 2){
    transactionData <- fromJSON(rawToChar(res$content))
  }
  else{return(NULL)}
  transactionData
}

make_transaction_table <- function(transactionData, facilityId){
  buyDataNoNA <- transactionData[!is.na(transactionData$buyFacilityId),]
  sellDataNoNA <- transactionData[!is.na(transactionData$sellFacilityId),]
  bought <- buyDataNoNA[buyDataNoNA$buyFacilityId == facilityId,]
  sold <- sellDataNoNA[sellDataNoNA$sellFacilityId == facilityId,]
  
  if (nrow(bought)==0){
    transactionBuyData <- data.frame(programCodeInfo=unique(transactionData$programCodeInfo), 
                                     `Total Bought` =0, `Number of Buys`=0)
  }
  else{
    programBoughtTotal <- aggregate(transactionTotal ~ programCodeInfo, data = bought, sum)
    colnames(programBoughtTotal) <- c("programCodeInfo", "Total Bought")
    numBuys <- bought %>% count(programCodeInfo)
    colnames(numBuys) <- c("programCodeInfo", "Number of Buys")
    transactionBuyData <- merge(programBoughtTotal, numBuys, by="programCodeInfo")
  }
  
  if (nrow(sold)==0){
    transactionSellData <- data.frame(programCodeInfo=unique(transactionData$programCodeInfo), 
                                      `Total Sold` = 0, `Number of Sells`=0)
  }
  else{
    programSoldTotal <- aggregate(transactionTotal ~ programCodeInfo, data = sold, sum)
    colnames(programSoldTotal) <- c("programCodeInfo", "Total Sold")
    numSold <- sold %>% count(programCodeInfo)
    colnames(numSold) <- c("programCodeInfo", "Number of Sells")
    transactionSellData <- merge(programSoldTotal, numSold, by="programCodeInfo")
  }
  
  transactionTableData <- merge(transactionBuyData, 
                                transactionSellData, 
                                by="programCodeInfo",
                                all=TRUE)
  colnames(transactionTableData) <- c("Program", 
                                      "Total Bought",
                                      "Number of Buys",
                                      "Total Sold",
                                      "Number of Sells")
  transactionTableData
}

#store functions

# to get all facility data for downloading off of app
store_facility_data <- function(unitData){
  unitData <- unitData %>%
    mutate("SO2 Controls Installed" = case_when(
      length(na.omit(so2ControlInfo)) != 0 ~ "Yes",
      length(na.omit(so2ControlInfo)) == 0 ~ "No"
    ))
  
  unitData <- unitData %>%
    mutate("NOx Controls Installed" = case_when(
      length(na.omit(noxControlInfo)) != 0 ~ "Yes",
      length(na.omit(noxControlInfo)) == 0 ~ "No"
    ))
  
  unitData <- unitData %>%
    mutate("Particulate Matter Controls Installed" = case_when(
      length(na.omit(pmControlInfo)) != 0 ~ "Yes",
      length(na.omit(pmControlInfo)) == 0 ~ "No"
    ))
  
  unitData <- unitData %>%
    mutate("Mercury Controls Installed" = case_when(
      length(na.omit(hgControlInfo)) != 0 ~ "Yes",
      length(na.omit(hgControlInfo)) == 0 ~ "No"
    ))
  
  facilityTableForDownload <- unitData[,c("facilityName","facilityId","stateCode",
                                          "stateName","county",
                                          "latitude","longitude","unitId","operatingStatus",
                                          "primaryFuelInfo","SO2 Controls Installed",
                                          "NOx Controls Installed", 
                                          "Particulate Matter Controls Installed",
                                          "Mercury Controls Installed",
                                          "year")]
  names(facilityTableForDownload) <- c("Facility Name","Facility Id","State Code",
                                       "State Name","County",
                                       "Latitude","Longitude","Unit Id","Operating Status",
                                       "Primary Fuels","SO2 Controls Installed",
                                       "NOx Controls Installed", 
                                       "Particulate Matter Controls Installed",
                                       "Mercury Controls Installed",
                                       "Year of Operation")
  
  facilityTableForDownload
}

