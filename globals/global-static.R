
# API info
apiUrlBase <- Sys.getenv("API_url_base")
apiKEY <- Sys.getenv("API_KEY")

# table to convert column name to appropriate lables for UI
mulitLabelConversion <- data.frame(columnName=c("programDescription", 
                                                "stateName", 
                                                "unitTypeGroupDescription", 
                                                "fuelGroupDescription", 
                                                "controlEquipParamDescription", 
                                                "year"), 
                                   label=c("Select up to 5 Regulatory Programs",
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

## global functions

get_annual_emiss_data <- function(emissionYears, programs=NULL, 
                                  unitType=NULL, unitFuelType=NULL, 
                                  states=NULL, facilities=NULL){
  pageIndex <- 1
  perPage <- 1000
  
  url <- paste0(apiUrlBase,"/emissions-mgmt/apportioned/annual?api_key=",apiKEY)
  query <- list(year=(paste0(emissionYears, collapse = '|')),
                perPage=as.character(perPage))
  if (!is.null(programs)){query <- append(query, list(programCodeInfo = (paste0(programs, collapse = '|'))))}
  if (!is.null(unitType)){query <- append(query, list(unitType = (paste0(unitType, collapse = '|'))))}
  if (!is.null(unitFuelType)){query <- append(query, list(unitFuelType = (paste0(fuelType, collapse = '|'))))}
  if (!is.null(states)){query <- append(query, list(stateCode = (paste0(states, collapse = '|'))))}
  if (!is.null(facilities)){query <- append(query, list(facilityId = (paste0(facilities, collapse = '|'))))}
  
  queryIndex <- append(query, list(page=pageIndex))
  
  res = GET(url, query = queryIndex)
  annualEmissData <- fromJSON(rawToChar(res$content))
  
  if (length(res$content) > 2){
    while(length(res$content) > 2){
      pageIndex <- pageIndex + 1
      queryIndex <- append(query, list(page=pageIndex))
      res = GET(url, query = queryIndex)
      newData <- fromJSON(rawToChar(res$content))
      
      annualEmissData <- rbind(annualEmissData, newData)
    }
  }
  else(return(NULL))
  annualEmissData
}

get_ozone_emiss_data <- function(emissionYears, programs=NULL, 
                                  unitType=NULL, unitFuelType=NULL, 
                                  states=NULL, facilities=NULL){
  pageIndex <- 1
  perPage <- 1000
  
  url <- paste0(apiUrlBase,"/emissions-mgmt/apportioned/ozone?api_key=",apiKEY)
  query <- list(year=(paste0(emissionYears, collapse = '|')),
                perPage=as.character(perPage))
  if (!is.null(programs)){query <- append(query, list(programCodeInfo = (paste0(programs, collapse = '|'))))}
  if (!is.null(unitType)){query <- append(query, list(unitType = (paste0(unitType, collapse = '|'))))}
  if (!is.null(unitFuelType)){query <- append(query, list(unitFuelType = (paste0(fuelType, collapse = '|'))))}
  if (!is.null(states)){query <- append(query, list(stateCode = (paste0(states, collapse = '|'))))}
  if (!is.null(facilities)){query <- append(query, list(facilityId = (paste0(facilities, collapse = '|'))))}
  
  queryIndex <- append(query, list(page=pageIndex))
  
  res = GET(url, query = queryIndex)
  annualEmissData <- fromJSON(rawToChar(res$content))
  
  if (length(res$content) > 2){
    while(length(res$content) > 2){
      pageIndex <- pageIndex + 1
      queryIndex <- append(query, list(page=pageIndex))
      res = GET(url, query = queryIndex)
      newData <- fromJSON(rawToChar(res$content))
      
      annualEmissData <- rbind(annualEmissData, newData)
    }
  }
  else(return(NULL))
  annualEmissData
}

get_facility_data <- function(years){
  pageIndex <- 1
  perPage <- 1000
  
  url <- paste0(apiUrlBase,"/facilities-mgmt/facilities/attributes?api_key=",apiKEY)
  query <- list(year=(paste0(years, collapse = '|')),
                perPage=as.character(perPage))
  
  queryIndex <- append(query, list(page=pageIndex))
  
  res = GET(url, query = queryIndex)
  yearFacilityData <- fromJSON(rawToChar(res$content))
  
  if (length(res$content) > 2){
    while(length(res$content) > 2){
      pageIndex <- pageIndex + 1
      queryIndex <- append(query, list(page=pageIndex))
      res = GET(url, query = queryIndex)
      newData <- fromJSON(rawToChar(res$content))
      # Some columns are missing in early data
      # this fills missing columns with NULL
      if (length(names(newData)) != length(names(yearFacilityData))){
        yearFacilityData[names(newData)[!(names(newData) %in% names(yearFacilityData))]] <- NA
      }
      yearFacilityData <- rbind(yearFacilityData, fromJSON(rawToChar(res$content)))
    }
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
  pageIndex <- 1
  perPage <- 1000
  
  url <- paste0(apiUrlBase,"/account-mgmt/allowance-compliance?api_key=",apiKEY)
  query <- list(year=(paste0(complianceYears, collapse = '|')),
                perPage=as.character(perPage))
  
  if (!is.null(programs)){query <- append(query, list(programCodeInfo = (paste0(programs, collapse = '|'))))}
  if (!is.null(states)){query <- append(query, list(stateCode = (paste0(states, collapse = '|'))))}
  if (!is.null(facilities)){query <- append(query, list(facilityId = (paste0(facilities, collapse = '|'))))}
  
  queryIndex <- append(query, list(page=pageIndex))
  
  res = GET(url, query = queryIndex)
  yearComplianceData <- fromJSON(rawToChar(res$content))
  
  if (length(res$content) > 2){
    while(length(res$content) > 2){
      pageIndex <- pageIndex + 1
      queryIndex <- append(query, list(page=pageIndex))
      res = GET(url, query = queryIndex)
      newData <- fromJSON(rawToChar(res$content))
      # Some columns are missing in early data
      # this fills missing columns with NULL
      if (length(names(newData)) != length(names(yearComplianceData))){
        yearComplianceData[names(newData)[!(names(newData) %in% names(yearComplianceData))]] <- NA
      }
      yearComplianceData <- rbind(yearComplianceData, fromJSON(rawToChar(res$content)))
    }
  }
  else{return(NULL)}
  yearComplianceData
}

####### CSAPR Budgets #######
state_budgets <- read.csv("./globals/csapr-state-assurance-levels.csv")


# drop these states in shapefiles
dropStates <- c("American Samoa", "American Samoa", 
                "Commonwealth of the Northern Mariana Islands",
                "Guam", "United States Virgin Islands")

####### County Search Data #######
USA <- st_read(dsn = "./globals/cb_2018_us_county_5m.shp")

countyDF <- USA[,c("STATEFP", "COUNTYNS","NAME","geometry"),]

names(countyDF) <- tolower(names(countyDF))
names(countyDF)[names(countyDF)=="name"] <- c("countyName")

county_sf <- st_as_sf(countyDF)

FIPS <- read.csv("./globals/stateFIPS.csv")

FIPS$FIPS <- formatC(FIPS$FIPS, width = 2, format = "d", flag = "0", big.mark = "-")

countyState <- merge(county_sf, FIPS, by.x="statefp", by.y="FIPS",all.x=TRUE)

countyState <- countyState[!(countyState$stateName %in% dropStates),]

####### Zip Code Search Data #######


####### State Search Data #######
USA <- st_read(dsn = "./globals/cb_2018_us_state_5m.shp")

stateDF <- USA[,c("NAME","geometry"),]

names(stateDF) <- tolower(names(stateDF))
names(stateDF)[names(stateDF)=="name"] <- c("stateName")

state_sf <- st_as_sf(stateDF)

state_sf <- state_sf[order(state_sf$stateName),]

state_sf <- state_sf[!(state_sf$stateName %in% dropStates),]

####### functions #######

# API call to get allowance holdings info for a facility
get_allow_holding_data <- function(facilityId){
  pageIndex <- 1
  perPage <- 1000
  
  url <- paste0(apiUrlBase,"/account-mgmt/allowance-holdings?api_key=",apiKEY)
  query <- list(facilityId=facilityId,
                perPage=as.character(perPage))
  res = GET(url, query)
  
  queryIndex <- append(query, list(page=pageIndex))
  
  res = GET(url, query = queryIndex)
  holdingData <- fromJSON(rawToChar(res$content))
  
  if (length(res$content) > 2){
    while(length(res$content) > 2){
      pageIndex <- pageIndex + 1
      queryIndex <- append(query, list(page=pageIndex))
      res = GET(url, query = queryIndex)
      newData <- fromJSON(rawToChar(res$content))
      holdingData <- rbind(holdingData, fromJSON(rawToChar(res$content)))
    }
  }
  else{return(NULL)}
  aggregate(totalBlock ~ programCodeInfo, 
            data = holdingData[c("programCodeInfo","totalBlock")], sum)
}

# API call to get transaction data by a facility
get_transaction_data <- function(facilityId, transactionBeginDate,
                                 transactionEndDate){
  pageIndex <- 1
  perPage <- 1000
  
  url <- paste0(apiUrlBase,"/account-mgmt/allowance-transactions?api_key=",apiKEY)
  query <- list(transactionBeginDate=transactionBeginDate,
                transactionEndDate=transactionEndDate,
                transactionType="Private Transfer",
                facilityId=facilityId,
                perPage=as.character(perPage))
  
  res = GET(url, query)
  
  queryIndex <- append(query, list(page=pageIndex))
  
  res = GET(url, query = queryIndex)
  transactionData <- fromJSON(rawToChar(res$content))
  
  if (length(res$content) > 2){
    while(length(res$content) > 2){
      pageIndex <- pageIndex + 1
      queryIndex <- append(query, list(page=pageIndex))
      res = GET(url, query = queryIndex)
      newData <- fromJSON(rawToChar(res$content))
      transactionData <- rbind(transactionData, fromJSON(rawToChar(res$content)))
    }
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

# filter necessary facility data
filter_facility_att_data <- function(data){
  fac_data <- data %>% select(stateCode, county, facilityId,
                              facilityName, year, programCodeInfo,
                              longitude, latitude, operatingStatus)
  fac_data = merge(x=fac_data, y=states[,c("stateCode","stateName")],
                   by="stateCode")
  fac_data <- unique(fac_data)
}

# filter facility lat/long and state data
filter_facility_latlong_data <- function(data){
  fac_data <- data %>% select(stateCode, stateName, county, facilityId,
                              facilityName, longitude, latitude)
  fac_data <- unique(fac_data)
}
