## global values
apiUrlBase <- Sys.getenv("API_url_base")
apiKEY <- Sys.getenv("API_KEY")

# Gett all programs and storing appropriate emission and compliance years
url <- paste0(apiUrlBase,"/master-data-mgmt/programs?API_KEY=",apiKEY)
res = GET(url)
allPrograms <- fromJSON(rawToChar(res$content))
allPrograms$programDescription <- paste0(
  allPrograms$programDescription, " (",
  allPrograms$programCode, ")")

# Get all allowance programs 
allCompliancePrograms <- allPrograms[allPrograms$allowanceUIFilter == TRUE,]

# adding emission and compliance year columns
allCompliancePrograms["emissionYears"] <- NA
allCompliancePrograms["complianceYears"] <- NA

# global function

get_latest_valid_vear <- function(url, program=NULL){
  latestYear <- as.integer(format(Sys.Date(), "%Y"))
  baseQuery <- list(page="1",
                perPage="1")
  runExit <- 0
  if (!is.null(program)){baseQuery <- append(baseQuery, list(programCodeInfo = program))}
  query <- append(baseQuery, list(year=latestYear))
  res = GET(url, query = query)
  testYear <- fromJSON(rawToChar(res$content))
  if ((length(testYear$statusCode) > 0 | length(testYear) == 0)){
    while((length(testYear$statusCode) > 0 | length(testYear) == 0)){
      latestYear <- latestYear - 1
      runExit <- runExit + 1
      if (runExit > 3){
        return(NA)
        break
      }
      query <- append(baseQuery, list(year=latestYear))
      res = GET(url, query = query)
      testYear <- fromJSON(rawToChar(res$content))
    }
  }
  
  return(latestYear)
}

# annual emissions url
annualEmissionsUrl <- paste0(apiUrlBase,"/emissions-mgmt/apportioned/annual?API_KEY=",apiKEY)
# compliance url
complianceUrl <- paste0(apiUrlBase,"/account-mgmt/allowance-compliance?API_KEY=",apiKEY)
# compliance url
facilitiesUrl <- paste0(apiUrlBase,"/facilities-mgmt/facilities/attributes/applicable?API_KEY=",apiKEY)

# adding CAIR years
allCompliancePrograms$emissionYears[which(allCompliancePrograms$programCode %in% c("CAIROS","CAIRNOX"))] <- list(c(seq(2008, 2014)))
allCompliancePrograms$emissionYears[which(allCompliancePrograms$programCode %in% c("CAIRSO2"))] <- list(c(seq(2009, 2014)))
allCompliancePrograms$complianceYears[which(allCompliancePrograms$programCode %in% c("CAIROS","CAIRNOX","CAIRSO2"))] <- list(c(seq(2009, 2014)))

# adding CSAPR (retired)
allCompliancePrograms$emissionYears[which(allCompliancePrograms$programCode %in% c("CSNOXOS"))] <- list(c(seq(2015, 2016)))
allCompliancePrograms$complianceYears[which(allCompliancePrograms$programCode %in% c("CSNOXOS"))] <- list(c(seq(2015, 2016)))

# adding NBP and OTC
allCompliancePrograms$emissionYears[which(allCompliancePrograms$programCode %in% c("NBP"))] <- list(c(seq(2003, 2008)))
allCompliancePrograms$complianceYears[which(allCompliancePrograms$programCode %in% c("NBP"))] <- list(c(seq(2003, 2008)))
allCompliancePrograms$emissionYears[which(allCompliancePrograms$programCode %in% c("OTC"))] <- list(c(seq(1999, 2002)))
allCompliancePrograms$complianceYears[which(allCompliancePrograms$programCode %in% c("OTC"))] <- list(c(seq(1999, 2002)))

startYears <- as.data.frame(allCompliancePrograms[allCompliancePrograms$retiredIndicator == FALSE,]$programCode)
colnames(startYears) <- c('programCode')
startYears["startYear"] <- NA
startYears$startYear[which(startYears$programCode %in% c("CSNOX","CSSO2G1","CSSO2G2"))] <- 2015
startYears$startYear[which(startYears$programCode %in% c("CSOSG1","CSOSG2"))] <- 2017
startYears$startYear[which(startYears$programCode %in% c("CSOSG3"))] <- 2021
startYears$startYear[which(startYears$programCode %in% c("TXSO2"))] <- 2019

allCompliancePrograms$emissionYears[which(allCompliancePrograms$programCode == "ARP")] <- list(
  (append(c(1980,1985,1990),seq(1995,get_latest_valid_vear(annualEmissionsUrl, c("ARP"))))))
allCompliancePrograms$complianceYears[which(allCompliancePrograms$programCode == "ARP")] <- list(
  (seq(1995,get_latest_valid_vear(complianceUrl, c("ARP")))))

# Only current programs
for (prg in allCompliancePrograms[allCompliancePrograms$retiredIndicator == FALSE,]$programCode){
  if(prg!="ARP"){
    latestEmissionYear <- get_latest_valid_vear(annualEmissionsUrl, c(prg))
    latestComplianceYear <- get_latest_valid_vear(complianceUrl, c(prg))
    if(!is.na(latestEmissionYear)){
      allCompliancePrograms$emissionYears[which(allCompliancePrograms$programCode == prg)] <- 
        list(seq(startYears$startYear[startYears$programCode==prg],
                 latestEmissionYear))
    }
    if(!is.na(latestComplianceYear)){
      allCompliancePrograms$complianceYears[which(allCompliancePrograms$programCode == prg)] <- 
        list(seq(startYears$startYear[startYears$programCode==prg],
                 latestComplianceYear))
    }
  }
}

currentCompliancePrograms <- allCompliancePrograms[allCompliancePrograms$retiredIndicator == FALSE,]

allLatestEmissionYears <- lapply(currentCompliancePrograms$programCode, function(program){
  max(unlist(currentCompliancePrograms$emissionYears[currentCompliancePrograms$programCode == program]))
})

latestEmissionsYear <- min(unlist(allLatestEmissionYears))

# Storing states 
url <- paste0(apiUrlBase,"/master-data-mgmt/states?API_KEY=",apiKEY)
res = GET(url)
states <- fromJSON(rawToChar(res$content))

# Storing control technologies
url <- paste0(apiUrlBase,"/master-data-mgmt/control-technologies?API_KEY=",apiKEY)
res = GET(url)
controlTechnologies <- fromJSON(rawToChar(res$content))
controlTechnologies <- mutate_at(controlTechnologies, c("controlEquipParamDescription"), ~replace(., is.na(.), "Other"))

# Storing fuel types
url <- paste0(apiUrlBase,"/master-data-mgmt/fuel-types?API_KEY=",apiKEY)
res = GET(url)
fuelTypes <- fromJSON(rawToChar(res$content))

# Storing unit types
url <- paste0(apiUrlBase,"/master-data-mgmt/unit-types?API_KEY=",apiKEY)
res = GET(url)
unitTypes <- fromJSON(rawToChar(res$content))

# table to convert column name to appropriate lables for UI
mulitLabelConversion <- data.frame(columnName=c("programDescription", 
                                                "stateName", 
                                                "unitTypeGroupDescription", 
                                                "fuelGroupDescription", 
                                                "controlEquipParamDescription", 
                                                "year"), 
                                   label=c("Select up to 5 regulatory programs",
                                           "Select up to 5 states(required)",
                                           "Select up to 5 unit types",
                                           "Select up to 5 fuel types",
                                           "Select up to 5 control technologies",
                                           "Select a range of years"))

singleLabelConversion <- data.frame(columnName=c("programDescription", 
                                                 "stateName", 
                                                 "facilityName",
                                                 "year"), 
                                    label=c("Select a regulatory program (required)",
                                            "Select a state (required)",
                                            "Select a facility (required)",
                                            "Select a year (required)"))

tableLabelConversion <- data.frame(columnName=c("programCode", 
                                                "programDescription", 
                                                "stateName", 
                                                "unitTypeGroupDescription", 
                                                "fuelGroupDescription", 
                                                "controlEquipParamDescription", 
                                                "year",
                                                "allocated",
                                                "totalAllowancesDeducted",
                                                "carriedOver"), 
                                   label=c("Program Code", 
                                           "Regulatory Program",
                                           "State Name",
                                           "Unit Types",
                                           "Fuel Types",
                                           "Control Technologies",
                                           "Year",
                                           "Allowances Allocated", 
                                           "Allowances Deducted", 
                                           "Allowances Banked"))

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
