## global values
apiUrlBase <- Sys.getenv("API_url_base")
apiKEY <- Sys.getenv("API_KEY")

# Getting all programs and storing appropriate emission and compliance years
url <- paste0(apiUrlBase,"/master-data-mgmt/programs?API_KEY=",apiKEY)
res = GET(url, query = list(allowanceUIFilter="true"))
allCompliancePrograms <- fromJSON(rawToChar(res$content))

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

## global functions

get_facility_data <- function(years){
  
  url <- paste0(apiUrlBase,"/facilities-mgmt/facilities/attributes/applicable?API_KEY=",apiKEY)
  startYear <- years[1]
  lastYear <- years[length(years)]
  query <- list(year=(paste0(lastYear, collapse = '|')))
  
  res = GET(url, query = query)
  facilityData <- fromJSON(rawToChar(res$content))
  
  # check for latest year of data
  while (length(facilityData$statusCode) > 0 | length(facilityData) == 0){
    lastYear <- lastYear-1
    query <- list(year=(paste0(lastYear, collapse = '|')))
    res = GET(url, query = query)
    facilityData <- fromJSON(rawToChar(res$content))
  }
  years <- seq(startYear,lastYear-1)
  query <- list(year=(paste0(years, collapse = '|')))
  res = GET(url, query = query)
  facilityData <- rbind(facilityData,fromJSON(rawToChar(res$content)))
  
  facilityData
}

facilityData <- get_facility_data(seq(1995,as.integer(format(Sys.Date(), "%Y"))))

