## Script for packaging csvs

library(dotenv)
library(plotly)
library(DT)
library(httr)
library(htmltools)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo)

load_dot_env(".env")

#globals
source("./globals/global-static.R")


### Program year data ###
# Gett all programs and storing appropriate emission and compliance years
res = GET(programMdmUrl)
allPrograms <- fromJSON(rawToChar(res$content))
allPrograms$programDescription <- paste0(
  allPrograms$programDescription, " (",
  allPrograms$programCode, ")")

# Get all allowance programs 
allAllowancePrograms <- allPrograms[allPrograms$allowanceUIFilter == TRUE,]

# adding emission and compliance year columns
allAllowancePrograms["emissionYears"] <- NA
allAllowancePrograms["complianceYears"] <- NA


# adding CAIR years
allAllowancePrograms$emissionYears[which(allAllowancePrograms$programCode %in% c("CAIROS","CAIRNOX"))] <- paste(seq(2008, 2014), collapse=',')
allAllowancePrograms$emissionYears[which(allAllowancePrograms$programCode %in% c("CAIRSO2"))] <- paste(seq(2009, 2014), collapse=',')
allAllowancePrograms$complianceYears[which(allAllowancePrograms$programCode %in% c("CAIROS","CAIRNOX","CAIRSO2"))] <- paste(seq(2009, 2014), collapse=',')

# adding CSAPR (retired)
allAllowancePrograms$emissionYears[which(allAllowancePrograms$programCode %in% c("CSNOXOS"))] <- paste(seq(2015, 2016), collapse=',')
allAllowancePrograms$complianceYears[which(allAllowancePrograms$programCode %in% c("CSNOXOS"))] <- paste(seq(2015, 2016), collapse=',')

# adding NBP and OTC
allAllowancePrograms$emissionYears[which(allAllowancePrograms$programCode %in% c("NBP"))] <- paste(seq(2003, 2008), collapse=',')
allAllowancePrograms$complianceYears[which(allAllowancePrograms$programCode %in% c("NBP"))] <- paste(seq(2003, 2008), collapse=',')
allAllowancePrograms$emissionYears[which(allAllowancePrograms$programCode %in% c("OTC"))] <- paste(seq(1999, 2002), collapse=',')
allAllowancePrograms$complianceYears[which(allAllowancePrograms$programCode %in% c("OTC"))] <- paste(seq(1999, 2002), collapse=',')

startYears <- as.data.frame(allAllowancePrograms[allAllowancePrograms$retiredIndicator == FALSE,]$programCode)
colnames(startYears) <- c('programCode')
startYears["startYear"] <- NA
startYears$startYear[which(startYears$programCode %in% c("CSNOX","CSSO2G1","CSSO2G2"))] <- 2015
startYears$startYear[which(startYears$programCode %in% c("CSOSG1","CSOSG2"))] <- 2017
startYears$startYear[which(startYears$programCode %in% c("CSOSG3"))] <- 2021
startYears$startYear[which(startYears$programCode %in% c("TXSO2"))] <- 2019

allAllowancePrograms$emissionYears[which(allAllowancePrograms$programCode == "ARP")] <- paste(
  append(c(1980,1985,1990),seq(1995,get_latest_valid_vear(annualEmissionsUrl, c("ARP")))), collapse=',')
allAllowancePrograms$complianceYears[which(allAllowancePrograms$programCode == "ARP")] <- paste(
  seq(1995,get_latest_valid_vear(compliancePageUrl, c("ARP"))), collapse=',')

for (prg in allAllowancePrograms[allAllowancePrograms$retiredIndicator == FALSE,]$programCode){
  if(prg!="ARP"){
    latestEmissionYear <- get_latest_valid_vear(annualEmissionsUrl, c(prg))
    latestComplianceYear <- get_latest_valid_vear(compliancePageUrl, c(prg))
    if(!is.na(latestEmissionYear)){
      allAllowancePrograms$emissionYears[which(allAllowancePrograms$programCode == prg)] <- 
        paste(seq(startYears$startYear[startYears$programCode==prg],
                  latestEmissionYear), collapse=',')
    }
    if(!is.na(latestComplianceYear)){
      allAllowancePrograms$complianceYears[which(allAllowancePrograms$programCode == prg)] <- 
        paste(seq(startYears$startYear[startYears$programCode==prg],
                  latestComplianceYear), collapse=',')
    }
  }
}

write.csv(allAllowancePrograms, file = "./data/allowanceProgramData.csv", row.names = FALSE)

compliancePrograms <- allAllowancePrograms

for (i in 1:nrow(allAllowancePrograms)){
  if (!is.na(allAllowancePrograms$complianceYears[i])){
    allAllowancePrograms$complianceYears[i] <- list(c(as.integer(unlist(strsplit(compliancePrograms$complianceYears[i], ",")))))
  }
  if (!is.na(allAllowancePrograms$emissionYears[i])){
    allAllowancePrograms$emissionYears[i] <- list(c(as.integer(unlist(strsplit(compliancePrograms$emissionYears[i], ",")))))
  }
}


### Collect compliance data for all years ###
complianceYears <- unique(na.omit(unlist(allAllowancePrograms$complianceYears)))

allYearAllowanceFacilityData <- get_allow_comp_data(complianceYears)

complianceFacilityDataLatestYear <- allYearAllowanceFacilityData[allYearAllowanceFacilityData$year == latestComplianceYear,]

write.csv(allYearAllowanceFacilityData, file = paste0(getwd(),"/data/allYearAllowanceFacilityData.csv"), row.names = FALSE)
write.csv(complianceFacilityDataLatestYear, file = paste0(getwd(),"/data/complianceFacilityDataLatestYear.csv"), row.names = FALSE)
######

currentCompliancePrograms <- allAllowancePrograms[allAllowancePrograms$retiredIndicator == FALSE,]

# Storing states 
url <- paste0(apiUrlBase,"/master-data-mgmt/states?API_KEY=",apiKEY)
res = GET(url)
states <- fromJSON(rawToChar(res$content))

### Collect applicable compliance data for program insights ###
res = GET(complianceApplicableUrl)
applicableAllowCompTable <- fromJSON(rawToChar(res$content))

applicableAllowCompTable = na.omit(merge(x=applicableAllowCompTable,
                                         y=currentCompliancePrograms[,c("programCode","programDescription")],
                                         by="programCode",all.x=TRUE))

applicableAllowCompTable = merge(x=applicableAllowCompTable,
                                 y=states[,c("stateCode","stateName")],
                                 by="stateCode",all.x=TRUE)

stateYeayProgramApplicable <- unique(applicableAllowCompTable[,c("stateCode","programCode","year")])

write.csv(applicableAllowCompTable, file = paste0(getwd(),"/data/applicableAllowCompFacility.csv"), row.names = FALSE)
write.csv(stateYeayProgramApplicable, file = paste0(getwd(),"/data/applicableAllowCompState.csv"), row.names = FALSE)
######


### ARP compliance data ###
ARPComplianceData <- get_allow_comp_data(unlist(currentCompliancePrograms[currentCompliancePrograms$programCode %in%
                                                       c("ARP"),]$complianceYears),
                                         programs=c("ARP"))
aggregatedARPData <- aggregate(ARPComplianceData[,c("allocated", 
                                                  "totalAllowancesDeducted",
                                                  "carriedOver")],
                            list(ARPComplianceData$programCode,
                                 ARPComplianceData$stateCode,
                                 ARPComplianceData$year),
                            sum)
colnames(aggregatedARPData)[1:3] <- c("programCode",
                                   "stateCode",
                                   "year")
aggregatedARPData = merge(x=aggregatedARPData,
                       y=states[,c("stateCode","stateName")],
                       by.x="stateCode")
aggregatedARPData = merge(x=aggregatedARPData,
                          y=currentCompliancePrograms[,c("programCode",
                                                         "programDescription")],
                          by.x="programCode")

write.csv(ARPComplianceData, file = paste0(getwd(),"/data/ARPComplianceData-FacilityLevel.csv"), row.names = FALSE)
write.csv(aggregatedARPData, file = paste0(getwd(),"/data/ARPComplianceData-StateLevel.csv"), row.names = FALSE)
######



#### Unit data for latest compliance year ###
unitData <- get_facility_data(latestComplianceYear)

unitData <- subset(unitData, select=-c(associatedStacks,epaRegion,nercRegion,countyCode,
                                       fipsCode,sourceCategory,so2Phase,noxPhase,
                                       commercialOperationDate,maxHourlyHIRate,
                                       associatedGeneratorsAndNameplateCapacity))

#unitData = merge(x=unitData, y=states[,c("stateCode","stateName")],
#                 by="stateCode")

write.csv(unitData, file = "./data/unitData.csv", row.names = FALSE)
######


### Filter unit data to get condensed facility data ###

programFacilityData <- unitData %>% select(stateCode, stateName, county, facilityId,
                                           facilityName, year, programCodeInfo,
                                           longitude, latitude)
programFacilityData <- unique(programFacilityData)

# convert program info for units to individual rows
programFacilityDataByProgram <- bind_rows(lapply(unique(programFacilityData$facilityId), function(id){
  singleFacilityData <- programFacilityData[programFacilityData$facilityId == id,]
  programs <- c(unique(unlist(strsplit(singleFacilityData$programCodeInfo, ", "))))
  isolatedFacilityData <- unique(programFacilityData[, -which(names(programFacilityData) == "programCodeInfo")])
  dataPrgCodes <- bind_rows(lapply(programs, function(prg){
    data <- data.frame(id, prg)
    colnames(data) <- c("facilityId", "programCode")
    data
  }))
  #isolatedFacilityData
  merge(x=dataPrgCodes,y=isolatedFacilityData,
        by="facilityId",all.x=TRUE)
}))

# add program description column
programFacilityDataByProgram = merge(x=programFacilityDataByProgram,
                            y=allPrograms[,c("programCode","programDescription")],
                            by="programCode",all.x=TRUE)

write.csv(programFacilityDataByProgram, file = "./data/programFacilityData.csv", row.names = FALSE)


### Collect facility data for all years ###
facilityDataTableForDownload <- store_facility_data(unitData)
write.csv(facilityDataTableForDownload, file = paste0(getwd(),"/data/facilityDataTableForDownload.csv"), row.names = FALSE)






