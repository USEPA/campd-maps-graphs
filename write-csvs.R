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
source("./globals/global-load.R")

#### Unit data for latest compliance year ###
unitData <- get_facility_data(latestComplianceYear)

unitData = merge(x=unitData, y=states[,c("stateCode","stateName")],
                 by="stateCode")

write.csv(unitData, file = paste0(getwd(),"/globals/unitData.csv"), row.names = FALSE)
######


### Filter unit data to get condensed facility data ###

programFacilityData <- unitData %>% select(stateCode, stateName, county, facilityId,
                            facilityName, year, programCodeInfo,
                            longitude, latitude)
programFacilityData <- unique(programFacilityData)

# convert program info for units to individual rows
programFacilityData <- bind_rows(lapply(unique(programFacilityData$facilityId), function(id){
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
programFacilityData = merge(x=programFacilityData,
                            y=allPrograms[,c("programCode","programDescription")],
                            by="programCode",all.x=TRUE)

write.csv(programFacilityData, file = paste0(getwd(),"/globals/programFacilityData.csv"), row.names = FALSE)
######


### Collect compliance data for all years ###
complianceYears <- unique(na.omit(unlist(allCompliancePrograms$complianceYears)))

allYearComplianceFacilityData <- get_allow_comp_data(complianceYears)

complianceFacilityDataLatestYear <- allYearComplianceFacilityData[allYearComplianceFacilityData$year == latestComplianceYear,]

write.csv(allYearComplianceFacilityData, file = paste0(getwd(),"/globals/allYearComplianceFacilityData.csv"), row.names = FALSE)
write.csv(complianceFacilityDataLatestYear, file = paste0(getwd(),"/globals/complianceFacilityDataLatestYear.csv"), row.names = FALSE)
######


### Collect facility data for all years ###
facilityDataTableForDownload <- store_facility_data()
write.csv(facilityDataTableForDownload, file = paste0(getwd(),"/globals/facilityDataTableForDownload.csv"), row.names = FALSE)


### Collect applicable compliance data for program insights ###
applicableAllowanceComplianceUrl <- paste0(apiUrlBase,"/account-mgmt/allowance-compliance/attributes/applicable?api_key=",apiKEY)
res = GET(applicableAllowanceComplianceUrl)
applicableAllowCompTable <- fromJSON(rawToChar(res$content))

applicableAllowCompTable = na.omit(merge(x=applicableAllowCompTable,
                                         y=currentCompliancePrograms[,c("programCode","programDescription")],
                                         by="programCode",all.x=TRUE))

applicableAllowCompTable = merge(x=applicableAllowCompTable,
                                 y=states[,c("stateCode","stateName")],
                                 by="stateCode",all.x=TRUE)

write.csv(applicableAllowCompTable, file = paste0(getwd(),"/globals/applicableAllowCompTable.csv"), row.names = FALSE)
######


### ARP compliance data ###
ARPComplianceData <- get_allow_comp_data(unlist(currentCompliancePrograms[currentCompliancePrograms$programCode %in%
                                                       c("ARP"),]$complianceYears),
                                         programs=c("ARP"))
write.csv(ARPComplianceData, file = paste0(getwd(),"/globals/ARPComplianceData.csv"), row.names = FALSE)
######

