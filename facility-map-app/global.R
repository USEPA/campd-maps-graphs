
# data for usage

# calls of functions for global variables

programfacilityData <- read.csv(file = paste0(getwd(),"/globals/programFacilityData.csv"))

facilityFilterIndices <- match(c("programCode","stateName","facilityName")
                               ,names(programfacilityData))

searchFilterIndices <- match(c("stateName","countyName")
                             ,names(countyState))

filter_facility_latlong_data <- function(facilityData){
  unique(facilityData[,c("facilityId","stateCode","stateName","county","facilityName","longitude","latitude")])
}

makecomplianceDataTableForDownload <- function(){
  allYearComplianceFacilityData <- read.csv(file = paste0(getwd(),"/globals/allYearComplianceFacilityData.csv"))
  complianceFacilityDataLatest <- read.csv(file = paste0(getwd(),"/globals/complianceFacilityDataLatestYear.csv"))
  
  complianceFacilityDataLatestFormat <- bind_rows(lapply(1:nrow(complianceFacilityDataLatest), function(row){
    if (is.na(complianceFacilityDataLatest$excessEmissions[row])){
      compStr <- "Yes"
    }
    else{compStr <- "No"}
    c("Facility Name"=complianceFacilityDataLatest$facilityName[row], 
      "Facility Id"=complianceFacilityDataLatest$facilityId[row], 
      "Account Number"=complianceFacilityDataLatest$accountNumber[row], 
      "Program"=complianceFacilityDataLatest$programCodeInfo[row], 
      "Year"=complianceFacilityDataLatest$year[row],
      "In compliance?"=compStr)
  }))
  
  
  allYearComplianceFacilityDataFormat <- bind_rows(lapply(1:nrow(allYearComplianceFacilityData), function(row){
    if (!is.na(allYearComplianceFacilityData[row,"excessEmissions"])){
      c("Facility Name"=allYearComplianceFacilityData$facilityName[row], 
        "Facility Id"=allYearComplianceFacilityData$facilityId[row], 
        "Account Number"=allYearComplianceFacilityData$accountNumber[row], 
        "Program"=allYearComplianceFacilityData$programCodeInfo[row], 
        "Year"=allYearComplianceFacilityData$year[row],
        "In compliance?"="No")
    }
  }))
  
  complianceDataTableForDownload <- rbind(complianceFacilityDataLatestFormat,allYearComplianceFacilityDataFormat)
}
