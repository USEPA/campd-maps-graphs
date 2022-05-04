
# data for usage

# calls of functions for global variables
unitData <- get_facility_data(latestComplianceYear)
facilityData <- filter_facility_att_data(unitData)

programMappedData <- bind_rows(lapply(unique(facilityData$facilityId), function(id){
  singleFacilityData <- facilityData[facilityData$facilityId == id,]
  programs <- c(unique(unlist(strsplit(singleFacilityData$programCodeInfo, ", "))))
  isolatedFacilityData <- unique(facilityData[, -which(names(facilityData) == "programCodeInfo")])
  dataPrgCodes <- bind_rows(lapply(programs, function(prg){
    data <- data.frame(id, prg)
    colnames(data) <- c("facilityId", "programCode")
    data
  }))
  #isolatedFacilityData
  merge(x=dataPrgCodes,y=isolatedFacilityData,
        by="facilityId",all.x=TRUE)
}))

programfacilityData = merge(x=programMappedData,
                                  y=allPrograms[,c("programCode","programDescription")],
                                  by="programCode",all.x=TRUE)

facilityLatLongData <- filter_facility_latlong_data(programfacilityData)

facilityFilterIndices <- match(c("programCode","stateName","facilityName")
                               ,names(programfacilityData))

searchFilterIndices <- match(c("stateName","countyName")
                             ,names(countyState))

