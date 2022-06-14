
# data for usage

global_fac_map_vars <- function(){
  programFacilityData <<- read.csv(file = paste0(gitRawBase,"/data/facilityLocationData.csv"))
  
}

# used to get rid of columns that would cause duplication of lat/long data
# programFacilityData has a program column and facilities can be in multiple programs
filter_facility_latlong_data <- function(facilityData){
  unique(facilityData[,c("facilityId","stateCode","stateName","county","facilityName","longitude","latitude")])
}

load_compliance_download_file <- function(){
  read.csv(file = paste0(gitRawBase,"/data/complianceDataTableForDownload.csv"))
}
load_facility_download_file <- function(){
  read.csv(file = paste0(gitRawBase,"/data/facilityDataTableForDownload.csv"))
}