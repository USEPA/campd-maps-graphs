
####### functions #######

# API call to get allowance holdings info for a facility
get_allow_holding_data <- function(facilityId){
  pageIndex <- 1
  perPage <- 1000
  
  url <- paste0(apiUrlBase,"/account-mgmt/allowance-holdings?api_key=",apiKEY)
  res = GET(url, query = list(facilityId=facilityId,
                              perPage=as.character(perPage)))
            
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
  else{retun(NULL)}
  holdingData
}

# filter necessary facility data
filter_facility_att_data <- function(data){
  fac_data <- data %>% select(stateCode, county, facilityId,
                              facilityName, year, programCodeInfo,
                              longitude, latitude)
  fac_data = merge(x=fac_data, y=states[,c("stateCode","stateName")],
                   by="stateCode")
  fac_data <- unique(fac_data)
}

# data for usage

# calls of functions for global variables
unitData <- get_facility_data(latestEmissionsYear)
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

programfacilityData = na.omit(merge(x=programMappedData,
                                  y=allPrograms[,c("programCode","programDescription")],
                                  by="programCode",all.x=TRUE))

#source("./facility-map-app/filter-logic-single-selects.R")
facilityFilterIndices <- match(c("programDescription","stateName")
                               ,names(programfacilityData))

