### Global

# API calls to get compliance data
# format queryList - list(stateCode = paste0(c("AL"), collapse = '|'),programCodeInfo = paste0(c("ARP"), collapse = '|'))
# where states is a c() vector of elements
get_allow_comp_data <- function(complianceYears, queryList){
  pageIndex <- 1
  perPage <- 1000
  
  url <- paste0(apiUrlBase,"/account-mgmt/allowance-compliance?api_key=",apiKEY)
  query <- append(queryList, list(year=(paste0(complianceYears, collapse = '|')),
                                       perPage=as.character(perPage)))
  
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
  else{retun(NULL)}
  yearComplianceData
}