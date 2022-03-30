### Global

# API calls to get emissions data
# format arguments as a c() vector of elements
get_annual_emiss_data <- function(emissionYears, unitType=NULL, unitFuelType=NULL, states=NULL, facilities=NULL){
  pageIndex <- 1
  perPage <- 1000
  
  url <- paste0(apiUrlBase,"/emissions-mgmt/apportioned/annual?api_key=",apiKEY)
  query <- list(year=(paste0(emissionYears, collapse = '|')),
                perPage=as.character(perPage))
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
