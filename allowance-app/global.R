### Global

applicableAllowanceComplianceUrl <- paste0(apiUrlBase,"/account-mgmt/allowance-compliance/attributes/applicable?api_key=",apiKEY)
res = GET(applicableAllowanceComplianceUrl)
applicableAllowCompTable <- fromJSON(rawToChar(res$content))

applicableAllowCompTable = merge(x=applicableAllowCompTable,
           y=allCompliancePrograms[,c("programCode","programDescription")],
           by="programCode",all.x=TRUE)

applicableAllowCompTable = merge(x=applicableAllowCompTable,
           y=states[,c("stateCode","stateName")],
           by="stateCode",all.x=TRUE)

uniquePrograms <- unique(applicableAllowCompTable$programDescription)
uniqueStates <- unique(applicableAllowCompTable$stateName)

allowanceBankFilterIndicesState <- match(c("programDescription","stateName")
                                         ,names(applicableAllowCompTable))

# API calls to get compliance data
# format queryList - list(stateCode = paste0(c("AL"), collapse = '|'),programCodeInfo = paste0(c("ARP"), collapse = '|'))
# where states is a c() vector of elements
get_allow_comp_data <- function(complianceYears, programs=NULL, states=NULL){
  pageIndex <- 1
  perPage <- 1000
  
  url <- paste0(apiUrlBase,"/account-mgmt/allowance-compliance?api_key=",apiKEY)
  query <- list(year=(paste0(complianceYears, collapse = '|')),
                perPage=as.character(perPage))
  if (!is.null(programs)){query <- append(query, list(programCodeInfo = (paste0(programs, collapse = '|'))))}
  if (!is.null(states)){query <- append(query, list(stateCode = (paste0(states, collapse = '|'))))}
  
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