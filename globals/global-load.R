
global_vars <- function(){
  
  ## global values
  
  # Get all programs and storing appropriate emission and compliance years
  res = GET(programMdmUrl)
  allPrograms <- fromJSON(rawToChar(res$content))
  allPrograms$programDescription <- paste0(
    allPrograms$programDescription, " (",
    allPrograms$programCode, ")")
  
  # Get all allowance programs 
  compliancePrograms <- read.csv(paste0(gitRawBase,"/data/allowanceProgramData.csv"))
  allCompliancePrograms <- compliancePrograms
  
  for (i in 1:nrow(allCompliancePrograms)){
    if (!is.na(allCompliancePrograms$complianceYears[i])){
      allCompliancePrograms$complianceYears[i] <- list(c(as.integer(unlist(strsplit(compliancePrograms$complianceYears[i], ",")))))
    }
    if (!is.na(allCompliancePrograms$emissionYears[i])){
      allCompliancePrograms$emissionYears[i] <- list(c(as.integer(unlist(strsplit(compliancePrograms$emissionYears[i], ",")))))
    }
  }
  
  currentCompliancePrograms <- allCompliancePrograms[allCompliancePrograms$retiredIndicator == FALSE,]
  
  latestComplianceYear <- min(na.omit(unlist(lapply(currentCompliancePrograms$programCode, function(program){
    max(unlist(currentCompliancePrograms$complianceYears[currentCompliancePrograms$programCode == program]))
  }))))
  
  latestEmissionsYear <- min(na.omit(unlist(lapply(currentCompliancePrograms$programCode, function(program){
    max(unlist(currentCompliancePrograms$emissionYears[currentCompliancePrograms$programCode == program]))
  }))))
  
  descriptionTable <- currentCompliancePrograms[,c("programCode",
                                                  "programDescription",
                                                  "compParameterCode",
                                                  "programGroupCode",
                                                  "programGroupDescription")]
  
  
  names(descriptionTable) <- c("Program Code",
                                      "Full Program Name",
                                      "Pollutant",
                                      "Program Group",
                                      "Program Group Description")
  
  
  # Storing states 
  url <- paste0(apiUrlBase,"/master-data-mgmt/states?API_KEY=",apiKEY)
  res = GET(url)
  states <- fromJSON(rawToChar(res$content))
  
  # Unit data
  unitData <<- read.csv(file = paste0(gitRawBase,"/data/unitData.csv"))
  
  ####### assigning global values #######
  programInfo$allPrograms <<- allPrograms
  programInfo$allCompliancePrograms <<- allCompliancePrograms
  programInfo$currentCompliancePrograms <<- currentCompliancePrograms
  programInfo$descriptionTable <<- descriptionTable
  programInfo$latestComplianceYear <<- latestComplianceYear
  programInfo$latestEmissionsYear <<- latestEmissionsYear
  
  statesMdm <<- states
  
}


# Storing control technologies
#url <- paste0(apiUrlBase,"/master-data-mgmt/control-technologies?API_KEY=",apiKEY)
#res = GET(url)
#controlTechnologies <- fromJSON(rawToChar(res$content))
#controlTechnologies <- mutate_at(controlTechnologies, c("controlEquipParamDescription"), ~replace(., is.na(.), "Other"))

# Storing fuel types
#url <- paste0(apiUrlBase,"/master-data-mgmt/fuel-types?API_KEY=",apiKEY)
#res = GET(url)
#fuelTypes <- fromJSON(rawToChar(res$content))

# Storing unit types
#url <- paste0(apiUrlBase,"/master-data-mgmt/unit-types?API_KEY=",apiKEY)
#res = GET(url)
#unitTypes <- fromJSON(rawToChar(res$content))