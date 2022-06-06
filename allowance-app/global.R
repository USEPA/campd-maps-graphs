### Global

global_allowance_app_vars <- function(){
  applicableAllowanceCompliance$data <<- read.csv(file = paste0(gitRawBase,"/data/applicableAllowCompTable.csv"))
  
  applicableAllowanceCompliance$uniquePrograms <<- unique(applicableAllowanceCompliance$data$programCode)
  
  filterIndices$allowanceBank <<- match(c("programCode","stateName")
                                        ,names(applicableAllowanceCompliance$data))
  
  csaprPrograms$noxAnnual <<- c("CSNOX")
  csaprPrograms$so2Annual <<- c("CSSO2G1",
                                "CSSO2G2")
  csaprPrograms$noxOzone <<- c("CSOSG1", 
                               "CSOSG2",
                               "CSOSG3")
  
  filterIndices$programBudget <<- match(c("programCode","year","assuranceFlag")
                                        ,names(csaprStateBudgets))
  #ARPComplianceData$stateLevel <<- read.csv(file = paste0(gitRawBase,"/data/ARPComplianceData-StateLevel.csv"))
  
  ####### CSAPR Budgets #######
  stateBudgets <- read.csv(paste0(gitRawBase,"/data/csapr-state-assurance-levels.csv"))
  stateBudgets <- merge(stateBudgets, programInfo$currentCompliancePrograms[,c("programCode","programDescription")],by="programCode",all.x=TRUE)
  stateBudgets <- merge(stateBudgets, statesMdm[,c("stateCode","stateName")],by="stateName",all.x=TRUE)
  
  latest_compliance_csapr <- max(na.omit(unlist(programInfo$currentCompliancePrograms[grep("^CS", programInfo$currentCompliancePrograms$programCode),]$emissionYears)))
  csaprG3LateYear <- max(stateBudgets$year[stateBudgets$programCode == "CSOSG3"])
  
  if (csaprG3LateYear > latest_compliance_csapr){
    stateBudgets <- anti_join(stateBudgets, subset(stateBudgets, programCode == "CSOSG3" & year == csaprG3LateYear))
  }
  
  if (max(stateBudgets$year) < latest_compliance_csapr){
    budgetsToCopy <- stateBudgets[stateBudgets$year == max(stateBudgets$year) & stateBudgets$programCode != "CSOSG3",]
    addedYearBudgets <- bind_rows(lapply(rep((max(stateBudgets$year)+1):latest_compliance_csapr), function(budgetYear){
      budgetsToCopy$year <- budgetYear
      budgetsToCopy
    }))
    stateBudgets <- rbind(stateBudgets, addedYearBudgets)
  }
  csaprStateBudgets <<- stateBudgets
  
  filterIndices$programBudget <<- match(c("programCode","year","assuranceFlag"),
                                       names(csaprStateBudgets))
  
  
}

load_ARP_data <- function(){
  ARPComplianceData$stateLevel <<- read.csv(file = paste0(gitRawBase,"/data/ARPComplianceData-StateLevel.csv"))
}
