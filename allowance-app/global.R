### Global

applicableAllowCompTable <- read.csv(file = paste0(getwd(),"/globals/applicableAllowCompTable.csv"))

uniquePrograms <- unique(applicableAllowCompTable$programCode)

allowanceBankFilterIndicesState <- match(c("programCode","stateName")
                                         ,names(applicableAllowCompTable))

noxAnnualPrograms <- c("CSNOX")
so2AnnualPrograms <- c("CSSO2G1",
                       "CSSO2G2")
noxOzonePrograms <- c("CSOSG1", 
                      "CSOSG2",
                      "CSOSG3")

state_budgets <- merge(state_budgets, currentCompliancePrograms[,c("programCode","programDescription")],by="programCode",all.x=TRUE)
state_budgets <- merge(state_budgets, states[,c("stateCode","stateName")],by="stateName",all.x=TRUE)

programBudgetFilterIndicesState <- match(c("programCode","year","assuranceFlag")
                                         ,names(state_budgets))

latest_compliance_csapr <- max(na.omit(unlist(currentCompliancePrograms[grep("^CS", currentCompliancePrograms$programCode),]$emissionYears)))
csaprG3LateYear <- max(state_budgets$year[state_budgets$programCode == "CSOSG3"])

if (csaprG3LateYear > latest_compliance_csapr){
  state_budgets <- anti_join(state_budgets, subset(state_budgets, programCode == "CSOSG3" & year == csaprG3LateYear))
}

if (max(state_budgets$year) < latest_compliance_csapr){
  budgetsToCopy <- state_budgets[state_budgets$year == max(state_budgets$year) & state_budgets$programCode != "CSOSG3",]
  addedYearBudgets <- bind_rows(lapply(rep((max(state_budgets$year)+1):latest_compliance_csapr), function(budgetYear){
    budgetsToCopy$year <- budgetYear
    budgetsToCopy
  }))
  state_budgets <- rbind(state_budgets, addedYearBudgets)
}
