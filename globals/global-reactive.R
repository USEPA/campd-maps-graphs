## global reactives

programInfo <- reactiveValues(allPrograms = NULL,
                              allCompliancePrograms = NULL,
                              currentCompliancePrograms = NULL,
                              descriptionTable = NULL,
                              latestComplianceYear = NULL,
                              latestEmissionsYear = NULL)


statesMdm <- reactiveVal(NULL)

# unit data
unitData <- reactiveVal(NULL)

# facility map data 
programFacilityData <- reactiveVal(NULL)
facilityFilterIndices <- reactiveVal(NULL)

# allowance app data
applicableAllowanceCompliance <- reactiveValues(data = NULL,
                                                uniquePrograms = NULL)
filterIndices <- reactiveValues(allowanceBank = NULL,
                                programBudget = NULL)
csaprStateBudgets <- reactiveVal(NULL)
csaprPrograms <- reactiveValues(noxAnnual = NULL,
                                so2Annual = NULL,
                                noxOzone = NULL)

ARPComplianceData <- reactiveValues(facilityLevel=NULL,
                                    stateLevel=NULL)