# make sure to install.packages("miniCRAN")
library(miniCRAN)

pkgs <- c("shiny", "dotenv","shinydashboard","shinybusy",
          "plotly","tidyverse","lubridate",
          "dplyr","leaflet.extras")
# Update r.yml file!

localCRAN <- file.path(getwd(), "vendor_r")

revolution <- c(CRAN = "https://cran.r-project.org")

pkgList <- pkgDep(pkgs, repos = revolution, type = "source", suggests = TRUE)

makeRepo(pkgList, path = localCRAN, repos = revolution)


