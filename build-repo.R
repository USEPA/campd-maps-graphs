# Go here to clone repo when using leaflet.
# https://github.com/testorg-2020/cflinuxfs3-CRAN/tree/master/cflinuxfs3/src/contrib

# Otherwise edit the pkgs to include all packages for the app
# you are deploying

# Remember to change the r.yml to include all the packages

# make sure to install.packages("miniCRAN")
library(miniCRAN)

pkgs <- c("shiny", "dotenv","shinydashboard","shinybusy",
          "plotly","tidyverse","lubridate")

localCRAN <- file.path(getwd(), "vendor_r")

revolution <- c(CRAN = "https://cran.r-project.org")

pkgList <- pkgDep(pkgs, repos = revolution, type = "source", suggests = TRUE)

makeRepo(pkgList, path = localCRAN, repos = revolution)


