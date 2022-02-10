# make sure to install.packages("miniCRAN")
library(miniCRAN)

pkgs <- c("shiny","dotenv","shinydashboard")
# Update r.yml file!

localCRAN <- file.path(getwd(), "vendor_r")

contribDir <- file.path(localCRAN, "src", "contrib")

binPaths <- list(
  win.binary = file.path("bin/windows/contrib"),
  mac.binary = file.path("bin/macosx/contrib"),
  mac.binary.mavericks = file.path("bin/macosx/mavericks/contrib"),
  mac.binary.leopard = file.path("bin/macosx/leopard/contrib")
)

binPaths <- lapply(binPaths, function(x) file.path(localCRAN, x))

revolution <- c(CRAN = "https://cran.r-project.org")

pkgList <- pkgDep(pkgs, repos = revolution, type = "source", suggests = FALSE)

makeRepo(pkgList, path = localCRAN, repos = revolution, 
         type = c("source", "win.binary", "mac.binary"))

lapply(binPaths, function(path) {
  tools::write_PACKAGES(path)
})
