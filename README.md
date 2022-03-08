# campd-maps-graphs
These R Shiny applications are for the Maps & Graphs section in CAMPD.

## Getting started
Make sure git and RStudio are installed on your machine. 
- Open RStudio 
- *File > New Project* Do you see an option to create from Version Control?
- Select *Version Control*
- Select *Git*
- Enter repo URL, make a directory name, and create project as subdirectory with something like "C:/Users/name/campd-maps-graphs"

## Run App Locally
- Use shiny::runApp(appfold) in app.R (top dir)

## Deploying to Cloud.Gov
- Use the build-repo.R to populate the vendor_r directory
- If using leaflet, you'll need to clone (with permissions) from https://github.com/testorg-2020/cflinuxfs3-CRAN
