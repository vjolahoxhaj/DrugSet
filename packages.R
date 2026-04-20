# Function to handle package installation and loading
load_or_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, repos = "http://cran.us.r-project.org")
      library(pkg, character.only = TRUE, quietly = TRUE)
    }
  }
}

# Function to install packages from GitHub if needed
load_or_install_github <- function(packages) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  for (pkg in names(packages)) {
    if (!require(pkg, character.only = TRUE)) {
      remotes::install_github(packages[pkg])
      library(pkg, character.only = TRUE, quietly = TRUE)
    }
  }
}

# Base CRAN Packages
cran_packages <- c(
  "stringr", "data.table", "DT", "shiny", "shinyjs", 
  "rstudioapi", "readxl", "htmltools", "utils", "tinytex", "shinyscreenshot"
)

# GitHub Packages
github_packages <- c("rmarkdown" = "rstudio/rmarkdown")

# Install and load CRAN packages
load_or_install(cran_packages)

# Install and load GitHub packages
load_or_install_github(github_packages)




