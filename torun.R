rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
folders_to_clean<-c("Codelist")
for(folder_ind in folders_to_clean){
  folder_path<-paste0(projectFolder, "/", folder_ind)
  if (dir.exists(folder_path)) {
    unlink(folder_path, recursive = F, force = TRUE)
    message("Folder deleted successfully.")
  } else {
    message("Folder does not exist.")
  }
}
path_to_app_directory<-paste0(projectFolder, "/app.R")
runApp(path_to_app_directory)

