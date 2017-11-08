
################## Project Settings

# Here we set the author and project name 
AuthorName <- "Eric N. Smith"
AuthorAffiliation <- "Stanford University"
ProjectName <- "Svelte Analysis Template"

# Here we'll want to set up where files will be loaded from and saved.

project_folder <- "~/Svelte-Analysis" # Root folder of this project
identified_data_folder <- "path_to_identified_data_folder" # Where your raw data files will be coming from
deidentified_data_folder <- "path_to_deidentified_data_folder" # Where to save deidentieid data to run analyses on

temporary_folder <- "~/Downloads"


################## Knitr Settings


# Options for easy changing of what is visible when knitted. This is set up so that the more general knitting option overwrites the more specific.

rmd_all <- T # Used for all analyses you usually don't want to see output from.
rmd_main <- T | rmd_all # Used for main analyses and extra infromation
rmd_core <- T | rmd_main | rmd_all # Used for core analyses without extra information
rmd_cache <- F # Set to T within the file to cache while doing analyses

# global chunk options
library("knitr")
opts_chunk$set(cache=F, autodep=FALSE)


################## Choose local files
# # You can either use the local files, or load the most up-to-date remote files
# package_script <- "Support/Functions/PackagesFunctions.R"
# all_script_folder <- "Support/Functions/CustomFunctions/"
# 
# source(package_script) # Load the packages needed
# 
# files <- list.files(all_script_folder,pattern=".R$")# Get all .R files in scripts folder
# 
# for(file in files){ 
#   source(paste0(all_script_folder,file)) # Load all the files
# }


################## OR remote files
# You can either use the local files, or load the most up-to-date remote files
require(RCurl) # In order to load R code from online
url_folder <- "https://raw.githubusercontent.com/ericnatsmith/Svelte-Analysis/master/Support/Functions/" # Clarify the folder

init_script <- "PackagesFunctions.R" # Clarify the main script
script <- getURL(paste0(url_folder,init_script), ssl.verifypeer = FALSE)
eval(parse(text = script))

# Load all support R functions
RSupportPath <- paste0(url_folder,"CustomFunctions/")
files <-c("ggplot_plots.R","report_functions.R","analysis_functions.R","mediation_plot_analysis.R","ggplot_themes.R") # Get all .R files needed
for(file in files){ 
  script <- getURL(paste0(RSupportPath,file), ssl.verifypeer = FALSE)
  eval(parse(text = script))
}



