
################## Project Settings

# Here we set the author and project name 
AuthorName <- "Eric N. Smith"
AuthorAffiliation <- "University of Texas at Austin"
ProjectName <- "Svelte Analysis Template"
ProjectKey <- "Svelte"

# Here we'll want to set up where files will be loaded from and saved.

project_folder <- "~/Svelte-Analysis/" # Root folder of this project
identified_data_folder <- "path_to_identified_data_folder/" # Where your raw data files will be coming from
deidentified_data_folder <- "path_to_deidentified_data_folder/" # Where to save deidentified data to run analyses on

temporary_folder <- "~/Downloads/"

svelte_folder <- "~/Svelte-Analysis/Support/Functions/" # Only needed if running locally

local_svelte <- TRUE # If TRUE, it will attempt to load locally first; If FALSE, it will default to loading the most up-to-date remote files


################## Knitr Settings
# global chunk options
library("knitr")
opts_chunk$set(cache=F, autodep=FALSE)

################## Load local files

if(local_svelte) {
# Load the packages needed
  source(paste0(svelte_folder,"PackagesFunctions.R")) 
# Get all .R files in scripts folder
  for(file in list.files(paste0(svelte_folder,"CustomFunctions/"),pattern=".R$")) {
    source(paste0(svelte_folder,"CustomFunctions/",file)) # Load all the files
  }
} else {
  
################## OR remote files
  require(RCurl) # In order to load R code from online

  url_folder <- "https://raw.githubusercontent.com/ericnatsmith/Svelte-Analysis/master/Support/Functions/" # Clarify the folder
  script <- getURL(paste0(url_folder,"PackagesFunctions.R"), ssl.verifypeer = FALSE)
  eval(parse(text = script))
  
  # Load all support R functions
  files <-c("ggplot_plots.R","report_functions.R","analysis_functions.R","mediation_plot_analysis.R","ggplot_themes.R") # Get all .R files needed
  for(file in files){ 
    script <- getURL(paste0(url_folder,"CustomFunctions/",file), ssl.verifypeer = FALSE)
    eval(parse(text = script))
  }
}

