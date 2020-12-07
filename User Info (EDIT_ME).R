
################## Project Settings

# NOTE: 0_Header.R should be local, updating information and directories below

# Here we set the author and project name 
AuthorName <- "Eric N. Smith"
AuthorAffiliation <- "University of Texas at Austin"
ProjectName <- "Svelte Analysis Template"
ProjectKey <- "Svelte-Analysis"
UseLocalSupportFolder <- "FALSE" # Set to TRUE if you want to prioritize local files for loading packages and functions. Set to FALSE if you would like to use most recent versions on https://github.com/ericnatsmith/Svelte-Analysis

# Here we'll want to set up where files will be loaded from and saved.

project_dir <- "~/" # Root folder where this RStudio project will go
identified_data_dir <- "~/Box/Identifiable/" # Where your raw data files will be coming from
deidentified_data_dir <- "~/Box/Deidentified/" # Where to save de-identified data to run analyses on
paper_dir <- "~/Google Drive/Smithwise/Psychology/In Progress Studies/"

# By default, all folders will be named with the ProjectKey assigned above, but you can overwrite as needed

project_folder <- paste0(project_dir,ProjectKey,"/") # Root folder of this project
identified_data_folder <- paste0(identified_data_dir,ProjectKey,"/") # Where your raw data files will be coming from
deidentified_data_folder <- paste0(deidentified_data_dir,ProjectKey,"/") # Where to save de-identified data to run analyses on
paper_folder <- "~/Google Drive/Smithwise/Psychology/In Progress Studies/"

temporary_folder <- "~/Downloads/" # Where you want temporary output to go



