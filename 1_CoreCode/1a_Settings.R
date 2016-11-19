

# Here we set the author and project name 
AuthorName <- "Eric N. Smith"
AuthorAffiliation <- "Stanford University"
ProjectName <- "Svelte Analysis Template"

# Here we'll want to set up where files will be loaded from and saved.

project_folder <- "~/Svelte-Analysis" # Root folder of this project
deidentified_data_folder <- "path_to_deidentified_data_folder" # Where your main data files will be coming from
identified_data_folder <- "path_to_identified_data_folder" # Where to save deidentieid data to run analyses on


################## Knitr Settings


# Options for easy changing of what is visible when knitted. This is set up so that the more general knitting option overwrites the more specific.

rmd_all <- T # Used for all analyses you usually don't want to see output from.
rmd_main <- T | rmd_all # Used for main analyses and extra infromation
rmd_core <- T | rmd_main | rmd_all # Used for core analyses without extra information
rmd_cache <- F # Set to T within the file to cache while doing analyses

# global chunk options
library("knitr")
opts_chunk$set(cache=F, autodep=TRUE)