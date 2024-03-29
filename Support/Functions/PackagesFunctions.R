# TITLE: Load Packages and Functions
# Acknowledgments to Joe Powers and Dave Paunesku

# packages ----------------------------------------------------------------
# List of needed packages

package_list <- c(
  'broom', # for tidy
  'broman', # for myround( to control digit length
  'car', # for Anova
  'corrplot', # Easy correlaiton plot
  'cowplot',
  'digest', # for deidentifying data
  #'effects', # for covariate adjusted means
  'ggplot2',
  'grid',
  'gridExtra', # for grid.arrange
  #'gtools',  # for smartbind
  'Hmisc',
  'knitr',
  'lme4',
  #'MASS'
  'lubridate', # easy time manipulations
  'lmerTest',
  'moments', # skew and kurtosis
  'nFactors', # For Screeplots and factor analysis
  #'pander',
  'psych', # for alphas
  'ppcor',
  'reshape2',   ### SHOULD BE REPLACED BY DPLYR FUNCTIONS
  'Rmisc',   # for summarySE AND multiplot
  #'splitstackshape', # for text to columns
  'sjstats',
  'stringr',
  'tidyr',
  'xtable',
  'dplyr' # order is important
)


## Install all needed packages that are not already present
### create a list of packages you've not yet installed
new_packages <- package_list[!(package_list  %in%  installed.packages()[,"Package"])]
### install those packages
if( length(new_packages) )  install.packages(new_packages)

## load all packages
lapply(package_list, library, character.only = TRUE)
