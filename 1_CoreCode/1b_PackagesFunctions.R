
# Add all Packages used here.  Below are some of the ones grouped by how frequently I use them.

## Frequent
library(tidyr)
library(digest) # Required for de-identification
library(dplyr)
library(lme4)
library(psych)
library(ggplot2)

## Common
library(Hmisc)
library(xtable)
library(lmerTest)
library(nFactors)

## Others


# Add any user-generated libraries of functions


# And put any functions below

## ksource - REQUIRED to pull out r code without knitting
### Found at http://stackoverflow.com/questions/10966109/how-to-source-r-markdown-file-like-sourcemyfile-r
### By author http://stackoverflow.com/users/559676/yihui
ksource = function(x, ...) {
  library(knitr)
  source(purl(x, output = tempfile()), ...)
}

# Other common variables

## I sometimes put cutoff dates, or project info variables to easily refer to here.