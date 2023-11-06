

if(exists("RmdTesting")){
  require(knitr)
  if(RmdTesting) {
    opts_template$set(
      RmdSkip = list(include = TRUE),
      RmdSetup = list(include = TRUE),
      RmdInclude = list(include = TRUE),
      RmdIncludeCode = list(include = TRUE),
      RmdCodeOnly = list(include = TRUE),
      RmdTable = list(include = TRUE, results = "asis")
    )
  } else {
    opts_template$set(
      RmdSkip = list(include = FALSE, eval = FALSE),
      RmdSetup = list(include = FALSE, warning = FALSE, message = FALSE),
      RmdInclude = list(include = TRUE, warning = FALSE, message = FALSE, echo = FALSE),
      RmdIncludeCode = list(include = TRUE, warning = FALSE, message = FALSE, echo = TRUE),
      RmdCodeOnly = list(include = FALSE, warning = FALSE, message = FALSE, echo = TRUE),
      RmdTable = list(include = TRUE, warning = FALSE, message = FALSE, echo = FALSE, results = "asis")
    )
  }
}
