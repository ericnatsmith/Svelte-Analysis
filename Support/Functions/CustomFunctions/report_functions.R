
## set intutive table names to replace variable names in your table
fix_names <- function(x){
  x %>%
    str_replace("orig_1", "New Wording 1") %>%
    str_replace("orig_2", "New Wording 2") %>%
    str_replace_all(":", " x ")
}


# Easy change to percent
as.pcnt <- function(x){paste0(x*100,"%")}

# Remove 0 before decimal for rs, alphas, etc.
rm_0 <- function(x){sub("^0","",as.character(x))}

# assorted custom functions  -------------------------------------------------

format_pval <- function(x){
  if (is.na(x) ) return(x)
  if (x < .001) return(paste('<', '.001'))
  if (x > .1) return(paste('=', rm_0(round(x,2))))
  paste('=', rm_0(round(x,3)))   # 3 = no. of digits to round p value to if .001 < p < .250.
}

# # examples:
# .0001  %>%  format_pval
# .016  %>%  format_pval
# .350  %>%  format_pval

r_pval <- function(m){
  # create table of stats from summary of model
  x <- m$p.value
  # format the output
  format_pval(x)
}

reg_rp <- function(m){
  paste0('*r*(', m$parameter, ') = ', broman::myround(m$estimate,2),
         ', *p* ',  r_pval(m) 
  )
}

# # Example
# m <- with(mtcars,cor.test(mpg, wt))
# reg_rp(m)
#

as.mystars <- function(p) {
  ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
}

corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

corstarsl2 <- function(y,z){ 
  require(Hmisc) 
  x <- as.matrix(cbind(y,z)) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove last column and return the matrix (which is now a data frame)
  a <- ncol(y)
  b <- ncol(z)
  Rnew <- Rnew[(a+1):(a+b),1:a]
  return(Rnew) 
}

## Make a partial correlation table
cor_w_sch <- function(x){cor.test(x,d$s1.rel.school)$estimate}
cor_w_sch_p <- function(x){cor.test(x,d$s1.rel.school)$p.value}

cor_partial <- function(df,item_main,x,items_controlled,output="estimate"){
  mydata <- df[,c(item_main,items_controlled)]
  mydata <- cbind(x,mydata) # Add raw data column to data
  
  # See if columns are duplicated (can't get name from summarize_each)
  mydata <- mydata[!duplicated(lapply(mydata, digest))] # And delete if same
  
  mydata <- mydata[complete.cases(mydata),]
  if(output == "pval"){pcor(mydata)$p.value[item_main,"x"]} else {
    pcor(mydata)$estimate[item_main,"x"]
  }
}
cor_partial_p <- function(df,item_main,x,items_controlled,output="pval") {
  cor_partial(df,item_main,x,items_controlled,output="pval")
}

make_pcor_table <- 

dim_change <- function(x,init=F) {
  y <- nrow(x)
  z <- ncol(x)
  if(init==F){
    if(y < pre_y){ message(paste("The df has",pre_y - y,"less rows\n"))}
    if(z < pre_z){ message(paste("The df has",pre_z - z,"less cols\n"))}
  }
  assign("pre_y",y, envir=.GlobalEnv)
  assign("pre_z",z, envir=.GlobalEnv)
  message(paste("The df has",y,"rows and",z,"columns"))
}

strip_special_characters = function(field){
  return(gsub("[^0-9A-Za-z]","",field))
}

ksource = function(x, ...) {
  library(knitr)
  source(purl(x, output = tempfile()), ...)
}

bval <- function(m, v, stat="Estimate"){
      # create table of stats from summary of model
      cm <- m %>% summary() %>% coef()
      # pull the stat you need from the table
      x <- cm[v, stat]
      # round that stat to two decimal places
      broman::myround(x, 2)
}

tval <- function(m, v, stat="t value"){
      # create table of stats from summary of model
      cm <- m %>% summary() %>% coef()
      # pull the stat you need from the table
      x <- cm[v, stat]
      # round that stat to two decimal places
      broman::myround(x, 2)
}

pval <- function(m, v, stat="Pr(>|t|)", raw=FALSE){
      # create table of stats from summary of model
      cm <- m %>% summary() %>% coef()
      # pull the stat you need from the table
      x <- cm[v, stat]
      if(raw) {
        x
      } else {
        format_pval(x)
      }
}

# # Examples
#     m <- lm(mpg ~ qsec, mtcars)
#     bval(m, "qsec")
#     tval(m, "qsec")
#     pval(m, "qsec")
#     m$df

reg_btp <- function(m, v, beta="b"){
  
  paste0(beta,' = ', bval(m, v),
         ', *t*(', m$df, ') = ', tval(m, v),
         ', *p* ',  pval(m, v)
         )
}
# 
# # Example
#     m <- lm(mpg ~ wt, mtcars)
#     reg_btp(m, 'wt')

reg_fp <- function(m, v = NULL, compare = F, row = nrow(m)){
  if(compare) {
    paste0('F(',m$Df[row],', ',m$Res.Df[row],') = ', round(m$F[row],2),
           ', *p* ',  format_pval(round(m$"Pr(>F)"[row],2)))
  } else {

    paste0('F(',m$Df[1],', ',m$Df[row],') = ', round(m$"F value"[1],2),
           ', *p* ',  format_pval(round(m$"Pr(>F)"[1],2)))
  }
}
# 
# # Example
#     model1 <- lm(mpg ~ as.factor(cyl), mtcars)
#     model2 <- lm(mpg ~ as.factor(cyl)*wt, mtcars)
#     m1 <- anova(model1)
#     m2 <- anova(model1,model2)
#     reg_fp(m1, compare=F)
#     reg_fp(m2, compare=T)

# ########################################################################
# ########################################################################
#
# ## Table and figure numbering functions:   ----------------------------------------------------------
# # <https://rmflight.github.io/posts/2012/10/papersinRmd.html>
incCount <- function(inObj, useName) {
    nObj <- length(inObj)
    useNum <- max(inObj) + 1
    inObj <- c(inObj, useNum)
    names(inObj)[nObj + 1] <- useName
    inObj
}
    # set fidure and table counts to zero
    figCount <- c(`_` = 0)
    tableCount <- c(`_` = 0)


pasteLabel <- function(preText, inObj, objName, insLink = TRUE) {
    objNum <- inObj[objName]

    useText <- paste(preText, objNum, sep = " ")
    if (insLink) {
        useText <- paste("[", useText, "](#", objName, ")", sep = "")
    }
    useText
}

# # EXAMPLE
# # tableCount <- incCount(tableCount, "t.blogPostDocs"); tableCount
# # in body of text you'd write `r I(pasteLabel("Table", tableCount, "t.blogPostDocs"))`
#

# # inline stat pulling functions for regression models  ----------------------------------------------------------

#
# # table Formatting functions -------------------------------------------------
#
# # subfunctions used by all table formatting functions ------------------------
#

# # fix Bar Plot names----
# bp_relabel <- function(x){
#   x %>%
#   str_replace("auto", "Automatic\nTrans") %>%
#   str_replace("manual", "Manual\nTrans") %>%
#   str_replace("Weak", "Weak\nEvidence") %>%
#   str_replace("Strong", "Strong\nEvidence") %>%
#   str_replace("Affirm", "Self-\nAffirmation") %>%
#   str_replace("Bas_Rt", "Basic\nRight\nFraming") %>%
#   str_replace("Precom", "Precommit\nto Criteria") %>%
#   str_replace("Tchr", "Teacher\nRetraining")
# }
#
# ## table_format_pval will reformat long p-values
# table_format_pval <- function(x){
#   if (is.na(x)) return('NA')
#   if (x < .001) return(paste('<', '.001') )
#   if (x > .250) return(paste('>', '.250') )
#   myround(x, 3) %>% str_replace("0.", ".") # remove leading zero
# }
#
# # Table Formatting functions   -------------------------------------------------
# print_reg_table <- function(x){
#       x %>%
#       tidy %>%
#       # reformat all p-values in table
#       mutate(p.value = lapply(p.value, table_format_pval)) %>%
#       # reduce all stats to 2 decimal places
#       mutate_each( funs(. %>% myround(digits = 2)),
#                    -term, -p.value) %>% # "-" means to exclude these cols
#       # fix names for variable column called "term"
#       mutate(term = fix_names(term)) %>%
#       set_colnames( c("Parameter", "Estimate", "SE", "_t_", "_p_") ) %>%
#       kable(align = "r")
#     }
#
#     ## Example of regression table
#     m <- lm(mpg ~ wt * cyl, mtcars)
#     print_reg_table(m)
#
# print_anova_table <- function(x){
#       x <- x %>%
#       tidy %>%
#       mutate(p.value = lapply(p.value, table_format_pval)) %>%
#       mutate_each( funs( . %>% myround(digits = 2)),
#                    -term, -p.value) %>% # "-" means to exclude these cols
#       mutate(term = fix_names(term))
#       # Remove NAs that would appear in the final table
#       x$statistic [x$term == "Residuals"] <- ""
#       x$p.value [x$term == "Residuals"] <- ""
#       # Clean up column names
#       x %>%
#       set_colnames( c("Parameter", "df", "SS", "MSE", "F", "_p_") ) %>%
#       kable(align = "r")
# }
#
#     # Example of anova table:
#     am <- anova(lm(mpg ~ wt * cyl, mtcars))
#     print_anova_table(am)
#
# # print summary stat table    --------------------------------------------------
print_ss_table <- function(x,align_val = "r") {
  # fix names of your columns
  colnames(x) <- colnames(x) %>% fix_names
  x %>% kable(align = align_val, digits = 2)
}

#     # example of print_ss_table in use
#     s <- Rmisc::summarySE(mtcars, "mpg", c("cyl", "am"))  # create your table
#     print_ss_table(s)
#
# summstat custom function ----------------------------
    summstat <- function(data, measurevar, groupvars){
      # create subset of complete cases
      df_cc <- data %>%
        subset(select = c(measurevar, groupvars)) %>%
        .[complete.cases(.),]
      # apply summarySE to subset of complete cases
      dfc <- df_cc %>%
        Rmisc::summarySE(measurevar, groupvars)
      # great paste function from
      # http://stackoverflow.com/questions/14568662/paste-multiple-columns-together-in-r
      if(length(groupvars) > 1) {
        dfc$predictor <- apply( dfc[ , groupvars ] , 1 , paste , collapse = "\n+\n" )
      } else {
        dfc$predictor <- dfc[[groupvars]]
      }
      dfc$barorder <- seq(1:nrow(dfc))
      dfc$ttl_n <- nrow(df_cc) # for use with (N = ) at top of plot
      dfc
    }

    # ## Example of function in use:
    # dfc <- summstat(mtcars, "mpg", c("vs", "cyl"))
    # 
    # ## View your summary stats
    # dfc %>% subset(select = -c(predictor, barorder)) %>% print_ss_table

# bar_compare function --------------------------------
    bar_compare <- function(data, bar1, bar2, dv, order = NA) {
      if(is.na(order)){
        data$order <- seq(1:nrow(data))
      }
      
      rownames(data) <- data$predictor
      
      # label important stats
      mn1 <- data[bar1, dv]
      mn2 <- data[bar2, dv]

      se1 <- data[bar1, 'se']
      se2 <- data[bar2, 'se']

      sd1 <- data[bar1, 'sd']
      sd2 <- data[bar2, 'sd']

      MSE <- ( sd1^2 + sd2^2 ) / 2

      n1 <- data[bar1, 'N']
      n2 <- data[bar2, 'N']

      t <- (mn1 - mn2) /
        ( sqrt( MSE*( 1/n1 + 1/n2 ) ) )

      # convert t-statistic to p-value
      df <- (n1 + n2) - 2 # I deducted 2 df b/c we calculated 2 means
      p <- 2*pt(-abs(t), df)

      # create dataframe of useful stats
      term <- c('t', 'p', 'df', 'mn1', 'mn2', 'se1', 'se2')
      stat <- c(t, p, df, mn1, mn2, se1, se2)
      bs <- rbind.data.frame(term, stat)
      colnames(bs) <- as.character( unlist(bs[1,]) )
      bs = bs[-1, ]
      # make values in your new df numeric
      bs[] <- lapply(bs, function(x) {
        if( is.factor(x) )
          as.numeric( as.character(x) )
        else x
      })
      # add non-numeric values to your dataframe
      bs$bar1 <- paste0(bar1)
      bs$bar2 <- paste0(bar2)
      # set a default x- & y-values for annotating your pvalue in a later plot
      bs$y <- with(bs, max(mn1, mn2) + 1.5*(with( bs, max(se1, se2))))
      bs$x <- data[bs$bar1,'order'] + .5*(data[bs$bar2,'order'] - data[bs$bar1,'order'])
      bs
    }

# bar_compare function in use ----------------------------------

# # SAVE YOUR SUMMARY STATS AS OBJECT: dq
# dq <- summstat(mtcars, 'mpg', c('am', 'cyl'))
# 
# # Bar plot
# g <- ggplot(data=dq, aes(x=predictor, y=mpg)) +
#     geom_bar(aes(fill=am),
#              position=position_dodge(),
#              stat="identity", colour="black", size=0) +
#     geom_errorbar(aes(ymin=mpg-se, ymax=mpg+se),
#                   size=.4, width=.1, position=position_dodge(9)) +
#     xlab("\n(Error bars represent standard errors)") +
#     ylab("\nMiles per Gallon\n") +
#     ggtitle( paste0("Interaction of Transmission Type and Country of Origin (N = ", sum(dq$N), ")") ) +
#     coord_cartesian(ylim=c(10, 30)) +
#     report_bar_theme
# 
# # FIRST RENAME YOUR ROWS FOR EASY STAT CALLING
# # rename rows for easy stat calls
# rownames(dq) <- dq$predictor
# dq$order <- seq(1:nrow(dq))
# 
# bs <- bar_compare(data = dq, bar1 = '0\n+\n4', bar2 = '0\n+\n8', dv = 'mpg')
# 
# # automatically add horizontal bar
#     pval_line <- annotate("errorbarh",              # useful shape for this task.
#                           xmin=bs$bar1,             # start point
#                           xmax=bs$bar2,             # end point
#                           x=1,                      # 1 is a place holder
#                           y=bs$y,                   # height of your bar.
#                           height=.5                 # height of error bar "fins"
#     )
# 
# # automatically add the annotation to the plot
#     pval_text <- annotate("text",
#                           # x-axis dim for text:
#                           x=bs$x,
#                           y=bs$y + 1, # add space between line and text
#                           label=paste0("p ", format_pval(bs$p)),
#                           size=3
#     )
# 
# # add annotation to plot
#     print(g + pval_line + pval_text)


multi_bar_compare <- function(graph = g, data = dq, dv = 'mid1_depend', order=NA,
                              bars = NA, fins = .1, p_offset = .2, y_offset = rep(0,length(bars)/2)) {
  graph_name <- graph
  graph <- get(graph)
  bars <- matrix(bars,ncol=2,byrow=T)
  for(i in 1:nrow(bars)) {
    bs <- bar_compare(data = data, bar1 = bars[i,1], bar2 = bars[i,2], dv = dv, order=order)
    
    # automatically add horizontal bar
    pval_line <- annotate("errorbarh",              # useful shape for this task.
                          xmin=bs$bar1,             # start point
                          xmax=bs$bar2,             # end point
                          x=1,                      # 1 is a place holder
                          y=bs$y + y_offset[i],                   # height of your bar.
                          height= fins                 # height of error bar "fins"
    )
    
    # automatically add the annotation to the plot
    pval_text <- annotate("text",
                          # x-axis dim for text:
                          x=bs$x,
                          y=bs$y + p_offset + y_offset[i], # add space between line and text
                          label=paste0("p ", format_pval(bs$p)),
                          size=3
    )
    
    graph <- graph + pval_line + pval_text
    assign(graph_name, graph, envir = .GlobalEnv)
  }
  
}
                                                  
  
  

