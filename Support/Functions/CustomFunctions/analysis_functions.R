########################################################################
########################################################################
scale_info <- list()
alphas <- list()
anomalies <- list()

escale <- function(x) {
  temp <- scale(x)
  attr(temp,"scaled:center") <- NULL
  attr(temp,"scaled:scale") <- NULL
  as.vector(temp)
  }

mcsd <- function(data, x) {
  # PRECAUTION: stop unless all names vars in x are in data
  stopifnot( x %in% names(data) )
  # custom function to be used within mcsd function
          mcsd_int <- function(x) {
              MC <- x - mean(x, na.rm=T)
              SD <- sd(MC, na.rm=T)
              data.frame(MC = MC, MCmsd = MC - SD, MCpsd = MC + SD)
          }
  # OUTPUT: a new dataframe in this case
  cbind(data, apply(data[x], 2, mcsd_int)) # 2 indicates [i] refers to cols, not rows (1).
}

# # Example of using mcsd
#   mtcars <- mcsd(mtcars, c("wt", "mpg"))
#   names(mtcars)


moveme <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}

## df <- moveme(df, c("b", "c"))
## df <- moveme(df, c("b", "c"), "first")
## df <- moveme(df, c("b", "c"), "before", "e") # will move b and c before e.
## df <- moveme(df, c("b", "c"), "after", "e")

se <- function(x) sd(x, na.rm=T)/sqrt(length(x[!is.na(x)]))

smart_cast_numeric <- function(DF){
  numeric_DF <- DF
  for(i in 1:ncol(DF)){
    numeric_DF[,i] <- all.is.numeric(DF[,i], what = c( "vector"), extras=c('.','NA','',NA))
  }
  return(numeric_DF)
}

#	cast character numerics to numeric and factors to character
smart_cast_all <- function(DF){
  DF <- smart_cast_numeric(DF)
  for(i in 1:ncol(DF)){
    if(is.factor(DF[,i])){
      DF[,i] <- as.character(DF[,i])
    }
  }
  return(DF)
}

#	quantile split;
#	e.g, split_quantiles =.5 is median split, c=(.25,.5,.75) quartile split
qs <- function( vec, split_quantiles=c(.5), labels=c() ){
  split_values <- quantile( vec, split_quantiles, na.rm=TRUE )
  quantile_vec <- NA

  #	if names for quantiles were not passed in, auto-generate
  if( length( labels ) == 0 ){
    labels[ 1 ] <- "above 0"
    for( i in 1:length(split_quantiles) ){
      labels[ i+1 ] <- paste("above", split_quantiles[i])
    }
  }else{
    if( length( labels ) != length( split_quantiles ) + 1 ){
      stop("There should be 1 fewer split quantiles than labels")
    }
  }

  quantile_vec[ !is.na( vec ) ] <- labels[1]
  for( i in 1:length(split_quantiles) ){
    quantile_vec[ vec > split_values[i] ] <- labels[ i + 1]
  }
  #	return as factor with levels explicitly set in correct order
  quantile_vec <- factor( quantile_vec, levels = labels )
  return( quantile_vec )
}

stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data=fun, colour="black", geom=geom, width=.5,  ...)
}


#  is this a numeric vector? (excluding NAs and spaces)
not_numeric <- function( vec ){
  vec <- vec[ ! vec %in% c( NA , "" ) ]
  return( any( is.na( as.numeric( vec ) ) ) )
}

#  returns the mean scale value and the Cronbach alpha report
make_scale <- function( DF , scale ){
  items <- names( DF )
  #  check if any columns are legitimately non-numeric (can't be turned to scales)
  non_numeric <- unlist( lapply( items , function(x) { not_numeric( DF[ , x ] ) } ) )
  if( TRUE %in% non_numeric ){
    warning_text <- paste( "\nScale" , scale , "has non numeric values" )
    warning( warning_text )
    anomalies[[scale]] <<- warning_text
    vec 	<- NULL
    alpha 	<- NULL
  }
  else{
    #	convert to numeric
    lapply( items , function(x) { DF[ , x ] <<- as.numeric( DF[ , x ] ) } )
    tryCatch( alpha	<- psych::alpha( DF[ , items ] ), error = function(e){
      anomalies[[scale]] <<- paste( "Scale",scale,"had errors!" )
    })
    vec		<- rowMeans( DF[ , items ] , na.rm=TRUE )
  }
  return( list( vec=vec, alpha=alpha ) )
}

# returns z scale for those with differing scale means
make_scale_z <- function( DF , scale ){
  items <- names( DF )
  #  check if any columns are legitimately non-numeric (can't be turned to scales)
  non_numeric <- unlist( lapply( items , function(x) { not_numeric( DF[ , x ] ) } ) )
  if( TRUE %in% non_numeric ){
    warning_text <- paste( "\nScale" , scale , "has non numeric values" )
    warning( warning_text )
    anomalies[[scale]] <<- warning_text
    vec   <- NULL
    alpha 	<- NULL
  }
  else{
    #	convert to numeric
    lapply( items , function(x) { DF[ , x ] <<- as.numeric( DF[ , x ] ) } )
    tryCatch( alpha	<- psych::alpha( DF[ , items ] ), error = function(e){
      anomalies[[scale]] <<- paste( "Scale",scale,"had errors!" )
    })
    vec		<- escale(rowMeans(
      apply(DF[ , items ],2, function(x) {escale(x)}), na.rm=TRUE ))
  }
  return( list( vec=vec, alpha=alpha ) )
}

bm.med<-function(x,med,y) {
  summary(lm(y~x))$coefficients[2,1]->c;
  summary(lm(y~x))$coefficients[2,4]->sigc;
  summary(lm(med~x))$coefficients[2,1]->a;
  summary(lm(med~x))$coefficients[2,2]->sa;
  summary(lm(med~x))$coefficients[2,4]->siga;
  summary(lm(y~x+med))$coefficients[2,1]->cprime;
  summary(lm(y~x+med))$coefficients[2,4]->sigcprime;
  summary(lm(y~x+med))$coefficients[3,1]->b;
  summary(lm(y~x+med))$coefficients[3,2]->sb;
  summary(lm(y~x+med))$coefficients[3,4]->sigb;
  sobelsab<-sqrt(b^2*sa^2+a^2*sb^2+sa^2*sb^2);
  sobelz<-abs(a*b)/sobelsab;
  goodmansab<-sqrt(b^2*sa^2+a^2*sb^2-sa^2*sb^2);
  goodmanz<-abs(a*b)/goodmansab;
  round(rbind(c(c=c, "c'"=cprime,a=a,b=b,ab=a*b,Sobel=sobelz,Goodman=goodmanz),c(sigc,sigcprime,siga,sigb,NA,2*(1-pnorm(sobelz)),2*(1-pnorm(goodmanz)))), 3)->output_table;
  rownames(output_table)<-c("Coeff","p val");
  print(output_table);
}

mediation_bootstrap = function(x, med, y, iterations = 1000, return = F){

  # setup some parameters
  N = length(x)
  df = as.data.frame(cbind(x, med, y))
  boot_ab = vector(length=iterations) # set up empty vector for storage

  # now go through a loop where we'll randomly sample, and get a a*b value
  for (i in 1:iterations){
    ind_boot = sample(c(1:N), N, replace=TRUE) # random indices
    df_boot = df[ind_boot,]

    iter_a = lm(df_boot$med ~ df_boot$x)$coefficients[2] # coeff of x
    iter_b = lm(df_boot$y ~ df_boot$med + df_boot$x)$coefficients[2] # coeff of mediator

    boot_ab[i] = iter_a * iter_b
  }

  # create plot
  hist(boot_ab,main=paste("Bootstrapped a*b, with",iterations,"iterations"),col="red");
  abline(v=0, col='black', lty=2, lwd=2)
  abline(v=c(quantile(boot_ab,c(.025,.975))), col='blue', lty=3)

  # Print results
  print("Bootstrap results:",quote=F);
  print(c(ab=mean(boot_ab)));
  print(quantile(boot_ab,c(.025,.975)))

  if(return==T){
    return(boot_ab)
  }
}




# Create costom scales without consistent item coding
## - creates new column and adds item information to scale_info

scale_newcol <- function(df,scaleName,grepItems, same_scale = T) {
  item_list <- names(df)[c(grep(grepItems,names(d)))] # Get list of scale items
  n_items <- length(item_list)
  # Aggregate pertinent info
  scale_info[[scaleName]][["alpha"]] <<- list(alpha(as.data.frame(df[,item_list]),use="pairwise.complete.obs"))
  scale_info[[scaleName]][["cor"]] <<- cor(df[,item_list],use="pairwise.complete.obs")
  if(same_scale == T) {
    temp_vec <- make_scale(df[,item_list])$vec
    alphas[[scaleName]] <<- alpha(df[,item_list])
  } else {
    temp_vec <- as.vector(make_scale_z(df[,item_list])$vec)
  }
  # Get splits
  d[,scaleName] <<- temp_vec
  d[,paste0(scaleName,"_ms")] <<- qs( temp_vec, c(.5), c("Bottom","Top") )
  d[,paste0(scaleName,"_ts")] <<- qs( temp_vec, c(.33,.66), c("Bottom","Middle","Top") )
  d[,paste0(scaleName,"_z")] <<- escale( temp_vec )

  print(paste0("Created column ", scaleName, " with ",n_items," items:"))

  print(item_list)
}


auto_scale <- function(scale_bases = NULL, suffix="_[0-9]") {
  for( scale in scale_bases){
    #  get all items that start with the scale name after s1, s2, etc.
    items  <- names(d)[grep( paste0( "^",scale, suffix ),names(d))]
    if( length( items ) > 0 ){
      temp <- make_scale( d[ , items ], scale )
      scale_info[[scale]][["alpha"]] <<- list(alpha(as.data.frame(d[,items]), use="pairwise.complete.obs"))
      scale_info[[scale]][["cor"]] <<- cor(d[,items],use="pairwise.complete.obs")
      temp_vec <- temp[["vec"]]
      d[,paste0(scale)] <<- temp_vec
      d[,paste0(scale,"_ms")] <<- qs( temp_vec, c(.5), c("Bottom","Top") )
      d[,paste0(scale,"_ts")] <<- qs( temp_vec, c(.33,.66), c("Bottom","Middle","Top") )
      d[,paste0(scale,"_z")] <<- escale( temp_vec )
      alphas[[scale]] <<- alpha(d[,items])
    } else{
      anomalies[[scale]] <- "was empty"
    }
  }
  assign("scale_info",scale_info)
}

# merging surveys in which the participant restarted survey
merge_restarts <- function(df, id_col, early_id, late_id) {
  df <- df
  early_row <- df[[id_col]] == early_id
  early_missing <- is.na( df[early_row,]) | df[early_row,] == "" | df[early_row,] == " "
  late_row <- df[[id_col]] == late_id
  df[early_row,early_missing] <- # Get missing of early row
    df[late_row,early_missing] # Replace with later of late row

  df <- df %>%
    filter_(paste0(id_col," != \'", late_id,"\'")) # delete later row

  return(df)
}

# Mutate in dplyr based on a condition
## Taken from https://stackoverflow.com/users/516548/g-grothendieck
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

## Example
# d <- d %>%
#   mutate_cond(id %in% list, # if id is in list
#               id = paste0(id,"_dup")) # Do something to only those rows

  # threewaycovar <- function(data, dv, ivs, covar) {
  #   stopifnot(all(c(dv, ivs, covar) %in% names(data)))
  #   form <- as.formula(paste(dv,"~", paste(ivs,collapse="*"), "+", paste(covar, collapse="+")))
  #   m <- lm(form, data)
  #   e <- effect(paste(ivs, collapse=":"), m)
  #   e_df <- data.frame(e)
  #   e_df$predictor <- apply(e_df[,(seq_along(ivs))], 1, paste, collapse="\n+\n") # merge     first three columns
  #   ifelse(names(e_df)=="fit", dv, names(e_df))->names(e_df)
  #   keep <- c(ivs, "predictor", dv, "se") # create list of names
  #   d2 <- e_df[,colnames(e_df) %in% keep] # select columns from e_df that match the list keep
  #   # add N and SD to your table
  #   dss <- summstat(data, dv, ivs)
  #   dn <- subset(dss, select = c(predictor, N))
  #   # merge with dq
  #   d2 <- merge(d2, dn, by = 'predictor')
  #   # add SD
  #   d2$sd <- d2$se * sqrt(d2$N)
  #   # apply final order
  #   target <- c(ivs, dv, 'se', 'sd', 'N', 'predictor')
  #   d2 <- d2[order(match(colnames(d2), target))]
  #   # return output
  #   return(d2)
  # }
  #
  #       # example
  #       threewaycovar(data=mtcars, dv='mpg', c('am', 'cyl', 'vs'), covar = 'carb')
  #
  # twowaycovar <- function(data, dv, ivs, covar) {
  #   stopifnot(all(c(dv, ivs, covar) %in% names(data)))
  #   form <- as.formula(paste(dv,"~",
  #                            paste(ivs,collapse="*"),
  #                            "+",
  #                            paste(covar, collapse="+")))
  #   m <- lm(form, data)
  #   e <- effect(paste(ivs, collapse=":"), m) # save cross table
  #   e_df <- data.frame(e)
  #   e_df$predictor <- apply(e_df[,(seq_along(ivs))], 1, paste, collapse="\n+\n") # merge first 2 columns
  #   names(e_df) <- ifelse( names(e_df)=="fit", dv, names(e_df) ) # rename fit as your dv's name
  #   keep <- c(ivs, "predictor", dv, "se") # create list of names
  #   d2 <- e_df[,colnames(e_df) %in% keep] # select cols that match the name-list keep
  #   # add N and SD to your table
  #   dss <- summstat(data, dv, ivs)
  #   dn <- subset(dss, select = c(predictor, N))
  #   # merge with dq
  #   d2 <- merge(d2, dn, by = 'predictor')
  #   # add SD
  #   d2$sd <- d2$se * sqrt(d2$N)
  #   # apply final order
  #   target <- c(ivs, dv, 'se', 'sd', 'N', 'predictor')
  #   d2 <- d2[order(match(colnames(d2), target))]
  #   # return output
  #   return(d2)
  # }
  #
  # # example
  # dp <- twowaycovar(data=mtcars, dv='mpg', c('am', 'cyl'), covar = 'vs'); dp


identify_identifiable_columns <- function(df, threshold = 0.8) {
  # Calculate the proportion of unique values in each column
  unique_prop <- sapply(df, function(col) {
    unique_count <- length(unique(col))
    total_count <- length(col)
    unique_count / total_count
  })

  # Identify columns with unique proportion above the threshold
  identifiable_cols <- names(unique_prop[unique_prop > threshold])

  # Return the identifiable columns
  return(identifiable_cols)
}

deidentify_id <- function(id, salt) {
  # Concatenate the ID and salt
  salted_id <- paste0(id, salt)

  # Hash the salted ID using the SHA-256 algorithm
  hashed_id <- openssl::sha256(salted_id)

  # Trim the hashed ID to 12 characters
  trimmed_hashed_id <- substr(hashed_id, 1, 12)

  # Return the hashed ID
  return(trimmed_hashed_id)
}


get_proper_nouns <- function(df) {

  proper_nouns <- c()

  # Function to process a single text and extract proper nouns
  process_text <- function(text) {
    tokens <- udpipe_annotate(ud_model, x = text)
    tokens_df <- as.data.frame(tokens)

    proper_nouns_indices <- which(tokens_df$upos == "PROPN")
    proper_nouns <<- c(proper_nouns, tokens_df$token[proper_nouns_indices])
  }

  # Apply the function to each column of the dataframe
  for (col in colnames(df)) {
    if (is.character(df[[col]])) {
      text <- unlist(df[[col]])
      process_text(text)
    }
  }

  # Count the frequency of proper nouns
  proper_nouns_freq <- table(proper_nouns)

  # Sort proper nouns by frequency in descending order
  proper_nouns_sorted <- sort(proper_nouns_freq, decreasing = TRUE)

  return(proper_nouns_sorted)
}


remove_proper_nouns <- function(text, proper_nouns) {
  # require(udpipe)
  #
  # if(!exists("ud_model", envir = environment(remove_proper_nouns))){ #if not already loaded
  #   # Load the English model for udpipe
  #   ud_model <- udpipe_download_model(language = "english")
  #   ud_model <- udpipe_load_model(ud_model$file_model)
  # }
  # Tokenize the text using udpipe
  if(!is.na(text)) {
    tokens <- udpipe_annotate(ud_model, x = text)
    tokens_df <- as.data.frame(tokens)
    # Replace proper nouns with "[NAME]" and concatenate the remaining tokens
    tokens_df$filtered_tokens <- ifelse(tokens_df$token %in% proper_nouns, "[NAME]", tokens_df$token)
    # Replace multiple consecutive instances of "[NAME]" with just one "[NAME]"
    tokens_df$filtered_tokens[is.na(tokens_df$filtered_tokens)] <- NULL
    if(length(tokens_df$filtered_tokens) > 1) {
      for (i in length(tokens_df$filtered_tokens):2) {
        if (tokens_df$filtered_tokens[i] == "[NAME]" && tokens_df$filtered_tokens[i-1] == "[NAME]") {
          tokens_df$filtered_tokens[i] <- NA
        }
      }
    }
    new_text <- gsub("\\s+\\.", ".", paste0(tokens_df$filtered_tokens[!is.na(tokens_df$filtered_tokens)], collapse = " "))
    return(new_text)
  } else (
    return(text)
  )
}



#
#   new_text <-character()
#   # Remove any trailing space before a period
#   for(doc_N in unique(tokens_df$doc_id)) {
#     new_text[1] <- tokens_df %>% filter(doc_id == doc_N) %>%
#
#
identify_character_columns <- function(df) {
  # Initialize an empty vector to store the open-response column names
  open_response_cols <- c()
  # Loop through each column in the data frame
  for (col_name in names(df)) {
    # Check if the column contains character strings using is.character()
    if (is.character(df[[col_name]])) {
      # If it does, add the column name to the list of open-response columns
      open_response_cols <- c(open_response_cols, col_name)
    }
  }
  # Return the list of open-response column names
  return(open_response_cols)
}




find_potential_identifiers <- function(dataset, indirect_identifiers) {
  # Initialize an empty list to store potential identifiers
  potential_identifiers <- list()

  # Convert dataset to a tibble
  dataset <- as_tibble(dataset)

  # Iterate over each indirect identifier
  # Group the dataset by the current indirect identifier
  grouped_data <- dataset %>%
    group_by_at(vars(indirect_identifiers)) %>%
    summarise(count = n(), .groups = "drop")

  # Find groups with less than 5 individuals
  potential_groups <- grouped_data[grouped_data$count < 5, ]

  # Return the list of potential identifiers
  return(potential_groups)
}



# # Sample dataset
# # Sample dataset
# dataset <- data.frame(
#   Name = c("John", "Jane", "Alice", "Bob", "Mark", "Emily", "David", "Sarah", "Michael", "Jessica",
#            "Daniel", "Olivia", "Matthew", "Sophia", "Andrew", "Ava", "William", "Emma", "James", "Mia",
#            "Joseph", "Isabella", "Joshua", "Abigail", "Christopher", "Emily", "David", "Elizabeth", "Daniel",
#            "Avery", "Benjamin", "Sofia", "Samuel", "Chloe", "Henry", "Ella", "Jacob", "Scarlett", "Mason",
#            "Grace", "Ethan", "Victoria", "Alexander", "Zoe", "Emma", "Liam", "Emily", "Noah", "Ava", "Oliver",
#            "Sophia", "Daniel", "Mia", "James", "Olivia", "Lucas", "Amelia", "Benjamin", "Charlotte", "Jacob",
#            "Lily", "William", "Harper", "Alexander", "Evelyn", "Michael", "Madison", "Matthew", "Avery", "Joseph",
#            "Ella", "Henry", "Scarlett", "David", "Chloe", "Christopher", "Victoria", "Andrew", "Zoe", "Joshua",
#            "Grace", "Samuel", "Lily", "Ethan", "Charlotte", "Daniel", "Harper", "Mason", "Evelyn", "Jack"),
#   Age = c(25, 30, 35, 40, 25, 35, 40, 45, 25, 35,
#           50, 55, 50, 55, 50, 55, 60, 65, 60, 65,
#           25, 30, 35, 40, 25, 35, 40, 45, 25, 35,
#           50, 55, 50, 55, 50, 55, 60, 65, 60, 65,
#           25, 30, 35, 40, 25, 35, 40, 45, 25, 35,
#           50, 55, 50, 55, 50, 55, 60, 65, 60, 65,
#           25, 30, 35, 40, 25, 35, 40, 45, 25, 35,
#           50, 55, 50, 55, 50, 55, 60, 65, 60, 65,
#           25, 30, 35, 40, 25, 35, 40, 45, 25, 35),
#   Occupation = c("Engineer", "Doctor", "Lawyer", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer",
#                  "Doctor", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher",
#                  "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer",
#                  "Doctor", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher",
#                  "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer",
#                  "Doctor", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher",
#                  "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer",
#                  "Doctor", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher",
#                  "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer", "Doctor", "Teacher", "Engineer", "Lawyer")
# )
#
# # table(dataset$Age,dataset$Occupation)
#
#
# # Specify the indirect identifiers
# indirect_identifiers <- c("Age", "Occupation")
#
# # Find potential indirect identifiers
# potential_identifiers <- find_potential_identifiers(dataset, indirect_identifiers)
#
