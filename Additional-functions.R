
# 
se <- function(x) sd(x, na.rm=T)/sqrt(length(x[!is.na(x)]))

smart_cast_numeric <- function(DF){
  numeric_DF <- DF
  for(i in 1:ncol(DF)){
    numeric_DF[,i] <- all.is.numeric(DF[,i], what = c( "vector"), extras=c('.','NA','',NA,NaN))
  }
  return(numeric_DF)
}

make_scale <- function( DF , scale ){
  items <- names( DF )
  #  check if any columns are legitimately non-numeric (can't be turned to scales)
  non_numeric <- unlist( lapply( items , function(x) { not_numeric( DF[ , x ] ) } ) )
  if( TRUE %in% non_numeric ){
    warning_text <- paste( "\nScale" , scale , "has non numeric values" )
    warning( warning_text )
    anomalies[[scale]] <<- warning_text
    vec   <- NULL
    alpha   <- NULL
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


#  is this a numeric vector? (excluding NAs and spaces)
not_numeric <- function( vec ){
  vec <- vec[ ! vec %in% c( NA , "" ) ]
  return( any( is.na( as.numeric( vec ) ) ) )
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
ksource = function(x, ...) {
  library(knitr)
  source(purl(x, output = tempfile()), ...)
}

## Set up ggplots
dodge <- position_dodge(width=0.9)

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

pre_y <- NA
pre_z <- NA
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

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
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

mediation_bootstrap = function(x, med, y, iterations = 1000){
  
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
  
  #return(boot_ab)
}

strip_special_characters = function(field){
  return(gsub("[^0-9A-Za-z]","",field))
}

expand_i <- function(x,n){
  c(x,x+n,x+2*n)
}

## Graphing shortcuts

rect_left <- c(-9,-2,5)
rectangles <- data.frame(
  xmin = c(-9,-2,5),
  xmax = c(-6,2,8), # Extra one in middle for labor day
  ymin = 0,
  ymax = Inf
)

dg_gray <- geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            fill='gray95', alpha=0.8)
  dg_to_days <- scale_x_continuous(breaks = c(-7,0,7,14), labels = c("Aug. 31st","Sept. 6th","Sept. 13th","Sept. 20th"))