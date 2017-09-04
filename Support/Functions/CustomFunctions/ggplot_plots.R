#	definte the dodge object for equivalent	error bar and geom_bar
dodge <- position_dodge( width=0.9 )

#	define a geom_bar
bars <- geom_bar( stat="summary" , fun.y="mean" , position=dodge )


# To plot line split by factors or SDs for continuous
es_lineplot <- function(df,x,y,id=NA,split=NA, simple=F) {
  if(is.na(id)){
    df$id <- 1:nrow(df)# if no id, assume rowwise
    id <- "id"
  }
  if(is.na(split)) { # If no split, then just graph split by x
    
    if(simple){ # If simple, change continuous x to +/- 1 SD
      data1$x <- escale(data1$x) # scale x
      new.df = data.frame(x=c(-1,1))
      fit <- lm(y  ~ x,data1)
      predictions <- predict(fit,new.df, interval="confidence", level=.68)
      predictions <- cbind(new.df,predictions)
      predictions$x <- as.factor(paste0(predictions$x," SD"))
      agg <- predictions
      names(agg) <- c("x","mean","lower","upper")
      
      ggplot(agg[!is.na(agg$x),],aes(x=x,y=mean)) + 
        geom_point() +
        geom_line(aes(group=1), size=2) + 
        theme_classic() +
        geom_errorbar(aes( ymax=upper, ymin=lower ) ,
                      size=2, width =.25, alpha=.9) +
        labs(x=x,y=y)
      
    } else {
      data1 <- df[,c(id,x,y)]
      names(data1) <- c("id","x","y")
      agg <- data1 %>% group_by(x) %>% 
        summarise(mean = mean(as.numeric(y),na.rm=T), 
                  se = se(as.numeric(y)), 
                  upper = mean + se, 
                  lower = mean - se)
      
      ggplot(agg[!is.na(agg$x),],aes(x=x,y=mean,color=x)) + 
        geom_point() +
        theme_classic() +
        geom_errorbar(aes( ymax=upper, ymin=lower ) ,
                      size=2, width =.25, alpha=.9) +
        labs(x=x,y=y)
    }
  } else { # If has a split, need to separated depending on if numeric
    data1 <- df[,c(id,x,y,split)]
    names(data1) <- c("id","x","y","split")
    
 
    if(!is.numeric(data1[,"split"])){ # if a factor, split by factor
      
      if(simple){ # If simple, change continuous x to +/- 1 SD
        data1$x <- escale(data1$x) # scale x
        new.df = data.frame(split=rep(unique(data1$split), times=2), 
                            x=rep(c(-1,+1), each=length(unique(data1$split))))
        fit <- lm(y  ~ x * split,data1)
        predictions <- predict(fit,new.df, interval="confidence", level=.68)
        predictions <- cbind(new.df,predictions)
        predictions$x <- as.factor(paste0(predictions$x," SD"))
        agg <- predictions
        names(agg) <- c("split","x","mean","lower","upper")
        
      } else { # Simple agg
        
      agg <- data1 %>% group_by(x,split) %>% 
        summarise(mean = mean(as.numeric(y),na.rm=T), 
                  se = se(as.numeric(y)), 
                  upper = mean + se, 
                  lower = mean - se)
      }
      
    } else { # if numeric, split by SD +1 / -1
      if(simple) {
        data1$x <- escale(data1$x) # scale x
        data1$split <- escale(data1$split)
        fit <- lm(y  ~ x * split,data1)
        new.df = data.frame(split=rep(c(-1,+1), times=2), 
                            x=rep(c(-1,+1), each=2))
        
        predictions <- predict(fit,new.df, interval="confidence", level=.68)
        predictions <- cbind(new.df,predictions)
        predictions$split <- as.factor(paste0(predictions$split," SD"))
        agg <- predictions
        names(agg) <- c("split","x","mean","lower","upper")
      } else {
        data1$split <- escale(data1$split)
        fit <- lm(y  ~ x * split,data1)
        new.df = data.frame(split=rep(c(-1,+1), times=length(unique(data1$x))), 
                            x=rep(unique(data1$x), each=2))
        
        predictions <- predict(fit,new.df, interval="confidence", level=.68)
        predictions <- cbind(new.df,predictions)
        predictions$split <- as.factor(paste0(predictions$split," SD"))
        agg <- predictions
        names(agg) <- c("split","x","mean","lower","upper")
      }
    }
    
    
    ggplot(agg[!is.na(agg$x) & !is.na(agg$split),],aes(x=x,y=mean,color=split, group=split)) + 
      geom_point() +
      geom_line(size=1.5) +
      theme_classic() +
      geom_errorbar(aes( ymax=upper, ymin=lower ) ,
                    size=2, width =.25, alpha=.7) +
      labs(x=x,y=y) +
      scale_color_discrete(guide = guide_legend(title = split))
      
  }
}


es_barplot <- function(df,x,y,id=NA,split=NA, keep_agg = F,split_bars = F) {
  if(is.na(id)){
    df$id <- 1:nrow(df)# if no id, assume rowwise
    id <- "id"
  }
  if(is.na(split)) { # If no split, then just graph split by x
    data1 <- df[,c(id,x,y)]
    names(data1) <- c("id","x","y")
    agg <- data1 %>% group_by(x) %>% 
      summarise(mean = mean(as.numeric(y),na.rm=T), 
                se = se(as.numeric(y)), 
                upper = mean + se, 
                lower = mean - se)
    
    g <- ggplot(agg[!is.na(agg$x),],aes(x=x,y=mean,fill=x)) + 
      geom_bar(stat="identity") +
      theme_classic() +
      geom_errorbar(aes( ymax=upper, ymin=lower ) ,
                    size=1, width =.25, alpha=.9) +
      labs(x=x,y=y)
  } else { # If has a split, need to separated depending on if numeric
    data1 <- df[,c(id,x,y,split)]
    names(data1) <- c("id","x","y","split")
    
    if(!is.numeric(data1[,"split"])){ # if a factor, split by factor
      agg <- data1 %>% group_by(x,split) %>% 
        summarise(mean = mean(as.numeric(y),na.rm=T), 
                  se = se(as.numeric(y)), 
                  upper = mean + se, 
                  lower = mean - se)
    } else { # if numeric, split by SD +1 / -1
      data1$split <- escale(data1$split)
      fit <- lm(y  ~ x * split,data1)
      new.df = data.frame(split=rep(c(-1,+1), times=length(unique(data1$x))), 
                          x=rep(unique(data1$x), each=2))
      
      predictions <- predict(fit,new.df, interval="confidence", level=.68)
      predictions <- cbind(new.df,predictions)
      predictions$split <- as.factor(paste0(predictions$split," SD"))
      agg <- predictions
      names(agg) <- c("split","x","mean","lower","upper")
    }
    
    agg$predictor <- paste0(agg$x, "\n+\n", agg$split)
    
    if(split_bars == T) {
      g <- ggplot(agg[!is.na(agg$x) & !is.na(agg$split),],aes(x=predictor,y=mean,fill=x,alpha=split, group=split)) + 
        geom_bar(stat="identity", position="dodge") +
        theme_classic() +
        geom_errorbar(aes( ymax=upper, ymin=lower ) ,
                      size=1, width =.9, alpha=.7, position="dodge") +
        labs(x=x,y=y) +
        scale_fill_discrete(guide = guide_legend(title = split)) +
        scale_alpha_manual(values=c(0.5,1),guide=F)
    } else {
      g <- ggplot(agg[!is.na(agg$x) & !is.na(agg$split),],aes(x=x,y=mean,fill=split, group=split)) + 
        geom_bar(stat="identity", position="dodge") +
        theme_classic() +
        geom_errorbar(aes( ymax=upper, ymin=lower ) ,
                      size=1, width =.9, alpha=.7, position="dodge") +
        labs(x=x,y=y) +
        scale_fill_discrete(guide = guide_legend(title = split))
        
    }
  }
  if(keep_agg == T) {
    return(list(graph = g,agg = agg))
  } else {
    return(g)
  }
}
