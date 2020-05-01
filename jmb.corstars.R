gCompanyForms <- c(",","llc", "corp", "inc.", "inc", "incorporated", "co.", "pllc", "ltd", "ltd.")


jmb.isentre <- function(text){
  
  if( grepl(tolower("Self"),tolower(text)) ){
    return("Y")
  }else if (grepl(tolower("Self"),tolower(text))){
    return("Y")
  }else if (grepl(tolower("Entre"),tolower(text))){
    return("Y")
  }else if (grepl(tolower("Free"),tolower(text))){
    return("Y")
  }else if (grepl(tolower("owner"),tolower(text))){
    return("Y")
  }else if (grepl(tolower("founder"),tolower(text))){
    return("Y")
  }else{
    return("N")
  }
}


jmb.diagnostics <- function(df, filename="filename",dummiesStartAt=0){
  
  library(psych)
  library(gridExtra)
  
  #drop dummies if requested
  if(dummiesStartAt != 0){
    df <- df[,1:dummiesStartAt]
  }
  
  dir.create("jmb.diagnostics")
  pdf(file=paste("jmb.diagonostics\\", filename, ".pdf", sep = ""))

  missing.values <- df %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>%
    mutate(pct = num.isna / total * 100)
  
  levels <-
    (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
  
  percentage.plot <- missing.values %>%
    ggplot() +
    geom_bar(aes(x = reorder(key, desc(pct)), 
                 y = pct, fill=isna), 
             stat = 'identity', alpha=0.8) +
    #scale_x_discrete(limits = levels) +

    scale_fill_manual(name = "", 
                      values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
    coord_flip() +
    labs(title = "Percentage of missing values", x =
           'Variable', y = "% of missing values")
  #ggsave(paste("jmb.diagnostics\\", filename, "-misssings", ".png", sep = ""), percentage.plot)
  print(percentage.plot)
  
  
  #print(plot(1:10,1:10,main=paste("This pdf was generated from the jmb.diagnostics function",paste("On ", dateS),sep="\n")))
  for(i in names(df)){
    
    df$winsored <- winsor(df[,i], 0.05)
    df$logged <- log(df[,i]+1)
    g1 <- ggplot(df, aes_string(x=i)) + 
      geom_histogram(aes(y=(..count..)/sum(..count..)), colour="black", fill="white") +
      #geom_density(alpha=.2, fill="#FF6666") + 
      labs(title = i, subtitle = paste("Missing: ", sum(is.na(df[,1])), " out of ", nrow(df)), x =
             paste('Variable:', i), y = "Percent of Total")
    g1q <- ggplot(df, aes(sample = df[,i])) +
      stat_qq() +
      stat_qq_line()
    
    g2 <- ggplot(df, aes(x=winsored)) + 
      geom_histogram(aes(y=(..count..)/sum(..count..)), colour="black", fill="white")+
      #geom_density(alpha=.2, fill="#FF6666") + 
      labs(title = paste("(winsor)",i), subtitle = paste("Missing: ", sum(is.na(df[,1])), " out of ", nrow(df)), x =
             paste('Winsored (0.05) Variable', i), y = "Percent of Total")
    g2q <- ggplot(df, aes(sample = winsored)) +
      stat_qq() +
      stat_qq_line()
    
    g3 <- ggplot(df, aes(x=logged)) + 
      geom_histogram(aes(y=(..count..)/sum(..count..)), colour="black", fill="white")+
      #geom_density(alpha=.2, fill="#FF6666") + 
      labs(title = paste("(log)", i), subtitle = paste("Missing: ", sum(is.na(df[,1])), " out of ", nrow(df)), x =
             paste('Logged Variable', i), y = "Percent of Total")
    g3q <- ggplot(df, aes(sample = logged)) +
      stat_qq() +
      stat_qq_line()
    
    grid.arrange(g1, g1q, g2, g2q, g3, g3q, ncol=2)
    #gr <- arrangeGrob(g1, g1q, g2, g2q, g3, g3q, ncol=2)
    #ggsave(paste("jmb.diagnostics\\", filename, "-", colnames(df)[i], ".png", sep = ""), gr)
  }
  dev.off()
}


jmb.searchwords <- function(str, searchword){
  library(stringr)
  str <- tolower(str)
  searchword <- tolower(searchword)
  if(str_detect(str, searchword, negate = FALSE)){
    return(1)
  }else{
    return(0)
  }
}


jmb.removewords <- function(str, removewords) {
  str <- tolower(str)
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% removewords], collapse = " ")
}

#zscore an entire dataframe
jmb.zscore <- function(df){
  library(tidyverse)
  library(psycho)
  #need the id to merge
  df <- df %>% mutate(jmb.zscore.id = row_number())
  df$jmb.zscore.id <- as.factor(df$jmb.zscore.id)
  #standardize dataframe, only deals with numerics 
  df1 <- standardize(df)
  colnames(df1) <- paste(colnames(df), "z", sep = "_")
  
  df$jmb.zscore.id <- as.numeric(df$jmb.zscore.id)
  df1$jmb.zscore.id_z <- as.numeric(df1$jmb.zscore.id_z)
  df3 <- merge(df, (select_if(df1, is.numeric)), by.x ="jmb.zscore.id", by.y="jmb.zscore.id_z")
  df3 <- df3[-1]
}

#builds dummies out of factors across a dataframe
jmb.builddummies <- function(df){
  if(!require(tidyverse)) install.packages("tidyverse")
  if(!require(dummies)) install.packages("dummies")
  if(!require(lubridate)) install.packages("dummies")
  toDummyVector <- c()
  for (i in names(df)){
    if(!is.numeric(df[,i]) && !is.Date(df[,i])){
      toDummyVector <- c(toDummyVector, colnames(df[i]))
    }
  }
  df <- as.data.frame(dummy.data.frame(df, toDummyVector, sep="_"))
  return(df)
}

#support function, will return the number of the column name passed to itS
jmb.gcn <- function(colNames, df){
  return(which((names(df)==colNames)))
}


#adapted from https://github.com/Cogitos/statxp/blob/master/R/corstars.R
jmb.corstars <-function(x, method=c("pearson", "spearman"), fileName, dummiesStartAt=0){
  if(fileName == ""){
    fileName="cor.html"
  }
  
  #set up packages
  if(!require(Hmisc)) install.packages("Hmisc")
  library(Hmisc)
  if(!require(xtable)) install.packages("xtable")
  library(xtable)
  if(!require(tidyverse)) install.packages("tidyverse")
  library(ggcorrplot)
  if(!require(ggcorrplot)) install.packages("ggcorrplot")
  #anyone not a numeric?
  #pull out factors/characters
  # y <- select_if(x, negate(is.numeric))
  
  #this method converts
  #x <- data.frame(sapply(x, function(z) as.numeric(as.character(z))))
  #this method returns the numerics 
  #x <- select_if(x, is.numeric)
  
  m <- c()
  for(zz in 1:ncol(x)){
    m <- c(m, format(round(mean(x[,zz],na.rm=TRUE), 2), nsmall=2))
  }
  
  
  # m1 <- c()
  # for(qq in 1:ncol(y)){
  #   s <- as.data.frame(table(y[,qq]))
  #   s$perc <- (s$Freq / sum(s$Freq))
  # }
  
  sdv <- c()
  for(zzz in 1:ncol(x)){
    sdv <- c(sdv,format(round(sd(x[,zzz],na.rm=TRUE), 2), nsmall=2))
  }
  
  msdv <- data.frame(m, sdv)
  colnames(msdv) <- c("Mean", "SD")
  
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 

  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "***", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  qq <- 1
  for(qq in 1:ncol(x)){
    colnames(x)[qq] <- paste(qq, colnames(x)[qq], sep = ". ")
  }
  rownames(Rnew) <- colnames(x)
  
  numCol <- c()
  for(ii in 1:ncol(x)){ numCol <- c(numCol, ii) }
  #numCol <- numCol[-length(numCol)]
 
  colnames(Rnew) <- (numCol) #paste(colnames(as.character(numCol)), "", sep="")

  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  Rnew <- cbind(msdv, Rnew)
  
  #future replace with htmlTable library
  code <- print(xtable(Rnew), type="html")
  
  write.table(code, 
              file=paste(fileName, ".html", sep=""), 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
  R.int <- as.data.frame(R, stringsAsFactors = FALSE)
 
   #dropping any dummies?
  if(dummiesStartAt != 0){ R.int <- R.int[1:dummiesStartAt,] }
  
  for(i in names(R.int)){ #convert characters to numeric
    R.int[,i] <- as.numeric(R.int[,i]) 
  }
  
  ggcorrplot(R.int, hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             lab_size = 3, 
             colors = c("#6D9EC1", "white", "#E46726"), 
             title="Correlogram")
  ggsave(paste(fileName, ".png", sep=""), width=10, height=10)
}


