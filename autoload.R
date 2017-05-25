library(MASS)
library(fmsb)
library(caret)
library(doParallel)
library(Hmisc)
db <- read.csv('./data.csv')
genderDb <- split(db, db$cinsiyet)
bfpCols <- names(db)[grepl('yagyuz', names(db))]
totalFatCols <- names(db)[grepl('totyag', names(db))]
indexCols <- names(db)[grepl('indeks_', names(db))]


inputCols <- names(db)[! (names(db) %in% c(bfpCols, totalFatCols, indexCols,
                                           'gozlemno', 'X')) ]


toFormula <- function(features, resp= 'DEXAyagyuz') {
  return(as.formula(paste(resp, '~', paste(features, collapse='+'))))
}


aic <- function(scopes, data, starting='DEXAyagyuz~1',
                direction='both', steps=100) {
  features <- list()
  starting <- as.formula(starting)
  
  fit <- glm(starting, data=data, control=glm.control(maxit=10000))
  
  for(scope in scopes) {
    step <- stepAIC(fit, scope, direction=direction, steps = steps, trace=T, k=2)
    features <- append(features, list(step$formula))
    
  }
  
  names(features) <- names(scopes)
  
  return(features)
}
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}
scaleDb <- function(dataset, inputCols) {
  return(as.data.frame(sapply(colnames(dataset), function(x) {
    if(is.numeric(dataset[, x]) && (x %in% inputCols)) {
      output <- scale(dataset[, x], center = TRUE, scale = TRUE)
    } else {
      output <- dataset[, x]
    }
    return(output)
    
  }))) 
}
trTest <- function(dataset, p = 0.75, scale.data=F) {
  dataset[, 'train'] <- ifelse(runif(nrow(dataset)) <= p, 1, 0)
  
  trData <- dataset[dataset[, 'train'] == 1,]
  tData <- dataset[dataset[, 'train'] == 0,]
  trData[,'train'] <- NULL
  
  tData[,'train'] <- NULL
  
  if(scale.data) {
    scaledtrain <- as.data.frame(lapply(trData, function(x) rep.int(NA, length(x))))
    scaledtest <- as.data.frame(lapply(tData, function(x) rep.int(NA, length(x))))
 
    for(feature in names(trData)) {
      if(!(feature %in% inputCols[-2])) {
        scaledtrain[, feature] <- trData[, feature]
        scaledtest[, feature] <- tData[, feature]
        next
      }
      x <- trData[, feature]
      newRow <- c(sd(x), mean(x))
      
      scaledtrain[, feature] <- (trData[, feature] - newRow[2]) / newRow[1]
      scaledtest[, feature] <- (tData[, feature] - newRow[2]) / newRow[1]
     
      
    }
    trData <- scaledtrain
    tData <- scaledtest
  }

  return(list(train=trData, test=tData))
}

rmse <- function(error){
  sqrt(mean(error^2))
}

mae <- function(error){
  mean(abs(error))
}
mape <- function(actual, predicted) {
  mean(abs((actual - predicted)/actual))
}
rSquared <- function(truth, prediction) {
  mTruth <- mean(truth)
  
  SStot <- sum((truth-mTruth)^2)
  
  SSres <- sum((truth-prediction)^2)
  
  return(1 - SSres/SStot)
  #return(cor(truth, predicted, method='pearsossn')^2)
}


fitResult <- function(predicted, truth, ...) {
  error <- truth - predicted
  data.frame(
    ...,
    RMSE=rmse(error),
    MAE=mae(error)
  )
}

getTrControlSeeds <- function() {
  seeds <- vector(mode = "list", length = 51)
  
  for(i in 1:50) {
    set.seed(1+i*100)
    seeds[[i]]<- sample.int(n=1000, 81)
  }
  set.seed(1)
  seeds[[51]]<-sample.int(1000, 1)
  
  return(seeds)
}
seed <- 5
