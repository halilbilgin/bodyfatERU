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

load('rfGA30-100.RData')
load('rf_sa.RData')
fms <- list(
  `1`=list(
    `All`=toFormula(inputCols[-2]),
    `SimulatedAnnealing`= toFormula(rf_sa_groups$`1`$optVariables),
    `GeneticAlgorithm`=toFormula(rf_groups$`1`$optVariables),
    `forward`=toFormula(c('kilo', 'cev_kalca', 'dkk_biceps', 
                          'dkk_quadriceps')),
    `backward`=toFormula(c('cev_kalca', 'cev_uyluk', 'cev_elblegi',
                           'cev_kulacuzun', 'dkk_triceps', 'dkk_sscapula',
                           'dkk_quadriceps'))
  ),
  `2`=list(
    `All`=toFormula(inputCols[-2]),
    `SimulatedAnnealing`=toFormula(rf_sa_groups$`2`$optVariables),
    `GeneticAlgorithm`=toFormula(rf_groups$`2`$optVariables),
    `forward`=toFormula(c('kilo', 'cev_bel', 'dkk_triceps',
                          'dkk_sscapula', 'dkk_quadriceps', 'dkk_gogus')),
    `backward`=toFormula(c('cev_boyun', 'cev_bel', 'cev_kulacuzun',
                           'dkk_sscapula', 'dkk_silliak',
                           'dkk_abdomen', 'dkk_quadriceps',
                           'dkk_gogus'))
  )
)
