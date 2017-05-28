load('rfGA30-100.RData')
load('rf_sa.RData')
load('glmStepAIC.RData')
combineMethods <- function(methods, trSplitSeeds, fileSuffix, scale.data=F, 
                        formulasForTest = c('All', 'SimulatedAnnealing', 'GeneticAlgorithm', 'forward',
                                            'backward', 'stepAIC'),
                        gendersForTest = c('1', '2')) {
  source('autoload.R')
  #initialize formulas list
  fms <- list(
    `1`=list(
      `All`=toFormula(inputCols[-2]),
      `SimulatedAnnealing`= toFormula(rf_sa_groups$`1`$optVariables),
      `GeneticAlgorithm`=toFormula(rf_groups$`1`$optVariables),
      `forward`=toFormula(c('kilo', 'cev_kalca', 'dkk_biceps', 
                            'dkk_quadriceps')),
      `backward`=toFormula(c('cev_kalca', 'cev_uyluk', 'cev_elblegi',
                             'cev_kulacuzun', 'dkk_triceps', 'dkk_sscapula',
                             'dkk_quadriceps')),
      `stepAIC`=toFormula(aic_groups$`1`)
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
                             'dkk_gogus')),
      `stepAIC`=toFormula(aic_groups$`2`)
    )
  )
  
  
  ## initialize result data frame
  result <- data.frame(
    gender=character(),
    seed=numeric(),
    method=character(),
    fm=character(),
    RMSE=numeric(),
    MAPE=numeric(),
    RSquared=numeric(),
    Spearman=numeric()
  )
  
  trControlSeeds <- getTrControlSeeds()
  
  for(genderDbID in gendersForTest) {
    #length is = (n_repeats*nresampling)+1
    
    for(trSplitSeed in trSplitSeeds) {
      set.seed(trSplitSeed)
      tt <- trTest(genderDb[[genderDbID]], p=.7, scale.data=scale.data)
      
      for(fmName in formulasForTest) {
        for(fmsForGender in fms[genderDbID]) {
          fm <- fmsForGender[[fmName]]
          cat(paste('Gender:', genderDbID, 'Training:', paste(methods, collapse='+'), 'Seed:', trSplitSeed,
                    'Formula:', fmName, "\n"))
          methodsTrainPredictions <- data.frame(DEXAyagyuz=tt$train$DEXAyagyuz)
          methodsTestPredictions <- data.frame(DEXAyagyuz=tt$test$DEXAyagyuz)
          for(method in methods) {
            
  
            fit <- try(train(as.formula(fm), data=tt$train, method=method,
                             trControl=trainControl(method = 'repeatedcv',
                                                    number=5,
                                                    seeds=trControlSeeds,
                                                    repeats=10)), TRUE)
            if(inherits(fit, "try-error")){
              cat(paste('Error..:', fit))
              cat('\n')
              next
            }
            
            methodsTrainPredictions[, method] <- predict(fit, newdata=tt$train)
            methodsTestPredictions[, method] <- predict(fit, newdata=tt$test)
          }
          

          fit <- try(train(DEXAyagyuz~., data=methodsTrainPredictions, method='glm',
                           trControl=trainControl(method = 'none')), TRUE)
          if(inherits(fit, "try-error")){
            cat(paste('Error..:', fit))
            cat('\n')
            next
          }
          predicted <- predict(fit, newdata=methodsTestPredictions)
          error <- tt$test$DEXAyagyuz - predicted
          curResult <- data.frame(
            gender=genderDbID,
            seed=trSplitSeed,
            method=paste(methods, collapse='+'),
            fm=fmName ,
            RMSE=rmse(error),
            MAPE=mape(tt$test$DEXAyagyuz, predicted),
            RSquared=rSquared(tt$test$DEXAyagyuz, predicted),
            Spearman=rcorr(tt$test$DEXAyagyuz, predicted, type="spearman")$r[1,2],
            MAE=mae(error)
          )
          
          result <- rbind(result, curResult)
          remove(fit)
          gc()
          print(curResult)
          save(result, file=paste('result-', gsub(":", "_", gsub(" ", "", fileSuffix)), '.RData', sep = ''))
          
        }
      }
    }
    
  }
  
}


