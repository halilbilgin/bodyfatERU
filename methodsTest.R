source('autoload.R')

methods <- c(
        'glm',
        'brnn',
        'ppr',
        'gbm',
        'kernelpls',
        'pls',
        'pcr',
        'bridge',
        'gaussprLinear',
        'gaussprPoly',
        'bayesglm',
        'svmLinear',
        'knn',
        'enet',
        'bagEarthGCV',
        'blassoAveraged',
        'spikeslab',
        'blasso',
        'svmPoly',
        'rvmLinear',
        'enet',
        'ridge',
        'evtree',
        'cubist',
        'enpls',
        'FIR.DM',
        'GFS.FR.MOGUL',
        'GFS.THRIFT',
        'HYFIS',
        'icr',
        'pythonKnnReg',
        'lars',
        'lm',
        'M5Rules',
        'M5',
        'nnet',
        'rqnc',
        'nnls',
        'penalized',
        'krlsPoly',
        'qrf',
        'qrnn',
        'rqlasso',
        'krlsRadial',
        'relaxo',
        'rvmPoly',
        'rvmRadial', 
        'rlm',
        'FS.HGD',
        'SBC',
        'lasso',
        'bartMachine',
        'gamboost',
        'glmboost',
        'BstLm',
        'bstSm',
        'blackboost',
        'rpart',
        'cforest',
        'ctree',
        'randomGLM',
        'xgbLinear',
        'elm',
        'gaussprRadial',
        'gamLoess',
        'bam',
        'mlpWeightDecay',
        'earth',
        'rbf',
        'ranger',
        'RRF',
        'xyf',
        'dnn',
        'svmRadial',
        'nodeHarvest'
        )

#initialize formulas list



load('rfGA30-100.RData')
load('rf_sa.RData')
fms <- list(
  `1`=list(
    `SimulatedAnnealing`= toFormula(rf_sa_groups$`1`$optVariables),
    `GeneticAlgorithm`=toFormula(rf_groups$`1`$optVariables),
    `forward`=toFormula(c('kilo', 'cev_kalca', 'dkk_biceps', 
                          'dkk_quadriceps')),
    `backward`=toFormula(c('cev_kalca', 'cev_uyluk', 'cev_elblegi',
                           'cev_kulacuzun', 'dkk_triceps', 'dkk_sscapula',
                           'dkk_quadriceps'))
  ),
  `2`=list(
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


## initialize result data frame
result <- data.frame(
  gender=character(),
  seed=numeric(),
  method=character(),
  fm=character(),
  RMSE=numeric(),
  MAPE=numeric(),
  RSquared=numeric(),
  Spearman=numeric(),
  MAE=numeric()
)

trControlSeeds <- getTrControlSeeds()
trSplitSeeds <- 1:1

for(genderDbID in names(genderDb)) {
  #length is = (n_repeats*nresampling)+1

  for(trSplitSeed in trSplitSeeds) {
    set.seed(trSplitSeed)
    tt <- trTest(genderDb[[genderDbID]], p=.7)
    
    for(method in methods) {
      for(fmsForGender in fms[genderDbID]) {
        for(fmName in names(fmsForGender)) {
          fm <- fmsForGender[[fmName]]
          cat(paste('Gender:', genderDbID, 'Training:', method, 'Formula:', fmName, "\n"))
          
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
          
          predicted <- predict(fit, newdata=tt$test)
          error <- tt$test$DEXAyagyuz - predicted
          curResult <- data.frame(
            gender=genderDbID,
            seed=trSplitSeed,
            method=method,
            fm=fmName ,
            RMSE=rmse(error),
            MAPE=mape(tt$test$DEXAyagyuz, predicted),
            RSquared=rSquared(tt$test$DEXAyagyuz, predicted),
            Spearman=rcorr(tt$test$DEXAyagyuz, predicted, type="spearman"),
            MAE=mae(error)
          )
          
          result <- rbind(result, curResult)
          
          print(curResult)
        }
      }
    }
  }
  
}

