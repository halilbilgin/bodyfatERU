
methodsTest <- function(methods, 
                        formulasForTest = c('All', 'SimulatedAnnealing', 'GeneticAlgorithm', 'forward',
                                    'backward', 'stepAIC'),
                        gendersForTest = c('1', '2'), 
                        trSplitSeeds = 1:50,
                        fileSuffix='try') {

  source('autoload.R')
  
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
      tt <- trTest(genderDb[[genderDbID]], p=.7)
      
      for(method in methods) {
        for(fmsForGender in fms[genderDbID]) {
          for(fmName in formulasForTest) {
            fm <- fmsForGender[[fmName]]
            cat(paste('Gender:', genderDbID, 'Training:', method, 'Seed:', trSplitSeed,
                      'Formula:', fmName, "\n"))
            set.seed(trSplitSeed)
            indices = createMultiFolds(tt$train$DEXAyagyuz, k = 10, times = 5)
            
            fit <- try(train(as.formula(fm), data=tt$train, method=method,
                             trControl=trainControl(method = 'repeatedcv',
                                                    number=5,
                                                    seeds=trControlSeeds,
                                                    repeats=10,
                                                    index= indices
                                                    )), TRUE)
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
  
}


