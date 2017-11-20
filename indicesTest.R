source('autoload.R')
#initialize formulas list

trSplitSeeds <- 1:50

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
methods <- bfpCols[-1]

trControlSeeds <- getTrControlSeeds()

for(genderDbID in names(genderDb)) {
  #length is = (n_repeats*nresampling)+1
  
  for(trSplitSeed in trSplitSeeds) {
    set.seed(trSplitSeed)
    tt <- trTest(genderDb[[genderDbID]], p=.7)
    
    for(method in methods) {
  
               cat(paste('Gender:', genderDbID, 'Training:', method, "\n"))
          truth <- tt$test$DEXAyagyuz
          predicted <- tt$test[, method]
          error <- truth - predicted
          curResult <- data.frame(
            gender=genderDbID,
            seed=trSplitSeed,
            method=method,
            fm='',
            RMSE=rmse(error),
            MAPE=mape(truth, predicted),
            RSquared=rSquared(truth, predicted),
            Spearman=rcorr(truth, predicted, type="spearman")$r[1,2],
            MAE=mae(error)
          )
          
          result <- rbind(result, curResult)

          print(curResult)
      
    }
  }
}
#AIzaSyCiAm3UjbsLzqIroncItsjnHeV61GVmMz0



save(result, file='indicesResults.RData')
  


