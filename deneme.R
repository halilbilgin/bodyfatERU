source('autoload.R', chdir=T)



tt <- trTest(db[db$cinsiyet==1,], inputCols, p=0.7)

vifFm <- vif_func(db[, inputCols], thresh = 5, trace=F)

fm <- aic(c(toFormula(inputCols[-2], 'DEXAyagyuz')), 
          db[db$cinsiyet==1, ], steps=1000,
          starting='DEXAyagyuz~1', direction='both')[[1]]


c('gaussprPoly', 'brnn', 'lars')

fit.brnn <- train(toFormula(inputCols[-2]), tt$train, 
                  method = "glm", 
      trControl = trainControl(method='repeatedcv', number=5,repeats=10))



predicted.brnn <- predict(fit.brnn, newdata=tt$test)
fitResult(predicted.brnn, tt$test$DEXAyagyuz)

fitResult(tt$test$BIAyagyuz, tt$test$DEXAyagyuz)

