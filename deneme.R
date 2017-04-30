source('autoload.R', chdir=T)



tt <- trTest(db[db$cinsiyet==2,], inputCols)

vifFm <- vif_func(db[, inputCols], thresh = 5, trace=F)

fm <- aic(c(toFormula(inputCols[-2], 'DEXAtotyag')), 
          db[db$cinsiyet==2,], steps=1000,
          starting='DEXAtotyag~1', direction='both')[[1]]


c('gaussprPoly', 'neuralnet', 'brnn')
fit.brnn <- train(fm, tt$train, method = "glm", 
      trControl = trainControl(method='none', number=5,repeats=3))



predicted.brnn <- predict(fit.brnn, newdata=tt$test)
fitResult(predicted.brnn, tt$test$DEXAtotyag)

fitResult(tt$test$BIAyagyuz, tt$test$DEXAtotyag)
