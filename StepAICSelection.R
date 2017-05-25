source('autoload.R')

aic <- function(scopes, data, starting='~.',
                direction='both', steps=100,k=2) {
  features <- list()
  starting <- as.formula(starting)
  
  fit <- glm(starting, data=data, control=glm.control(maxit=10000), family=gaussian)
  
  for(scope in scopes) {
    step <- stepAIC(fit, scope, direction=direction, steps = steps, trace=T, k=k)
    features <- append(features, list(step$formula))
    
  }
  
  names(features) <- names(scopes)
  
  return(features)
}

allFm <- as.formula(paste('DEXAyagyuz ~ ', paste(inputCols, collapse='+')))

fmFromBeginning <- aic(c(allFm), genderDb$`1`, starting='DEXAyagyuz ~ 1' , direction='both')
fmFromEnd <- aic(c(allFm), genderDb$`1`, starting=paste(format(allFm), collapse='') , direction='both')

first <- strsplit(gsub('DEXAyagyuz ~ ', '', paste(fmFromBeginning)), ' + ', T)[[1]]
second <- strsplit(gsub('DEXAyagyuz ~ ', '', paste(fmFromEnd), fixed=T), ' + ', T)[[1]]

intersect(second, first)
setdiff(second, first)

aic_groups <- lapply(genderDb, function(gdb) {
  
  names(train(allFm, gdb, method='glmStepAIC', 
                             trControl = trainControl(method = 'repeatedcv',
                             number=5,
                             seeds=get('getTrControlSeeds')(),
                             repeats=10))$finalModel$coefficients)[-1]

})

save(aic_groups, file='glmstepAIC.RData')
