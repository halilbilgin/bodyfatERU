source('autoload.R')
library(doParallel)
registerDoParallel(cores=3)
methodsMen <- c('spikeslab', 'rqlasso', 'gaussprLinear', 'lm', 'bayesglm',
                'ridge', 'penalized', 'lars', 'enet', 'lasso', 'glm.nb',
                'svmLinear', 'brnn')
names(methodsMen) <- methodsMen

methodsWomen <- c('lasso', 'glm.nb', 'spikeslab', 'lm', 'bayesglm', 'lars',
                  'rlm', 'penalized', 'ridge', 'enet', 'brnn', 'svmLinear', 
                  'gaussprLinear')
names(methodsWomen) <- methodsWomen

fitsMen <- lapply(methodsMen, function(method) {
    fms <- get('fms')
    genderDb <- get('genderDb')
    train(fms$`2`$forward, genderDb$`2`, method=method)
})

fitsWomen <- lapply(methodsWomen, function(method) {
  fms <- get('fms')
  genderDb <- get('genderDb')
  train(fms$`1`$backward, genderDb$`1`, method=method)
})

allFits <- list(Male=fitsMen, Female=fitsWomen)

save(allFits, file='allFits.RData')
