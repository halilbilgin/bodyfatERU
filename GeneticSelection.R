source('autoload.R', chdir=T)

ga_ctrl <- gafsControl(functions = caretGA, # Assess fitness with RF
                       method = "cv",
                       number = 5,
                       genParallel=TRUE, # Use parallel programming
                       allowParallel = TRUE,
                       verbose = T)
## 

femaleDb <- db[db$cinsiyet==1, ]
set.seed(10)

system.time(rf_ga3 <- gafs(x = femaleDb[, inputCols[-2]], y = femaleDb[, 'DEXAyagyuz'],
                           iters = 100,
                           popSize = 30,
                           gafsControl = ga_ctrl,
                           method='glm'))

final <- rf_ga3$ga$final # Get features selected by GA
print(final)

save(rf_ga3, final, file='rfGA30-100.RData')
