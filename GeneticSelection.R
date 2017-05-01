source('autoload.R', chdir=T)

ga_ctrl <- gafsControl(functions = rfGA,
                       method = "repeatedcv",
                       number = 5,
                       repeats=10,
                       genParallel=TRUE, # Use parallel programming
                       allowParallel = TRUE,
                       verbose = T)
 


rf_groups <- lapply(genderDb, function(gdb){ 
    gafs(x = gdb[, inputCols[-2]], y = gdb[, 'DEXAyagyuz'],
                           iters = 100,
                           popSize = 30,
                           gafsControl = get('ga_ctrl'))
})
final <- rf_ga3$ga$final # Get features selected by GA
print(final)

save(rf_groups, file='rfGA30-100.RData')
