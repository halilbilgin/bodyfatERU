source('autoload.R')


ctrl <- safsControl(functions = caretSA,
                    method = "repeatedcv",
                    repeats = 10,
                    number=5,
                    ## What should we optimize? 
                    metric = c(internal = "AIC",
                               external = "AIC"),
                    maximize = c(internal = FALSE,
                                 external = FALSE),
                    improve = 25,
                    allowParallel = TRUE)
knn_groups <- lapply(genderDb, function(gdb) {
  safs(x = gdb[, get('inputCols')[-2]],
       y = gdb$DEXAyagyuz,
       iters = 500,
       safsControl = ctrl,
       method='glm'
       )
})


save(knn_groups, file='glm_sa.RData')

