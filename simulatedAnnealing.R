source('autoload.R')

knnSA <- caretSA

ctrl <- safsControl(functions = knnSA,
                    method = "repeatedcv",
                    repeats = 10,
                    number=5,
                    ## What should we optimize? 
                    metric = c(internal = "RMSE",
                               external = "RMSE"),
                    maximize = c(internal = FALSE,
                                 external = FALSE),
                    improve = 25,
                    allowParallel = TRUE)
knn_groups <- lapply(genderDb, function(gdb) {
  safs(x = gdb[, get('inputCols')[-2]],
       y = gdb$DEXAyagyuz,
       iters = 500,
       safsControl = ctrl)
})


save(knn_groups, file='knn_sa.RData')

