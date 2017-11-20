modelInfo <- list(label = "nu-SVR with Radial Basis Function Kernel",
                  library = "kernlab",
                  type = c("Regression"),
                  parameters = data.frame(parameter = c("nu", "C", "sigma"),
                                          class = c("numeric", "numeric", "numeric"),
                                          label = c("Nu", "Cost", "Sigma")),
                  loop = NULL,
                  grid=NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    if(any(names(list(...)) == "prob.model") | is.numeric(y)) {
                      out <- ksvm(x = as.matrix(x), y = y,
                                  type='nu-svr',
                                  kernel = rbfdot,
                                  kpar= list(sigma = param$sigma),
                                  C = param$C,
                                  nu = param$nu, ...)
                    }
                    out            
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    svmPred <- function(obj, x) {
                      hasPM <- !is.null(unlist(obj@prob.model))
                      if(hasPM) {
                        pred <- lev(obj)[apply(predict(obj, x, type = "probabilities"), 
                                               1, which.max)]
                      } else pred <- predict(obj, x)
                      pred
                    }
                    out <- try(svmPred(modelFit, newdata), silent = TRUE)
                    if(is.character(lev(modelFit))) {
                      if(class(out)[1] == "try-error") {
                        warning("kernlab class prediction calculations failed; returning NAs")
                        out <- rep("", nrow(newdata))
                        out[seq(along = out)] <- NA
                      }
                    } else {
                      if(class(out)[1] == "try-error") {
                        warning("kernlab prediction calculations failed; returning NAs")
                        out <- rep(NA, nrow(newdata))
                      } 
                    }
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- try(predict(modelFit, newdata, type="probabilities"),
                               silent = TRUE)
                    if(class(out)[1] != "try-error") {
                      ## There are times when the SVM probability model will
                      ## produce negative class probabilities, so we
                      ## induce vlaues between 0 and 1
                      if(any(out < 0)) {
                        out[out < 0] <- 0
                        out <- t(apply(out, 1, function(x) x/sum(x)))
                      }
                      out <- out[, lev(modelFit), drop = FALSE]
                    } else {
                      warning("kernlab class probability calculations failed; returning NAs")
                      out <- matrix(NA, nrow(newdata) * length(lev(modelFit)), ncol = length(lev(modelFit)))
                      colnames(out) <- lev(modelFit)
                    }
                    out
                  },
                  predictors = function(x, ...){
                    if(hasTerms(x) & !is.null(x@terms)) {
                      out <- predictors.terms(x@terms)
                    } else {
                      out <- colnames(attr(x, "xmatrix"))
                    }
                    if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
                    if(is.null(out)) out <-NA
                    out
                  },
                  tags = c("Kernel Method", "Support Vector Machines", "Radial Basis Function",
                           "Robust Methods"),
                  levels = function(x) lev(x),
                  sort = function(x) {
                    # If the cost is high, the decision boundary will work hard to
                    # adapt. Also, if C is fixed, smaller values of sigma yeild more
                    # complex boundaries
                    x[order(x$C, -x$sigma),]
                  })

nuSVRGrid <- expand.grid(
  sigma = c(0.0000305176,0.000122070,0.000488281,0.00195313,0.0078125,
            0.03125,0.125,0.5,2,8,32),
  nu = c(0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,
         0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0),
  C = c(0.0000305176,0.000122070,0.000488281,0.00195313,0.0078125,
        0.03125,0.125,0.5,2,8,32)
  
)
maeSummary <- function (data,
                        lev = NULL,
                        model = NULL) {
  out <- ModelMetrics::mae(data$obs, data$pred)  
  names(out) <- "MAE"
  out
}

fit <- train(toFormula(inputCols[-1]), genderDb$`1`, method=modelInfo, 
             trControl= trainControl(method='cv', number=5, summaryFunction = maeSummary),
             tuneGrid = nuSVRGrid, metric='MAE', maximize=F)

print(fit)
print(fit$bestTune)
