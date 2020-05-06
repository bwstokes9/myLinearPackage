#' Run regression of Y on X
#'
#' This will run a linear regression of Y on X and produce a scattergram, coefficients & corresponding p-values.
#'
#' @param X A matrix of covariates
#' @param Y A vector of outcomes
#' @param sub A list of subjects (i.e. a set of integers corresponding to rows in X)
#' @return A list containing the coefficients form the linear regression of Y on X and corresponding p-values.
#' @export

myLinearRegression <- function(X=NULL, Y=NULL, sub=c(1:length(Y))) {
  y2                  <- Y[sub,]
  x2                  <- X[sub,]
  ExRegression <- lm(y2 ~ x2, data = ExData)
  coef               <- summary(ExRegression)$coefficients[,1]
  pvals              <- summary(ExRegression)$coefficients[,4]
  if (ncol(X) < 6) {
    GGally::ggpairs(data = X, title = "Scatterplots")
    print(list("coef"=coef, "pvals"=pvals))
   } else {
    print("Too Many Variables")
   }
}


