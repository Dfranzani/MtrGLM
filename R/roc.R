#' ROC curve: receiver operating characteristic curve
#'
#' @description
#' model_roc() returns the ROC curve by calculating the true positive rate (TPR) and false positive rate (FPR), as well as the area under the curve. The graph allows you to evaluate the discrimination capacity of the model.
#'
#' @param y_train Values of the response variable in training set.
#' @param y_train_prob Probabilities of the response variable in training set.
#' @param y_test Values of the response variable in test set.
#' @param y_test_prob Probabilities of the response variable in test set.
#'
#' @export
#'
#' @importFrom graphics legend par points
model_roc = function(y_train, y_train_prob, y_test, y_test_prob){

  AUC = c()

  calc = function(x, x_prob){
    x = x[order(x_prob, decreasing = TRUE)]
    TPR = cumsum(x)/sum(x)
    FPR = cumsum(!x)/sum(!x)
    n = length(TPR)
    AUC <<- c(AUC, format(round(sum((FPR[2:n] - FPR[1:(n - 1)])*TPR[2:n]), 2), nsmall = 2))
    return(data.frame("TPR" = TPR, "FPR" = FPR))
  }

  aux = rbind(calc(y_train, y_train_prob), calc(y_test, y_test_prob))
  aux$Set = c(rep("Train", length(y_train)), rep("Test", length(y_test)))

  par(mfrow = c(1, 1), las = 1)
  plot(x = aux$FPR[aux$Set == "Train"], y = aux$TPR[aux$Set == "Train"], col = "darkred", type = "l",
       xlim = c(0,1), ylim = c(0,1), xlab = "FPR", ylab = "TPR", main = "ROC")
  points(x = aux$FPR[aux$Set == "Test"], y = aux$TPR[aux$Set == "Test"], col = "darkblue", type = "l")
  points(x = c(0,1), y = c(0,1), col = "black", type = "l", lty = 2)
  legend(x = "bottomright", legend = c(paste0("AUC Train: ", AUC[1], collapse = ""), paste0("AUC Test: ", AUC[2], collapse = "")),
         text.col = c("darkred", "darkblue"), bty = "n", cex = 0.8)
}
