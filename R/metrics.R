#' Metrics of a model from the confusion matrix
#'
#' @description
#' metrics() provides the classification values associated with a confusion matrix, and various performance metrics.
#'
#' @details
#' The nomenclature of the values associated with the confusion matrix returned by the function are TP: true positive, FN: false negative, FP: false positive and TN: true negative.
#'
#' @param real Values of the response variable in test set.
#' @param predicted Predicted values of the test set response variable in the model.
#'
#' @return Confusion matrix: TP FN FP TN. Recall, Precision, Accuracy, F1 Score.
#' @export
metrics = function(real, predicted){
  TP = sum(real == 1 & predicted == 1)
  FN = sum(real == 1 & predicted == 0)
  FP = sum(real == 0 & predicted == 1)
  TN = sum(real == 0 & predicted == 0)
  return(
    list(
      "Values" = c("TP" = TP, "FN" = FN, "FP" = FP, "TN" = TN),
      "Metrics" = c(
        "Recall" = TP/(TP + FN),
        "Precision" = TP/(TP + FP),
        "Accuracy" = (TP + TN)/(TP + TN + FP + FN),
        "F1 Score" = 2*TP/(2*TP+FP+FN))
      )
  )
}
