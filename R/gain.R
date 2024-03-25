#' Gain curve
#'
#' @description
#' model_gain() returns the gain curve, which is constructed from the percentage of the tested population and the cumulative proportion of successes. The graph allows evaluating the percentage of gain in the prediction of success in relation to the percentage of the tested population.
#'
#'
#' @param y_test Values of the response variable in test set.
#' @param y_test_prob Probabilities of the response variable in test set.
#'
#' @export
#'
#' @importFrom graphics axis
model_gain = function(y_test, y_test_prob){
  aux = data.frame("Real" = as.numeric(y_test), "Prob" = y_test_prob)

  aux = aux[order(aux$Prob, decreasing = TRUE),]
  aux$Index = (1:dim(aux)[1])/dim(aux)[1]
  aux$AcumReal = cumsum(aux$Real)/sum(aux$Real)
  aux$AcumPerfect = cumsum(aux[order(aux$Real, decreasing = TRUE),1])/sum(aux$Real)

  aux = rbind(c(0,0,0,0,0), aux)

  par(mfrow = c(1, 1), las = 1)
  plot(x = aux$Index, y = aux$AcumPerfect, col = "darkgreen", type = "l",
       xlim = c(0,1), ylim = c(0,1), xaxt = "n",
       xlab = "%Tested", ylab = "Cumulative proportion", main = "Gain curve")
  axis(side = 1, at = seq(from = 0, to = 1, by = 0.2), labels = seq(from = 0, to = 100, by = 20))
  points(x = aux$Index, y = aux$AcumReal, col = "darkblue", type = "l")
  points(x = c(0,1), y = c(0,1), col = "darkred", type = "l")
  legend(x = "bottomright", legend = c("Perfect", "Average","Random"),
         lty = 1, lwd = 1, col = c("darkgreen", "darkblue","darkred"),
         text.col = c("darkgreen", "darkblue","darkred"), bty = "n", cex = 0.8)
}
