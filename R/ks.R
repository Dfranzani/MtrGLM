#' KS curve: Kolmogorov - Smirnov curve
#'
#' @description
#' model_ks() returns the Kolmogorov-Smirnov graph, which seeks to compare the cumulative distributions of successes versus failures. It is possible to choose between different graphs to study this, the first illustrates the cumulative distributions, while the second uses the true positive ratio (TPR) and the false positive ratio (FPR). The graphs allow the discrimination capacity of the model to be evaluated.
#'
#' @param y_train Values of the response variable in training set.
#' @param y_train_prob Probabilities of the response variable in training set.
#' @param y_test Values of the response variable in test set.
#' @param y_test_prob Probabilities of the response variable in test set.
#' @param type 1 indicates that the KS curve is plotted using the cumulative proportions of real zeros and ones, while 2 indicates that the FPR and TPR are used. Value 3 generates both graphs (default value).
#'
#' @export
model_ks = function(y_train, y_train_prob, y_test, y_test_prob, type = 3){

  graph_1 = function(){

    ks = cut_point = c()

    calc = function(y_ks, y_ks_prob){
      y_ks = y_ks[order(y_ks_prob, decreasing = TRUE)]
      p_cum_one = cumsum(y_ks)/sum(y_ks)
      p_cum_zero = cumsum(y_ks - 1)/sum(y_ks - 1)
      ind = seq(from = 1, to = length(y_ks))/length(y_ks)
      ks <<- append(ks, paste0(format(round(max(p_cum_one - p_cum_zero)*100, 2), nsmall = 1), "%"))
      cut_point <<- append(cut_point, which.max(p_cum_one - p_cum_zero))
      return(data.frame("Failed" = p_cum_one, "Approved" = p_cum_zero, "Index" = ind))
    }

    aux_train = calc(y_train, y_train_prob)
    aux_test = calc(y_test, y_test_prob)

    par(mfrow = c(1, 1), las = 1)
    plot(x = c(0, aux_train$Index), y = c(0, aux_train$Failed), col = "darkred", type = "l", xaxt = "n",
         xlim = c(0,1), ylim = c(0,1), xlab = "%Tested", ylab = "Cumulative proportion", main = "KS curve")
    axis(side = 1, at = seq(from = 0, to = 1, by = 0.2), labels = seq(from = 0, to = 100, by = 20))
    points(x = c(0, aux_train$Index), y = c(0, aux_train$Approved), col = "darkred", type = "l")
    points(x = c(0, aux_test$Index), y = c(0, aux_test$Failed), col = "darkblue", type = "l")
    points(x = c(0, aux_test$Index), y = c(0, aux_test$Approved), col = "darkblue", type = "l")
    points(x = rep(aux_train$Index[cut_point[1]], 2),
           y = c(aux_train$Failed[cut_point[1]], aux_train$Approved[cut_point[1]]),
           col = "darkred", type = "l", lty = 2)
    points(x = rep(aux_test$Index[cut_point[2]], 2),
           y = c(aux_test$Failed[cut_point[2]], aux_test$Approved[cut_point[2]]),
           col = "darkblue", type = "l", lty = 2)
    legend(x = "bottomright",
           legend = c(paste("Train:", ks[1]), paste("Test:", ks[2])),
           text.col = c("darkred", "darkblue"), bty = "n", cex = 0.8)
  }

  graph_2 = function(){

    ks = cut_point = c()

    calc = function(y_ks, y_ks_prob){

      aux = data.frame("TPR" = rep(NA, length(y_ks)), "FPR" = rep(NA, length(y_ks)))

      for(i in 1:dim(aux)[1]){
        p = ifelse(y_ks_prob <= y_ks_prob[i], 0, 1)
        m = metrics(y_ks , p)[[1]]
        aux[i,] = c(m[1]/(m[1] + m[2]), m[3]/(m[3] + m[4]))
      }

      aux$Probs = y_ks_prob
      aux = aux[order(aux$Probs, decreasing = TRUE),]

      ks <<- append(ks, paste0(format(round(max(aux$TPR - aux$FPR)*100, 2), nsmall = 1), "%"))
      cut_point <<- append(cut_point, which.max(aux$TPR - aux$FPR))

      return(aux)
    }

    aux_train = calc(y_train, y_train_prob)
    aux_test = calc(y_test, y_test_prob)

    par(mfrow = c(1, 1), las = 1)
    plot(x = aux_train$Probs, y = aux_train$FPR, col = "darkred", type = "l",
         xlim = c(0,1), ylim = c(0,1), xlab = "Probability", ylab = "Rate", main = "KS curve")
    points(x = aux_train$Probs, y = aux_train$TPR, col = "darkred", type = "l")
    points(x = aux_test$Probs, y = aux_test$FPR, col = "darkblue", type = "l")
    points(x = aux_test$Probs, y = aux_test$TPR, col = "darkblue", type = "l")
    points(x = rep(aux_train$Probs[cut_point[1]], 2),
           y = c(aux_train$FPR[cut_point[1]], aux_train$TPR[cut_point[1]]),
           col = "darkred", type = "l", lty = 2)
    points(x = rep(aux_test$Probs[cut_point[2]], 2),
           y = c(aux_test$FPR[cut_point[2]], aux_test$TPR[cut_point[2]]),
           col = "darkblue", type = "l", lty = 2)
    legend(x = "bottomleft",
           legend = c(paste("Train:", ks[1]), paste("Test:", ks[2])),
           text.col = c("darkred", "darkblue"), bty = "n", cex = 0.8)
  }

  if(type == 3){
    graph_1()
    graph_2()
  } else if(type == 1){
    graph_1()
  } else{
    graph_2()
  }
}
