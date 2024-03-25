#' PR curve: Precision - Recall curve
#'
#' @description
#' model_pr() returns the Precision - Recall curve, which is based on the confusion matrix (Precision and Recall). The graph allows you to study cases of population imbalance.
#'
#' @param y_train Values of the response variable in training set.
#' @param y_train_prob Probabilities of the response variable in training set.
#' @param y_test Values of the response variable in test set.
#' @param y_test_prob Probabilities of the response variable in test set.
#' @param type 1 indicates that the PR curve is plotted using Recall on the x-axis and Precision on the y-axis, while 2 indicates that both Recall and Precision are plotted using probabilities on the x-axis. Value 3 generates both graphs (default).
#'
#' @export
model_pr = function(y_train, y_train_prob, y_test, y_test_prob, type = 3){

  calc = function(y_pr, probs_pr){
    df_pr = apply(X = as.matrix(probs_pr), MARGIN = 1, FUN = function(prob_pr){
      y_predicted_pr = ifelse(probs_pr < prob_pr, 0, 1)
      metrics_pr = metrics(y_pr, y_predicted_pr)[[2]][c(1,2)]
      return(metrics_pr)
    })
    df_pr = as.data.frame(t(df_pr))
    df_pr = cbind(df_pr, probs_pr)
    colnames(df_pr) = c("Recall", "Precision", "Probabilities")
    df_pr = df_pr[order(df_pr$Recall),]
    return(df_pr)
  }

  graph_1 = function(){
    aux_train = calc(y_train, y_train_prob)
    aux_test = calc(y_test, y_test_prob)
    par(mfrow = c(1, 1), las = 1)
    plot(x = aux_train$Recall, y = aux_train$Precision, col = "darkred", type = "l",
         xlim = c(0,1), ylim = c(0,1), xlab = "Recall", ylab = "Precision", main = "PR curve")
    points(x = aux_test$Recall, y = aux_test$Precision, col = "darkblue", type = "l")
    legend(x = "bottomleft", legend = c("Train", "Test"), text.col = c("darkred", "darkblue"), bty = "n", cex = 0.8)
  }

  graph_2 = function(){
    aux_train = calc(y_train, y_train_prob)
    aux_train = aux_train[order(aux_train$Probabilities),]
    aux_test = calc(y_test, y_test_prob)
    aux_test = aux_test[order(aux_test$Probabilities),]
    par(mfrow = c(1,1), las = 1)
    plot(x = aux_train$Probabilities, y = aux_train$Precision, col = "darkred", type = "l",
         xlim = c(0,1), ylim = c(0,1), xlab = "Probabilities", ylab = "", main = "PR curve")
    points(x = aux_train$Probabilities, y = aux_train$Recall, col = "darkred", type = "l", lty = 2)
    points(x = aux_test$Probabilities, y = aux_test$Precision, col = "darkblue", type = "l")
    points(x = aux_test$Probabilities, y = aux_test$Recall, col = "darkblue", type = "l", lty = 2)
    legend(x = "bottomleft", legend = paste(c("Train", "Test"),
                                            round(c(aux_train$Probabilities[which.min(abs(aux_train$Precision - aux_train$Recall))],
                                                    aux_test$Probabilities[which.min(abs(aux_test$Precision - aux_test$Recall))]),
                                                  digits = 2)),
           text.col = c("darkred", "darkblue"), bty = "n", cex = 0.7,
           title = "Intersections", title.cex = 0.7, title.col = "black")
    legend(x = "bottom", legend = c("Recall", " Precision"), lty = c(2, 1), cex = 0.7, bty = "n", title = "Curves", title.cex = 0.7, title.col = "black")
    par(mfrow = c(1, 1))
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
