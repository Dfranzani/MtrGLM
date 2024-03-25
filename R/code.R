#' Generalized linear model metrics
#' @description
#' mtrglm() generates different performance metrics and graphs associated with a confusion matrix of a binary variable. It also includes graphs constructed from the predicted values. Although the function is not focused on creating a generalized linear model, it includes options for basic elaboration.
#'
#' @param x List that has Values and Probabilities of the response variable in training and test set (the columns must be in this specific order). In case of creating a GLM through the stats::glm() function, you must provide a data frame that includes the response variable and the model covariates.
#' @param basic_model Indicates whether a GLM should be created.
#' @param y In case of preparing a GLM, this parameter corresponds to the column name of the response variable.
#' @param link In case of preparing a GLM, this parameter corresponds to the link function of the binomial family ("logit", "probit", or "cloglog", default is "logit").
#' @param partition In case of preparing a GLM, this parameter corresponds to training set size ratio, default is 0.75.
#' @param seed Seed for partition random processes, default is 200.
#' @param type_ks Value 0, 1, or 3 indicating the type of KS curve graph that you want to create, look at the `type` argument of the function [model_ks()].
#' @param type_pr Value 0, 1, or 3 indicating the type of PR curve graph that you want to create, look at the `type` argument of the function [model_pr()].
#'
#' @return Confusion matrix, Recall, Precision, Accuracy y F1 Score. ROC, PR, KS and Gain curves.
#' @export
#'
#' @seealso
#'    [metrics()] to calculate the metrics associated with the confusion matrix,
#'    [model_roc()] to plot ROC curve,
#'    [model_pr()] to plot Precision - Recall curve,
#'    [model_ks()] to plot Kolmogorov - Smirnov curve,
#'    [model_gain()] to plot Gain curve.
#'
#' @examples
#' # example code
#'
#' sleep_data$Gender = as.numeric(ifelse(sleep_data$Gender == "Male", 0 , 1))
#' mtrglm(y = "Gender", x = sleep_data, basic_model = TRUE, type_ks = 3, type_pr = 3)
#'
#' @importFrom stats binomial glm predict as.formula

mtrglm = function(x, basic_model = FALSE, y = NULL, link = "logit", partition = 0.75, seed = 200, type_ks = 2, type_pr = 2){

  if(!is.logical(basic_model) | !(link %in% c("logit", "cloglog", "probit")) | !(partition > 0) | !(partition < 1) | !is.numeric(seed) |
     !(type_ks %in% c(1,2,3)) | !(type_pr %in% c(1,2,3))){
    stop("Problem in at least one of the arguments: basic_model, link, partition, seed, type_ks or type_pr")
  }

  if("data.frame" == class(x)){
    if(!is.character(y) | !(y %in% colnames(x))){
      stop("The column y is not in the database or does not have the character class")
    }
    if(FALSE %in% (x[[y]] %in% c(0,1))) {
      stop("The response variable is not binary")
    }
    if(is.character(x[[y]])){
      message("The response variable is binary but is of character type")
    }
  } else if("list" == class(x)) {

    if(!is.numeric(unlist(x))){
      stop("At least one of the elements in the list is not numeric")
    }

    if(length(unique(x[[1]])) != 2 | length(unique(x[[3]])) != 2){
      stop("The response variable in the test or training set is not binary")
    }

    if(FALSE %in% (x[[2]] <= 1) | FALSE %in% (x[[2]] >= 0) | FALSE %in% (x[[4]] <= 1) | FALSE %in% (x[[4]] >= 0)){
      stop("The probabilities of response variable in the test or training set are outside the range 0 - 1")
    }

  } else {
    stop("x must be a data frame or a list")
  }

  if(basic_model){
    set.seed(seed)

    n = c(
      sample(x = which(x[[y]] == 1), size = round(x = partition*sum(x[[y]] == 1), digits = 0)),
      sample(x = which(x[[y]] == 0), size = round(x = partition*sum(x[[y]] == 0), digits = 0))
    )

    x_train = x[n,]
    x_test = x[-n,]

    model = glm(formula = as.formula(paste(y ,"~ .")), data = x_train, family = binomial(link = link))
    data = list(
      "y_train" = x_train[[y]], "y_train_prob" = predict(object = model, type = "response"),
      "y_test" = x_test[[y]], "y_test_prob" = predict(object = model, newdata = x_test, type = "response")
      )
  } else {
    data = x
    names(data) = c("y_train", "y_train_prob", "y_test", "y_test_prob")
  }

  results = metrics(real = data[["y_test"]], predicted = ifelse(data[["y_test_prob"]] < 0.5, 0, 1))

  model_roc(y_train = data[["y_train"]], y_train_prob = data[["y_train_prob"]],
            y_test = data[["y_test"]], y_test_prob = data[["y_test_prob"]])

  model_pr(y_train = data[["y_train"]], y_train_prob = data[["y_train_prob"]],
           y_test = data[["y_test"]], y_test_prob = data[["y_test_prob"]], type = type_pr)

  model_ks(y_train = data[["y_train"]], y_train_prob = data[["y_train_prob"]],
           y_test = data[["y_test"]], y_test_prob = data[["y_test_prob"]], type = type_ks)

  model_gain(y_test = data[["y_test"]], y_test_prob = data[["y_test_prob"]])

  return(results)
}
