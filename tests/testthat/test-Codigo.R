test_that("Data exist", {
  expect_no_error(sleep_data)
})

test_that("Metrics and Model", {
  sleep_data$Gender = as.numeric(ifelse(sleep_data$Gender == "Male", 0 , 1))
  expect_no_error(mtrglm(y = "Gender", x = sleep_data, basic_model = TRUE, seed = 300, link = "probit", partition = 0.81, type_ks = 1, type_pr = 3))
})

test_that("Separate metrics and graphs without Model", {
  sleep_data$Gender = as.numeric(ifelse(sleep_data$Gender == "Male", 0 , 1))
  x = sleep_data
  y = "Gender"
  partition = 0.75
  set.seed(100)
  n = c(
    sample(x = which(x[[y]] == 1), size = round(x = partition*sum(x[[y]] == 1), digits = 0)),
    sample(x = which(x[[y]] == 0), size = round(x = partition*sum(x[[y]] == 0), digits = 0))
  )

  x_train = x[n,]
  x_test = x[-n,]

  model = glm(Gender ~ Age + Stress.Level + Heart.Rate, family = binomial(link = "logit"), data = x_train)
  data = list(
    "y_train" = x_train[[y]], "y_train_prob" = predict(object = model, type = "response"),
    "y_test" = x_test[[y]], "y_test_prob" = predict(object = model, newdata = x_test, type = "response")
  )

  expect_no_error(metrics(data[["y_train"]], data[["y_train_prob"]]))
  expect_no_error(metrics(data[["y_test"]], data[["y_test_prob"]]))
  expect_no_error(model_roc(data[[1]], data[[2]], data[[3]], data[[4]]))
  expect_no_error(model_pr(data[[1]], data[[2]], data[[3]], data[[4]], type = 3))
  expect_no_error(model_ks(data[[1]], data[[2]], data[[3]], data[[4]], type = 3))
  expect_no_error(model_gain(data[[3]], data[[4]]))
  expect_no_error(mtrglm(x = data))
})

test_that("Error handle of arguments and data frame", {
  sleep_data$Gender = as.numeric(ifelse(sleep_data$Gender == "Male", 0 , 1))
  expect_error(mtrglm(y = "Gender", x = sleep_data, basic_model = "error", seed = 300, link = "probit", partition = 0.81, type_ks = 2, type_pr = 2))
  expect_error(mtrglm(y = "Gender", x = sleep_data, basic_model = FALSE, seed = "error", link = "probit", partition = 0.81, type_ks = 2, type_pr = 2))
  expect_error(mtrglm(y = "Gender", x = sleep_data, basic_model = TRUE, seed = 300, link = "error", partition = 0.81, type_ks = 2, type_pr = 2))
  expect_error(mtrglm(y = "Gender", x = sleep_data, basic_model = TRUE, seed = 300, link = "probit", partition = -2, type_ks = 2, type_pr = 2))
  expect_error(mtrglm(y = "error", x = sleep_data, basic_model = TRUE, seed = 300, link = "probit", partition = 0.81, type_ks = 2, type_pr = 2))
  expect_error(mtrglm(y = "Gender", x = sleep_data, basic_model = TRUE, seed = 300, link = "probit", partition = 0.81, type_ks = NaN, type_pr = 2))
  expect_error(mtrglm(y = "Gender", x = sleep_data, basic_model = TRUE, seed = 300, link = "probit", partition = 0.81, type_ks = 2, type_pr = NA))
  sleep_data$Gender[1] = 2
  expect_error(mtrglm(y = "Gender", x = sleep_data, basic_model = TRUE, seed = 300, link = "probit", partition = 0.81, type_ks = 2, type_pr = 2))
})

test_that("Error handle of list", {
  sleep_data$Gender = as.numeric(ifelse(sleep_data$Gender == "Male", 0 , 1))
  x = sleep_data
  y = "Gender"
  partition = 0.75
  set.seed(100)
  n = c(
    sample(x = which(x[[y]] == 1), size = round(x = partition*sum(x[[y]] == 1), digits = 0)),
    sample(x = which(x[[y]] == 0), size = round(x = partition*sum(x[[y]] == 0), digits = 0))
  )

  x_train = x[n,]
  x_test = x[-n,]

  model = glm(Gender ~ Age + Stress.Level + Heart.Rate, family = binomial(link = "logit"), data = x_train)
  data1 = list(
    "y_train" = "error", "y_train_prob" = predict(object = model, type = "response"),
    "y_test" = x_test[[y]], "y_test_prob" = predict(object = model, newdata = x_test, type = "response")
  )

  data2 = list(
    "y_train" = x_train[[y]], "y_train_prob" = predict(object = model, type = "response"),
    "y_test" = c(0,1,3), "y_test_prob" = predict(object = model, newdata = x_test, type = "response")
  )

  data3 = list(
    "y_train" = x_train[[y]], "y_train_prob" = c(0.1,0.2,3),
    "y_test" = x_test[[y]], "y_test_prob" = predict(object = model, newdata = x_test, type = "response")
  )

  expect_error(mtrglm(x = data1))
  expect_error(mtrglm(x = data2))
  expect_error(mtrglm(x = data3))
})
