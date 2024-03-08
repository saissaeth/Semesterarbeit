## Libraries##################

require(randomForest)
require(Hmisc)
require(glue)
require(tidyverse)
##############################

############################# Apply DGP to simulate data and test performance of random forest ##############################
dgp1 <-
  dgp(
    p = 0.5,
    m = 0,
    t = 0,
    sd = 1,
    ol = 0,
    model = c("normal"),
    xmodel = c("normal"),
    rmvar = NULL
  ) #continuous outcome
#dgp2 <- dgp(p = 0.5, m = 0, t = 0, sd = 1, ol = 0, model = c("weibull"), xmodel = c("normal"), rmvar = NULL)
dgp3 <-
  dgp(
    p = 0.5,
    m = 0,
    t = 0,
    sd = 1,
    ol = 0,
    model = c("binomial"),
    xmodel = c("normal"),
    rmvar = NULL
  ) #binary outcome
dgp4 <-
  dgp(
    p = 0.5,
    m = 0,
    t = 0,
    sd = 1,
    ol = 0,
    model = c("polr"),
    xmodel = c("normal"),
    rmvar = NULL
  ) #ordinal outcome

dgp5 <-
  dgp(
    p = 0.5,
    m = 0,
    t = 0,
    sd = 1,
    ol = 0,
    model = c("normal"),
    xmodel = c("unif"),
    rmvar = NULL
  ) # continuous outcome
#dgp6 <- dgp(p = 0.5, m = 0, t = 0, sd = 1, ol = 0, model = c("weibull"), xmodel = c("unif"), rmvar = NULL)
dgp7 <-
  dgp(
    p = 0.5,
    m = 0,
    t = 0,
    sd = 1,
    ol = 0,
    model = c("binomial"),
    xmodel = c("unif"),
    rmvar = NULL
  ) #binary outcome
dgp8 <-
  dgp(
    p = 0.5,
    m = 0,
    t = 0,
    sd = 1,
    ol = 0,
    model = c("polr"),
    xmodel = c("unif"),
    rmvar = NULL
  ) #ordinal outcome

######################### simulate ###############
num_sim <- 1000
sim1 <- simulate.dgp(dgp1, num_sim)
#sim2 <- simulate.dgp(dgp2, num_sim)
sim3 <- simulate.dgp(dgp3, num_sim)
sim4 <- simulate.dgp(dgp4, num_sim)
sim5 <- simulate.dgp(dgp5, num_sim)
#sim6 <- simulate.dgp(dgp6, num_sim)
sim7 <- simulate.dgp(dgp7, num_sim)
sim8 <- simulate.dgp(dgp8, num_sim)
#################################################


# Create a train test split of simulated data ##################
train_test_split <- function(data, train_size = 0.8) {
  train_index <- sample(1:nrow(data), train_size * nrow(data))
  train <- data[train_index, ]
  test <- data[-train_index, ]
  return(list(train = train, test = test))
}

######### Apply train test split to all simulated data ##############
sim1_split <- train_test_split(sim1)
#sim2_split <- train_test_split(sim2)
sim3_split <- train_test_split(sim3)
sim4_split <- train_test_split(sim4)
sim5_split <- train_test_split(sim5)
#sim6_split <- train_test_split(sim6)
sim7_split <- train_test_split(sim7)
sim8_split <- train_test_split(sim8)
########################################################################


#Find MSE on test performance
mse <- function(train, test, model) {
  rf <-
    randomForest(y ~ ., data = train[, c("y",  unlist(strsplit(model, "\\+")))], ntree = 100)
  y_hat <- predict(rf, test)
  return(mean((as.numeric(y_hat) - test$y) ^ 2)) #return MSE
}

#Find missclassification rate
missclassification_rate <- function(train, test, model) {
  rf <-
    randomForest(y ~ ., data = train[, c("y",  unlist(strsplit(model, "\\+")))], ntree = 100)
  y_hat <- predict(rf, test)
  return(sum(y_hat != test$y) / length(test$y)) #return missclassification rate
}

#Find concordance index
concordance.index <- function(train, test, model) {
  rf <- randomForest(y ~ paste0(model), data = train, ntree = 100)
  y_hat <- predict(rf, test)
  return(y_hat) #return concordance index
  #Find err.rate on predictions on test data
}
############ Create all combinations of X1, X2, X3, X4 of size 3,2,1

#glue rows into one string x1+x2+x3
combinations_3 <- combn(c("X1", "X2", "X3", "X4"), 3, simplify = FALSE) %>%
  map(~ glue_collapse(.x, "+")) %>%
  unlist()

# Create a table of combinations of c("X1","X2","X3","X4") of size 2
combinations_3 <- combn(c("X1", "X2", "X3", "X4"), 2, simplify = FALSE) %>%
  map(~ glue_collapse(.x, "+")) %>%
  unlist()

# Create and glue for size 1
combinations_1 <- c("X1","X2","X3","X4")
result_1 <- list()
result_2 <- list()
result_3 <- list()
result_4 <- list()
result_5 <- list()
result_6 <- list()
result_7 <- list()
result_8 <- list()

for (k in c(1,5)) {
  assign(glue("result_", k),
         data.frame(
           MSE = numeric(length(combinations_3) * 3),
           Model_Size = rep(c(3, 2, 1), each = length(combinations_3)),
           Variables = character(length(combinations_3) * 3)
         ))

  index <- 1

  for (i in seq_along(combinations_3)) {
    for (j in 3:1) {
      mse_result <- mse(get(glue("sim", k, "_split"))$train,
                        get(glue("sim", k, "_split"))$test,
                        model = combinations_3[[i]])
      assign(glue("result_", k),
             within(get(glue("result_", k)), {
               MSE[index] <- mse_result
               Variables[index] <- combinations_3[[i]]
             }),
             envir = .GlobalEnv)
      index <- index + 1
    }
  }
}

for (k in c(3,7)) {
  assign(glue("result_", k),
         data.frame(
           missclassification = numeric(length(combinations_3) * 3),
           Model_Size = rep(c(3, 2, 1), each = length(combinations_3)),
           Variables = character(length(combinations_3) * 3)
         ))

  index <- 1

  for (i in seq_along(combinations_3)) {
    for (j in 3:1) {
      miss_rate <- missclassification_rate(get(glue("sim", k, "_split"))$train,
                        get(glue("sim", k, "_split"))$test,
                        model = combinations_3[[i]])
      assign(glue("result_", k),
             within(get(glue("result_", k)), {
               missclassification[index] <- miss_rate
               Variables[index] <- combinations_3[[i]]
             }),
             envir = .GlobalEnv)
      index <- index + 1
    }
  }
}

miss_rate
