## Libraries##################
require(dplyr)
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
all_combinations <- map(1:3, ~ combn(c("X1", "X2", "X3", "X4"), .x, simplify = FALSE)) %>%
  flatten() %>%
  map(~ glue_collapse(.x, "+")) %>%
  unlist()

# Create a data frame with combinations and their sizes
combinations_df <- map_dfr(1:3, ~ {
  size <- .x
  combinations <-
    combn(c("X1", "X2", "X3", "X4"), size, simplify = FALSE) %>%
    map( ~ glue_collapse(.x, "+")) %>%
    unlist()
  data.frame(Model = combinations, size = size)
})

#Create an empty tibble with columns for MSE, Model_Size, and Variables
result_normal_dgp <- tibble(
  MSE = numeric(length(combinations_3) * 3),
  Model_Size = rep(c(3, 2, 1), each = length(combinations_3)),
  Variables = character(length(combinations_3) * 3)
)
#For loop to fill in the tibble with the MSE results
index <- 1
for (i in seq_along(combinations_3)) {
  for (j in 3:1) {
    mse_result <- mse(sim1_split$train, sim1_split$test, model = combinations_3[[i]])
    result_normal_dgp <- within(result_normal_dgp, {
      MSE[index] <- mse_result
      Variables[index] <- combinations_3[[i]]
    })
    index <- index + 1
  }
}






for (i in 1:length(combinations_3)) {
  mse_results_sim1_3[i] <- mse(sim1_split$train, sim1_split$test, model = glue(combinations_3[i]))
  mse_results_sim1_2[i] <- mse(sim1_split$train, sim1_split$test, model = glue(combinations_2[i]))
  mse_results_sim1_1[i] <- mse(sim1_split$train, sim1_split$test, model = glue(combinations_1[i]))

  mse_results_sim5_3[i] <- mse(sim5_split$train, sim5_split$test, model = glue(combinations_3[i]))
  mse_results_sim5_2[i] <- mse(sim5_split$train, sim5_split$test, model = glue(combinations_2[i]))
  mse_results_sim5_1[i] <- mse(sim5_split$train, sim5_split$test, model = glue(combinations_1[i]))
}


mse_results_sim3_3 <- c()
mse_results_sim3_2 <- c()
mse_results_sim3_1 <- c()

mse_results_sim7_3 <- c()
mse_results_sim7_2 <- c()
mse_results_sim7_1 <- c()
for (i in 1:length(combinations_3)) {
  mse_results_sim3_3[i] <- missclassification_rate(sim3_split$train, sim3_split$test, model = glue(combinations_3[i])) %>% bind_cols(., combinations_3[i])
  mse_results_sim3_2[i] <- missclassification_rate(sim3_split$train, sim3_split$test, model = glue(combinations_2[i]))
  mse_results_sim3_1[i] <- missclassification_rate(sim3_split$train, sim3_split$test, model = glue(combinations_1[i]))

  mse_results_sim7_3[i] <- missclassification_rate(sim7_split$train, sim7_split$test, model = glue(combinations_3[i]))
  mse_results_sim7_2[i] <- missclassification_rate(sim7_split$train, sim7_split$test, model = glue(combinations_2[i]))
  mse_results_sim7_1[i] <- missclassification_rate(sim7_split$train, sim7_split$test, model = glue(combinations_1[i]))
}


# combine the mse results into a dataframe in tidy
mse_results <- data.frame(
  MSE = c(mse_results_sim1_3, mse_results_sim1_2, mse_results_sim1_1, mse_results_sim5_3, mse_results_sim5_2, mse_results_sim5_1),
  Model_Size = rep(c(3, 2, 1), each = length(combinations_3)),
  Variables = rep(combinations_3, 6)
)



