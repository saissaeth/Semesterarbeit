require(randomForest)
require(Hmisc)
dgp1 <- dgp(p = 0.5, m = 0, t = 0, sd = 1, ol = 0, model = c("normal"), xmodel = c("normal"), rmvar = NULL) #continuous outcome
#dgp2 <- dgp(p = 0.5, m = 0, t = 0, sd = 1, ol = 0, model = c("weibull"), xmodel = c("normal"), rmvar = NULL)
dgp3 <- dgp(p = 0.5, m = 0, t = 0, sd = 1, ol = 0, model = c("binomial"), xmodel = c("normal"), rmvar = NULL) #binary outcome
dgp4 <- dgp(p = 0.5, m = 0, t = 0, sd = 1, ol = 0, model = c("polr"), xmodel = c("normal"), rmvar = NULL) #ordinal outcome

dgp5 <- dgp(p = 0.5, m = 0, t = 0, sd = 1, ol = 0, model = c("normal"), xmodel = c("unif"), rmvar = NULL) # continuous outcome
#dgp6 <- dgp(p = 0.5, m = 0, t = 0, sd = 1, ol = 0, model = c("weibull"), xmodel = c("unif"), rmvar = NULL)
dgp7 <- dgp(p = 0.5, m = 0, t = 0, sd = 1, ol = 0, model = c("binomial"), xmodel = c("unif"), rmvar = NULL) #binary outcome
dgp8 <- dgp(p = 0.5, m = 0, t = 0, sd = 1, ol = 0, model = c("polr"), xmodel = c("unif"), rmvar = NULL) #ordinal outcome

num_sim <- 1000
sim1 <- simulate.dgp(dgp1, num_sim)
#sim2 <- simulate.dgp(dgp2, num_sim)
sim3 <- simulate.dgp(dgp3, num_sim)
sim4 <- simulate.dgp(dgp4, num_sim)
sim5 <- simulate.dgp(dgp5, num_sim)
#sim6 <- simulate.dgp(dgp6, num_sim)
sim7 <- simulate.dgp(dgp7, num_sim)
sim8 <- simulate.dgp(dgp8, num_sim)

# Create a train test split of simulated data
train_test_split <- function(data, train_size = 0.8) {
  train_index <- sample(1:nrow(data), train_size * nrow(data))
  train <- data[train_index, ]
  test <- data[-train_index, ]
  return(list(train = train, test = test))
}

sim1_split <- train_test_split(sim1)
#sim2_split <- train_test_split(sim2)
sim3_split <- train_test_split(sim3)
sim4_split <- train_test_split(sim4)
sim5_split <- train_test_split(sim5)
#sim6_split <- train_test_split(sim6)
sim7_split <- train_test_split(sim7)
sim8_split <- train_test_split(sim8)

#Find MSE on test performance
mse <- function(train, test, model) {
  rf <- randomForest(y ~ ., data = train[, c("y", model)], ntree = 100)
  y_hat <- predict(rf, test)
  return(mean((as.numeric(y_hat) - test$y)^2)) #return MSE
}

#Find missclassification rate
missclassification_rate <- function(train, test,model) {
  rf <- randomForest(y ~ glue(model), data = train, ntree = 100)
  y_hat <- predict(rf, test)
  return(sum(y_hat != test$y)/length(test$y)) #return missclassification rate
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
combinations_2 <- combn(c("X1","X2","X3","X4"), 2) %>%
  as_tibble() %>%
  t() %>%
  apply(X = ., 1, function(x) glue_collapse(x, "+"))

# Create and glue for size 1
combinations_1 <- c("X1","X2","X3","X4")


# Create a loop which finds mse for all combinations of X1, X2, X3, X4 of sizes 3,2,1
mse_results_sim1 <- c()
mse_results_sim5 <- c()
for (i in 1:length(combinations_3)) {
  mse_results_sim1[i] <- mse(sim1_split$train, sim1_split$test, model = glue(combinations_3[i]))
  mse_results_sim5[i] <- mse(sim5_split$train, sim5_split$test, model = glue(combinations_3[i]))
}
#choose only variables x1 and x2 in tidy with subset


mse_results_sim1 <- numeric(length(combinations_3))
mse_results_sim5 <- numeric(length(combinations_3))

for (i in seq_along(combinations_3)) {
  mse_results_sim1[i] <- mse(sim1_split$train, sim1_split$test, model = combinations_3[[i]])
  mse_results_sim5[i] <- mse(sim5_split$train, sim5_split$test, model = combinations_3[[i]])
}
