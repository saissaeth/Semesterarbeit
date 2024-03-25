## Libraries##################
library(randomForest)
library(Hmisc)
library(glue)
library(tidyverse)
library(grf)
##############################

source("R/dgp.R") #source the dgp function
############################# Apply DGP to simulate data and test performance of random forest ##############################
dgp1_zero <-
  dgp(
    p = 0.5,
    m = 0,
    t = 0,
    sd = 1,
    ol = 0,
    model = c("normal"),
    xmodel = c("normal"),
    rmvar = NULL
  )
dgp1_small <-
  dgp(
    p = 0.5,
    m = 0,
    t = 0.25,
    sd = 1,
    ol = 0,
    model = c("normal"),
    xmodel = c("normal"),
    rmvar = NULL
  )
dgp1_med <-
  dgp(
    p = 0.5,
    m = 0,
    t = 0.5,
    sd = 1,
    ol = 0,
    model = c("normal"),
    xmodel = c("normal"),
    rmvar = NULL
  )
dgp1_big <-
  dgp(
    p = 0.5,
    m = 0,
    t = 0.75,
    sd = 1,
    ol = 0,
    model = c("normal"),
    xmodel = c("normal"),
    rmvar = NULL
  )
dgp1_one <-
  dgp(
    p = 0.5,
    m = 0,
    t = 0,
    sd = 1,
    ol = 0,
    model = c("normal"),
    xmodel = c("normal"),
    rmvar = NULL
  )
#continuous outcome

# dgp5 <-
#   dgp(
#     p = 0.5,
#     m = 0,
#     t = 0.123,
#     sd = 1,
#     ol = 0,
#     model = c("normal"),
#     xmodel = c("unif"),
#     rmvar = 10
#   ) # continuous outcome



######################### simulate ###############
num_sim <- 1600
sim1 <- simulate.dgp(dgp1, num_sim,dim=20)
str(sim1)
#sim2 <- simulate.dgp(dgp2, num_sim)
# sim3 <- simulate.dgp(dgp3, num_sim)
# sim4 <- simulate.dgp(dgp4, num_sim)
# sim5 <- predict.dgp(dgp5, num_sim,dim = 20)
#sim6 <- simulate.dgp(dgp6, num_sim)
# sim7 <- simulate.dgp(dgp7, num_sim)
#sim8 <- simulate.dgp(dgp8, num_sim)
#################################################
apply_cf <- function(dgp, num_sim, dim) {
  sim <- simulate.dgp(dgp, num_sim,dim=20)
  excluded_vars <- c("y", "trt")
  x_vars <- paste0("X", 1:dim)
  X <- as.matrix(sim[, c(x_vars), drop = FALSE])
  Y <- c(sim$y)
  W <- as.numeric(c(sim$trt))
  rf <- causal_forest(X=as.matrix(X), Y=Y, W=W, num.trees=1000, seed=290875)
  pml <- predict(rf, type = "coef", OOB =TRUE)
  pm <- do.call("rbind", pml)
  pmd <- as.data.frame(pm)
  names(pmd)[length(names(pmd))] <- "tau"
  return(pmd$tau)
}

pmd_A_1 <- apply_cf(dgp1_zero, 1600, 20)
pmd_A_2 <- apply_cf(dgp1_small, 1600, 20)
pmd_A_3 <- apply_cf(dgp1_med, 1600, 20)
pmd_A_4 <- apply_cf(dgp1_big, 1600, 20)
pmd_A_5 <- apply_cf(dgp1_one, 1600, 20)

#Add lines to ggplot with tau of model B and C
ptau <- ggplot(pmd_A, aes(tau)) +
  geom_density() +
  xlab(expression(hat(tau)(bold(x)))) +
  scale_linetype_manual(name = "", values = 2, guide = guide_legend(override.aes = list(color = c("black")))) +
  theme(legend.position = c(0.2, 0.8), legend.title = element_blank())

ptau <- ggplot(pmd, aes(tau)) +
  geom_density() +
  xlab(expression(hat(tau)(bold(x)))) +
  scale_linetype_manual(name = "", values = 2, guide = guide_legend(override.aes = list(color = c("black")))) +
  theme(legend.position = c(0.2, 0.8), legend.title = element_blank())
ptau


# Apply the function to the dgp1
pmd1 <- apply_cf(dgp1, 1600, 20)
pmd2 <-











#Find MSE on test performance
mse <- function(train, test, model) {
  rf <-
    causal_forest(y ~ ., data = train[, c("y",  unlist(strsplit(model, "\\+")))], ntree = 100)
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

