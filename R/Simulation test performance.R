## Libraries##################
library(randomForest)
library(Hmisc)
library(glue)
library(tidyverse)
library(grf)
library(gridExtra)
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
    t = 1,
    sd = 1,
    ol = 0,
    model = c("normal"),
    xmodel = c("normal"),
    rmvar = NULL
  )

# Create a function to apply DGP and simulate data for different rho sizes
apply_dgp_rho <- function(dgp, num_sim, dim, rho) {
  sim <- simulate.dgp(dgp, num_sim, dim = 20, rho = rho)
  excluded_vars <- c("y", "trt")
  x_vars <- paste0("X", dim)
  X <- as.matrix(sim[, c(x_vars), drop = FALSE])
  Y <- c(sim$y)
  W <- as.numeric(c(sim$trt))
  rf <- causal_forest(X = as.matrix(X), Y = Y, W = W, num.trees = 1000, seed = 290875)
  pml <- predict(rf, type = "coef", OOB = TRUE)
  resid <- pml$predictions - Y
  pm <- do.call("rbind", pml)
  pmd <- as.data.frame(pm)
  names(pmd)[length(names(pmd))] <- "tau"
  tau <- pmd[["tau"]][1]
  return(list(tau_hat = tau, resid = resid))
}

# Initialize vectors to collect the data
rhos <- numeric()
models <- character()
taus <- numeric()
tau_hats <- numeric()
residuals <- list()
results <- list()
dims <- list()
suffixes <- c("zero", "small", "med", "big", "one")
tau_vec <- c(0, 0.25, 0.5, 0.75, 1)
rho_sizes <- c(0,0.25,0.5,0.75)
# Define your dimensions and corresponding labels
dim_vec <- list(c(1:20), c(1:5), c(1:3, 6:20), c(2:20), c(3:20), c(4:20))
labels <- t(c("A", "B", "C", "D", "E","F"))
bind_cols(dim_vec, labels)

# Create the tibble
model_dim_tibble <- tibble(Model = labels, Dimensions = dim_vec, True_Tau = tau_vec, Rho = rho_sizes, stringsAsFactors = FALSE)

# Loop through the dimensions and rho sizes
single_iteration <- function(Model, rho, Dim) {
  model_name <- glue("dgp1_{Model}")
  result <- apply_dgp_rho(get(model_name), 10, Dim, rho) # Your function call
  list(tau_hat = result$tau_hat, resid = result$resid, rho = rho, Forest = paste0(model_dim_tibble$Model), model = model_name)
}

# Expand grid for all combinations of dimensions, suffixes and rho sizes
all_iterations <- model_dim_tibble %>%
  expand(Model=model_dim_tibble$Model , Dim = model_dim_tibble$Dimensions, rho = as.numeric(rho_sizes), True_Tau= suffixes)

# Apply the function to all combinations
results_list <- apply(all_iterations, 1, function(row) single_iteration(model_dim_tibble[row["Dim"]], row["Rho"], row["True_Tau"]))



results_list <- apply(all_iterations, 1, function(row) {
  rho <- as.numeric(row["Rho"])  # Convert rho to numeric
  single_iteration(
    dim_vec[[row["Dim"]]],  # Access the correct dimension from dim_vec using double brackets
    rho,
    row["True_Tau"]
  )
})


# for (dim in dim_vec){
#   for (rho in rho_sizes) {
#     for (suffix in suffixes) {
#       model_name <- glue("dgp1_{suffix}")
#         result <- apply_dgp_rho(get(model_name), 10, dim, rho) # Placeholder for your function's result
#         # Collect the data
#         #Extract pred
#         results <- c(results, result)
#         tau_hats <- c(tau_hats, result$tau_hat)
#         residuals <- c(residuals, result$resid)
#         taus <- c(taus, tau)
#         rhos <- c(rhos, rho)
#         dim <- c(dims,dim)
#
#         models <- c(models, model_name)
#     }
#   }
# }
#
# length(tau_hats)
#
# # Now create the tibble
# pmd_rho <- tibble(rho = rhos, model = models, tau = taus, tau_hats = tau_hats, dim = as_factor(glue(dims)))
# # Print the results

pmd_rho %>%
  filter(tau == 0.25) %>%
  ggplot(aes(x=dim, y=tau_hats)) +
  geom_point()+
  #facet_wrap(~model)+
  facet_grid(rho~model)+
  theme_classic()


# TODO fix Dims variable to be ABCDE
# Add additional plots for different values of tau
# Add additional sims
# Ask Hothorn about tau hat
# loop through taus
# Add MSE visualization for residuals
