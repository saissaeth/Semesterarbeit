## Libraries##################
library(randomForest)
library(Hmisc)
library(glue)
library(tidyverse)
library(grf)
library(dplyr)
library(gridExtra)
library("htesim")
library(tidyr)
library(purrr)
library(partykit)
##############################
pkgs <- c("randomForest", "Hmisc", "glue", "tidyverse", "grf", "dplyr", "gridExtra", "htesim", "tidyr", "purrr")
source("R/dgp.R") #source the dgp function
source("R/functions.R")
source("inst/empeval/paper1/run.R")
dgp1 <- dgp(p=0.5,m=0,t=0,sd=1,ol=0.5,model="normal",xmodel="unif",rmvar="X20")
sim <- simulate.dgp(object=dgp1,rho=0,nsim=1600,d=20,nsimtest=1000)

# lapply(pkgs,require)
############################# Apply DGP to simulate data and test performance of random forest ##############################
# set.seed(8008)
#
funA <- function(Varmiss){
  dgpA <- dgp(p = pF_eta_x1_x2 , m = mF_sin_x1_x5, t = tF_div_x1_x2, sd=2, ol = 0.5, model = "normal",
              xmodel = "unif",rmvar=Varmiss)
  return(dgpA)
}

funB <- function(Varmiss){
  dgpB <- dgp(p = 0.5 , m = mF_max_x1_x5, t = tF_log_x1_x2, sd = 1, ol = 0.5,model = "normal",
              xmodel = "unif",rmvar=Varmiss)
  return(dgpB)
}
funC <- function(Varmiss){
  dgpC <- dgp(p = pF_x2_x3 , m = mF_log_x1_x3, t = 1, sd = 1, ol = 0.5, model = "normal",
              xmodel = "unif",rmvar=Varmiss)
  return(dgpC)
}

funD <- function(Varmiss){
  dgpD <- dgp(p = pF_exp_x1_x2 , m = mF_max2_x1_x5, t = tF_max_x1_x5, sd = 1 , ol = 0.5, model = "normal",
              xmodel = "unif",rmvar=Varmiss)
  return(dgpD)
}

dim_vec <-
  list(
    c(),c(6:20),c(4,5),1L,c(1,2),c(1:3)
  )
dim_vec <- lapply(dim_vec, function(x) glue("X{x}"))
run_new <- function(sim){
  x_vars <- grep("^X", names(sim), value = TRUE)
  X <- as.matrix(sim[, x_vars, drop = FALSE])
  Y <- c(sim$y)
  W <- as.numeric(c(sim$trt))
  rf <- causal_forest(X = as.matrix(X), Y = Y, W = W, num.trees = 1000, seed = 8008)
  X_testdf <- attr(sim, "testxdf") %>% select(all_of(x_vars))
  tau.hat  <- predict(rf, X_testdf)$predictions
}


# Create the main tibble with nested vectors
nested_tibble <- tibble(
  Row = LETTERS[1:4],
  Data = list(dim_vec, dim_vec, dim_vec, dim_vec)
) %>%
  mutate(
    # Nest each dim_vec into a tibble, apply functions based on Row, and then apply simulate.dgp
    Data = map2(Data, Row, ~ tibble(
      dim_vec = .x,
      FunCall = glue("fun{.y}"),  # Dynamic function names based on Row
      Result = map2(.x, FunCall, ~ match.fun(get(.y))(.x)),  # Apply dynamic functions
      Simulated = map(
        Result,
        ~ simulate.dgp(object = ., rho = 0, nsim = 1600L, d = 20L, nsimtest = 1000L)),
      RunResult = map(Simulated, ~ run_new(sim = .))  # Apply run
    ))
  )
#
# X <- as.matrix(sim1[, c(x_vars), drop = FALSE])
# Y <- c(sim1$y)
# W <- as.numeric(c(sim1$trt))
# rf <- causal_forest(X = as.matrix(X), Y = Y, W = W, num.trees = 1000, seed = seed)
# X_testdf <- attr(sim1, "testxdf") %>% select(-y, -trt)
# tau.hat  <- predict(rf, X_testdf)$predictions
#
#
# # head(sim1)
# # nrow(sim1)
# #
# #
# # testdf <- attr(sim1, "testxdf")
# # head(testdf)
# #
# #
# # predtest <- predict(object = dgp1, newdata = testdf)
# # head(predtest)
# # load(system.file("extdata/dandl.rda", package = "htesim"))
# # head(dandl)
# # #select setup repl p m t sd ol model xmodel nsim dim nsimtest seed from dandl
# # dim_vec <- list(c(1:20), c(1:5), c(1:3, 6:20), c(2:20), c(3:20), c(4:20))
# #
# # selected_data <- dandl %>%
# #   select(setup,repl, p, m, t, sd, ol, nsim, nsimtest, seed) %>%
# #   mutate(nsim = 1600) %>%
# #   distinct() %>%
# #   ungroup() %>%
# #   group_by(setup) %>%
# #   View()
# #   slice(rep(1:n(), each = 6)) %>%
# #   ungroup() %>%  #to remove duplicates as only nsim =1600
# #   mutate(dim_vec = rep(dim_vec, 1600)) %>%
# #   mutate(dim = as.integer(rep(20,9600)))
#
#
# # subset <- dandl %>%
# #   select(setup, p, m, t, nsim, nsimtest, seed) %>%
# #   mutate(nsim = 10) %>%
# #   mutate(nsimtest = 10) %>%
# #   mutate(t=0) %>%
# #   distinct() %>%
#
# #
# # study1 <- selected_data[1,]
# #
# #
# dgp1 <- do.call(dgp,
#                  as.list(selected_data)[c("p", "m", "t", "sd", "ol", "model", "xmodel")])
# #
#  sim1 <- do.call(simulate.dgp, c(list(object = dgp1),
#                              as.list(selected_data)[c("nsim", "dim", "seed", "nsimtest")]))
# # #
# #  head(sim1)
# # #
# # #
# # testdf <- attr(sim1, "testxdf")
# # head(testdf)
# # #
# # tau <- predict(dgp1, newdata = testdf)[, "tfct"]
# # head(tau)
# #
#
# #Apply the function for all the setups
# selected_data <- dandl
#
#
#
#
# selected_data <- sim1 %>%
#   rowwise() %>%
#   mutate(result = run(setup)) %>%
#   ungroup()
#
#
# run(data = selected_data, causal_forest = TRUE,
#     prognostic_effect = FALSE, honesty = FALSE)
#
#
#
# fun.cf <- function(instance, ...) {
#   run(instance, causal_forest = TRUE,
#       prognostic_effect = FALSE, honesty = FALSE)
# }
#
# apply_dgp_rho <- function(dim,seed,p, m, t, sd, ol) {
#   dgp1 <- dgp(p = p, m = m, t = t, model = "normal", xmodel = "unif")
#   sim1 <- simulate.dgp(object = dgp1, rho = 0, nsim = 1000, d = 20, nsimtest = 1600)
#   excluded_vars <- c("y", "trt")
#   x_vars <- paste0("X", dim)
#   X <- as.matrix(sim1[, c(x_vars), drop = FALSE])
#   Y <- c(sim1$y)
#   W <- as.numeric(c(sim1$trt))
#   rf <- causal_forest(X = as.matrix(X), Y = Y, W = W, num.trees = 1000, seed = seed)
#   testdf <- attr(sim1, "testxdf")
#   pml <- predict(rf, type = "coef", OOB = TRUE)
#   resid <- pml$predictions - Y
#   pm <- do.call("rbind", pml)
#   pmd <- as.data.frame(pm)
#   tau_hat <- rf$tau.hat
#   return(list(tau_hat = pmd$tau, resid = resid))
# }
#
# results <- selected_data %>%
#   rowwise() %>%
#   mutate(result = list(apply_dgp_rho(dim = dim_vec, seed = seed,p, m, t, sd, ol))) %>%
#   ungroup() %>%
#   group_by(setup, dim_vec) %>%
#   summarise(tau_mse = result$tau-, y_mse = result$resid)
#
#
# results_random <- selected_data %>%
#   group_by(setup, dim_vec,repl) %>%
#   summarise(tau = rnorm(10, 0,1), mse = abs(rnorm(10,1,1)))
#
#
# methodnams <- c("1-20", "1-5", "-c(4,5)", "-1", "-c(1,2)","-c(1,3)")
#
#
#
# cols <- c("1-20" = "snow", "1-5" = "snow4",
#           "-c(4,5)" = "tan",  "-1" = "tan4",
#           "-c(1,2)" = "darkorchid1", "-c(1,3)" = "darkorchid4")
# # lookup <- c(
# #   "1-20" = "1-20",
# #   "1-5" = "1-5",
# #   "-c(4,5)" = "-c(4,5)",
# #   "-1" = "-1",
# #   "-c(1,2)" = "-c(1,2)",
# #   "-c(1,3)" = "-c(1,3)"
# # )
# colornams <- cols
#
# methodnams <- c("1-20", "1-5", "-c(4,5)", "-1", "-c(1,2)","-c(1,3)")
#
# library("lattice")
# library("latticeExtra")
#
# # Lattice options and plot appearance
# trellis.par.set(list(plot.symbol = list(col="black",pch=18, cex=0.75),
#                      box.rectangle = list(col=1),
#                      box.umbrella = list(lty=1, col=1),
#                      strip.background = list(col = "white")))
# ltheme <- canonical.theme(color = FALSE)     # black and white theme
# ltheme$strip.background$col <- "transparent" # change strip background
# lattice.options(default.theme = ltheme)
#
# # Setup rows and columns of plot
# sc <- function(which.given, ..., factor.levels) {
#   if (which.given == 2) {
#     strip.default(which.given = which.given, ..., factor.levels)
#   } else {
#     strip.default(which.given = 1, ...,
#                   factor.levels)
#   }
# }
#
# # Fill each entry with boxplots
# mypanel <- function(x, y, groups, subscripts, ...) {
#   fill <- cols[intersect(levels(x), unique(x))]
#   panel.bwplot(x = x, y = y, fill = fill, ...)
#   tapply(1:length(y), groups[subscripts], function(i) {
#     xi <- 1:nlevels(x)
#     yi <- y[i][match(xi, unclass(x[i]))]
#     llines(x = xi, y = yi,
#            col = rgb(0.1, 0.1, 0.1, 0.03))
#   })
# }
#
# # set up legend at top of plots
# mykey <- list(space="top", rectangles=list(col=cols),
#               text=list(colornams), columns = ceiling(length(methodnams)/2))
#
# # label of y-axis, MSE
# ylab <- expression(paste(frac(1, 1000), "",
#                          sum((tau(x[i]) - hat(tau)(x[i]))^2, i == 1, 1000)))
#
# # plotting function
# plot_results <- function(results_random, sc, ylim) {
#   plt <- bwplot(tau ~ as.numeric(setup) + dim_vec %>%
#                   unlist() %>%
#                   factor() %>%
#                   as.numeric(), data = results_random,
#                 ylab = list(ylab), ylim = ylim,
#                 groups = repl, panel = mypanel,
#                 as.table = TRUE, strip = sc, key = mykey,
#                 scales = list(relation = "same", x = list(rot = 60, labels = colornams)))
#   useOuterStrips(plt, strip = sc)
# }
# res <- results_random
# plot_results(res, sc = TRUE)
#
# lookup
# # TODO fix Dims variable to be ABCDE
# # Add additional plots for different values of tau
# # Add additional sims
# # Ask Hothorn about tau hat
# # loop through taus
# # Add MSE visualization for residuals
#
#
#
#
#
