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
library("lattice")
library("latticeExtra")
##############################
# pkgs <- c("randomForest", "Hmisc", "glue", "tidyverse", "grf", "dplyr", "gridExtra", "htesim", "tidyr", "purrr")
source("R/dgp.R") #source the dgp function
source("R/functions.R")
source("inst/empeval/paper1/run.R")
source("R/functions.R")
# dgp1 <- dgp(p=0.5,m=0,t=0,sd=1,ol=0.5,model="normal",xmodel="unif",rmvar="X20")
# sim <- simulate.dgp(object=dgp1,rho=0,nsim=1600,d=20,nsimtest=1000)

# lapply(pkgs,require)
############################# Apply DGP to simulate data and test performance of random forest ##############################
set.seed(8008)
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
##########################################################################################

################################## Initialise the helper functions ####################
dim_vec <-
  list(
    c(),c(6:20),c(4,5),1L,c(1,2),c(1:3)
  )
dim_vec <- lapply(dim_vec, function(x) glue("X{x}"))

run_new <- function(Setup,Varmiss,rhos){
  dgp <- get(glue("fun{Setup}"))(Varmiss = Varmiss)
  sim <- simulate.dgp(object = dgp, rho = rhos, nsim = 1600L, d = 20L, nsimtest = 1000L)
  x_vars <- grep("^X", names(sim), value = TRUE)
  X <- as.matrix(sim[, x_vars, drop = FALSE])
  Y <- c(sim$y)
  W <- as.numeric(c(sim$trt))
  rf <- causal_forest(X = as.matrix(X), Y = Y, W = W, num.trees = 1000, seed = 8008)
  X_test_all <- attr(sim, "testxdf")
  X_test <- attr(sim, "testxdf") %>% select(all_of(x_vars))
  true_tau <- predict(sim,X_test_all)$tfct
  tau.hat  <- predict(rf, X_test)$predictions
  return(MSE = mean((true_tau - tau.hat)^2))
}

results <-
  expand_grid(
    Setup = LETTERS[1:4],
    Varmiss = paste0(dim_vec),
    repl = seq(1, 10, 1),
    rho = c(0,0.25,0.5,0.75)
  ) %>%
  mutate(Setup = as.factor(Setup)) %>%
  mutate(Varmiss = paste0(Varmiss)) %>%
  mutate(Varmiss = as.factor(Varmiss)) %>%
  rowwise() %>%
  mutate(MSE = run_new(
    Setup = Setup,
    Varmiss = Varmiss,
    rhos = rho
  ))

save(results,file="results_cf.Rda")
# methodnams <- c("1-20", "1-5", "-c(4,5)", "-1", "-c(1,2)","-c(1,3)")
#
#
# levels(grouped_result$dim_vec) <- methodnams
#
# #################################################################################
# ############################## Plot the results #################################
# #################################################################################
# ######
#
# plt <- bwplot(All_Mse ~ setup | dim_vec, data = data,
#               ylab = list(ylab), ylim = ylim,
#               groups = setup, panel = mypanel,
#               as.table = TRUE, strip = sc, key = mykey,
#               scales = list(relation = "same", x = list(rot = 60, labels = colornams)))
# # Plotting function
# plot_results <- function(data, sc, ylim) {
#   plt <- bwplot(All_Mse ~ setup | dim_vec, data = data,
#                 ylab = list(ylab), ylim = ylim,
#                 groups = setup, panel = mypanel,
#                 as.table = TRUE, strip = sc, key = mykey,
#                 scales = list(relation = "same", x = list(rot = 60, labels = colornams)))
#   useOuterStrips(plt, strip = sc)
# }
#
# # Now call the plot_results function with the long-form data
# plot_results(results_long, sc = sc, ylim = c(0, max(unlist(grouped_result$All_Mse))))
#
# results %>%
#   mutate(Varmiss = as.factor(Varmiss)) %>%
#   mutate(Setup = as.factor(Setup))
#
# p <- ggplot(results, aes(x = Varmiss, y = MSE)) +
#   geom_boxplot() +
#   facet_wrap(~ Setup, scales = 'free') +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   labs(x = "Varmiss", y = "MSE", title = "MSE by Varmiss and Setup")
# print(p)
#
#
#
# ####
# methodnams <- c("1-20", "1-5", "-c(4,5)", "-1", "-c(1,2)","-c(1,3)")
# #
# #
# #
# # cols <- c("1-20" = "snow", "1-5" = "snow4",
# #           "-c(4,5)" = "tan",  "-1" = "tan4",
# #           "-c(1,2)" = "darkorchid1", "-c(1,3)" = "darkorchid4")
# # # lookup <- c(
# # #   "1-20" = "1-20",
# # #   "1-5" = "1-5",
# # #   "-c(4,5)" = "-c(4,5)",
# # #   "-1" = "-1",
# # #   "-c(1,2)" = "-c(1,2)",
# # #   "-c(1,3)" = "-c(1,3)"
# # # )
# # colornams <- cols
# #
# # methodnams <- c("1-20", "1-5", "-c(4,5)", "-1", "-c(1,2)","-c(1,3)")
# #
# # library("lattice")
# # library("latticeExtra")
# #
# # # Lattice options and plot appearance
# # trellis.par.set(list(plot.symbol = list(col="black",pch=18, cex=0.75),
# #                      box.rectangle = list(col=1),
# #                      box.umbrella = list(lty=1, col=1),
# #                      strip.background = list(col = "white")))
# # ltheme <- canonical.theme(color = FALSE)     # black and white theme
# # ltheme$strip.background$col <- "transparent" # change strip background
# # lattice.options(default.theme = ltheme)
# #
# # # Setup rows and columns of plot
# # sc <- function(which.given, ..., factor.levels) {
# #   if (which.given == 2) {
# #     strip.default(which.given = which.given, ..., factor.levels)
# #   } else {
# #     strip.default(which.given = 1, ...,
# #                   factor.levels)
# #   }
# # }
# #
# # # Fill each entry with boxplots
# # mypanel <- function(x, y, groups, subscripts, ...) {
# #   fill <- cols[intersect(levels(x), unique(x))]
# #   panel.bwplot(x = x, y = y, fill = fill, ...)
# #   tapply(1:length(y), groups[subscripts], function(i) {
# #     xi <- 1:nlevels(x)
# #     yi <- y[i][match(xi, unclass(x[i]))]
# #     llines(x = xi, y = yi,
# #            col = rgb(0.1, 0.1, 0.1, 0.03))
# #   })
# # }
# #
# # # set up legend at top of plots
# # mykey <- list(space="top", rectangles=list(col=cols),
# #               text=list(colornams), columns = ceiling(length(methodnams)/2))
# #
# # # label of y-axis, MSE
# # ylab <- expression(paste(frac(1, 1000), "",
# #                          sum((tau(x[i]) - hat(tau)(x[i]))^2, i == 1, 1000)))
# #
# # # plotting function
# # plot_results <- function(results_random, sc, ylim) {
# #   plt <- bwplot(tau ~ as.numeric(setup) + dim_vec %>%
# #                   unlist() %>%
# #                   factor() %>%
# #                   as.numeric(), data = results_random,
# #                 ylab = list(ylab), ylim = ylim,
# #                 groups = repl, panel = mypanel,
# #                 as.table = TRUE, strip = sc, key = mykey,
# #                 scales = list(relation = "same", x = list(rot = 60, labels = colornams)))
# #   useOuterStrips(plt, strip = sc)
# # }
# # res <- results_random
# # plot_results(res, sc = TRUE)
# #
# # lookup
# # # TODO fix Dims variable to be ABCDE
# # # Add additional plots for different values of tau
# # # Add additional sims
# # # Ask Hothorn about tau hat
# # # loop through taus
# # Add MSE visualization for residua
