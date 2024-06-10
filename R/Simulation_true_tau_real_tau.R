## Libraries##################
#library(randomForest)
library(Hmisc)
library(glue)
library(tidyverse)
library(grf)
library(dplyr)
library(gridExtra)
#library("htesim")
library(tidyr)
library(purrr)
#library(partykit)
#library("lattice")
#library("latticeExtra")
##############################
pkgs <- c("randomForest", "Hmisc", "glue", "tidyverse", "grf", "dplyr", "gridExtra", "htesim", "tidyr", "purrr")
#Set wd one layer before
source("R/dgp.R") #source the dgp function
source("R/functions.R")
#source("inst/empeval/paper1/run.R")
source("R/functions.R")
# dgp1 <- dgp(p=0.5,m=0,t=0,sd=1,ol=0.5,model="normal",xmodel="unif",rmvar="X20")
# sim <- simulate.dgp(object=dgp1,rho=0,nsim=1600,d=20,nsimtest=1000)

# lapply(pkgs,require)
############################# Apply DGP to simulate data and test performance of random forest ##############################
set.seed(8008)
# #
# funA <- function(Varmiss){
#   dgpA <- dgp(p = pF_eta_x1_x2 , m = mF_sin_x1_x5, t = tF_div_x1_x2, sd=2, ol = 0.5, model = "normal",
#               xmodel = "unif",rmvar=Varmiss)
#   return(dgpA)
# }
#
# funB <- function(Varmiss){
#   dgpB <- dgp(p = 0.5 , m = mF_max_x1_x5, t = tF_log_x1_x2, sd = 1, ol = 0.5,model = "normal",
#               xmodel = "unif",rmvar=Varmiss)
#   return(dgpB)
# }
# funC <- function(Varmiss){
#   dgpC <- dgp(p = pF_x2_x3 , m = mF_log_x1_x3, t = 1, sd = 1, ol = 0.5, model = "normal",
#               xmodel = "unif",rmvar=Varmiss)
#   return(dgpC)
# }
#
# funD <- function(Varmiss){
#   dgpD <- dgp(p = pF_exp_x1_x2 , m = mF_max2_x1_x5, t = tF_max_x1_x5, sd = 1 , ol = 0.5, model = "normal",
#               xmodel = "unif",rmvar=Varmiss)
#   return(dgpD)
# }
# ##########################################################################################
#
# ################################## Initialise the helper functions ####################
# dim_vec <-
#   list(
#     c(),c(6:20),c(4,5),1L,c(1,2),c(1:3)
#   )
#
# dim_vec <- lapply(dim_vec, function(x) glue("X{x}"))
#
#
# dgpB <- funB(Varmiss = "")
# simB <- simulate.dgp(object = dgpB, rho = rhos, nsim = 1600L, d = 20L, nsimtest = 1000L)
# x_vars_B <- grep("^X", names(simB), value = TRUE)
# X_B <- as.matrix(simB[, x_vars_B, drop = FALSE])
# Y_B <- c(simB$y)
# W_B <- as.numeric(c(simB$trt))
# rf_B <- causal_forest(X = as.matrix(X_B), Y = Y_B, W = W_B, num.trees = 1000, seed = 8008)
# X_test_B <- attr(simB, "testxdf")[,1:20]
# X_test_B[,1] = runif(1000,0.001,0.999)
# X_test_B[,2] = runif(1000,0.001,0.999)
#
# tau.hat  <- predict(rf_B, X_test_B)$predictions
# true_tau <- predict(simB,X_test_B)$tfct
#
# tibble(tau_hat = tau.hat, true_tau = true_tau,difference= tau.hat-true_tau)
#
# dgpA <- funA(Varmiss = "")
# simA <- simulate.dgp(object = dgpA, rho = rhos, nsim = 1600L, d = 20L, nsimtest = 1000L)
# x_vars_A <- grep("^X", names(simA), value = TRUE)
# X_A <- as.matrix(simA[, x_vars_A, drop = FALSE])
# Y_A <- c(simB$A)
# W_A <- as.numeric(c(simA$trt))
# rf_A <- causal_forest(X = as.matrix(X_A), Y = Y_B, W = W_B, num.trees = 1000, seed = 8008)
# X_test_B <- attr(simB, "testxdf")[,1:20]
# X_test_B[,1] = runif(1000,0.001,0.999)
# X_test_B[,2] = runif(1000,0.001,0.999)
#
# X_A_Varmiss <- as.data.frame(X_A) %>% select(-any_of(dim_vec[[1]]))
#
# tau.hat  <- predict(rf_B, X_test_B)$predictions
# true_tau <- predict(simB,X_test_B)$tfct
#
# run_new <- function(Setup,dim_vec){
#   dgp <- get(glue("fun{Setup}"))(Varmiss = "")
#   sim <- simulate.dgp(object = dgp, rho = 0, nsim = 1600L, d = 20L, nsimtest = 1000L)
#   x_vars <- grep("^X", names(sim), value = TRUE)
#   X <- as.matrix(sim[, x_vars, drop = FALSE])
#   Y <- c(sim$y)
#   W <- as.numeric(c(sim$trt))
#   if(sum(is.na(dim_vec))==0){
#     X_Varmiss <- as.data.frame(X) %>% select(-any_of(dim_vec))
#   }
#   rf <- causal_forest(X = as.matrix(X_Varmiss), Y = Y, W = W, num.trees = 1000, seed = 8008)
#   X_test <- attr(sim, "testxdf")[,1:20]
#   X_test[,1] = runif(1000,0.001,0.999)
#   X_test[,2] = runif(1000,0.001,0.999)
#   true_tau <- predict(sim,X_test)$tfct
#   if(sum(is.na(dim_vec))==0){
#     X_Varmiss_test <- as.data.frame(X_test) %>% select(-any_of(dim_vec))
#   }
#   tau.hat  <- predict(rf, X_Varmiss_test)$predictions
#   return(tibble(tau_hat = tau.hat, true_tau = true_tau))
# }
#
#
# resultsy <-
#   expand_grid(
#     Setup = LETTERS[1:2],
#     dim_vec = dim_vec,
#     repl = seq(1, 1000, 1)
#     #rho = c(0,0.25,0.5,0.75)
#   ) %>%
#   mutate(Setup = as.factor(Setup)) %>%
#   mutate(dim_vec = paste0(dim_vec)) %>%
#   mutate(dim_vec = as.factor(dim_vec)) %>%
#   rowwise() %>%
#   mutate(diff = list(run_new(
#     Setup = Setup,
#     dim_vec = dim_vec
#     )))
#
# #save(results,file="results_cf.Rda")
# save(resultsy,file="tau_vs_true_tau.Rda")
load("tau_vs_true_tau.Rda")

results_unnested <- resultsy %>% unnest(diff)

methodnams <- c("1", "2", "3", "4", "5","6")

levels(results_unnested$dim_vec) <- methodnams




cols <- RColorBrewer::brewer.pal(6,"Spectral")



paper_theme <- function() {
  theme(
    # plot
    plot.title = element_text(
      face = "bold",
      size = rel(.9),
      hjust = .5,
      vjust = 2.5,
      color = "#130f09"
    ),
    plot.subtitle = element_text(size = rel(.7), hjust = .5, margin = margin(b = 10, unit = "pt")),
    plot.caption = element_text(size = rel(.7), hjust = 1),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    # panel
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_rect("#a5a5a5", fill = "transparent", linewidth = rel(2)),
    panel.grid.major = element_line(colour = "#eeeeee", linewidth = rel(1.2)),
    panel.grid.minor = element_blank(),
    # axis
    axis.ticks = element_line(color = "#a5a5a5"),
    axis.text = element_text(size = rel(.8)),
    axis.title.x = element_text(vjust = -.2),
    axis.title.y = element_text(angle = 90, vjust = 2),
    axis.text.y = element_text(
      size = rel(.8),
      vjust = 0.2,
      hjust = 1,
      margin = margin(r = 3)
    ),
    axis.text.x = element_text(size = rel(.8), margin = margin(2, 0, 0, 0)),
    axis.title = element_text(face = "bold", size = rel(.8)),
    # legend
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.title = element_text(
      face = "italic",
      size = rel(.8),
      hjust = .5
    ),
    legend.direction = "vertical",
    legend.text = element_text(size = rel(.8), vjust = 1),
    legend.box.spacing = unit(.2, "cm"),
    legend.key = element_rect(fill = "transparent", color = "transparent"),
    legend.key.size = unit(.3, "cm"),
    # facets
    strip.background = element_rect(fill = "#a5a5a5", color = NA),
    strip.text = element_text(
      face = "bold",
      size = rel(.7),
      margin = margin(t = 2.5, b = 2.5)
    )
  )
}

# load("results_cf.Rda")
p <- ggplot(results_unnested, aes(x = true_tau, y = tau_hat, fill = dim_vec)) +
  geom_tile() +
  scale_fill_viridis_d(name = "Dim Vec") +
  labs(title = "Heatmap of tau_hat vs. true_tau by dim_vec",
       x = "True Tau",
       y = "Tau Hat") +
  theme_minimal() +
  theme(legend.position = "right")

ggsave("p.pdf")
#print(p)

