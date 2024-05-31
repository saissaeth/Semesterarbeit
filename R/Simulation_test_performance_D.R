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

# run_new <- function(Setup,Varmiss,rhos){
#   dgp <- get(glue("fun{Setup}"))(Varmiss = Varmiss)
#   sim <- simulate.dgp(object = dgp, rho = rhos, nsim = 1600L, d = 20L, nsimtest = 1000L)
#   x_vars <- grep("^X", names(sim), value = TRUE)
#   X <- as.matrix(sim[, x_vars, drop = FALSE])
#   Y <- c(sim$y)
#   W <- as.numeric(c(sim$trt))
#   rf <- causal_forest(X = as.matrix(X), Y = Y, W = W, num.trees = 1000, seed = 8008)
#   X_test_all <- attr(sim, "testxdf")
#   X_test <- attr(sim, "testxdf") %>% select(all_of(x_vars))
#   true_tau <- predict(sim,X_test_all)$tfct
#   tau.hat  <- predict(rf, X_test)$predictions
#   return(MSE = mean((true_tau - tau.hat)^2))
# }
run_new_y <- function(Setup,Varmiss,rhos){
  dgp <- get(glue("fun{Setup}"))(Varmiss = Varmiss)
  sim <- simulate.dgp(object = dgp, rho = rhos, nsim = 2600L, d = 20L)
  x_vars <- grep("^X", names(sim), value = TRUE)
  X <- as.matrix(sim[, x_vars, drop = FALSE] %>% slice(1:1600))
  Y <- c(sim$y)[1:1600]
  W <- as.numeric(c(sim$trt))[1:1600]
  rf <- causal_forest(X = as.matrix(X), Y = Y, W = W, num.trees = 1000, seed = 8008)
  test_df <- sim %>% slice(1601:2600)
  X_test <- as.matrix(test_df[, x_vars, drop = FALSE])
  Y_test <- test_df$y
  #X_test <- attr(sim, "testxdf") %>% select(all_of(x_vars))
  y_hat <- as.numeric(unlist(c(predict(rf, X_test))))
  return(MSE = mean(y_hat - Y_test)^2)
}
 # results  <-
 #   expand_grid(
 #     Setup = LETTERS[1],
 #     Varmiss = paste0(dim_vec),
 #     repl = seq(1, 1, 1),
 #     rho = c(0)#,0.25,0.5,0.75)
 #   ) %>%
 #   mutate(Setup = as.factor(Setup)) %>%
 #   mutate(Varmiss = paste0(Varmiss)) %>%
 #   mutate(Varmiss = as.factor(Varmiss)) %>%
 # rowwise() %>%
 #   mutate(MSE = run_new(
 #     Setup = Setup,
 #     Varmiss = Varmiss,
 #     rhos = rho
 #   ))

resultsy <-
  expand_grid(
    Setup = LETTERS[4],
    Varmiss = paste0(dim_vec),
    repl = seq(1, 1, 1),
    rho = c(0)#,0.25,0.5,0.75)
  ) %>%
  mutate(Setup = as.factor(Setup)) %>%
  mutate(Varmiss = paste0(Varmiss)) %>%
  mutate(Varmiss = as.factor(Varmiss)) %>%
  rowwise() %>%
  mutate(MSE = run_new_y(
    Setup = Setup,
    Varmiss = Varmiss,
    rhos = rho
  ))

#hallo
#save(results,file="results_cf.Rda")
#save(resultsy,file="resultsy_rho0_cf.Rda")
# Load necessary library
library(dplyr)

# Define the function to append data to an existing Rda file
append_to_rda <- function(new_data, file) {
  # Check if the file exists
  if (file.exists(file)) {
    # Load the existing data
    load(file)

    # Assume the existing data is stored in a variable named 'resultsy'
    # If the variable name is different, adjust the following line accordingly
    existing_data <- result_sy

    # Append the new data
    combined_data <- bind_rows(existing_data, new_data)

    # Save the combined data back to the file
    result_sy <- combined_data
    save(result_sy, file = file)
  } else {
    # If the file doesn't exist, save the new data directly
    result_sy <- new_data
    save(result_sy, file = file)
  }
}

# Example usage
# Assuming 'resultsy' is your new data frame
# Replace 'new_data' with your actual data frame
new_data <- resultsy

# Call the function to append data to the file
append_to_rda(new_data, "resultsy_rho0_cf.Rda")


load("resultsy_rho0.75_cf.Rda")
results_0.75 <- resultsy
results <- rbind(results_0, results_0.25, results_0.5, results_0.75)
methodnams <- c("1", "2", "3", "4", "5","6")

levels(results$Varmiss) <- methodnams


cols <- RColorBrewer::brewer.pal(6,"Spectral")

p <- ggplot(results, aes(x = Setup, y = MSE, fill = Varmiss)) +
  geom_violin() +
  paper_theme() +
  geom_vline(xintercept = c(1.5,2.5,3.5), linetype = "solid", color = "darkslategray") +
  scale_y_continuous(limits = c(0, 0.6)) +
  facet_wrap(~ rho, scales = 'free_x', ncol = 1, labeller = labeller(rho = label_parsed)) +
  theme(legend.position = "right") +
  labs(fill = "Varmiss") +
  scale_fill_manual(values = cols)

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

print(p)

