load("results_cf.Rda")
methodnams <- c("1-20", "1-5", "-c(4,5)", "-1", "-c(1,2)","-c(1,3)")
#
#
levels(results$Varmiss) <- methodnams
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
####
methodnams <- c("1-20", "1-5", "-c(4,5)", "-1", "-c(1,2)","-c(1,3)")



cols <- c("1-20" = "snow", "1-5" = "snow4",
          "-c(4,5)" = "tan",  "-1" = "tan4",
          "-c(1,2)" = "darkorchid1", "-c(1,3)" = "darkorchid4")
lookup <- c(
  "1-20" = "1-20",
  "1-5" = "1-5",
  "-c(4,5)" = "-c(4,5)",
  "-1" = "-1",
  "-c(1,2)" = "-c(1,2)",
  "-c(1,3)" = "-c(1,3)"
)
colornams <- cols

methodnams <- c("1-20", "1-5", "-c(4,5)", "-1", "-c(1,2)","-c(1,3)")

library("lattice")
library("latticeExtra")

# Lattice options and plot appearance
trellis.par.set(list(plot.symbol = list(col="black",pch=18, cex=0.75),
                     box.rectangle = list(col=1),
                     box.umbrella = list(lty=1, col=1),
                     strip.background = list(col = "white")))
ltheme <- canonical.theme(color = FALSE)     # black and white theme
ltheme$strip.background$col <- "transparent" # change strip background
lattice.options(default.theme = ltheme)

# Setup rows and columns of plot
sc <- function(which.given, ..., factor.levels) {
  if (which.given == 2) {
    strip.default(which.given = which.given, ..., factor.levels)
  } else {
    strip.default(which.given = 1, ...,
                  factor.levels)
  }
}

# Fill each entry with boxplots
mypanel <- function(x, y, groups, subscripts, ...) {
  fill <- cols[intersect(levels(x), unique(x))]
  panel.bwplot(x = x, y = y, fill = fill, ...)
  tapply(1:length(y), groups[subscripts], function(i) {
    xi <- 1:nlevels(x)
    yi <- y[i][match(xi, unclass(x[i]))]
    llines(x = xi, y = yi,
           col = rgb(0.1, 0.1, 0.1, 0.03))
  })
}

# set up legend at top of plots
mykey <- list(space="top", rectangles=list(col=cols),
              text=list(colornams), columns = ceiling(length(methodnams)/2))

# label of y-axis, MSE
ylab <- expression(paste(frac(1, 1000), "",
                         sum((tau(x[i]) - hat(tau)(x[i]))^2, i == 1, 1000)))

# plotting function
plot_results <- function(results_random, sc, ylim) {
  plt <- bwplot(MSE ~ Setup | Varmiss, data = results,
                ylab = list(ylab), ylim = ylim,
                groups = repl, panel = mypanel,
                as.table = TRUE, strip = sc, key = mykey,
                scales = list(relation = "same", x = list(rot = 60, labels = colornams)))
  useOuterStrips(plt, strip = sc)
}
res <- results
plot_results(res, sc = TRUE)

# save as pdf
pdf("results.pdf", width = 8.5, height = 11)
#
