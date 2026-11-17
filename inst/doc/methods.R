## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 3.2)

## -----------------------------------------------------------------------------
library(splineKANr)
knots <- kan_make_grid(1, grid_size = 5, spline_order = 3)[1, ]
x <- seq(-1, 1, length.out = 201)
B <- kan_bspline_basis_r(x, knots, 3, right_closed = TRUE)
range(rowSums(B))          # partition of unity on the domain

## -----------------------------------------------------------------------------
v <- kan_validate_basis(grid_sizes = c(3, 5), spline_orders = 1:3,
                        backend = "r")
v[, c("grid_size", "spline_order", "max_err_splineDesign", "pass")]
g <- kan_validate_gradient(widths = list(c(2, 3, 1)), extrapolate = "linear",
                           backend = "r")
g[, c("width", "comparison", "max_rel_err", "pass")]

## -----------------------------------------------------------------------------
src <- system.file("applications", "pls_pm.R", package = "splineKANr")
source(src)
set.seed(42)
n <- 600
quality <- rnorm(n)
image <- 0.5 * quality + sqrt(0.75) * rnorm(n)
sat <- 0.4 * quality + 0.9 * pmax(image, 0) + rnorm(n, sd = 0.4)
items <- function(lv, k, prefix) {
  z <- sapply(seq_len(k), function(i) 0.8 * scale(lv)[, 1] +
                0.6 * rnorm(length(lv)))
  out <- pmax(pmin(round(5.5 + 1.6 * z), 10), 1)
  colnames(out) <- paste0(prefix, seq_len(k))
  out
}
survey <- data.frame(items(quality, 5, "q"), items(image, 5, "im"),
                     items(sat, 3, "s"))
blocks <- list(Quality = paste0("q", 1:5), Image = paste0("im", 1:5),
               Satisfaction = paste0("s", 1:3))
paths <- rbind(c("Quality", "Satisfaction"), c("Image", "Satisfaction"))
pls <- pls_pm(survey, blocks, paths)
pls_path_table(pls, "pls")
round(rbind(CR = pls$cr, AVE = pls$ave), 3)

## -----------------------------------------------------------------------------
sc <- pls$scores
train <- seq_len(400)
kfit <- kan_fit(sc[train, c("Quality", "Image")], sc[train, "Satisfaction"],
                hidden = integer(), grid_size = 5, epochs = 300,
                backend = "r", seed = 1)
lfit <- lm(Satisfaction ~ Quality + Image, data = as.data.frame(sc[train, ]))
test <- as.data.frame(sc[-train, ])
rbind(
  kan = kan_metrics(test$Satisfaction,
                    predict(kfit, test[, c("Quality", "Image")],
                            backend = "r")),
  ols = kan_metrics(test$Satisfaction, predict(lfit, test))
)

## ----fig.height = 3-----------------------------------------------------------
op <- par(mfrow = c(1, 2))
for (v in c("Quality", "Image")) {
  plot(kan_partial_dependence(kfit, sc[train, c("Quality", "Image")], v),
       main = v, ylab = "PD of Satisfaction")
}
par(op)

