## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6,
                      fig.height = 4)
has_torch <- torch::torch_is_installed()
backend <- if (has_torch) "torch" else "r"

## -----------------------------------------------------------------------------
library(splineKANr)
d  <- kan_synthetic("interaction", n = 400, seed = 1)   # y = x1 * x2
te <- kan_synthetic("interaction", n = 500, seed = 2)

fit <- kan_fit(d$x, d$y, hidden = 4, epochs = 150, batch_size = 64,
               lr = 0.02, patience = 30, grid_update_every = 10,
               grid_update_until = 50, seed = 1, backend = backend)
fit
kan_metrics(te$y, predict(fit, te$x))

## -----------------------------------------------------------------------------
kan_plot_history(fit)

## ----fig.height = 6-----------------------------------------------------------
plot(fit, layer = 1, data = d$x)

## -----------------------------------------------------------------------------
kan_plot_network(fit, d$x)
plot(kan_partial_dependence(fit, d$x, "x1"))

## -----------------------------------------------------------------------------
fit$activation_check

## -----------------------------------------------------------------------------
fine <- kan_refine(fit, d$x, grid_size = 10)       # G = 5 -> 10
fine <- kan_fit(d$x, d$y, init = fine, epochs = 50, batch_size = 64,
                lr = 0.01, seed = 1, backend = backend)
kan_metrics(te$y, predict(fine, te$x))

pruned <- kan_prune(fine, d$x)
pruned$pruning$removed_nodes

## ----eval = FALSE-------------------------------------------------------------
# fit_sparse <- kan_fit(d$x, d$y, hidden = 4, epochs = 200,
#                       reg = kan_reg_control(lambda = 1e-3))

## -----------------------------------------------------------------------------
f <- tempfile(fileext = ".rds")
kan_save(fine, f)
same <- kan_load(f)
max(abs(predict(same, te$x) - predict(fine, te$x)))

## -----------------------------------------------------------------------------
v <- kan_validate_basis(grid_sizes = 5, backend = "r")
v[, c("spline_order", "max_err_splineDesign", "max_err_partition", "pass")]
g <- kan_validate_gradient(widths = list(c(2, 3, 1)), backend = "r")
g[, c("extrapolate", "comparison", "max_rel_err", "pass")]

