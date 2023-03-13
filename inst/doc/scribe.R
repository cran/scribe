## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(scribe)

## -----------------------------------------------------------------------------
ca <- command_args(c("-n", "5", "--method", "numbers"))
ca$add_argument("-n", default = 1L)
ca$add_argument("--method", default = "letters")
args <- ca$parse()

out <- seq_len(args$n)

method <- match.arg(args$method, c("letters", "numbers"))
if (method == "letters") {
  out <- letters[out]
}

out

## -----------------------------------------------------------------------------
my_summary <- function(data, levels = 7, sig_figs = 3, q_type = 7) {
  data <- get(data, mode = "list")
  stopifnot(is.data.frame(data))
  summary(data, maxsum = levels, digits = sig_figs, quantile.type = q_type)
}

my_model <- function(data, correlation = FALSE) {
  data <- get(data, mode = "list")
  stopifnot(is.data.frame(data))
  form <- stats::DF2formula(data)
  mod <- stats::lm(form, data)
  summary(mod, correlation = correlation)
}

## -----------------------------------------------------------------------------
ca <- command_args(string = "CO2 --levels 3 --sig-figs 2 --q-type 3")
ca$add_description("Summarise a dataset")
ca$add_argument(
  "data",
  info = "Name of the dataset to find"
)
ca$add_argument(
  "--levels",
  default = 7L,
  info = "Maximum number of levels shown for factors"
)
ca$add_argument(
  "--sig-figs",
  default = 3L,
  info = "Number of significant figures"
)
ca$add_argument(
  "--q-type",
  default = 7L,
  info = "Quantile type"
)
args <- ca$parse()
do.call(my_summary, args)

## -----------------------------------------------------------------------------
ca <- command_args(string = "attitude --correlation")
ca$add_argument(
  "data",
  info = "Name of the dataset to find"
)
ca$add_argument(
  "--correlation",
  action = "flag",
  info = "When set, prints the correlation matrix of estimated parameters"
)
args <- ca$parse()
do.call(my_model, args)

