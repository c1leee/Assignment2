#q1
# writing function to load quasar data from file
load_data <- function(quasar_number, band, dir = "quasar_csv_data") {
  if (length(quasar_number) != 1 || length(band) != 1) {
    stop("quasar_number and band must be of length 1.")
  }
  if (!is.numeric(quasar_number) || quasar_number < 1 || quasar_number > 190) {
    stop("quasar_number must be a value from 1 to 190.")
  }
  if (!(band %in% c("g", "r", "i"))) {
    stop('The only valid bands are "g", "r", and "i".')
  }
  file_path <- file.path(dir, paste0("quasar", quasar_number, "_", band, ".csv"))
  read.csv(file_path, stringsAsFactors = FALSE)
}
#calling the load_data function
head(load_data(120, "r"))

#q2
# Computing the weighted mean
weighted_mean <- function(x, w) {
  if (length(x) != length(w)) stop("x and w must be the same length.")
  sum(w * x) / sum(w)
}
# testing on assignment inputs
weighted_mean(c(1, 2, 3), c(1, 2, 1))


#q4
# The gaussian log-likelihood function
log_likelihood1 <- function(mu, y, err) {
  n <- length(y)
  -0.5 * (n * log(2 * pi) + sum(log(err^2)) + sum((y - mu)^2 / err^2))
}

# loading the test data and call function
q120r <- load_data(120, "r")
log_likelihood1(19.9, q120r$magnitude, q120r$errorbar)

#Q5
# maximising the log-likelihood using optim()
neg_log_likelihood1 <- function(mu, y, err) {
  -log_likelihood1(mu, y, err)
}
optim(par = 20, fn = neg_log_likelihood1, y = q120r$magnitude, err = q120r$errorbar)

#q6
# new log-likelihood to work with vector input mu
log_likelihood2 <- function(mu, y, err) {
  n <- length(y)
  -0.5 * (n * log(2 * pi) + sum(log(err^2)) + sum((y - mu[1])^2 / err^2))
}
# viewing the results of the rewritten function
optim(par = 20, fn = function(mu) -log_likelihood2(mu, q120r$magnitude, q120r$errorbar))
