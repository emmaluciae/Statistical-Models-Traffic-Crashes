#install packages if you do not have them
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("dgof")
#install.packages("MASS")
#install.packages("sf")

#libraries (if 'there is no package..' install them above)
library(dplyr)
library(lubridate)
library(ggplot2)
library(dgof)
library(MASS)
library(sf)

my_data <- read.csv("data/crashes.csv")

#Lets see what we got here haha
View(my_data)

#Part 1
# Create new variable for serious injuries and the start of week
my_data <- my_data %>%
  mutate(
    CRASH_DATE = as.Date(CRASH_DATE),
    serious_injuries = INJURIES_FATAL + INJURIES_INCAPACITATING,
    week_start = floor_date(CRASH_DATE, unit = "week", week_start = 7)
  )

# Add serious injuries per week
my_processed_data <- my_data %>%
  group_by(week_start) %>%
  summarise(Seriouscount = sum(serious_injuries, na.rm = TRUE))

# Plot serious injuries
ggplot(my_processed_data, aes(x = week_start, y = Seriouscount)) +
  geom_point() +
  labs(x = "Week", y = "Serious Injury Count")

# Poisson K–S test
mle_lambda <- mean(my_processed_data$Seriouscount)
ks_result <- ks.test(my_processed_data$Seriouscount, "ppois", lambda = mle_lambda)
print(ks_result)

# Histogram + theoretical Poisson line
# Create integer x-values covering your data range
x_vals <- seq(min(my_processed_data$Seriouscount),
              max(my_processed_data$Seriouscount), by = 1)

# Theoretical Poisson density values
theoretical_pois <- data.frame(
  x = x_vals,
  y = dpois(x_vals, lambda = mle_lambda)
)

# Plot: histogram + smoothed Poisson curve, smoothed
ggplot(my_processed_data, aes(x = Seriouscount)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2, fill = "#F7CAC9", color = "black") +
  geom_line(data = theoretical_pois, aes(x = x, y = y),
            color = "#4CAF50", size = 1.2) +
  labs(
    title = "Observed Weekly Serious Injury Counts vs. Poisson Fit",
    x = "Serious Injury Count per Week",
    y = "Density"
  )  +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#Part 2
# Fit a Negative Binomial model to your weekly counts
nb_fit <- fitdistr(my_processed_data$Seriouscount, "Negative Binomial")

nb_fit

#Use ks test for -binomial
ks_nb <- ks.test(
  my_processed_data$Seriouscount,
  "pnbinom",
  size = nb_fit$estimate["size"],
  mu = nb_fit$estimate["mu"]
)

print(ks_nb)

#Histogram + theoretical -binomial line
# Create a small data frame for the theoretical NB curve
x_vals <- seq(min(my_processed_data$Seriouscount),
              max(my_processed_data$Seriouscount), by = 1)

theoretical_nb <- data.frame(
  x = x_vals,
  y = dnbinom(x_vals,
              size = nb_fit$estimate["size"],
              mu = nb_fit$estimate["mu"])
)

# Plot using that new data frame, smoothed
ggplot(my_processed_data, aes(x = Seriouscount)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2, fill = "#F7CAC9", color = "black") +
  geom_line(data = theoretical_nb, aes(x = x, y = y),
            color = "#4CAF50", size = 1.2) +
  labs(title = "Observed Weekly Serious Injury Counts vs. Negative Binomial Fit",
       x = "Serious Injury Count per Week",
       y = "Density")  +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#Part 3
#95th percentile of serous injury count
cutoff_pois <- qpois(0.95, lambda = mle_lambda) # 58 serious injuries or more
                                                # happen weekly 5% of the time
# Actual proportion of weeks >= that cutoff
prop_pois_obs <- mean(my_processed_data$Seriouscount >= cutoff_pois)

# Negative Binomial 95th percentile
size_hat <- as.numeric(nb_fit$estimate["size"])
mu_hat   <- as.numeric(nb_fit$estimate["mu"])

cutoff_nb <- qnbinom(0.95, size = size_hat, mu = mu_hat)# 69 serious injuries or
                                                        # or more weekly 5% of time
# Actual proportion of weeks >= that cutoff
prop_nb_obs <- mean(my_processed_data$Seriouscount >= cutoff_nb)

# Combine results into a tidy comparison table
part3_summary <- data.frame(
  Model = c("Poisson", "Negative Binomial"),
  `95th Percentile Cutoff` = c(cutoff_pois, cutoff_nb),
  `Observed % of Weeks ≥ Cutoff` = round(c(prop_pois_obs, prop_nb_obs) * 100, 2)
)
View(part3_summary)

#Part 4
# Identify the week with the highest total injuries
worst_week <- my_processed_data %>%
  slice_max(Seriouscount, n = 1) %>%
  pull(week_start)

# Filter crash records for that week
my_data$CRASH_DATE <- as.Date(my_data$CRASH_DATE)#correct format

week_data <- my_data %>%
  mutate(
    serious_injuries = INJURIES_FATAL + INJURIES_INCAPACITATING,
    week_start = floor_date(CRASH_DATE, unit = "week", week_start = 7)
  ) %>%
  filter(week_start == worst_week, serious_injuries > 0,
         !is.na(LATITUDE), !is.na(LONGITUDE))
View(week_data)

# Convert to sf object (spatial)
points_sf <- st_as_sf(week_data, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Read local major streets shapefile (only specify .shp — R will grab the rest)
chicago_streets <- st_read("data/Major_Streets.shp", quiet = TRUE)

ggplot() +
  geom_sf(data = chicago_streets, color = "#9E9E9E", linewidth = 0.3) +
  geom_sf(data = points_sf, color = "#FF69B4", alpha = 0.8, size = 1.7) +
  labs(
    title = paste("Serious Injuries & Fatal Crashes - Week of", worst_week),
    subtitle = "Each point = a crash with serious injuries or deaths",
    caption = "Data: City of Chicago Traffic Crashes, Major Streets overlay"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

#Part 5
# Filter crashes involving cell phone use
cell_phone_crashes <- my_data %>%
  filter(PRIM_CONTRIBUTORY_CAUSE %in% c("TEXTING", "CELL PHONE USE OTHER THAN TEXTING"))

# Count how many of those had serious injuries or deaths
cell_phone_serious <- cell_phone_crashes %>%
  mutate(serious_injuries = INJURIES_FATAL + INJURIES_INCAPACITATING) %>%
  filter(serious_injuries > 0)

# Basic counts
n_total <- nrow(my_data)
n_phone_serious <- nrow(cell_phone_serious)

# Sample proportion
p_hat <- n_phone_serious / n_total
cat("Sample proportion:", round(p_hat, 6), "\n")

# Normal-approximation (one-sided, lower bound = 0)
z <- qnorm(0.95)  # 1.645 for 95% one-sided
se <- sqrt(p_hat * (1 - p_hat) / n_total)
lower_normal <- max(0, p_hat - z * se)
cat("\nNormal Approximation 95% One-Sided CI:\n")
cat("[", round(lower_normal, 6), ", 1 ]\n")

# Exact binomial one-sided interval
binom_result <- binom.test(n_phone_serious, n_total, conf.level = 0.95, alternative = "greater")
cat("\nExact (Clopper-Pearson) 95% One-Sided CI:\n")
cat("[", round(binom_result$conf.int[1], 6), ", 1 ]\n")

#Part 6
# Derive the Poisson log-likelihood
# Define helper functions for score and Hessian
poisson_score <- function(lambda, x) {
  # Score (first derivative)
  if (lambda <= 0) return(-Inf)
  sum(x) / lambda - length(x)
}

poisson_hessian <- function(lambda, x) {
  # Second derivative
  - sum(x) / (lambda^2)
}

#Implement Newton’s Method using analytic derivatives
newton_poisson <- function(x, x0 = mean(x), tol = 1e-10, max.iter = 50) {
  n <- length(x)
  sx <- sum(x)
  score   <- function(lambda)  sx / lambda - n
  hessian <- function(lambda) -sx / (lambda^2)
  
  lambda <- max(x0, .Machine$double.eps)     # ensure λ > 0
  out <- data.frame(iter = 0, lambda = lambda, score = score(lambda))
  
  i <- 0
  while (abs(out$score[nrow(out)]) > tol && i < max.iter) {
    i <- i + 1
    g  <- score(lambda)
    H  <- hessian(lambda)
    step <- - g / H                           # Newton update: λₙ₊₁ = λₙ - g/H
    lambda <- max(lambda + step, .Machine$double.eps)
    out <- rbind(out, data.frame(iter = i, lambda = lambda, score = score(lambda)))
  }
  out
}

# Numeric-derivative version for comparison
ddx <- function(value, func, delta = 1e-6) {
  (func(value + delta) - func(value - delta)) / (2 * delta)
}

newton_numeric <- function(f, x0, tol = 1e-8, max.iter = 50) {
  x <- max(x0, .Machine$double.eps)
  fx <- f(x)
  out <- data.frame(iter = 0, x = x, fx = fx)
  i <- 0
  while (abs(fx) > tol && i < max.iter) {
    dfx <- ddx(x, f)
    if (!is.finite(dfx) || dfx == 0) break
    x <- max(x - fx / dfx, .Machine$double.eps)  # minus sign corrected
    fx <- f(x); i <- i + 1
    out <- rbind(out, data.frame(iter = i, x = x, fx = fx))
  }
  out
}

# Example run using your weekly counts
x <- my_processed_data$Seriouscount

# Analytic Newton’s Method
trace_analytic <- newton_poisson(x, x0 = mean(x) * 0.5)
print(tail(trace_analytic, 1))

# Numeric-derivative Newton’s Method
score_fun <- function(lambda) poisson_score(lambda, x)
trace_numeric <- newton_numeric(score_fun, x0 = mean(x) * 1.5)
print(tail(trace_numeric, 1))

# Check against theoretical MLE
mle_estimate <- mean(x)
cat("\nClosed-form MLE (mean of data):", round(mle_estimate, 4), "\n")

cat("\nAnalytic Newton estimate converged to:",
    round(tail(trace_analytic$lambda, 1), 4), "\n")

cat("Numeric Newton estimate converged to:",
    round(tail(trace_numeric$x, 1), 4), "\n")

#gradient decent for negative binomial distribution
# Log-likelihood for NB (parameterized by size = r, mean = mu)
loglik_nb <- function(params, x) {
  r  <- exp(params[1])  # enforce positivity
  mu <- exp(params[2])
  sum(
    lgamma(x + r) - lgamma(r) - lgamma(x + 1) +
      r * log(r) + x * log(mu) - (r + x) * log(r + mu)
  )
}

# Numerical gradient using finite differences
grad_nb <- function(params, x, delta = 1e-5) {
  p1 <- params; p2 <- params
  grads <- numeric(length(params))
  for (i in seq_along(params)) {
    p1[i] <- params[i] + delta
    p2[i] <- params[i] - delta
    grads[i] <- (loglik_nb(p1, x) - loglik_nb(p2, x)) / (2 * delta)
    p1[i] <- params[i]; p2[i] <- params[i]
  }
  grads
}

# Gradient Descent routine
grad_descent_nb <- function(x, start = c(log(10), log(mean(x))),
                            lr = 1e-3, tol = 1e-6, max.iter = 2000) {
  params <- start
  loglik_old <- loglik_nb(params, x)
  history <- data.frame(iter = 0,
                        r = exp(params[1]),
                        mu = exp(params[2]),
                        logLik = loglik_old)
  
  for (i in 1:max.iter) {
    g <- grad_nb(params, x)
    params <- params + lr * g               # move toward higher log-likelihood
    loglik_new <- loglik_nb(params, x)
    
    history <- rbind(history,
                     data.frame(iter = i,
                                r = exp(params[1]),
                                mu = exp(params[2]),
                                logLik = loglik_new))
    
    if (abs(loglik_new - loglik_old) < tol) break
    loglik_old <- loglik_new
  }
  history
}

# Example run using your weekly counts
x <- my_processed_data$Seriouscount
trace_nb <- grad_descent_nb(x, lr = 1e-4)
tail(trace_nb, 1)

# Compare with MASS::fitdistr estimates
nb_fit <- MASS::fitdistr(x, "Negative Binomial")
print(nb_fit$estimate)

cat("\nGradient Descent estimates:\n")
cat("size (r):", round(tail(trace_nb$r, 1), 3),
    " | mu:", round(tail(trace_nb$mu, 1), 3), "\n")
