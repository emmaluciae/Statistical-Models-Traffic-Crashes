#install packages if you do not have them
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("dgof")

#libraries (if 'there is no package..' install them above)
library(dplyr)
library(lubridate)
library(ggplot2)
library(dgof)
library(MASS)

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


