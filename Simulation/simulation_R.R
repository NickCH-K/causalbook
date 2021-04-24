{
  library(tidyverse)
  library(cowplot)
  library(extrafont)
  library(ggpubr)
  library(vtable)
  library(purrr)
  library(broom)
}

# heteroskedasticity
set.seed(1000)

create_het_data <- function(N = 200) {
  d <- tibble(X = runif(N)) %>%
    # Let the standard deviation of the error
    # Be related to X. Heteroskedasticity!
    mutate(Y = 3*X + rnorm(N, sd = X*5)) 
  
  return(d)
}

d <- create_het_data(500)

ggplot(d, aes(x = X, y = Y)) + 
  geom_point(alpha = .4) + 
  geom_smooth(method = 'lm', se = FALSE, color = 'black', size = 1.5) + 
  theme_pubr() + 
  theme(text = element_text(family = 'Garamond', size = 13))
ggsave('heteroskedastic_data.pdf', width = 6, height = 5,device=cairo_pdf)

library(tidyverse)
library(purrr)
set.seed(1000)

# Estimate our model 1000 times (from 1 to 1000)
estimates <- 1:1000 %>%
  # Run the est_model function each time
  map_dbl(function(x) est_model(N = 200, T = 5))
mean(estimates)
sd(estimates)


# A function for estimation
est_model_se <- function(N, T) {
  # Get our data
  # This uses the create_clus_data function
  # from the previous section
  d <- create_clus_data(N, T)
  
  # Run a model that should be unbiased
  # if clustered errors themselves don't bias us!
  m <- lm(Y ~ X + W, data = d)
  
  # Get the coefficient on X, which
  # SHOULD be the true value 3 on average
  x_coef <- coef(summary(m))['X', 'Std. Error']
  
  return(x_coef)
}
set.seed(1000)
estimates <- 1:300 %>%
  # Run the est_model function each time
  map_dbl(function(x) est_model_se(N = 200, T = 5))
mean(estimates)

library(fixest)
# A function for estimation
est_model_het <- function(N, T) {
  # Get our data
  # This uses the create_clus_data function
  # from the previous section
  d <- create_clus_data(N, T)
  
  # Run a model that should be unbiased
  # if clustered errors themselves don't bias us!
  m <- feols(Y ~ X + W, data = d, se = 'hetero')
  
  # Get the coefficient on X, which
  # SHOULD be the true value 3 on average
  x_coef <- broom::tidy(m)$std.error[2]
  
  return(x_coef)
}
set.seed(1000)
estimates <- 1:300 %>%
  # Run the est_model function each time
  map_dbl(function(x) est_model_het(N = 200, T = 5))
mean(estimates)

est_model_clus <- function(N, T) {
  # Get our data
  # This uses the create_clus_data function
  # from the previous section
  d <- create_clus_data(N, T)
  
  # Run a model that should be unbiased
  # if clustered errors themselves don't bias us!
  m <- feols(Y ~ X + W, data = d, cluster = ~ID)
  
  # Get the coefficient on X, which
  # SHOULD be the true value 3 on average
  x_coef <- broom::tidy(m)$std.error[2]
  
  return(x_coef)
}
set.seed(1000)
estimates <- 1:300 %>%
  # Run the est_model function each time
  map_dbl(function(x) est_model_clus(N = 200, T = 5))
mean(estimates)


# power analysis
set.seed(1000)

# Follow the description in the text for data creation
# Since we want to get minimum sample size
# and minimum detectable effect, allow both
# sample size and effect to vary
# diff is the difference in effects between boys and girls
create_data <- function(N, effect, diff) {
  d <- tibble(W = rnorm(N),
              girl = sample(0:1, N, replace = TRUE)) %>%
    # A one-SD change in W makes treatment 10% more likely
    mutate(Training = runif(N) + .1*W < .5) %>%
    mutate(Test = effect*Training + diff*girl*Training + 
             4*W + rnorm(N, sd = 9))
  
  return(d)
}

# Our estimation function
est_model <- function(N, effect, diff) {
  d <- create_data(N, effect, diff)
  
  # Our model
  m <- lm(Test~girl*Training + W, data = d)
  tidied <- tidy(m)
  
  # By looking we can spot that the interaction
  # term is in the 5th row
  sig <- tidied$p.value[5] < .05
  
  return(sig)
}

# Iteration function!
iterate <- function(N, effect, diff, iters) {
  results <-  1:iters %>%
    map_dbl(function(x) {
      # To keep track of progress
      if (floor(x/100) == x/100) {print(x)}
      
      # Run our model and return the result
      return(est_model(N, effect, diff))
    })
  
  # We want to know statistical power, 
  # i.e. the proportion of significant results
  return(mean(results))
}

# Let's find the minimum sample size
mss <- tibble(N = c(10000, 15000, 20000, 25000))
mss$power <- c(10000, 15000, 20000, 25000) %>%
  # Before we had to do function(x) here,
  # but now the argument we're passing is the first
  # argument of iterate() so we don't need it
  map_dbl(iterate, effect = 2, diff = .8, iter = 500)
# Look for the first N with power above 90%
mss %>%
  rename(Power = power) %>%
  dftoLaTeX(file = 'minimum_sample_size',
          title = 'Results of Minimum-Sample-Size Power Analysis Simulation',
          anchor = 'tab:simulation-mss',
          align = 'lc')

mde <- tibble(effect = c(.8, 1.6, 2.4, 3.2))
mde$power <-  c(.8, 1.6, 2.4, 3.2) %>%
  map_dbl(function(x) iterate(N = 2000, effect = 2,
                              diff = x, iter = 500))
mde %>%
  rename(Power = power,
         Effect = effect) %>%
  dftoLaTeX(file = 'minimum_detectable_effect',
            title = 'Results of Minimum-Detectable-Effect Power Analysis Simulation',
            anchor = 'tab:simulation-mde',
            align = 'lc')