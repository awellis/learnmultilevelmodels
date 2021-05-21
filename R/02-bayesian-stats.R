## ----setup, include=FALSE-------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)


## -------------------------------------------------------------
wins <- 6
games <- 9


## -------------------------------------------------------------
theta <- wins/games
theta


## -------------------------------------------------------------
library(tidyverse)


## ----echo=TRUE------------------------------------------------
tibble(x = seq(from = 0, to = 1, by = .01)) %>% 
  mutate(density = dbinom(6, 9, x)) %>% 
  
  ggplot(aes(x = x, ymin = 0, ymax = density)) +
  geom_ribbon(size = 0, alpha = 1/4, fill = "steelblue") +
  geom_vline(xintercept = theta, linetype = 2, size = 1.2) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(0, 1)) +
  xlab("Wahrscheinlichkeit") +
  theme(panel.grid = element_blank(),
        legend.position = "none")


## -------------------------------------------------------------
n_points <- 100
theta_grid <- seq(from = 0 , to = 1 , length.out = n_points)


## -------------------------------------------------------------
likelihood <- dbinom(wins , size = games , prob = theta_grid)

## -------------------------------------------------------------
prior <- dbeta(x = theta_grid, shape1 = 4,  shape2 = 4)


## -------------------------------------------------------------
unstandardized_posterior <- likelihood * prior
posterior <- unstandardized_posterior / sum(unstandardized_posterior)


## ----echo=TRUE------------------------------------------------
d <- tibble(theta_grid, prior, likelihood, posterior)

d %>%
  pivot_longer(-theta_grid, names_to = "distribution", values_to = "density") %>% 
  mutate(distribution = as_factor(distribution)) %>% 
  ggplot(aes(theta_grid, density, color = distribution)) +
  geom_line(size = 1.5) +
  geom_vline(xintercept = 9/10, linetype = "dashed") +
  scale_color_viridis_d(end = 0.8) +
  xlab("Theta Werte") +
  ylab("") +
  facet_wrap(~distribution, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none")


## -------------------------------------------------------------
n_samples <- 1e4
set.seed(3) # wegen Reproduzierbarkeit


## -------------------------------------------------------------
library(rmarkdown)

## ----echo=TRUE------------------------------------------------
d %>%
    paged_table(options = list(rows.print = 6))


## -------------------------------------------------------------
samples <-
  d %>% 
  slice_sample(n = n_samples, weight_by = posterior, replace = TRUE) %>%
  mutate(sample_number = 1:n())


## ----echo=TRUE------------------------------------------------
samples %>%
    paged_table(options = list(rows.print = 6))


## -------------------------------------------------------------
samples %>%
  ggplot(aes(x = sample_number, y = theta_grid)) +
  geom_point(alpha = 1/10) +
  scale_y_continuous("Erfolgswahrscheinlichkeit", limits = c(0, 1)) +
  xlab("sample number")


## ----echo=FALSE, message=FALSE, warning=FALSE-----------------
samples %>% 
  ggplot(aes(x = theta_grid)) +
  geom_density(fill = "steelblue") +
  scale_x_continuous("Erfolgswahrscheinlichkeit", limits = c(0, 1))


## -------------------------------------------------------------
sum(posterior[theta_grid > 0.5])


## -------------------------------------------------------------
d %>% 
  filter(theta_grid > .5) %>% 
  summarise(sum = sum(posterior)) 


## -------------------------------------------------------------
samples %>% #<<
  filter(theta_grid > .5) %>% #<<
  summarise(sum = n() / n_samples) #<<


## -------------------------------------------------------------
samples %>% 
  count(theta_grid > .5) %>% 
  mutate(probability = n / sum(n))


## -------------------------------------------------------------
quantile(samples$theta_grid, prob = c(.25, .75))


## -------------------------------------------------------------
library(tidybayes)
median_qi(samples$theta_grid, .width = .5)


## -------------------------------------------------------------
library(tidybayes)
median_qi(samples$theta_grid, .width = c(.5, .8, .99))


## -------------------------------------------------------------
mode_hdi(samples$theta_grid, .width = .5)


## -------------------------------------------------------------
hdi(samples$theta_grid, .width = .5)


## ----echo = TRUE----------------------------------------------
library(patchwork)

p1 <-
  d %>% 
  ggplot(aes(x = theta_grid)) +
  # check out our sweet `qi()` indexing
  geom_ribbon(data = d %>% filter(theta_grid > qi(samples$theta_grid, .width = .5)[1] & 
                                    theta_grid < qi(samples$theta_grid, .width = .5)[2]),
              aes(ymin = 0, ymax = posterior),
              fill = "grey75") +
  geom_line(aes(y = posterior)) +
  labs(subtitle = "50% Percentile Interval",
       x = "Erfolgswahrscheinlichkeit",
       y = "density")


p2 <-
  d %>% 
  ggplot(aes(x = theta_grid)) +
  geom_ribbon(data = . %>% filter(theta_grid > hdi(samples$theta_grid, .width = .5)[1] & 
                                    theta_grid < hdi(samples$theta_grid, .width = .5)[2]),
              aes(ymin = 0, ymax = posterior),
              fill = "grey75") +
  geom_line(aes(y = posterior)) +
  labs(subtitle = "50% HPDI",
       x = "Erfolgswahrscheinlichkeit",
       y = "density")

p1 | p2


## ----echo = FALSE---------------------------------------------
set.seed(3)
d <- tibble(draws = rbinom(1e4, size = 9, prob = .7))

# the histogram
d %>% 
  ggplot(aes(x = draws)) +
  geom_histogram(binwidth = 1, center = 0,
                 color = "grey92", size = 1/10) +
  scale_x_continuous("Erfolge",
                     breaks = seq(from = 0, to = 9, by = 2)) +
  ylab("Häufigkeit") +
  coord_cartesian(xlim = c(0, 9)) +
  theme(panel.grid = element_blank())


## ----echo = TRUE----------------------------------------------
n_draws <- 1e5

simulate_binom <- function(n, probability) {
  set.seed(3)
  rbinom(n_draws, size = n, prob = probability) 
}

d <-
  crossing(n           = c(3, 6, 9),
           probability = c(.3, .6, .9)) %>% 
  mutate(draws = map2(n, probability, simulate_binom)) %>% 
  ungroup() %>% 
  mutate(n           = str_c("n = ", n),
         probability = str_c("theta = ", probability)) %>% 
  unnest(draws)

d %>% 
  ggplot(aes(x = draws)) +
  geom_histogram(binwidth = 1, center = 0,
                 color = "grey92", size = 1/10) +
  scale_x_continuous("Erfolge",
                     breaks = seq(from = 0, to = 9, by = 2)) +
  ylab("Häufigkeit") +
  coord_cartesian(xlim = c(0, 9)) +
  theme(panel.grid = element_blank()) +
  facet_grid(n ~ probability)


## ----echo = FALSE---------------------------------------------
n_samples <- 1e4
n <- 100
n_success <- 6
n_trials <- 9

d <-
  tibble(theta_grid = seq(from = 0, to = 1, length.out = n),
         prior  = dbeta(theta_grid, shape1 = 4, shape2 = 4)) %>% 
  mutate(likelihood = dbinom(n_success, size = n_trials, prob = theta_grid)) %>% 
  mutate(posterior = (likelihood * prior) / sum(likelihood * prior))

samples <-
  d %>% 
  mutate(prior = prior/sum(prior)) %>%
  slice_sample(n = n_samples, weight_by = prior, replace = T) %>% 
  mutate(k = purrr::map_dbl(theta_grid, rbinom, n = 1, size = 9))

samples %>% 
  ggplot(aes(x = k)) +
  geom_histogram(binwidth = 1, center = 0,
                 color = "grey92", size = 1/10) +
  scale_x_continuous("Erfolge",
                     breaks = seq(from = 0, to = 9, by = 3)) +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("Prior predictive distribution") +
  coord_cartesian(xlim = c(0, 9),
                  ylim = c(0, 3000)) +
  theme(panel.grid = element_blank())


## ----echo = FALSE---------------------------------------------
n_samples <- 1e4

# make it reproducible
set.seed(3)

samples <-
  d %>% 
  slice_sample(n = n_samples, weight_by = posterior, replace = T) %>% 
  mutate(k = purrr::map_dbl(theta_grid, rbinom, n = 1, size = 9))

samples %>% 
  ggplot(aes(x = k)) +
  geom_histogram(binwidth = 1, center = 0,
                 color = "grey92", size = 1/10) +
  scale_x_continuous("Erfolge",
                     breaks = seq(from = 0, to = 9, by = 3)) +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("Posterior predictive distribution") +
  coord_cartesian(xlim = c(0, 9),
                  ylim = c(0, 3000)) +
  theme(panel.grid = element_blank())


## ----binomial-graphical-model, echo=FALSE, fig.cap="Graphical Model für binomialverteilte Daten."----
knitr::include_graphics("images/binomial-graphical-model.png")


## ----bernoulli-graphical-model, echo=FALSE, fig.cap="Graphical Model für bernoulliverteilte Daten."----
knitr::include_graphics("images/bernoulli-graphical-model.png")


## ----normal-graphical-model, echo=FALSE, fig.cap="Graphical Model für normalverteilte Daten."----
knitr::include_graphics("images/normal-graphical-model.png")


## -------------------------------------------------------------
n <- 20

mu_sigma <- 5

set.seed(6)
mu <- rnorm(n = 1, mean = 120, 
            sd = mu_sigma)
sigma <- runif(n = 1, 1, 10)
y <- rnorm(n = n, mean = mu, 
           sd = sigma) %>% 
     round(2)


## ----echo = TRUE----------------------------------------------
hist(y, col = 'skyblue3', breaks = 10) 


## -------------------------------------------------------------
library(brms)
d <- tibble(y)


## ----eval = FALSE---------------------------------------------
## priors <- set_prior("normal(120, 5)", class = "Intercept") +
##     set_prior("uniform(1, 10)", class = "sigma")
## 
## fit <- brm(y ~ 1,
##            family = gaussian,
##            prior = priors,
##            data = d,
##            cores = parallel::detectCores())


## -------------------------------------------------------------
library(brms)
d <- tibble(y)


## -------------------------------------------------------------
get_prior(y ~ 1,
          family = gaussian,
          data = d)


## -------------------------------------------------------------
priors <- set_prior("normal(120, 5)", class = "Intercept") +
  set_prior("cauchy(0, 1)", class = "sigma")


## ----echo=TRUE, message=FALSE, warning=FALSE, results='hide'----
m <- brm(y ~ 1,
           family = gaussian,
           prior = priors,
           data = d,
           cores = parallel::detectCores(),
           file = "models/model_1")


## ----eval=FALSE, include=TRUE---------------------------------
## summary(m)


## -------------------------------------------------------------
plot(m)


## -------------------------------------------------------------
library(tidybayes)


## -------------------------------------------------------------
m %>%
  spread_draws(b_Intercept) %>% 
  median_qi(.width = c(.50, .80, .95)) %>% 
  kableExtra::kbl()


## -------------------------------------------------------------
m %>%
  spread_draws(b_Intercept) %>%
  ggplot(aes(x = b_Intercept)) +
  stat_halfeye(.width = c(.50, .80, .95))

