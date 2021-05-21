## ----setup, include=FALSE-------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)
library(tidyverse)


## ----echo=TRUE------------------------------------------------
library(tidyverse)
library(kableExtra)

set.seed(12)

# Number of people wearing fancy hats
N_fancyhats <- 50 

# Number of people not wearing fancy hats
N_nofancyhats <- 50

# Population mean of creativity for people wearing fancy hats
mu_fancyhats <- 103 

# Population mean of creativity for people wearing no fancy hats
mu_nofancyhats <- 98 

# Average population standard deviation of both groups
sigma <- 15 

# Generate data
fancyhats = tibble(Creativity = rnorm(N_fancyhats, mu_fancyhats, sigma),
               Group = "Fancy Hat")

nofancyhats = tibble(Creativity = rnorm(N_nofancyhats, mu_nofancyhats, sigma),
                 Group = "No Fancy Hat")


FancyHat <- bind_rows(fancyhats, nofancyhats)  %>%
    mutate(Group = fct_relevel(as.factor(Group), "No Fancy Hat"))


## -------------------------------------------------------------
kbl(FancyHat) %>%
  kable_paper() %>%
  scroll_box(width = "500px", height = "200px")


## -------------------------------------------------------------
# plot both groups
FancyHat %>% 
    ggplot() +
    geom_boxplot ((aes(y = Creativity, x = Group))) +
    labs(title= "Box Plot of Creativity Values") +
    theme_bw()


## -------------------------------------------------------------
fancyhat_ttest <- t.test(Creativity ~ Group,
       var.equal = TRUE,
       data = FancyHat)


## -------------------------------------------------------------
fancyhat_ttest_tab <- broom::tidy(fancyhat_ttest)


## -------------------------------------------------------------
fancyhat_ttest_tab %>%
    select(estimate, estimate1, estimate2, statistic, p.value, conf.low, conf.high) %>%
    round(3) %>% 
    kbl() %>%
    kable_classic(full_width = FALSE, html_font = "Cambria")


## ----gamma-dist, fig.cap = "Wahrscheinlichkeitsverteilungen", layout="l-body-outset", fig.width=6, fig.height=1.5----
library(tidyverse)

tibble(x = seq(from = 0, to = 60, by = .1)) %>% 
  tidyr::expand(x, nesting(alpha = c(2, 4), 
                    beta  = c(0.1, 1))) %>% 
  mutate(density = dgamma(x, alpha, beta),
         group   = rep(letters[1:2], times = n() / 2)) %>% 
  
  ggplot(aes(x = x, ymin = 0, ymax = density, 
             group = group, fill = group)) +
  geom_ribbon(size = 0, alpha = 3/4) +
  scale_fill_viridis_d(option = "B", direction = -1, 
                       begin = 1/3, end = 2/3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(0, 50)) +
  theme(panel.grid = element_blank(),
        legend.position = "none")


## -------------------------------------------------------------
wins <- 6
games <- 9


## -------------------------------------------------------------
dbinom(x = wins, size = games, prob = 0.5)


## -------------------------------------------------------------
pbinom(q = 5, size = games, prob = 0.5)


## -------------------------------------------------------------
1 - pbinom(q = 5, size = games, prob = 0.5)


## -------------------------------------------------------------
pbinom(q = 5, size = games, prob = 0.5, lower.tail = FALSE)


## -------------------------------------------------------------
theta <- wins/games
theta


## ----maxlikbinom, fig.cap = "Maximum-Likelihood Schätzung ", layout="l-body-outset", fig.width=6, fig.height=1.5----
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
p_grid <- seq( from=0 , to=1 , length.out = n_points )


## -------------------------------------------------------------
likelihood <- dbinom(wins , size = games , prob = p_grid)


## ----eval=FALSE, include=FALSE--------------------------------
## compute_posterior = function(likelihood, prior){
##   # compute product of likelihood and prior
##   unstandardized_posterior <- likelihood * prior
## 
##   # standardize the posterior, so it sums to 1
##   posterior <- unstandardized_posterior / sum(unstandardized_posterior)
## 
##   out <- tibble(prior, likelihood, posterior) %>%
##     pivot_longer(everything(), names_to = "distribution", values_to = "val") %>%
##     mutate(distribution = as_factor(distribution))
##   out
## }


## -------------------------------------------------------------
compute_posterior = function(likelihood, prior){
  # compute product of likelihood and prior
  unstandardized_posterior <- likelihood * prior
  
  # standardize the posterior, so it sums to 1
  posterior <- unstandardized_posterior / sum(unstandardized_posterior)
  
  par(mfrow=c(1, 3))
  plot(p_grid , prior, type="l", main="Prior", col = "dodgerblue3", lwd = 2)
  plot(p_grid , likelihood, type="l", main="Likelihood", col = "firebrick3", lwd = 2)
  plot(p_grid , posterior , type="l", main="Posterior", col = "darkorchid3", lwd = 2)
}


## -------------------------------------------------------------
prior1 <- rep(0.5 , length(p_grid))
compute_posterior(likelihood, prior1)


## -------------------------------------------------------------
prior2 <- ifelse(p_grid < 0.5, 0, 1)
compute_posterior(likelihood, prior2)


## -------------------------------------------------------------
prior3 <- exp(-10 * abs(p_grid - 0.5))
compute_posterior(likelihood, prior3)


## -------------------------------------------------------------
length <- 1e4
d <- crossing(shape1 = c(.1, 1:4),
           shape2 = c(.1, 1:4)) %>%
  tidyr::expand(nesting(shape1, shape2),
         x = seq(from = 0, to = 1, length.out = length)) %>% 
  mutate(a = str_c("a = ", shape1),
         b = str_c("b = ", shape2),
         group = rep(1:length, each = 25))

## ----betadists, fig.cap = "Beta Verteilungen", layout="l-body-outset", fig.width=6, fig.height=5.5----
d %>% 
  ggplot(aes(x = x, group = group)) +
  
  geom_line(aes(y = dbeta(x, shape1 = shape1, shape2 = shape2)),
            color = "steelblue4", size = 1.1) +
  scale_x_continuous(expression(theta), breaks = c(0, .5, 1)) +
  coord_cartesian(ylim = c(0, 3)) +
  labs(title = "Beispiele von Beta Verteilungen",
       y = expression(p(theta*"|"*a*", "*b))) +
  theme(panel.grid = element_blank()) +
  facet_grid(b~a)


## -------------------------------------------------------------
prior4 <- dbeta(x = p_grid, shape1 = 20, shape2 = 4)
compute_posterior(likelihood, prior4)


## -------------------------------------------------------------
prior5 <- dbeta(x = p_grid, shape1 = 4, shape2 = 20)
compute_posterior(likelihood, prior5)


## data {

##   int<lower=0> n; // number of games

##   int<lower=0> k; // number of wins

## 

## }

## 
## parameters {

##   real<lower=0, upper=1> theta;

## }

## 
## model {

##   theta ~ beta(4, 4);

##   k ~ binomial(n, theta);

## }


## ----echo=TRUE, message=FALSE, warning=FALSE------------------
library(rstan)


## -------------------------------------------------------------
stan_data <- list(
  n = 9,
  k = 6
)


## ----eval=FALSE, message=FALSE, warning=FALSE, include=TRUE, results='hide'----
## fit <- stan(file = "binomial-model.stan",  # Stan program
##             data = stan_data,    # named list of data
##             chains = 4,          # number of Markov chains
##             iter = 2000,         # total number of iterations per chain
##             cores = 4)           # number of cores (could use one per chain)


## ----message=FALSE, warning=FALSE, include=FALSE, results='hide'----
if (fs::file_exists("models/binomial-1.rds")) {
  fit <- readRDS("models/binomial-1.rds")
} else {
  fit <- stan(file = "binomial-model.stan",  # Stan program
            data = stan_data,    # named list of data
            chains = 4,          # number of Markov chains
            iter = 2000,         # total number of iterations per chain
            cores = 4)           # number of cores (could use one per chain)
  saveRDS(fit, file = "models/binomial-1.rds") 
}
    


## -------------------------------------------------------------
print(fit)


## -------------------------------------------------------------
traceplot(fit)


## ---- layout="l-body-outset", fig.width=6, fig.height=1.5-----
bayesplot::mcmc_intervals(fit, "theta")


## -------------------------------------------------------------
bayesplot::mcmc_areas(fit, "theta")


## ----message=FALSE, warning=FALSE-----------------------------
library(brms)

## -------------------------------------------------------------
data <- tibble(k = 6, n = 9)


## ----include=TRUE, echo=TRUE----------------------------------
priors <- prior(beta(4, 4), class = b, lb = 0, ub = 1)

m1 <- brm(k | trials(n) ~ 0 + Intercept, family = binomial(link = "identity"),
          prior = priors,
          data = data,
          control = list(adapt_delta = 0.9),
          file = "models/binomial-2")


## ---- layout="l-body-outset", fig.width=6, fig.height=1.5-----
plot(m1)


## -------------------------------------------------------------
library(tidybayes)


## -------------------------------------------------------------
m1 %>%
  spread_draws(b_Intercept) %>% 
  median_qi(.width = c(.50, .80, .95)) %>% 
  kableExtra::kbl()


## -------------------------------------------------------------
m1 %>%
  spread_draws(b_Intercept) %>%
  ggplot(aes(x = b_Intercept)) +
  stat_halfeye(.width = c(.50, .80, .95))

