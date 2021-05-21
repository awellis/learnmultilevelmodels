## ----setup, include=FALSE-------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)
library(tidyverse)


## ----load-packages, include=FALSE, warning=FALSE--------------
library(tidyverse)
library(rmarkdown)


## ----normal-graphical-model, echo = FALSE, fig.cap="Graphical Model für normalverteilte Daten.", fig.width=2, fig.height=2, out.width = "50%"----
knitr::include_graphics("images/normal-graphical-model-2.png")


## -------------------------------------------------------------
library(tidyverse)


## -------------------------------------------------------------
sequence_length <- 100

d1 <- crossing(y = seq(from = 50, to = 150, length.out = sequence_length),
              mu = c(87.8, 100, 112),
              sigma = c(7.35, 12.2, 18.4)) %>%
    mutate(density = dnorm(y, mean = mu, sd = sigma),
           mu = factor(mu, labels = str_c("mu==", c(87.8, 100, 112))),
           sigma = factor(sigma, 
                          labels = str_c("sigma==", c(7.35, 12.2, 18.4))))


## -------------------------------------------------------------
theme_set(
  theme_bw() +
    theme(text = element_text(color = "white"),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "white"),
          legend.background = element_blank(),
          legend.box.background = element_rect(fill = "white",
                                               color = "transparent"),
          legend.key = element_rect(fill = "white",
                                    color = "transparent"),
          legend.text = element_text(color = "black"),
          legend.title = element_text(color = "black"),
          panel.background = element_rect(fill = "white",
                                          color = "white"),
          panel.grid = element_blank()))



## ----maxlik, fig.cap = "Kombinationen von $\\mu$ und $\\sigma$ Parameterwerten."----
d1 %>% 
  ggplot(aes(x = y)) +
  geom_ribbon(aes(ymin = 0, ymax = density),
              fill = "steelblue") +
  geom_vline(xintercept = c(85, 100, 115), 
             linetype = 3, color = "white") +
  geom_point(data = tibble(y = c(85, 100, 115)),
             aes(y = 0.002),
             size = 2, color = "red") +
  scale_y_continuous(expression(italic(p)(italic(y)*"|"*mu*", "*sigma)), 
                     expand = expansion(mult = c(0, 0.05)), breaks = NULL) +
  ggtitle("Welche Normalverteilung?") +
  coord_cartesian(xlim = c(60, 140)) +
  facet_grid(sigma ~ mu, labeller = label_parsed) +
  theme_bw() +
  theme(panel.grid = element_blank())


## -------------------------------------------------------------
smart = tibble(IQ = c(101,100,102,104,102,97,105,105,98,101,100,123,105,103,
                      100,95,102,106,109,102,82,102,100,102,102,101,102,102,
                      103,103,97,97,103,101,97,104,96,103,124,101,101,100,
                      101,101,104,100,101),
               Group = "SmartDrug")

placebo = tibble(IQ = c(99,101,100,101,102,100,97,101,104,101,102,102,100,105,
                        88,101,100,104,100,100,100,101,102,103,97,101,101,100,101,
                        99,101,100,100,101,100,99,101,100,102,99,100,99),
                 Group = "Placebo")


## -------------------------------------------------------------
TwoGroupIQ <- bind_rows(smart, placebo) %>%
    mutate(Group = fct_relevel(as.factor(Group), "Placebo"))


## -------------------------------------------------------------
library(kableExtra)

TwoGroupIQ %>%
  group_by(Group) %>%
  summarise(mean = mean(IQ),
            sd = sd(IQ)) %>%
  mutate(across(where(is.numeric), round, 2)) %>% 
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


## -------------------------------------------------------------
t.test(IQ ~ Group,
       data = TwoGroupIQ)


## -------------------------------------------------------------
d <- TwoGroupIQ %>% 
  filter(Group == "SmartDrug") %>% 
  mutate(Group = fct_drop(Group))


## -------------------------------------------------------------
d %>% 
  ggplot(aes(x = IQ)) +
  geom_histogram(fill = "skyblue3", binwidth = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  theme_tidybayes()


## ----message=FALSE, warning=FALSE-----------------------------
library(brms)


## -------------------------------------------------------------
priors <- get_prior(IQ ~ 1,
          data = d)
priors


## ----tdist, fig.cap = "Prior für $\\sigma$."------------------
tibble(x = seq(from = 0, to = 10, by = .025)) %>% 
  mutate(d = dt(x, df = 3)) %>% 
  ggplot(aes(x = x, ymin = 0, ymax = d)) +
  geom_ribbon(fill = "skyblue3") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = NULL) +
  coord_cartesian(xlim = c(0, 8),
                  ylim = c(0, 0.35)) +
  xlab(expression(sigma)) +
  labs(subtitle = "Half-student-t Distribution: Prior für Standardabweichung.") +
  theme_bw(base_size = 14)


## ----normaldist, fig.cap = "Prior für $\\mu$."----------------
tibble(x = seq(from = 0, to = 200, by = .025)) %>% 
  mutate(d = dnorm(x, mean = 102, sd = 3)) %>% 
  ggplot(aes(x = x, ymin = 0, ymax = d)) +
  geom_ribbon(fill = "skyblue3") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = NULL) +
  coord_cartesian(xlim = c(50, 150),
                  ylim = c(0, 0.15)) +
  xlab(expression(mu)) +
  labs(subtitle = "Normalverteilter Prior für Mittelwert") +
  theme_bw(base_size = 14)


## -------------------------------------------------------------
m1_prior <- brm(IQ ~ 1,
          prior = priors,
          data = d,
          sample_prior = "only",
          file = "models/twogroupiq-prior-1")


## -------------------------------------------------------------
summary(m1_prior)


## -------------------------------------------------------------
plot(m1_prior)


## -------------------------------------------------------------
library(tidybayes)

prior_pred_1 <- d %>%
  modelr::data_grid(Group) %>%
  add_predicted_draws(m1_prior) %>%
  ggplot(aes(y = Group, x = .prediction)) +
  stat_interval(.width = c(.50, .80, .95, .99)) +
  geom_point(aes(x = IQ), alpha = 0.4,  data = d) +
  scale_color_brewer() +
  theme_tidybayes()

prior_pred_1


## -------------------------------------------------------------
m1 <- brm(IQ ~ 1,
          prior = priors,
          data = d,
          file = "models/twogroupiq-1")


## -------------------------------------------------------------
plot(m1)


## -------------------------------------------------------------
print(m1)


## -------------------------------------------------------------
mcmc_plot(m1, pars = "b_") 

## -------------------------------------------------------------
mcmc_plot(m1, pars = "sigma")


## -------------------------------------------------------------
samples <- posterior_samples(m1) %>% 
  transmute(mu = b_Intercept, sigma = sigma)


## -------------------------------------------------------------
library(tidybayes)

samples %>% 
  select(mu) %>% 
  median_qi(.width = c(.50, .80, .95, .99))


## ----echo=TRUE------------------------------------------------
samples %>% 
  select(mu) %>% 
  median_qi(.width = c(.50, .80, .95, .99)) %>% 
  ggplot(aes(x = mu, xmin = .lower, xmax = .upper)) +
  geom_pointinterval() +
  ylab("") +
  theme_tidybayes()


## ----echo=TRUE------------------------------------------------
samples %>% 
  select(mu) %>% 
  ggplot(aes(x = mu)) +
  stat_halfeye() +
  theme_tidybayes()


## ----echo=TRUE------------------------------------------------
samples %>% 
  select(sigma) %>% 
  ggplot(aes(x = sigma)) +
  stat_halfeye(point_interval = mode_hdi) +
  theme_tidybayes()


## -------------------------------------------------------------
post_pred_1 <- d %>%
  modelr::data_grid(Group) %>%
  add_predicted_draws(m1) %>%
  ggplot(aes(y = Group, x = .prediction)) +
  stat_interval(.width = c(.50, .80, .95, .99)) +
  geom_point(aes(x = IQ), alpha = 0.4,  data = d) +
  scale_color_brewer() +
  theme_tidybayes()

post_pred_1


## -------------------------------------------------------------
# cowplot fall nötig installieren
if (!("cowplot" %in% installed.packages())) {install.packages("cowplot")}

cowplot::plot_grid(prior_pred_1, 
                   post_pred_1, 
                   labels = c('Prior predictive', 'Posterior predictive'), 
                   label_size = 12,
                   align = "h",
                   nrow = 2)


## -------------------------------------------------------------
TwoGroupIQ %>%
   ggplot(aes(x = IQ, fill = Group)) +
      geom_dotplot(binwidth = 1) + 
      scale_fill_manual(values = c("#0288b7", "#a90010"), guide = FALSE) +
      scale_y_continuous(breaks = NULL) +
      labs(y = "Count", x = "IQ") +
      facet_wrap(~ Group, nrow = 2) +
      plot_annotation(title = "IQ difference",
                      subtitle = "Smart drug vs placebo",
                      theme = theme(plot.title = element_text(face = "bold",
                                                          size = rel(1.5))))


## ----echo=TRUE, message=FALSE, warning=FALSE------------------
levels(TwoGroupIQ$Group)


## ----echo=TRUE------------------------------------------------
fit_ols <- lm(IQ ~ Group,
              data = TwoGroupIQ)


## ----echo=TRUE------------------------------------------------
summary(fit_ols)


## -------------------------------------------------------------
contrasts(TwoGroupIQ$Group)


## -------------------------------------------------------------
mm1 <- model.matrix(~ Group, data = TwoGroupIQ)
head(mm1)


## -------------------------------------------------------------
as_tibble(mm1) %>% 
  group_by(GroupSmartDrug) %>% 
  slice_sample(n= 3)


## -------------------------------------------------------------
as_tibble(mm1) %>% 
  group_by(GroupSmartDrug) %>% 
  slice_sample(n= 3)


## -------------------------------------------------------------
mm2 <- model.matrix(~ 0 + Group, data = TwoGroupIQ)
as_tibble(mm2) %>% 
  group_by(GroupSmartDrug) %>% 
  slice_sample(n= 3)


## ----normal-graphical-model-2, echo = FALSE, out.width = "20%", fig.cap="Graphical Model für 2 Gruppen."----
knitr::include_graphics("images/two-group-iq-graphical-model.png")


## -------------------------------------------------------------
priors2 <- get_prior(IQ ~ 1 + Group,
                     data = TwoGroupIQ)


## -------------------------------------------------------------
priors2


## -------------------------------------------------------------
priors3 <- get_prior(IQ ~ 0 + Group,
                     data = TwoGroupIQ)


## -------------------------------------------------------------
priors3


## -------------------------------------------------------------
priors2_b <- prior(normal(0, 2), class = b)


## -------------------------------------------------------------
m2_prior <- brm(IQ ~ 1 + Group,
          prior = priors2_b,
          data = TwoGroupIQ,
          sample_prior = "only",
          file = "models/twogroupiq-prior-2")


## -------------------------------------------------------------
prior_pred_2 <- TwoGroupIQ %>%
  modelr::data_grid(Group) %>%
  add_predicted_draws(m2_prior) %>%
  ggplot(aes(y = Group, x = .prediction)) +
  stat_interval(.width = c(.50, .80, .95, .99)) +
  geom_point(aes(x = IQ), alpha = 0.4,  data = TwoGroupIQ) +
  scale_color_brewer() +
  theme_tidybayes()


## ----echo=FALSE-----------------------------------------------
prior_pred_2


## -------------------------------------------------------------
priors3_b <- prior(normal(100, 10), class = b)


## -------------------------------------------------------------
m3_prior <- brm(IQ ~ 0 + Group,
          prior = priors3_b,
          data = TwoGroupIQ,
          sample_prior = "only",
          file = "models/twogroupiq-prior-3")


## -------------------------------------------------------------
prior_pred_3 <- TwoGroupIQ %>%
  modelr::data_grid(Group) %>%
  add_predicted_draws(m3_prior) %>%
  ggplot(aes(y = Group, x = .prediction)) +
  stat_interval(.width = c(.50, .80, .95, .99)) +
  geom_point(aes(x = IQ), alpha = 0.4,  data = TwoGroupIQ) +
  scale_color_brewer() +
  theme_tidybayes()


## ----echo=FALSE-----------------------------------------------
prior_pred_3


## -------------------------------------------------------------
m2 <- brm(IQ ~ 1 + Group,
          prior = priors2_b,
          data = TwoGroupIQ,
          file = "models/twogroupiq-2")


## -------------------------------------------------------------
m3 <- brm(IQ ~ 0 + Group,
          prior = priors3_b,
          data = TwoGroupIQ,
          file = "models/twogroupiq-3")


## -------------------------------------------------------------
summary(m2)


## -------------------------------------------------------------
summary(m3)


## -------------------------------------------------------------
mcmc_plot(m2, "b_GroupSmartDrug")


## -------------------------------------------------------------
mcmc_plot(m3, "b")


## -------------------------------------------------------------
samples_m3 <- posterior_samples(m3) %>% 
    transmute(Placebo = b_GroupPlacebo, 
              SmartDrug = b_GroupSmartDrug,
              sigma = sigma)


## -------------------------------------------------------------
samples_m3 <- samples_m3 %>% 
  mutate(diff = SmartDrug - Placebo,
         effect_size = diff/sigma)


## -------------------------------------------------------------
samples_m3 %>% 
  select(diff) %>% 
  median_qi()


## -------------------------------------------------------------
samples_m3 %>% 
  select(diff) %>% 
  ggplot(aes(x = diff)) +
  stat_halfeye(point_interval = median_qi) +
  theme_tidybayes()


## -------------------------------------------------------------
samples_m3 %>% 
  select(effect_size) %>% 
  ggplot(aes(x = effect_size)) +
  stat_halfeye(point_interval = median_qi) +
  theme_tidybayes()


## -------------------------------------------------------------
fit_eqvar <- brm(IQ ~ Group,
                 data = TwoGroupIQ,
                 file = here::here("models/fit_eqvar"))


## -------------------------------------------------------------
fit_eqvar %>%
    gather_draws(b_GroupSmartDrug) %>%
    ggplot(aes(y = .variable, x = .value)) +
    stat_halfeye(fill = "Steelblue4") +
    geom_vline(xintercept = 0, color = "white", linetype = 1, size = 1) +
    ylab("") +
    xlab("Estimated difference") +
    theme_tidybayes()


## ----eval=FALSE, echo=TRUE------------------------------------
## grid <- TwoGroupIQ %>%
##     modelr::data_grid(Group)
## 
## fits_IQ <- grid %>%
##     add_fitted_draws(fit_eqvar)
## 
## preds_IQ <- grid %>%
##     add_predicted_draws(fit_eqvar)
## 
## pp_eqvar <- TwoGroupIQ %>%
##     ggplot(aes(x = IQ, y = Group)) +
##     stat_halfeye(aes(x = .value),
##                   scale = 0.7,
##                   position = position_nudge(y = 0.1),
##                   data = fits_IQ,
##                   .width = c(.66, .95, 0.99)) +
##     stat_interval(aes(x = .prediction),
##                    data = preds_IQ,
##                    .width = c(.66, .95, 0.99)) +
##     scale_x_continuous(limits = c(75, 125)) +
##     geom_point(data = TwoGroupIQ) +
##     scale_color_brewer() +
## 	labs(title = "Equal variance model predictions") +
##   theme_tidybayes()


## ----echo=FALSE-----------------------------------------------
grid <- TwoGroupIQ %>%
    modelr::data_grid(Group)

fits_IQ <- grid %>%
    add_fitted_draws(fit_eqvar)

preds_IQ <- grid %>%
    add_predicted_draws(fit_eqvar)

TwoGroupIQ %>%
    ggplot(aes(x = IQ, y = Group)) +
    stat_halfeye(aes(x = .value),
                  scale = 0.7,
                  position = position_nudge(y = 0.1),
                  data = fits_IQ,
                  .width = c(.66, .95, 0.99)) +
    stat_interval(aes(x = .prediction),
                   data = preds_IQ,
                   .width = c(.66, .95, 0.99)) +
    scale_x_continuous(limits = c(75, 125)) +
    geom_point(data = TwoGroupIQ) +
    scale_color_brewer() +
	labs(title = "Equal variance model predictions") +
  theme_tidybayes()

