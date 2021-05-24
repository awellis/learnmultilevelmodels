library(tidyverse)
library(brms)



d <- tibble(y = c(85, 100, 115))


funs = list(mean = mean, sd = sd)

d %>%
    summarise(across(y, funs, .names = "{.fn}"))

get_prior(y ~ 1, data = d)



fit1_prior <- brm(y ~ 1,
            prior = prior(student_t(3, 100, 22.2), class = Intercept) +
                    prior(student_t(3, 100, 22.2), class = sigma),
            sample_prior = "only",
            data = d)

fit1 <- brm(y ~ 1,
                  prior = prior(student_t(3, 100, 22.2), class = Intercept) +
                      prior(student_t(3, 100, 22.2), class = sigma),
                  data = d)

summary(fit1_prior)

out <- posterior_predict(fit1)
pp_check(fit1)



l1 <- loo(fit1)
plot(l1)




d <- tibble(y1 = rnorm(10, 100, 15),
            y2 = c(y1[1:9], 30))

funs = list(mean = mean, sd = sd)

d %>%
    summarise(across(everything(), funs))


get_prior(y1 ~ 1, data = d)


fit1 <- brm(y1 ~ 1,
            prior = prior(student_t(3, 100, 22.2), class = Intercept) +
                prior(student_t(3, 100, 22.2), class = sigma),
            data = d)


fit2 <- brm(y2 ~ 1,
            prior = prior(student_t(3, 100, 22.2), class = Intercept) +
                prior(student_t(3, 100, 22.2), class = sigma),
            data = d)
fit1
fit2
l1 <- loo(fit1)
l2 <- loo(fit2)
plot(l1)
plot(l2)
pp_check(fit2)

predict(fit2, summary = F, subset = 1)
