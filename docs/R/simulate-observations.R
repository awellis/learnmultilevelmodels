simulate_data <- function() {


}
a <-  3.5  # average pre-treatment score
b <- -1    # average difference between pre and post
sigma_a <-  1    # std dev in intercepts
sigma_b <-  0.8  # std dev in slopes -> set to 0.5
rho <- -.7   # correlation between intercepts and slopes -> set to -0.7

# the next three lines of code simply combine the terms, above
mu     <- c(a, b)
cov_ab <- sigma_a * sigma_b * rho
sigma  <- matrix(c(sigma_a^2, cov_ab,
                   cov_ab, sigma_b^2), ncol = 2)

# how many subjects?
n_subjects <- 3

varying_effects <-
    MASS::mvrnorm(n_subjects, mu, sigma) |>
   as_tibble(.name_repair = "unique") |>
    set_names("a_j", "b_j")



n_trials <- 10
sigma    <-  0.5  # std dev within subjects

# set.seed(13)  # used to replicate example

d_linpred <-
    varying_effects |>
    mutate(subject  = 1:n_subjects) |>
    expand(nesting(subject, a_j, b_j), post = c(0, 1)) |>
    mutate(mu = a_j + b_j * post,
           sigma = sigma) |>
    mutate(treatment = ifelse(post == 0, "pre", "post"),
           treatment = factor(treatment, levels = c("pre", "post")))



d <- d_linpred |>
    slice(rep(1:n(), each = n_trials)) |>
    mutate(response = rnorm(n = n(), mean = mu, sd = sigma))




varying_effects %>%
    ggplot(aes(x = a_j, y = b_j)) +
    geom_point(color = "#80A0C7") +
    geom_rug(color = "#8B9DAF", size = 1/7)


d |>
    ggplot(aes(x = treatment, y = mu)) +
    geom_point(aes(color = treatment), size = 2) +
    geom_line(color = "#8B9DAF") +
    scale_color_manual(values = c("#80A0C7", "#EEDA9D")) +
    coord_cartesian(ylim = c(0, 6)) +
    ylab("Response") +
    theme(legend.position = "none",
          axis.ticks.x    = element_blank()) +
    facet_wrap(~ subject)



d |>
    ggplot(aes(x = treatment, y = response)) +
    geom_jitter(aes(color = treatment), width = 0.1, size = 2) +
    scale_color_manual(values = c("#80A0C7", "#EEDA9D")) +
    coord_cartesian(ylim = c(0, 6)) +
    ylab("Response") +
    theme(legend.position = "none",
          axis.ticks.x    = element_blank()) +
    facet_wrap(~ subject)
