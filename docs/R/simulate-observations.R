a       <-  3.5  # average morning wait time
b       <- -1    # average difference afternoon wait time
sigma_a <-  1    # std dev in intercepts
sigma_b <-  0.8  # std dev in slopes -> set to 0.5
rho     <- -.7   # correlation between intercepts and slopes -> set to -0.7

# the next three lines of code simply combine the terms, above
mu     <- c(a, b)
cov_ab <- sigma_a * sigma_b * rho
sigma  <- matrix(c(sigma_a^2, cov_ab,
                   cov_ab, sigma_b^2), ncol = 2)


sigmas <- c(sigma_a, sigma_b)          # standard deviations
rho    <- matrix(c(1, rho,             # correlation matrix
                   rho, 1), nrow = 2)

# now matrix multiply to get covariance matrix
sigma <- diag(sigmas) %*% rho %*% diag(sigmas)

# how many cafes would you like?
n_cafes <- 20

set.seed(13)  # used to replicate example
vary_effects <-
    MASS::mvrnorm(n_cafes, mu, sigma) %>%
    data.frame() %>%
    set_names("a_cafe", "b_cafe")

head(vary_effects)

vary_effects %>%
    ggplot(aes(x = a_cafe, y = b_cafe)) +
    geom_point(color = "#80A0C7") +
    geom_rug(color = "#8B9DAF", size = 1/7)


n_visits <- 10
sigma    <-  0.5  # std dev within cafes

set.seed(13)  # used to replicate example
d <-
    vary_effects %>%
    mutate(cafe      = 1:n_cafes) %>%
    expand(nesting(cafe, a_cafe, b_cafe), visit = 1:n_visits) %>%
    mutate(afternoon = rep(0:1, times = n() / 2)) %>%
    mutate(mu        = a_cafe + b_cafe * afternoon) %>%
    mutate(wait      = rnorm(n = n(), mean = mu, sd = sigma))


d %>%
    mutate(afternoon = ifelse(afternoon == 0, "M", "A"),
           day       = rep(rep(1:5, each = 2), times = n_cafes)) %>%
    filter(cafe %in% c(3, 5)) %>%
    mutate(cafe = ifelse(cafe == 3, "cafe #3", "cafe #5")) %>%

    ggplot(aes(x = visit, y = wait, group = day)) +
    geom_point(aes(color = afternoon), size = 2) +
    geom_line(color = "#8B9DAF") +
    scale_color_manual(values = c("#80A0C7", "#EEDA9D")) +
    scale_x_continuous(NULL, breaks = 1:10,
                       labels = rep(c("M", "A"), times = 5)) +
    coord_cartesian(ylim = c(0, 6)) +
    ylab("wait time in minutes") +
    theme(legend.position = "none",
          axis.ticks.x    = element_blank()) +
    facet_wrap(~cafe, ncol = 1)
