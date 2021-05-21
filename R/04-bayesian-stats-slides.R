## ----child = "setup.Rmd"--------------------------------------

## ----setup, include=FALSE-------------------------------------
knitr::opts_chunk$set(include = TRUE,
                      fig.width=9, fig.height=3.5, fig.retina=3,
                      out.width = "100%",
                      dpi = 300,
                      cache = FALSE,
                      echo = TRUE,
                      message = FALSE, 
                      warning = FALSE,
                      fig.show = TRUE,
                      hiline = TRUE)
options(htmltools.dir.version = FALSE)

# fontawesome
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# htmltools::tagList(
#   xaringanExtra::use_clipboard(
#     button_text = "<i class=\"fa fa-clipboard\"></i>",
#     success_text = "<i class=\"fa fa-check\" style=\"color: #90BE6D\"></i>",
#     error_text = "<i class=\"fa fa-times-circle\" style=\"color: #F94144\"></i>"
#   ),
#   rmarkdown::html_dependency_font_awesome()
# )

# magick
# dev.off <- function(){
#   invisible(grDevices::dev.off())
# }
# set seed
# set.seed(324)


## ----xaringanthemer, include=FALSE----------------------------
library(xaringanthemer)
# style_duo_accent(
#   primary_color = "#4C566B",
#   secondary_color = "#C16069",
#   inverse_header_color = "#FFFFFF"
# )

style_duo_accent(
  # primary_color = "#5e81ac", # "#4C566B",
  # secondary_color = "#d08770",
  # inverse_header_color = "#FFFFFF",
  primary_color      = "#0F4C81", # pantone classic blue
  secondary_color    = "#A2B8D4", # pantone cashmere blue
  header_font_google = google_font("Raleway"),
  text_font_google   = google_font("Raleway", "300", "300i"),
  code_font_google   = google_font("Fira Code"),
  # text_font_size     = "30px",
  outfile            = "css/xaringan-themer.css"
)

xaringanExtra::use_tile_view()
xaringanExtra::use_panelset()
xaringanExtra::use_clipboard()
xaringanExtra::use_extra_styles(
  hover_code_line = TRUE,         #<<
  mute_unhighlighted_code = TRUE  #<<
)
xaringanExtra::use_scribble(pen_color = "#bf616a")
xaringanExtra::use_search(show_icon = FALSE,
                          position = "bottom-right",
                          auto_search = TRUE)



## ----load-packages, include=FALSE, warning=FALSE--------------
library(tidyverse)
library(rmarkdown)
library(countdown)


## -------------------------------------------------------------
IQwide <- tribble(
  ~A, ~B, ~C,
  110, 105, 115,
  105, 112, 108,
  102, 113, 130
)


## -------------------------------------------------------------
IQdata <- IQwide %>% 
  pivot_longer(everything(), names_to = "Person", values_to = "IQ") %>% 
  mutate(Person = as_factor(Person)) %>% 
  arrange(Person)


## -------------------------------------------------------------
library(kableExtra)
IQdata %>% 
  kbl() %>% 
  scroll_box(width = "500px", height = "200px")


## -------------------------------------------------------------
se <- function(x) sd(x)/sqrt(length(x))

IQdata %>% 
  group_by(Person) %>% 
  summarise(mean = mean(IQ),
            sd = sd(IQ),
            se = se(IQ))


## -------------------------------------------------------------
IQdata %>% 
  ggplot(aes(Person, IQ)) +
  geom_point()


## ----echo=FALSE-----------------------------------------------
countdown(minutes = 3)


## -------------------------------------------------------------
library(brms)
get_prior(IQ ~ 0 + Person,
          data = IQdata)


## ----message=FALSE, warning=FALSE-----------------------------
m_no_pool <- brm(IQ ~ 0 + Person,
                 data = IQdata,
                 file = "models/m_no_pool")


## -------------------------------------------------------------
get_prior(IQ ~ 1,
          data = IQdata)


## ----message=FALSE, warning=FALSE-----------------------------
m_comp_pool <-  brm(IQ ~ 1,
                 data = IQdata,
                 file = "models/m_comp_pool")


## -------------------------------------------------------------
get_prior(IQ ~ 1 + (1 | Person),
                 data = IQdata)


## ----echo=FALSE-----------------------------------------------
library(tidybayes)
priors <- get_prior(IQ ~ 1 + (1 | Person),
                 data = IQdata)

priors %>%
  parse_dist(prior) %>%
  ggplot(aes(y = class, dist = .dist, args = .args)) +
  stat_dist_halfeye() +
  labs(
    title = "Prior Verteilungen",
    subtitle = "im Partial Pooling Modell",
    x = NULL
  )


## ----message=FALSE, warning=FALSE-----------------------------
m_part_pool <-  brm(IQ ~ 1 + (1 | Person),
                 data = IQdata,
                 file = "models/m_part_pool")


## -------------------------------------------------------------
m_no_pool


## -------------------------------------------------------------
m_comp_pool


## -------------------------------------------------------------
m_part_pool


## -------------------------------------------------------------
f <- fixef(m_part_pool, summary = FALSE)
r <- ranef(m_part_pool, summary = FALSE)


## -------------------------------------------------------------
library(tidybayes)

get_variables(m_part_pool)

person_effects <- m_part_pool %>%
  spread_draws(b_Intercept, r_Person[Person, Intercept]) %>%
  # add the grand mean to the person-specific deviations
  mutate(mu = b_Intercept + r_Person)


## -------------------------------------------------------------
person_effects %>% 
  median_qi(mu)


## -------------------------------------------------------------
fixef(m_no_pool)


## ---- echo=TRUE, eval=FALSE-----------------------------------
## person_effects %>%
##   ggplot(aes(mu, Person)) +
##   stat_halfeye(fill = "#A2BF8A") +
##   geom_vline(xintercept = fixef(m_part_pool)[1, 1], color = "#839496", size = 1) +
##   geom_vline(xintercept = fixef(m_part_pool)[1, 3:4], color = "#839496", linetype = 2) +
##   labs(x = expression("Personen-spezifische Mittelwerte"),
##        y = "Personen") +
##   theme(panel.grid   = element_blank(),
##         axis.ticks.y = element_blank(),
##         axis.text.y  = element_text(hjust = 0))


## ----echo=FALSE-----------------------------------------------
person_effects %>% 
  ggplot(aes(mu, Person)) +
  stat_halfeye(fill = "#A2BF8A") +
  geom_vline(xintercept = fixef(m_part_pool)[1, 1], color = "#839496", size = 1) +
  geom_vline(xintercept = fixef(m_part_pool)[1, 3:4], color = "#839496", linetype = 2) +
  labs(x = expression("Personen-spezifische Mittelwerte"),
       y = "Personen") +
  theme(panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0)) 


## ---- echo=TRUE, eval=FALSE-----------------------------------
## col <- viridis::viridis(3, begin = 0.2, end = 0.8)
## 
## person_effects %>%
##   ggplot(aes(mu, Person, fill = Person)) +
##   stat_halfeye(alpha = 0.6) +
##   geom_vline(xintercept = fixef(m_part_pool)[1, 1], color = "#ECCC87",
##              size = 1) +
##   geom_vline(xintercept = fixef(m_no_pool)[1, 1], color = col[1],
##              size = 1) +
##   geom_vline(xintercept = fixef(m_no_pool)[2, 1], color = col[2],
##              size = 1) +
##    geom_vline(xintercept = fixef(m_no_pool)[3, 1], color = col[3],
##               size = 1) +
##   scale_fill_viridis_d(begin = 0.2, end = 0.8) +
##   labs(x = expression("Personen-spezifische Mittelwerte"),
##        y = "Personen") +
##   theme(panel.grid   = element_blank(),
##         axis.ticks.y = element_blank(),
##         axis.text.y  = element_text(hjust = 0))


## ---- echo=FALSE----------------------------------------------
col <- viridis::viridis(3, begin = 0.2, end = 0.8)

person_effects %>% 
  ggplot(aes(mu, Person, fill = Person)) +
  stat_halfeye(alpha = 0.6) +
  geom_vline(xintercept = fixef(m_part_pool)[1, 1], color = "#ECCC87", 
             size = 1) +
  geom_vline(xintercept = fixef(m_no_pool)[1, 1], color = col[1], 
             size = 1) +
  geom_vline(xintercept = fixef(m_no_pool)[2, 1], color = col[2], 
             size = 1) +
   geom_vline(xintercept = fixef(m_no_pool)[3, 1], color = col[3], 
              size = 1) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  labs(x = expression("Personen-spezifische Mittelwerte"),
       y = "Personen") +
  theme(panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0)) 


## -------------------------------------------------------------
library(tidyverse)

intervention <- rep(c('treat', 'control'), each = 5)
pre <- c(20, 10, 60, 20, 10, 50, 10, 40, 20, 10)
post <- c(70, 50, 90, 60, 50, 20, 10, 30, 50, 10)


## -------------------------------------------------------------
dwide <- tibble(id = factor(1:10), 
            intervention, pre, post) %>% 
  mutate(diff = post - pre,
         id = as_factor(id), 
         intervention =  factor(intervention, levels = c("control", "treat")))


## -------------------------------------------------------------
dwide %>% 
  paged_table()


## -------------------------------------------------------------
d <- dwide %>% 
  select(-diff) %>% 
  pivot_longer(cols = pre:post, names_to = "time", values_to = "score") %>% 
  mutate(time = as_factor(time))

d %>% 
  paged_table()


## -------------------------------------------------------------
d %>% 
  summarize(id = n_distinct(id))


## -------------------------------------------------------------
d %>% 
  group_by(id, intervention) %>% 
  count() %>% 
  rmarkdown::paged_table()


## ----echo=FALSE-----------------------------------------------
countdown(minutes = 3)


## -------------------------------------------------------------
se <- function(x) sd(x)/sqrt(length(x))


## -------------------------------------------------------------
d %>% 
  group_by(intervention, time) %>% 
  summarise(mean = mean(score),
            sd = sd(score),
            se = se(score))


## -------------------------------------------------------------
dwide %>% 
  group_by(intervention) %>% 
  summarise(mean = mean(diff),
            sd = sd(diff),
            se = se(diff))


## ----echo=TRUE, eval=FALSE------------------------------------
## d %>%
##   ggplot(aes(time, score, color = intervention)) +
##   geom_line(aes(group = id), linetype = 1, size = 1) +
##   geom_point(size = 4) +
##   scale_color_viridis_d(end = 0.8) +
##   theme_bw()


## ----echo=FALSE-----------------------------------------------
d %>% 
  ggplot(aes(time, score, color = intervention)) +
  geom_line(aes(group = id), linetype = 1, size = 1) +
  geom_point(size = 4) +
  scale_color_viridis_d(end = 0.8) +
  theme_bw()


## -------------------------------------------------------------
t.test(diff ~ intervention,
       data = dwide, 
       var.equal = TRUE)


## -------------------------------------------------------------
library(lme4)
lme_model <- lmer(score ~ intervention * time + (1|id), 
                  data = d)


## -------------------------------------------------------------
lme_model %>% 
  sjPlot::tab_model()


## -------------------------------------------------------------
library(afex)
mixed(score ~ intervention * time + (1|id), 
      type = 3, method = "KR",
      data = d)


## -------------------------------------------------------------
mm <- model.matrix(~ intervention * time,
             data = d)
head(mm)


## -------------------------------------------------------------
library(brms)

priors <- get_prior(score ~ intervention*time,
          data = d)



## -------------------------------------------------------------
priors


## -------------------------------------------------------------
m2 <- brm(score ~ intervention*time + (1 | id),
          data = d,
          file =  "models/04-treat-time") 


## -------------------------------------------------------------
summary(m2)


## -------------------------------------------------------------
m2 %>% 
  mcmc_plot("b_")

