# Learn Bayesian multilevel modelling
Graduate School workshop 2021

[Online Skript](https://awellis.github.io/learnmultilevelmodels/)

## Workshop contents
This workshop is designed to provide a practical introduction to basic
and advanced multilevel models. Participants will learn how to fit models using
both maximum likelihood and Bayesian methods, although the focus will be on
Bayesian parameter estimation and model comparison. We will start with a short
introduction to multilevel modelling and to Bayesian statistics in general,
followed by an introduction to Stan, a probabilistic programming language for
fitting Bayesian models. We will then learn how to use the R package brms, which
provides a user-friendly interface to Stan. The package supports a wide range of
response distributions and modelling options, and allows us to fit multilevel
generalized linear models. Depending on participants' wishes, we will take a
closer look at modelling various types of data, such as choices, response times,
ordinal or longitudinal data.

Specific topics include:

* Bayesian inference: an introduction

* Bayesian parameter estimation
  
* Model comparison & hypothesis testing
    + Bayes factors
    + Out-of-sample predictive accuracy (LOO)
  
* Specifying multilevel generalized linear models
  
* Understanding statistical models through data simulation
  
* A principled Bayesian workflow for data analysis

We will focus on learning new topics during the morning sessions, and
participants should work through the assignments during the afternoon sessions.
Participants are also encouraged to bring their own datasets.

## Prerequisites

Basic knowledge of regression models and R is a necessity. I strongly recommend
that you prepare for the workshop by working through this online script:
[https://methodenlehre.github.io/intro-to-rstats](https://methodenlehre.github.io/intro-to-rstats).
Previous exposure to multilevel models and longitudinal models would be helpful,
but is not strictly necessary. Knowledge of Bayesian statistics is not required.

## Software

We will be using R and Rstudio, as well a variety of R packages. It is
advisable to ensure that you have a working R installation before the workshop
starts, and that you install the two R packages `rstan` and `brms`. Detailed
instructions for installing these packages on all platforms can be found at
[https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
and [https://paul-buerkner.github.io/brms](https://paul-buerkner.github.io/brms).
