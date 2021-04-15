# Learn Bayesian multilevel modelling
Graduate School workshop 2021

[Online Skript](https://kogpsy.github.io/learnmultilevelmodels/)



This workshop is designed to provide a practical introduction to basic
and advanced multilevel models. Participants will learn how to fit models using
both maximum likelihood and Bayesian methods, although the focus will be on
Bayesian parameter estimation and model comparison. We will start with a short
introduction to multilevel modeling and to Bayesian statistics in general,
followed by an introduction to Stan, a probabilistic programming language for
fitting Bayesian models. We will then learn how to use the R package brms, which
provides a user-friendly interface to Stan. The package supports a wide range of
response distributions and modeling options, and allows us to fit multilevel
generalized linear models.

Specific topics include:
- Understanding 


Participants learn how to train and assess predictive models with several common
machine learning algorithms, as well as how to do feature engineering to improve
the predictive accuracy of their models. We focus on teaching intuitive
explanations of the models and best practices for predictive modeling. Along the
way, we introduce several core tidymodels packages, which provide a grammar for
modeling that makes it easy to the right thing, and harder to accidentally do
the wrong thing.




Soecific topics include:
* understanding statistical models through data simulation
* Bayesian inference: an introduction
    * Bayesian parameter estimation
    * model comparison
* specifying multilevel generalized linear models
* estimating parameters
    * fitting models
        * least squares
        * maximum likelihood
        * Bayesian inference
* comparing models
    * Bayes factors
    * out of sample predictive accuracy (LOO)






## 1
The talk will be about Bayesian multilevel models and their implementation in R
using the package brms. We will start with a short introduction to multilevel
modeling and to Bayesian statistics in general followed by an introduction to
Stan, which is a flexible language for fitting open-ended Bayesian models. We
will then explain how to access Stan using the standard R formula syntax via the
brms package. The package supports a wide range of response distributions and
modeling options such as splines, autocorrelation, and censoring all in a
multilevel context. A lot of post-processing and plotting methods are
implemented as well. Some examples from Psychology and Medicine will be
discussed.




## HW

The goal of this course is to help you transition from writing individual functions to writing families of functions that work well together and fit seamlessly into the tidyverse. About half the course will be spent on the mechanics of R packages, the primary tool for distributing code, data, and documentation. You'll also learn how to take dependencies on other packages (including when to do so), and the basics of tidy evaluation that you need to know as a package author. The remainder of the course will focus on function "interfaces", i.e. how a function exposes itself to the world. Thinking about and critiquing function interfaces is an important skill that helps you write functions that "just work", so that people can pick them up and use them without struggle or unpleasant surprises. You'll analyse a bunch of base R and tidyverse functions, learn the basics of S3, and how to generate effective errors.

Evenings aboard R/V Garvin will include informal practice and discussion sessions allowing participants to maximize their learning and one-on-one time with a world renowned statistician and R developer. 

This style of learning is ideal for mastering complex skills, giving
participants plenty of time to process information, practice new techniques, and
ask questions. Students are encouraged to bring their own data sets and will
have opportunities to work on their own projects and troubleshoot with the
instructor.


## 3

Prerequisites (knowledge of topic)
A strong background in linear regression is a necessity. Background exposure to maximum likelihood models like logistic regression would be very helpful but is not strictly necessary. Some previous background exposure to multilevel, longitudinal, panel, or mixed effects models would be very helpful but is not necessary. People without a background in multilevel models should (time permitting) order a copy of either Multilevel Analysis: Techniques and applications by Joop Hox, Mirjam Moerbeek, and Rens van de Schoot (2017) or Multilevel Analysis by Tom Snijders and Roel Bosker (2011) and attempt to read the early chapters ahead of time. Again, this is not requirement to attend the class but will help you to absorb the material in lecture much more easily.

Hardware
A laptop—preferably a PC as that is what I use. Please insure that you have administrator access on your machine or that someone who does can help you install needed software prior to the workshop. Without doing this you will be unable to follow along with the labs in class.

Software
The course will use R and RStudio which are both free and open source. We will mainly be using the R packages lme4 and brms as well as some extensions. Please have both programs and the specific packages installed on your machine before you arrive. Note that brms will require rtools to be installed on your machine and that requires administrator access. An install script for all needed packages will be provided for registered students.

Note: if you are primarily a Stata user then I can provide you with some code (for version 16) to do many of the things covered in the course. However, we will not have time to go through it in class.

Course content
This course is designed to provide a practical guide to fitting advanced multilevel models. It is pitched for people from widely different backgrounds, so a significant amount of attention is paid to translating concepts across fields. My approach to the class combines work from econometrics, statistics/biostatistics, and psychometrics. The class is structured using a maximum likelihood framework with practical applied Bayesian extensions on different topics. R packages are selected specifically to make the transition from MLE to Bayesian multilevel models as straightforward and seamless as possible. This is a very applied course with annotated code provided and time in class for lab work. However, it is necessary to spend a class time working through theory and interpretation as well as the logic of mixed effects models.

Specific topics include:
•    Random intercept and random slope models
•    Cross-classified and multiple membership models
•    Generalized linear mixed models
•    Special topics chosen by students

The last day of class will have material chosen by the students from a predetermined list of possible topics. In order for your topic to be considered you must respond to the course survey by the end of lunch on Monday so that we can discuss updates during the afternoon.

While you will not be an expert in multilevel modeling after one week—this takes years of practice—you will have the tools to go home and fit many advanced models in your own work. By the end of the week you will have practical experience fitting both Bayesian and likelihood versions of basic and advanced multilevel models with RStudio. You will be able to produce diagnostics and results and hopefully interpret them correctly. If you use the models in your own work and read the supplementary materials for the course, you will end up with a very high level of knowledge in multilevel modeling over time. While we do cover Bayesian extensions for multilevel models, this course is not a substitute for a fully-fledged course on Bayesian data analysis. However, it will leave you very well prepared for such a course or for reading a Bayesian analysis textbook