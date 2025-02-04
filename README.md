Bayesian Causal Inference with rstanarm

Overview

This repository demonstrates Bayesian causal inference using the rstanarm package in R. The analysis follows examples from the Lalonde (1986) NSW study and an Electric Company education intervention, using Bayesian regression to estimate treatment effects.

Dependencies

Ensure you have the following R packages installed:

install.packages("rstanarm")
install.packages("ggplot2")
install.packages("dplyr")

Lalonde NSW Study Example

This section fits a Bayesian regression model to estimate the average treatment effect (ATE) using a simple linear model.

Steps:

Define the dataset:

W = c(0,1,0,1,0,1)
Y = c(0,9.9,12.4,3.6,0,24.9)
nsw = data.frame(cbind(W,Y))

Fit a Bayesian regression model:

library(rstanarm)
mod1 <- stan_glm(Y ~ W, data = nsw)
print(mod1)

Compute mean differences:

mean(Y[W==1]) - mean(Y[W==0])

Impute missing potential outcomes:

nsw0 = nsw; nsw0$W = 0
nsw1 = nsw; nsw1$W = 1
predW0 = predict(mod1, newdata = nsw0)
predW1 = predict(mod1, newdata = nsw1)

Estimate PATE and SATE using posterior simulations:

predW0_pate = posterior_linpred(mod1, newdata = nsw0)
predW1_pate = posterior_linpred(mod1, newdata = nsw1)

Analyze results:

hist(predW0_sate[, 1], main = "Dist. of Predicted Potential Outcomes for Unit 1")

Electric Company Example

This section extends the methodology to a real-world educational intervention dataset.

Steps:

Load and preprocess data:

electric_wide <- read.table("electric_wide.txt", header=TRUE)
electric_wide_4 = subset(electric_wide, grade == 4)

Fit a Bayesian regression model:

fit_4 <- stan_glm(post_test ~ treatment + pre_test + treatment * pre_test, data=electric_wide_4)

Estimate potential outcomes and compute effects:

predt0_sate = posterior_predict(fit_4, newdata = dat_t0)
predt1_sate = posterior_predict(fit_4, newdata = dat_t1)

Compute mean effect and summarize results:

mean_effect <- rowMeans(effect)
print(paste("Mean =", round(mean(mean_effect), 2), "SD =", round(sd(mean_effect), 2)))

Conclusion

This repository illustrates Bayesian causal inference using rstanarm for treatment effect estimation. The examples demonstrate how Bayesian regression can be used to estimate and impute potential outcomes, leading to robust causal estimates.
