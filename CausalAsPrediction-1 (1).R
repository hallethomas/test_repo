library(rstanarm)

#' Enter the example data of n=6 units from the Lalond (1986)
#' NSW study
W = c(0,1,0,1,0,1)
Y = c(0,9.9,12.4,3.6,0,24.9)
nsw = data.frame(cbind(W,Y))

#' Fit a plain linear model for the ATE
mod1 <- stan_glm(Y~W, data=nsw)
sims <- as.matrix(mod1)
print(mod1)

#' Same as just calculating the mean difference between gorups
#' (except with this small sample the default priors in stan_glm make
#' a little bit of difference)
mean(Y[W==1]) - mean(Y[W==0])

#' Create a data set that has every unit with W=0
nsw0 = nsw
nsw0$W = 0
#' And also for W=1
nsw1 = nsw
nsw1$W = 1

#' generate a single imputation for each unit's missing 
#' potential outcome
predW0 = predict(mod1, newdata=nsw0)
predW1 = predict(mod1, newdata=nsw1)

Y1 = rep(NA, 6)
Y0 = rep(NA, 6)
# Observed potential outcomes
Y1[W==1] = Y[W==1]
Y0[W==0] = Y[W==0]
# Missing potential outcomes
Y1[W==0] = predW1[W==0]
Y0[W==1] = predW0[W==1]

mean(Y1 - Y0)
print(mod1)

#' Now 'multiply impute' accounting for uncertainty in the linear
#' predictor and uncertainty around a new observation
predW0_pate = posterior_linpred(mod1, newdata=nsw0)
predW1_pate = posterior_linpred(mod1, newdata=nsw1) 

predW0_sate = posterior_predict(mod1, newdata=nsw0)
predW1_sate = posterior_predict(mod1, newdata=nsw1)

#' Each of the above objects is a matrix of predictions
head(predW0_sate)

#' Can even look at the distribution of predictions
#' for a single unit
hist(predW0_sate[, 1], main = "Dist. of Predicted Potential Outcomes for Unit 1")

SATE = rep(NA, dim(predW0_sate)[[1]])
PATE = SATE
for (i in 1:length(SATE)){
  # Observed potential outcomes
  Y1[W==1] = Y[W==1]
  Y0[W==0] = Y[W==0]
  # Missing potential outcomes
  Y1[W==0] = predW1_sate[i,W==0]
  Y0[W==1] = predW0_sate[i,W==1]
  
  SATE[i] = mean(Y1 - Y0)
  
  PATE[i] = mean(predW1_pate[i,] - predW0_pate[i,])
}
mean(SATE)
mean(PATE)
#' Of course, because this is just a linear regression model, these results
#' match what we get with the posterior simulations of the "W" coefficient
mean(sims[, "W"])

sd(SATE)
sd(PATE)
#' Of course, because this is just a linear regression model, the results
#' for PATE match what we get with the posterior simulations of the 
#' "W" coefficient
sd(sims[, "W"])

hist(SATE)
hist(PATE)



#### - Now do the same with the Electric Company Example
#' #### Load data
electric_wide <- read.table("electric_wide.txt", header=TRUE)
# Subset only to grade 4 for illustration
electric_wide_4 = subset(electric_wide, grade==4)
head(electric_wide_4)
attach(electric_wide_4)

# Organize data
post_test <- c(treated_posttest, control_posttest)
pre_test <- c(treated_pretest, control_pretest)
grade <- rep(electric_wide_4$grade, 2)
treatment <- rep(c(1,0), rep(length(treated_posttest),2))
supp <- rep(NA, length(treatment))
n_pairs <- nrow(electric_wide_4)
pair_id <- rep(1:n_pairs, 2)
supp[treatment==1] <- ifelse(supplement=="Supplement", 1, 0)
n <- length(post_test)
electric <- data.frame(post_test, pre_test, grade, treatment, supp, pair_id)
head(electric)

fit_4 <- stan_glm(post_test ~ treatment + pre_test + treatment * pre_test,
                  data=electric, refresh = 0)
print(fit_4)
sim_4 <- as.matrix(fit_4)
head(sim_4)

#' Create a data set that has every unit with treatment=0
dat_t0 = electric
dat_t0$treatment = 0
#' Create a data set that has every unit with treatment=1
dat_t1 = electric
dat_t1$treatment = 1


#' Now 'multiply impute' accounting for uncertainty in the linear
#' predictor and uncertainty around a new observation
predt0_pate = posterior_linpred(fit_4, newdata=dat_t0)
predt1_pate = posterior_linpred(fit_4, newdata=dat_t1) 

predt0_sate = posterior_predict(fit_4, newdata=dat_t0)
predt1_sate = posterior_predict(fit_4, newdata=dat_t1)


SATE = rep(NA, dim(predt0_sate)[[1]])
PATE = SATE
Y0 = rep(NA, length(treatment))
Y1 = Y0
for (i in 1:length(SATE)){
  # Observed potential outcomes
  Y1[treatment==1] = post_test[treatment==1]
  Y0[treatment==0] = post_test[treatment==0]
  # Missing potential outcomes
  Y1[treatment==0] = predt1_sate[i,treatment==0]
  Y0[treatment==1] = predt0_sate[i,treatment==1]
  
  SATE[i] = mean(Y1 - Y0)
  
  PATE[i] = mean(predt1_pate[i,] - predt0_pate[i,])
}
mean(SATE)
mean(PATE)


#' Now let's calculate using the simulation output directly for comparison
#' (since this is linear regression this should give us the same thing)
#' #### Mean effect
n_sims <- dim(sim_4)[[1]]
effect <- array(NA, c(n_sims, sum(grade==4)))
for (i in 1:n_sims)
  effect[i,] <- sim_4[i,2] + sim_4[i,4]*pre_test[grade==4]

# This gives a 1000x42 matrix of 
# 1000 simulations of the effect for each of the 42 4th grade units
head(effect) 

# 1000 simulations of the mean effect across all of the 4 grade units
mean_effect <- rowMeans(effect) # average effect in the 4th grade
mean_effect[1:10] 

# print the posterior mean and SD of the average effect in 4th grade
print(paste("Mean =",round(mean(mean_effect), 2), "SD =", round(sd(mean_effect), 2)))

mean(PATE)
sd(PATE)

mean(SATE)
sd(SATE)

hist(PATE)
hist(SATE)
