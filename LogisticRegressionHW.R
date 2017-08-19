## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.


# Load the National Health Interview Survey data:

Nath <- readRDS("/Users/hb/Downloads/logistic_regression/dataSets/NatHealth2011.rds")

# Subset data isolating only everwrk, r_maritl and age_p

NH = subset(Nath, select = c("everwrk, age_p, r_maritl"))

# Let's check out the structure of the data

str(NH)  

# Remove NA and unknown values from dependent variable everwrk

NH$everwrk <- factor(NH$everwrk, levels = c("1 Yes", "2 No"))

# Use droplevels on r_maritl

NH$r_maritl <- factor(NH$r_maritl)
NH$r_maritl <- droplevels(NH$r_maritl)

# Build model and look at summary

mod1 <- glm(everwrk ~ age_p + r_maritl, data = NH, family = binomial)      

# Make coefficients easier to interpret

mod1.tab <- coef(summary(mod1))
mod1.tab[, "Estimate"] <- exp(coef(mod1))
mod1.tab

# Predict probability for working for each level of marital status

library(effects)
plot(allEffects(mod1))
