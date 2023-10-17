
#setwd('C:/Users')

library(tidyverse)
library(VineCopula)
library(dplyr)

library(rvinecopulib)

# Load Data for the different stations
# NBO_data_Emp.csv, KGL_data_Emp.csv, CT_data_Emp.csv
data = read_csv("KGL_data_Emp.csv",show_col_types = FALSE)

# Fit the vine copula to the data
fit <- vinecop(data,var_types = rep("c", NCOL(data)), family_set = c("gaussian", "t","clayton", "frank", "gumbel"), structure = NA,
  par_method = "mle", nonpar_method = "constant",  mult = 1, selcrit = "bic",  weights = numeric(), psi0 = 0.9,
  presel = TRUE, trunc_lvl = Inf, tree_crit = "tau", threshold = 0, keep_data = FALSE, show_trace = FALSE,
  cores = 1)

# R_vine
print(fit)
View(summary(fit))
contour(fit)
write.csv(fit)

plot(fit, tree = c(1:7), edge_labels = "pair")

## D-vine
fit1 <- vinecop(data, structure = dvine_structure(1:8), family = c("gaussian", "t","clayton", "frank", "gumbel"))
print(fit1)
contour(fit1, cex = 3.5)
View(summary(fit1))
plot(fit1, tree = c(1:7), edge_labels = "pair")

## C-vine
fit2 <- vinecop(data, structure = cvine_structure(1:8), family = c("gaussian", "t","clayton", "frank", "gumbel"))
print(fit2)
contour(fit2)
View(summary(fit2))
plot(fit2, tree = c(1:7), edge_labels = "pair")


## r-vine
##print(fit3)
#contour(fit3)

#summary(fit3)
#plot(fit3, tree = c(1:6), edge_labels = "pair")
