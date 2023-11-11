library(tidyverse)
library(car)
library(psych)
library(Hmisc)
library(GGally)
library(corpcor)
library(mctest)
library(ppcor)

data("States")
df <- States
model <- lm(percent ~ pop + SATV + SATM + dollars + pay, df)
par(mfrow=c(2,3))
GGally::ggpairs(df[,2:7])
plot(model, 1:6, add.smooth = T)

corpcor::cor2pcor(cov(df[,2:7]))
#Farrar – Glauber Test
mctest::omcdiag(model)
mctest::imcdiag(model)
pcor(df[,2:7])
vif(model)
