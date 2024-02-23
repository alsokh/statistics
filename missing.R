### there are multiple methods
# three types of missing: 1. MCAR 2. MAR 3. MNAR
# 1. List-wise deletion (complete-case analysis): just deleting the rows with missing data
# 2. Removing variables: just removing variables with considerable missnig
# 3. Mean/Median substitution: take mean/median and substitute the missing datapoints
#   If categorical an option is to add a bin as missing  
# 4. LOCF and BOCF functions using fill function
# 5. linear imputation using imputeTS package and na_interpolation function
# 6. Multiple imputation

# we will go to implement multiple imputation using mice package
library(tidyverse)
library(mice)
library(imputeTS)

data("nhanes")
df <- nhanes

# 1. explore
head(df)
all(!is.na(c(T, F, NA, T, F)))
sum(is.na(df))
summary(is.na(df))

# 2. omit
omit.df <- na.omit(df)
sum(is.na(omit.df))
head(df)
lm(chl ~ bmi, df, na.action = na.omit)

# 3. mean/median imputation
mean.missing <- apply(df, 2, mean, na.rm = TRUE)
median.missing <- apply(df, 2, mean, na.rm = TRUE)
# the 2 in above function shows that it should be applied to the column
sum(is.na(mean.missing))
sum(is.na(median.missing))

# 4. LOCF/BOCF
# Last observation carried forward (LOCF)
locf <- df %>% fill(bmi, .direction = "up")
sum(is.na(locf$bmi))
# baseline observation carried forward (BOCF)
bocf <- df %>% fill(bmi, .direction = "down")
sum(is.na(bocf$bmi))

# 5. linear interpolation
line.imp <- na_interpolation(df, option = "linear")#, maxgap = 3)

# 6. multiple imputation
# imputing missing values with predictive mean matching method (pmm)
df.imp <- mice(df, m = 5, method = "norm.predict")

# to see the third dataset created with imputation 
head(complete(data = df.imp, 3))

# showing imputed values
df.imp$imp
imp.complete <- complete(df.imp, action = "long", include = T)
# Now, the .imp variable identifies which imputation each belongs to.
table(imp.complete$.imp)
# `cci` returns logical whether its input is complete at each observation. 
imp.complete$bmi.NA <- cci(df$bmi)
head(imp.complete[, c("bmi", "bmi.NA")])
# let's plot
ggplot(imp.complete,
       aes(x = .imp, y = bmi, color = bmi.NA)) + 
  geom_jitter(show.legend = T,
              width = .1)
md.pattern(imp.complete)
stripplot(df.imp)
densityplot(df.imp)

# to run tests
with(df.imp, mean(bmi))
with(df.imp, t.test(bmi ~ hyp))
with(df.imp, lm(bmi ~ age + chl))

# to pool results
mod1 <- with(df.imp, lm(bmi ~ age + chl))
pool(mod1)
round(summary(pool(mod1)), 3)
pool.r.squared(mod1)

