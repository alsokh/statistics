# a guide on hierarchial mixed models
library(readxl); library(tidyr); library(tidyverse); library(lme4); library(nlme); library(lmerTest)

df_ol <- read_csv(url("https://raw.githubusercontent.com/methodenlehre/data/master/salary-data.csv"))
head(df_ol)
# factorize
df_ol <- df_ol %>% mutate(
  firma = as.factor(firma),
  sector = as.factor(sector)
)
# get summary
summary(df_ol)

# 1. only-intercept one-level mixed-model --> in fact it is two-level model but 
intercept.model <- lmer(formula =  salary ~ 1 + (1 | firma), data = df_ol, REML = T)
summary(intercept.model)
ranef(intercept.model)
ranova(intercept.model)
plot(intercept.model)
predicted.salary.ol <- predict(intercept.model)

ggplot(df, aes(x = experience, y = salary)) +
  geom_point(alpha = 0.5, aes(color = firma)) +  # Scatter plot of salary vs. experience
  geom_smooth(method = "lm", se = TRUE, aes(y = predicted.salary.ol, color = firma)) +  # Smoothed line with confidence interval
  labs(title = "Salary vs. Experience",
       x = "Experience (Years)",
       y = "Salary ($)") + theme_minimal()


# 2. the null model
ggplot(df, aes(x = experience, y = salary)) +
  geom_point(alpha = 0.5, aes(color = firma)) +  # Scatter plot of salary vs. experience
  geom_smooth(method = "lm", se = TRUE, aes(y = predict(lm(salary ~ 1, data = df_ol)))) +  # Smoothed line with confidence interval
  labs(title = "Salary vs. Experience",
       x = "Experience (Years)",
       y = "Salary ($)") + theme_minimal()
# ploting random effects
random_effects <- ranef(intercept.model)$firma  # Assuming 'firma' is the random effect level

# Add random effects to the data frame
df$random_effect <- random_effects[match(df$firma, rownames(random_effects)),]

# Plot with random effects
# ggplot(df, aes(x = experience, y = salary)) +
#   geom_point(aes(color = firma), alpha = 0.5) +  # Plot actual data points
#   geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Smoothed line for observed data
#   geom_point(aes(y = random_effect), color = "red", size = 1) +  # Random effects
#   labs(title = "Observed Salaries with Random Effects",
#        x = "Experience (Years)",
#        y = "Salary ($)") +
#   theme_minimal()

# 3. the following is the one-level fixed model
summary(lm(salary ~ firma, data = df_ol))

## 4. random-intercept one-level mixed-model
random.intercept <- lmer(salary ~ experience + (1 | firma), data = df_ol, REML = T)
random.intercept.pred <- predict(random.intercept)
summary(random.intercept)
ranef(random.intercept)

ggplot(df, aes(x = experience, y = salary)) +
  geom_point(alpha = 0.5, aes(color = firma)) +  # Scatter plot of salary vs. experience
  geom_smooth(method = "lm", se = TRUE, aes(y = random.intercept.pred, color = firma)) +  # Smoothed line with confidence interval
  labs(title = "Salary vs. Experience",
       x = "Experience (Years)",
       y = "Salary ($)") + theme_minimal()


# 5. random-coefficient mixed-model --> three-level
random.coef <- lmer(salary ~ experience + (experience|firma), data = df_ol, REML = T)
random.coef.pred <- predict(random.coef)
ranef(random.coef)

ggplot(df, aes(x = experience, y = salary)) +
  geom_point(alpha = 0.5, aes(color = firma)) +  # Scatter plot of salary vs. experience
  geom_smooth(method = "lm", se = TRUE, aes(y = random.coef.pred, color = firma)) +  # Smoothed line with confidence interval
  labs(title = "Salary vs. Experience",
       x = "Experience (Years)",
       y = "Salary ($)") + theme_minimal()

# To compare the model with random intercept
anova(random.coef, random.intercept, refit = F)
chi <- anova(random.coef, random.intercept, refit = F)$Chisq[2]
#The exact computation of the p value based on the mixture distribution is done by averaging the p values of the two distributions with df=1 and df=2

0.5 * pchisq(chi, 1, lower.tail = FALSE) + 0.5 * pchisq(chi, 2, lower.tail = FALSE)
