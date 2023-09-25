library(tidyverse)
# normal distribution
a <- rnorm(100, mean = 1, sd = 2)
summary(a)

b <- rnorm(250, mean = 0, sd = 1)
psych::describe(b)

# cumulative distribution probability
c <- pnorm(1.96, mean = 0, sd = 1)
paste(100 * round(1 - c, 3), "%")

# quantiles and percentiles
q <- qnorm(0.025, mean = 0, sd = 1)
round(-q, 3)

# probability density
d <- dnorm(1.96, mean = 0, sd = 1)
d_neg <- dnorm(-1.96, mean = 0, sd = 1)

d2 <- dnorm(seq(-1.96, 1.96, by = .01), log = F)
plot(d2)
# fit to normal
library(MASS)

data <- rnorm(1000, mean = 126, sd = 2)
fit <- fitdistr(data, "normal")

# visualize
hist(data, probability = TRUE, main = "Fitted Normal Distribution", breaks = 20)
curve(dnorm(1, fit$estimate[1], fit$estimate[2]), add = TRUE)

# lets do the same with ggplot
plt <- ggplot(data = data.frame(data), aes(x = data))
plt + geom_histogram(binwidth = 0.1, fill = "lightblue", aes(y = ..density..)) + 
  stat_function(fun = dnorm, args = list(mean = mean(data), sd = sd(data)), color = 'blue') +
  theme_classic()

# or
plt + geom_histogram(binwidth = 0.1, fill = "lightblue", aes(y = ..density..)) + 
  geom_density(stat = "density")
