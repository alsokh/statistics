g1 <- seq(1, 57, 2)
g2 <- seq(20, 75, 5)
sapply(list(g1, g2), mean)
df <- data.frame(age = c(g1, g2), group = c(rep("group1", length(g1)), rep("group2", length(g2))))
model <- glm(age ~ group, df, family = gaussian)
summary(model)
p <- summary(model)$coefficients[,4][[2]]
t <- t.test(g1, g2, var.equal = T)
t
all.equal(t$p.value, p)
t$p.value == p
# these are not equal due to floating-point precision
