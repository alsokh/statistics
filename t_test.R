library(tidyverse)
# descriptive methods
library(pastecs)
library(Hmisc)
# robust methods 
library(WRS2)

df1 <- read.delim2("data_files/SpiderLong.dat", header = T)
df1 <- df1 %>% mutate(id = row_number()) 
df2 <- unstack(df1, Anxiety ~ Group)
df3 <- read.delim2("data_files/SpiderWide.dat")

# to draw error bar for independent measure
plt1 <- ggplot(data = df1, aes(x = Group, y = Anxiety))
plt2 <- plt1 + stat_summary(fun = mean, aes(fill = Group), geom = "bar", alpha = 0.4) + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "lightblue") + 
  theme(panel.background = element_rect(fill = "black", color = "gray"), 
        panel.grid = element_line(color = "darkgray", size = .1)) + 
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10))
plt2
# to draw error bar for repeated measure
# to calculate mean for each participant
df3$mean <- (df3$picture + df3$real)/2
# calculate grand mean
grandmean <- mean(c(df3$picture, df3$real))
# calculate adjusted mean
df3$adj <- grandmean - df3$mean
# adjust values 
df1$adj <- df1$Anxiety + c(df3$adj, df3$adj)

plt1 <- ggplot(data = df1, aes(x = Group, y = adj))
plt3 <- plt1 + stat_summary(fun = mean, aes(fill = Group), geom = "bar", alpha = 0.4) + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "lightblue") + 
  theme(panel.background = element_rect(fill = "black", color = "gray"), 
        panel.grid = element_line(color = "darkgray", size = .1)) + 
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10))
plt3
library(cowplot)
cowplot::plot_grid(plt2, plt3)

# this function can do adjustment for repeated measures
#adjust the repeated measures data
rmMeanAdjust<-function(dataframe)
{
  varNames<-names(dataframe)
  
  pMean<-(dataframe[,1] + dataframe[,2])/2
  grandmean<-mean(c(dataframe[,1], dataframe[,2]))
  adj<-grandmean-pMean
  varA_adj<-dataframe[,1] + adj
  varB_adj<-dataframe[,2] + adj
  
  output<-data.frame(varA_adj, varB_adj)
  names(output)<-c(paste(varNames[1], "adj", sep = "_"), paste(varNames[2], "_adj", sep = "_"))
  return(output)
}

mean(c(38.5, 43.5, 41.0, 36.0, 36.0, 33.5, 46.0, 38.5, 43.5, 41.0, 33.5, 49.0))
mean(c(48.5, 43.5, 46.0, 51.0, 51.0, 53.5, 41.0, 48.5, 43.5, 46.0, 53.5, 38.0))
df1 %>% 
  group_by(Group) %>% 
  dplyr::summarize(mean = mean(Anxiety, na.rm = T), n = n())
aggregate(Anxiety ~ Group, df1, FUN = function(x) list(mean = mean(x), sd = sd(x), sum = sum(x)))
df1$Group <- factor(df1$Group)
model1 <- lm(data = df1, formula = Anxiety ~ Group)
model2 <- glm(data = df1, formula = Group ~ Anxiety, family = "binomial" )
summary(model1)
model2

t.test(df3$picture, df3$real)

# the following is usefull to get p-value from effect size in t-distribution
pt()

spid.long <- data.frame(Group = df1$Group, Anxiety = df1$Anxiety)

# to get the summary statistics
by(spid.long$df1.Anxiety, spid.long$df1.Group, stat.desc, basic = FALSE, norm = TRUE)
ind.t.test <- t.test(Anxiety ~ Group, data = spid.long)
ind.t.test
# robust model for regression to adjust for heteroscedasticity to be similar to the welch method
# sandwich::sandwich()

## robust methods
yuen(Anxiety ~ Group, data = spid.long)
# if want to only trim 10%
yuen(Anxiety ~ Group, data = spid.long, tr = 0.1)
# bootstrap
yuenbt(Anxiety ~ Group, data = spid.long)
# M-estimator
pb2gen(Anxiety ~ Group, data = spid.long)

# convert t value to r
t_to_r <- function(t, df){
  return(sqrt(t^2/(t^2+df)))
}

t_to_r(ind.t.test$statistic[[1]], ind.t.test$parameter[[1]])
# or 
psych::t2r(ind.t.test$statistic[[1]], ind.t.test$parameter[[1]])

# dependent t test
dep.t.test <- t.test(Anxiety ~ Group, data = spid.long, paired = T)
dep.t.test
# robust
yuend(Anxiety ~ Group, data = spid.long)
# if want to only trim 10%
yuenbt(Anxiety ~ Group, data = spid.long, tr = 0.1, paired = T)
# M-estimator
pb2gen(Anxiety ~ Group, data = spid.long, paired = T)

