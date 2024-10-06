library(tidyverse); library(ggdist); library(tidyquant)

# make a sample database
set.seed(43)
x <- data.table::data.table(
  label = rep(c("operation", "conservative"), each = 200),
  satisfaction = c(rnorm(200, mean = 7), rnorm(200, mean = 5.5))
)

RC.plt <- ggplot(x, aes(x = label, y = satisfaction, fill = label))
# simple box plot
RC.plt + 
  geom_boxplot()

# violin
RC.plt + 
  geom_violin(trim = T)

# violin + box
RC.plt + 
  geom_violin(alpha = 0.3) +
  geom_boxplot()
  
# let's plot violin
# let's plot RC
  RC.plt +
  # half violin
  ggdist::stat_halfeye(adjust = 0.5, width = 0.6, .width = 0, 
                       justification = -0.3, point_color = NA, side = "right") + 
  # box
  geom_boxplot(width = 0.12, outlier.shape = NA) + 
  # data points
  ggdist::stat_dots(justification = 1.2, binwidth = 0.07, side = "left", size = .1) +
  # OR geom_jitter(width = 0.15, size = 1, alpha = 0.5, side = "left") +
  theme_minimal() +
  labs(title = "Rain Cloud Plot", x = "Group", y = "Measure") 
  

show(RC.plt)

