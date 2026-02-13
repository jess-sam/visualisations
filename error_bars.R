
library(ggplot2)
library(gridExtra)
library(dplyr)

df_summary <- mtcars %>% 
  group_by(cyl) %>% 
  summarize(
    mean_mpg = mean(mpg),
    sd_mpg = sd(mpg),
    ymin = mean_mpg - sd_mpg, # Lower limit
    ymax = mean_mpg + sd_mpg  # Upper limit
  )



df_error <- data.frame(n_rows = rep(NA, 3), 
                       mean_val = rep(NA, 3), 
                       sd_val = rep(NA, 3),
                       y_min = rep(NA, 3),
                       y_max = rep(NA, 3))
df_error

############################################################
# Visualisation ----

mu <- ggplot(df_summary, aes(x = factor(cyl), y = mean_mpg)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  labs(title = expression("Model 1 - S1;  " ~ hat(mu)), 
       x = "Number of rows (n)", 
       y = "Value" )+
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5))

a1 <- ggplot(df_summary, aes(x = factor(cyl), y = mean_mpg)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  labs(title = expression("Model 1 - S1;  " ~ hat(alpha)[1]), 
       x = "Number of rows (n)", 
       y = "Value" )+
  geom_hline(yintercept = -2, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5))

a2 <- ggplot(df_summary, aes(x = factor(cyl), y = mean_mpg)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  labs(title = expression("Model 1 - S1;  " ~ hat(alpha)[2]), 
       x = "Number of rows (n)", 
       y = "Value" )+
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5))

b1 <- ggplot(df_summary, aes(x = factor(cyl), y = mean_mpg)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  labs(title = expression("Model 1 - S1;  " ~ hat(beta)[1]), 
       x = "Number of rows (n)", 
       y = "Value" )+
  geom_hline(yintercept = -2, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5))

b2 <- ggplot(df_summary, aes(x = factor(cyl), y = mean_mpg)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  labs(title = expression("Model 1 - S1;  " ~ hat(beta)[2]), 
       x = "Number of rows (n)", 
       y = "Value" )+
  geom_hline(yintercept = -1.5, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5))

b3 <- ggplot(df_summary, aes(x = factor(cyl), y = mean_mpg)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  labs(title = expression("Model 1 - S1;  " ~ hat(beta)[3]), 
       x = "Number of rows (n)", 
       y = "Value" )+
  geom_hline(yintercept = 0.3, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5))

b4 <- ggplot(df_summary, aes(x = factor(cyl), y = mean_mpg)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  labs(title = expression("Model 1 - S1;  " ~ hat(beta)[4]), 
       x = "Number of rows (n)", 
       y = "Value" )+
  geom_hline(yintercept = 1, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5))

p1 <- ggplot(df_summary, aes(x = factor(cyl), y = mean_mpg)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  labs(title = expression("Model 1 - S1;  " ~ hat(pi)[1]), 
       x = "Number of rows (n)", 
       y = "Value" )+
  geom_hline(yintercept = 0.333, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(df_summary, aes(x = factor(cyl), y = mean_mpg)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  labs(title = expression("Model 1 - S1; " ~ hat(pi)[2]), 
       x = "Number of rows (n)", 
       y = "Value" )+
  geom_hline(yintercept = 0.333, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5))


grid.arrange(mu, a1, a2, b1, b2, b3, b4, p1, p2, ncol = 3)
