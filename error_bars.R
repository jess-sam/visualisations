
library(ggplot2)
library(gridExtra)
library(dplyr)

alpha_vec <- c(-2, 0)
beta_vec <- c(-2, -1.5, 0.3, 1)
mu <- 0

true_params <- c(mu, alpha_vec, beta_vec)

df_summary <- mtcars %>% 
  group_by(cyl) %>% 
  summarize(
    mean_mpg = mean(mpg),
    sd_mpg = sd(mpg),
    ymin = mean_mpg - sd_mpg, # Lower limit
    ymax = mean_mpg + sd_mpg  # Upper limit
  )

mu_df_n1000[1,]

make_df <- function(param, scen){
  
  if(param == 1){
    expr_txt <- substitute(hat(mu))
  }
  if(param == 2){
    expr_txt <- substitute(hat(alpha)[1])
  }
  if(param == 3){
    expr_txt <- substitute(hat(alpha)[2])
  }
  if(param == 4){
    expr_txt <- substitute(hat(beta)[1])
  }
  if(param == 5){
    expr_txt <- substitute(hat(beta)[2])
  }
  if(param == 6){
    expr_txt <- substitute(hat(beta)[3])
  }
  if(param == 7){
    expr_txt <- substitute(hat(beta)[4])
  }
  if(param == 8){
    expr_txt <- substitute(hat(pi)[1])
  }
  if(param == 9){
    expr_txt <- substitute(hat(pi)[2])
  }

  dash_line_val <- true_params[param]
  
  if(param > 7){
    dash_line_val <- 1/3
  }
  
  df <- data.frame(n_rows = c(300, 1000),
             mean_val = c(mu_df_n300[param, scen], 
                          mu_df_n1000[param, scen]),
             sd_val = c(sd_df_n300[param, scen],
                        sd_df_n1000[param, scen]))
  df$ymin <- df$mean_val - df$sd_val
  df$ymax <- df$mean_val + df$sd_val
  
  plot <- ggplot(df, aes(x = factor(n_rows), y = mean_val)) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
    labs(title = bquote("S" *.(scen)~"-"~ .(expr_txt)), 
         x = "Number of rows (n)", 
         y = "Value" )+
    geom_hline(yintercept = dash_line_val, linetype = "dashed") + 
    geom_point(size = 2) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot)
}

m10s10_mu <- make_df(param = 1, scen = 1)
m10s10_a1 <- make_df(param = 2, scen = 1)
m10s10_a2 <- make_df(param = 3, scen = 1)
m10s10_b1 <- make_df(param = 4, scen = 1)
m10s10_b2 <- make_df(param = 5, scen = 1)
m10s10_b3 <- make_df(param = 6, scen = 1)
m10s10_b4 <- make_df(param = 7, scen = 1)
m10s10_p1 <- make_df(param = 8, scen = 1)
m10s10_p2 <- make_df(param = 9, scen = 1)

m10s30_mu <- make_df(param = 1, scen = 2)
m10s30_a1 <- make_df(param = 2, scen = 2)
m10s30_a2 <- make_df(param = 3, scen = 2)
m10s30_b1 <- make_df(param = 4, scen = 2)
m10s30_b2 <- make_df(param = 5, scen = 2)
m10s30_b3 <- make_df(param = 6, scen = 2)
m10s30_b4 <- make_df(param = 7, scen = 2)
m10s30_p1 <- make_df(param = 8, scen = 2)
m10s30_p2 <- make_df(param = 9, scen = 2)

m10s50_mu <- make_df(param = 1, scen = 3)
m10s50_a1 <- make_df(param = 2, scen = 3)
m10s50_a2 <- make_df(param = 3, scen = 3)
m10s50_b1 <- make_df(param = 4, scen = 3)
m10s50_b2 <- make_df(param = 5, scen = 3)
m10s50_b3 <- make_df(param = 6, scen = 3)
m10s50_b4 <- make_df(param = 7, scen = 3)
m10s50_p1 <- make_df(param = 8, scen = 3)
m10s50_p2 <- make_df(param = 9, scen = 3)
############################################################
# Visualisation ----

grid.arrange(m10s10_mu, m10s10_a1, m10s10_a2, 
             m10s10_b1, m10s10_b2, m10s10_b3,
             m10s10_b4, m10s10_p1, m10s10_p2, ncol = 3)

grid.arrange(m10s30_mu, m10s30_a1, m10s30_a2, 
             m10s30_b1, m10s30_b2, m10s30_b3,
             m10s30_b4, m10s30_p1, m10s30_p2, ncol = 3)

grid.arrange(m10s50_mu, m10s50_a1, m10s50_a2, 
             m10s50_b1, m10s50_b2, m10s50_b3,
             m10s50_b4, m10s50_p1, m10s50_p2, ncol = 3)

###########################################################
# m30  df ----

m30s10_mu <- make_df(param = 1, scen = 4)
m30s10_a1 <- make_df(param = 2, scen = 4)
m30s10_a2 <- make_df(param = 3, scen = 4)
m30s10_b1 <- make_df(param = 4, scen = 4)
m30s10_b2 <- make_df(param = 5, scen = 4)
m30s10_b3 <- make_df(param = 6, scen = 4)
m30s10_b4 <- make_df(param = 7, scen = 4)
m30s10_p1 <- make_df(param = 8, scen = 4)
m30s10_p2 <- make_df(param = 9, scen = 4)

m30s30_mu <- make_df(param = 1, scen = 5)
m30s30_a1 <- make_df(param = 2, scen = 5)
m30s30_a2 <- make_df(param = 3, scen = 5)
m30s30_b1 <- make_df(param = 4, scen = 5)
m30s30_b2 <- make_df(param = 5, scen = 5)
m30s30_b3 <- make_df(param = 6, scen = 5)
m30s30_b4 <- make_df(param = 7, scen = 5)
m30s30_p1 <- make_df(param = 8, scen = 5)
m30s30_p2 <- make_df(param = 9, scen = 5)

m30s50_mu <- make_df(param = 1, scen = 6)
m30s50_a1 <- make_df(param = 2, scen = 6)
m30s50_a2 <- make_df(param = 3, scen = 6)
m30s50_b1 <- make_df(param = 4, scen = 6)
m30s50_b2 <- make_df(param = 5, scen = 6)
m30s50_b3 <- make_df(param = 6, scen = 6)
m30s50_b4 <- make_df(param = 7, scen = 6)
m30s50_p1 <- make_df(param = 8, scen = 6)
m30s50_p2 <- make_df(param = 9, scen = 6)


############################################################
# Visualisation - m30 ----

grid.arrange(m30s10_mu, m30s10_a1, m30s10_a2, 
             m30s10_b1, m30s10_b2, m30s10_b3,
             m30s10_b4, m30s10_p1, m30s10_p2, ncol = 3)

grid.arrange(m30s30_mu, m30s30_a1, m30s30_a2, 
             m30s30_b1, m30s30_b2, m30s30_b3,
             m30s30_b4, m30s30_p1, m30s30_p2, ncol = 3)

grid.arrange(m30s50_mu, m30s50_a1, m30s50_a2, 
             m30s50_b1, m30s50_b2, m30s50_b3,
             m30s50_b4, m30s50_p1, m30s50_p2, ncol = 3)
