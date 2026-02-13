
# Functions ----
get_mu <- function(n_res_list, index){
  mu <- colMeans(n_res_list[[index]])
  return(mu)
}

get_sd <- function(n_res_list, index){
  stdev <- apply(n_res_list[[index]], 2, sd)
  return(stdev)
}

# True parameters ----

alpha_vec <- c(-2, 0)
beta_vec <- c(-2, -1.5, 0.3, 1)
mu <- 0

true_params <- matrix(c(mu, alpha_vec, beta_vec), ncol = 1)

# Compute for n = 300 ----

# mu
mu_est_n300 <- lapply(1:length(res300), function(i){get_mu(res300, i)})
mu_df_n300 <- as.data.frame(do.call(rbind, mu_est_n300))[-10,]

colnames(mu_df_n300) <- c("m10s10","m10s30","m10s50", 
                          "m30s10", "m30s30", "m30s50")
row.names(mu_df_n300) <- NULL

# sd 
sd_est_n300 <- lapply(1:length(res300), function(i){get_sd(res300, i)})
sd_df_n300 <- as.data.frame(do.call(rbind, sd_est_n300))[-10,]


colnames(sd_df_n300) <- c("m10s10","m10s30","m10s50", 
                          "m30s10", "m30s30", "m30s50")

row.names(sd_df_n300) <- NULL

sd_df_n300[1]

