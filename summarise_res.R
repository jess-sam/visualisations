
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

mu_df_n300[11,]

sd_df_n300[6]

# Compute for n = 1000

# mu
mu_est_n1000 <- lapply(1:length(res1000), function(i){get_mu(res1000, i)})
mu_df_n1000 <- as.data.frame(do.call(rbind, mu_est_n1000))[-10,]

colnames(mu_df_n1000) <- c("m10s10","m10s30","m10s50", 
                          "m30s10", "m30s30", "m30s50")
row.names(mu_df_n1000) <- NULL

# sd 
sd_est_n1000 <- lapply(1:length(res1000), function(i){get_sd(res1000, i)})
sd_df_n1000 <- as.data.frame(do.call(rbind, sd_est_n1000))[-10,]


colnames(sd_df_n1000) <- c("m10s10","m10s30","m10s50", 
                          "m30s10", "m30s30", "m30s50")

row.names(sd_df_n1000) <- NULL

mu_df_n1000[11,]

sd_df_n1000[6]

# Compute for n = 3000

# mu
mu_est_n3000 <- lapply(1:length(res3000), function(i){get_mu(res3000, i)})
mu_df_n3000 <- as.data.frame(do.call(rbind, mu_est_n3000))[-10,]

colnames(mu_df_n3000) <- c("m10s10","m10s30","m10s50", 
                           "m30s10", "m30s30", "m30s50")
row.names(mu_df_n3000) <- NULL

# sd 
sd_est_n3000 <- lapply(1:length(res3000), function(i){get_sd(res3000, i)})
sd_df_n3000 <- as.data.frame(do.call(rbind, sd_est_n3000))[-10,]


colnames(sd_df_n3000) <- c("m10s10","m10s30","m10s50", 
                           "m30s10", "m30s30", "m30s50")

row.names(sd_df_n3000) <- NULL

matrix(mu_df_n3000[,6])

matrix(sd_df_n3000[, 6])
#############################

save(mu_df_n300, mu_df_n1000, mu_df_n3000, 
     sd_df_n300, sd_df_n1000, sd_df_n3000,
     file = "res_sum.Rdata")


###############################
# Compute for n = 300

# mu
mu_est_n300_correct <- lapply(1:length(correct_res300), 
                              function(i){get_mu(correct_res300, i)})
mu_df_n300_correct <- as.data.frame(do.call(rbind, mu_est_n300_correct))[-10,]

colnames(mu_df_n3000_correct) <- c("m10","m30")
row.names(mu_df_n3000_correct) <- NULL

# sd 
sd_est_n300_correct <- lapply(1:length(correct_res300), 
                              function(i){get_sd(correct_res300, i)})
sd_df_n300_correct <- as.data.frame(do.call(rbind, sd_est_n300_correct))[-10,]


colnames(sd_df_n300_correct) <- c("m10", "m30")
row.names(sd_df_n300_correct) <- NULL


matrix(mu_df_n300_correct[,2])

matrix(sd_df_n300_correct[,2])
############################################################################

# Compute for n = 1000

# mu
mu_est_n1000_correct <- lapply(1:length(correct_res1000), 
                              function(i){get_mu(correct_res1000, i)})
mu_df_n1000_correct <- as.data.frame(do.call(rbind, mu_est_n1000_correct))[-10,]

colnames(mu_df_n1000_correct) <- c("m10","m30")
row.names(mu_df_n1000_correct) <- NULL

# sd 
sd_est_n1000_correct <- lapply(1:length(correct_res1000), 
                              function(i){get_sd(correct_res1000, i)})
sd_df_n1000_correct <- as.data.frame(do.call(rbind, sd_est_n1000_correct))[-10,]


colnames(sd_df_n1000_correct) <- c("m10", "m30")
row.names(sd_df_n1000_correct) <- NULL



matrix(mu_df_n1000_correct[,2])
matrix(sd_df_n1000_correct[,2])
