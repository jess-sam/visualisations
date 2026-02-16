

min_max <- function(data_list, scen, pi_num){
  collect <- rep(NA, 100)
  
  for(i in 1:100){
    collect[i] <- data_list[[i]][[7]][scen, pi_num]
  }
  
  return(c(min(collect), max(collect)))
}


m10s10_prop_300 <- lapply(1:3, function(i){min_max(all_n300, 1, i)})
m10s30_prop_300 <- lapply(1:3, function(i){min_max(all_n300, 2, i)})
m10s50_prop_300 <- lapply(1:3, function(i){min_max(all_n300, 3, i)})
m30s10_prop_300 <- lapply(1:3, function(i){min_max(all_n300, 4, i)})
m30s30_prop_300 <- lapply(1:3, function(i){min_max(all_n300, 5, i)})
m30s50_prop_300 <- lapply(1:3, function(i){min_max(all_n300, 6, i)})

n300 <- matrix(unlist(list(m10s10_prop_300, m10s30_prop_300, m10s50_prop_300,
                           m30s10_prop_300, m30s30_prop_300, m30s50_prop_300)), 
       ncol = 6, 
       byrow = TRUE)

matrix(n300[,6])

m10s10_prop_1000 <- lapply(1:3, function(i){min_max(all_n1000, 1, i)})
m10s30_prop_1000 <- lapply(1:3, function(i){min_max(all_n1000, 2, i)})
m10s50_prop_1000 <- lapply(1:3, function(i){min_max(all_n1000, 3, i)})
m30s10_prop_1000 <- lapply(1:3, function(i){min_max(all_n1000, 4, i)})
m30s30_prop_1000 <- lapply(1:3, function(i){min_max(all_n1000, 5, i)})
m30s50_prop_1000 <- lapply(1:3, function(i){min_max(all_n1000, 6, i)})

n1000 <- matrix(unlist(list(m10s10_prop_1000, m10s30_prop_1000, m10s50_prop_1000,
                           m30s10_prop_1000, m30s30_prop_1000, m30s50_prop_1000)), 
               ncol = 6, 
               byrow = TRUE)

matrix(n1000[,6])


m10s10_prop_3000 <- lapply(1:3, function(i){min_max(all_n3000, 1, i)})
m10s30_prop_3000 <- lapply(1:3, function(i){min_max(all_n3000, 2, i)})
m10s50_prop_3000 <- lapply(1:3, function(i){min_max(all_n3000, 3, i)})
m30s10_prop_3000 <- lapply(1:3, function(i){min_max(all_n3000, 4, i)})
m30s30_prop_3000 <- lapply(1:3, function(i){min_max(all_n3000, 5, i)})
m30s50_prop_3000 <- lapply(1:3, function(i){min_max(all_n3000, 6, i)})

n3000 <- matrix(unlist(list(m10s10_prop_3000, m10s30_prop_3000, m10s50_prop_3000,
                            m30s10_prop_3000, m30s30_prop_3000, m30s50_prop_3000)), 
                ncol = 6, 
                byrow = TRUE)

matrix(n3000[,6])


#####################################
min_max_correct <- function(data_list, scen, pi_num){
  collect <- rep(NA, 100)
  
  for(i in 1:100){
    collect[i] <- data_list[[i]][[3]][scen, pi_num]
  }
  
  return(c(min(collect), max(collect)))
}

m10_prop_300_correct <- lapply(1:3, function(i){min_max_correct(correct_300, 1, i)})
m30_prop_300_correct <- lapply(1:3, function(i){min_max_correct(correct_300, 2, i)})

n300_correct <- matrix(unlist(list(m10_prop_300_correct, m30_prop_300_correct)), 
                ncol = 6, 
                byrow = TRUE)

m10_prop_1000_correct <- lapply(1:3, function(i){min_max_correct(correct_1000, 1, i)})
m30_prop_1000_correct <- lapply(1:3, function(i){min_max_correct(correct_1000, 2, i)})

n1000_correct <- matrix(unlist(list(m10_prop_1000_correct, m30_prop_1000_correct)), 
                       ncol = 6, 
                       byrow = TRUE)
