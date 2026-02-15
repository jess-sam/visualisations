
all_n300[[1]][[7]]


# calculate mean for each cell of the matrix

# wait instead of mean, maybe i go min and max values for all the iterations 
# so min max for each of the pis but also across all of the replicates

mean_prop <- function(data_list, scen, pi_num){
  
  total <- 0
  for(i in 1:100){
    total <- total + data_list[[i]][[7]][scen, pi_num]
  }
  return(total/100)
}

mean_prop(all_n300, 6, 2)
