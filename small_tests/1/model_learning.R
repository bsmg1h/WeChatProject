library(igraph)

multi_simulation <- function(g,pb_matrix,simulation_round = 100,ini_active_nodes_amt = 3){
  n_nodes <- length(V(g))
  event <- matrix(rep(0,n_nodes*simulation_round),nrow = simulation_round,ncol = n_nodes)
  ##a
  for(n in 1:simulation_round){
    ##  1. Set the initial active nodes:
    ini_active_nodes = sample(1:n_nodes,size = ini_active_nodes_amt)
    
    ##  2. Set the initial nodes status:
    nodes_status = rep(n_nodes+1,n_nodes)
    nodes_status[ini_active_nodes] = 0
    
    ##  3. simulation part:
    t = 1
    n_nodes_active_this_turn = 1
    
    while(n_nodes_active_this_turn > 0){
      n_nodes_active_this_turn = 0
      # the loop will stop if there is no node actived this turn
      affecting_nodes = which(nodes_status==(t-1))
      for(i in affecting_nodes){
        affected_nodes = as.numeric(neighbors(g,i))
        for(j in affected_nodes){
          if(pb_matrix[i,j] > runif(1)){
            if(nodes_status[j] == (n_nodes+1)){
              # if this nodes has not been actived yet, it will be actived in this turn
              nodes_status[j] = t
            }
          }
        }
      }
      
      n_nodes_active_this_turn = length(nodes_status[nodes_status==t])
      t = t+1
    }
    
    event[n,] <- nodes_status
    print(n)
  }
  return(event)
}
  

weight_learning <- function(g,event){
  n_nodes <- length(V(g))
  simulation_round <- nrow(event)
  
  ## 1. To calculate how many times each node got actived
  
  actived_matrix <- event<(n_nodes+1)
  total_actived_times <- colSums(actived_matrix)
  total_actived_times[total_actived_times==0] <- 1
  
  
  ## 2. To calculate the accumulated partial credit of each link
  weight_matrix <- matrix(rep(0,n_nodes^2),ncol = n_nodes,nrow = n_nodes)
  loss_series <- rep(0,simulation_round)
  for(i in 1:simulation_round){
    event_this_turn <- event[i,]
    actived_nodes <- which(event_this_turn < (n_nodes+1) & event_this_turn > 0)
    for(j in actived_nodes){
      actived_time <- event_this_turn[j]
      actived_nodes_last_turn <- which(event_this_turn == (actived_time-1))
      probable_affecting_nodes <- actived_nodes_last_turn[actived_nodes_last_turn %in% as.numeric(neighbors(g,j))]
      weight_matrix[probable_affecting_nodes,j] <- weight_matrix[probable_affecting_nodes,j] + 1/length(probable_affecting_nodes)
    }
    print(i)
    weight_matrix_this_turn = weight_matrix
    
    if(i==1){
      total_actived_times_this_turn=1
    }
    if(i >1){
      total_actived_times_this_turn <- colSums(actived_matrix[1:i,])
      total_actived_times_this_turn[total_actived_times_this_turn==0] <- 1
    }
    
    for(j in 1:n_nodes){
      weight_matrix_this_turn[,j] <- weight_matrix_this_turn[,j]/total_actived_times_this_turn
    }
    loss_series[i] <- sd(pb_matrix - weight_matrix_this_turn)
  }
  
  
  for(j in 1:n_nodes){
    weight_matrix[,j] <- weight_matrix[,j]/total_actived_times
  }
  
  result <- list(weight_matrix = weight_matrix, loss_series = loss_series)
  return(result)
}

  
#######################################################################
  
load("model1")
g <- model.list$g
coords <- layout.kamada.kawai(g)
#coords <- layout_with_fr(g)
#result <- model.list$result
pb_matrix <- model.list$pb_matrix

event <- multi_simulation(g,pb_matrix,simulation_round = 400)
result <- weight_learning(g,event)

## evaluation
sum(result$weight_matrix)
sum(pb_matrix)

heat_map_learn <- heatmap(result$weight_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))
heat_map_model <- heatmap(as.matrix(pb_matrix), Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))

plot(result$loss_series, type = "l", ylab = "loss")
sd(result$weight_matrix-pb_matrix)

