library(igraph)
colorbar <- c("#fdbb84","#de2d26","#FFFFFF")

simulate <- function(g, pb_matrix, ini_active_nodes = c(), ini_active_nodes_amt = 3){
  
  n_nodes <- length(V(g))
  
  ##  1. Set the initial active nodes:
  if(length(ini_active_nodes) == 0){
    ini_active_nodes = sample(1:n_nodes,size = ini_active_nodes_amt)
  }
  
  ##  2. Set the initial nodes status:
  nodes_name = 1:n_nodes
  active_time = rep(n_nodes+1,n_nodes)
  nodes_status = cbind(nodes_name,active_time)
  nodes_status[ini_active_nodes,2] = 0
  
  ##  3. simulation part:
  t = 1
  n_nodes_active_this_turn = 1
  while(n_nodes_active_this_turn > 0){
    n_nodes_active_this_turn = 0
    # the loop will stop if there is no node acived this turn
    affecting_nodes = nodes_status[nodes_status[,2]==(t-1),1]
    for(i in affecting_nodes){
      affected_nodes = as.numeric(neighbors(g,i))
      for(j in affected_nodes){
        if(pb_matrix[i,j] > runif(1)){
          if(nodes_status[j,2] == n_nodes+1){
            nodes_status[j,2] = t
          }
        }
      }
    }
    
    n_nodes_active_this_turn = length(nodes_status[nodes_status[,2]==t,2])
    print(t)
    t = t+1
  }
  
  result = list(time = t, status = nodes_status)
  
  return(result)
  
}



node_color <- function(g,simulation_result,i){
  c = rep(3,100)
  c[simulation_result$status[,2]<i] = 1
  c[simulation_result$status[,2]==i] = 2
    
  
  return(c)
}

load("model1")
g <- model.list$g
pb_matrix <- model.list$pb_matrix
coords <- layout.kamada.kawai(g)

#result <- model.list$result

