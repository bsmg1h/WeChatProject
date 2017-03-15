library(igraph)
colorbar <- c("#fdbb84","#de2d26","#FFFFFF")
####  Set the original condition:

##  1. Set the original network structure

#g <- sample_smallworld(2, 10, 1, 1)
#g <- sample_gnp(100, 0.043)

#coords <- layout.kamada.kawai(g)

# c = rep(3,100)
# c[nodes_status[,2]<5] = 1
# c[nodes_status[,2]==5] = 2
# 
# plot(g, layout = coords,
#      vertex.size = 6,
#      vertex.label = NA,
#      edge.width = 1,
#      edge.arrow.size = 0.2,
#      edge.arrow.width = 1,
#      vertex.color = colorbar[c])

#ad_matrix <- as_adjacency_matrix(g)

## 2. Set the original stochastic probability matix

#pb_matrix <- matrix(runif(n=100^2,min = 0,max = 0.65),
#                    nrow = 100,
#                    ncol = 100)

#pb_matrix <- pb_matrix * ad_matrix




####  Start model simulation:
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



diffusion_process <- function(g,simulation_result){
  image_list <- vector(mode = "list", length=simulation_result$time)
  for(i in 1:simulation_result$time){
    c = rep(3,100)
    c[simulation_result$status[,2]<i] = 1
    c[simulation_result$status[,2]==i] = 2
    
    image_list[[i]] <- plot(g, layout = coords,
         vertex.size = 6,
         vertex.label = NA,
         edge.width = 1,
         edge.arrow.size = 0.2,
         edge.arrow.width = 1,
         vertex.color = colorbar[c])
  }
  return(image_list)
}

####################################################################################################
####  Random model simulation
g <- sample_gnp(100, 0.043)

coords <- layout.kamada.kawai(g)
ad_matrix <- as_adjacency_matrix(g)

pb_matrix <- matrix(runif(n=100^2,min = 0,max = 0.65),
                    nrow = 100,
                    ncol = 100)

pb_matrix <- pb_matrix * ad_matrix



result <- simulate(g,pb_matrix)
p <- diffusion_process(g,result)

# model.list <- list(g = g, result = result, pb_matrix = pb_matrix)
# save(model.list,file="model1")


###################################################################################################
####  Specific model simulation
load("model1")
g <- model.list$g
coords <- layout.kamada.kawai(g)
#coords <- layout_with_fr(g)
result <- model.list$result
pb_matrix <- model.list$pb_matrix


p <- diffusion_process(g,result)


