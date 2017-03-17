library(igraph)
library(igraphdata)
library(dplyr)
colorbar <- c("#fdbb84","#de2d26","#FFFFFF")

# 1. Set the original network structure -----------------------------------
n_nodes = 200
#g <- sample_smallworld(2, 10, 1, 1)
#g <- sample_gnp(100, 0.043)
#g <- sample_cit_types(n=n_nodes, edges = 1)
g <- sample_smallworld(1, n_nodes, 3, 0.05)

coords <- layout.kamada.kawai(g)

## what does our example look like ?
g %>% plot(layout = coords,
	         vertex.size = 6,
					 vertex.label = NA,
					 edge.width = 1,
					 edge.arrow.size = 0.2,
					 edge.arrow.width = 1)

g %>% degree() %>% plot()

ad_matrix <- as_adjacency_matrix(g)

# 2. Set the original stochastic probability matix ----

pb_matrix <- matrix(runif(n=n_nodes^2,min = 0,max = 0.65),
                   nrow = n_nodes,
                   ncol = n_nodes)

pb_matrix <- pb_matrix * ad_matrix




# 3. Model simulation --------------------------------------------------
  
simulate <- function(g, pb_matrix, ini_active_nodes = c(), ini_active_nodes_amt = 3){
  ###### TODO: write help here ####
	
  n_nodes <- length(V(g))
  
  ######  3.1. Set the initial active nodes: ######
  if(length(ini_active_nodes) == 0){
    ini_active_nodes = sample(1:n_nodes,size = ini_active_nodes_amt)
  }
  
	#  nodes_color: 0:white, 1:red, 2: grey
  
  ######  3.2. Set the initial nodes status: ######
  nodes_name = 1:n_nodes
  active_time = rep(n_nodes+1,n_nodes)
  nodes_color = rep(0,n_nodes)
  #nodes_status = cbind(nodes_name,active_time,nodes_color)
  nodes_status = data.frame(list(nodes_name = nodes_name,
  															 active_time = active_time,
  															 nodes_color = nodes_color))
  nodes_status[ini_active_nodes,2] = 0
  nodes_status[ini_active_nodes,3] = 1

  ######  3.3. simulation part: ######
  t = 0
  n_nodes_red = length(ini_active_nodes)
  while(TRUE){
  	
  	if(t >= 24){
  		break
  	}

  	t = t + 1
  	red_nodes = nodes_status[nodes_status$nodes_color == 1, 1]
  	print(length(red_nodes))
  	if (length(red_nodes) == 0){
  		nodes_status[i,3] = 2
  		break
  	}
  	for(i in red_nodes){
  		########   method 1   ########
  		
  		
  		i_out_neighbors = neighbors(g,i,mode="out")
  		n_neighbors = length(i_out_neighbors)
  		i_out_white_neighbors_status = nodes_status %>% filter(nodes_name %in% i_out_neighbors,
  																										       nodes_color == 0)
  		if(nrow(i_out_white_neighbors_status) == 0){
  			next
  		}

  		i_out_white_neighbors_weight = pb_matrix[i,i_out_white_neighbors_status$nodes_name]

  		relative_t = t-nodes_status[i,2]-1
  		t_possibility = (exp(-relative_t)-exp(-relative_t-1))*i_out_white_neighbors_weight
  		# i_out_white_neighbors_active_possibility =
  		# 	list(nodes_name = i_out_white_neighbors_status$nodes_name,
  		# 			 active_possibility = t_possibility)

  		possibility_base = runif(n=nrow(i_out_white_neighbors_status))

  		nodes_active_this_turn = i_out_white_neighbors_status$nodes_name[t_possibility>possibility_base]
  		nodes_status[nodes_active_this_turn,2] = t
  		nodes_status[nodes_active_this_turn,3] = 1
  		
  		########   method 2   ########
  		
  		# if(sum(nodes_status$nodes_color[neighbors(g,i,mode = "out")] == 1) ==
  		# 	 degree(g,i,mode = "out")){
  		# 	nodes_status[i,3] = 2
  		# 	break
  		# }
  		# relative_t = t-nodes_status[i,2]
  		# t_possibility = (exp(-relative_t)-exp(-relative_t-1))*pb_matrix[i,]
  		# possibility_base = runif(n=n_nodes)
  		# nodes_active_this_turn = intersect(which(t_possibility>possibility_base),
  		# 																	 nodes_status$nodes_name[nodes_status$nodes_color == 0])
  		# nodes_status[nodes_active_this_turn,3] = 1
  	}
  	
  }
  
  
  ##  3. simulation part:
  # t = 1
  # n_nodes_active_this_turn = 1
  # while(n_nodes_active_this_turn > 0){
  #   n_nodes_active_this_turn = 0
  #   # the loop will stop if there is no node acived this turn
  #   affecting_nodes = nodes_status[nodes_status[,2]==(t-1),1]
  #   for(i in affecting_nodes){
  #     affected_nodes = as.numeric(neighbors(g,i))
  #     for(j in affected_nodes){
  #       if(pb_matrix[i,j] > runif(1)){
  #         if(nodes_status[j,2] == n_nodes+1){
  #           nodes_status[j,2] = t
  #         }
  #       }
  #     }
  #   }
  #   
  #   n_nodes_active_this_turn = length(nodes_status[nodes_status[,2]==t,2])
  #   print(t)
  #   t = t+1
  # }
  
  result = list(time = t, status = nodes_status)
  
  return(result)
  
}
  ## Single diffusion process simulation function:
  ## it returns a list:   time: how long does this diffusion process continues
  ##                      status: nodes_name, active_time, nodes_color

multi_simulate <- function(rounds, g, pb_matrix){
	# n = 0
	# log = list(nodes_name = c(),
	# 					 active_time = c(),
	# 					 nodes_color = c(),
	# 					 passageID = c()) %>% data.frame()
	# 
	output <- Reduce(rbind,
									 Map(function(x){
									 	   passageID = rep(x,200)
									 	   cbind(simulate(g, pb_matrix)$status,passageID) %>% filter(nodes_color != 0)
									     }, 
									     1:rounds
									 		)
									 )
	return(output)
}

# 4. Save the log of the simulated diffusion process ----------------------
log = multi_simulate(rounds = 300,
										 g,pb_matrix)
wd = getwd()
write.csv(log, paste(wd,"small_tests","2 Time Dependent Cascade model Simulation and log generation","log.csv",sep = "/"),row.names = F)

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


