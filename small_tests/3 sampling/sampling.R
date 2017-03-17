library(igraph)
library(igraphdata)
library(dplyr)
colorbar <- c("#fdbb84","#de2d26","#FFFFFF")


# 1. Load network and log ----------------------------------------------------------------
wd = getwd()
log <- read.csv(paste(wd,"small_tests","3 sampling","log.csv",sep = "/"))
load("model")
g = network$g
pb_matrix = network$pb_matrix

# 2. Sampling -------------------------------------------------------------

Positive_Sampling <- function(passageID = sample(1:300,1), log, g, pb_matrix){
	diffusion_result = log %>% filter(passageID == passageID)
	n_actived_point = nrow(diffusion_result)
	if(actived_point < 20){
		return()
	} 
	sample = diffusion_result[sample(1:n_actived_point,1),]
	if(sample[2] <= 2){
		return()
	}
	t = sample[2] - 1
	output = list(passageID = passageID,
								sample_point = sample[1],
								t=t)
}


Negetive_Sampling <- function(passageID = sample(1:300,1), log, g, pb_matrix){
	diffusion_result = log %>% filter(passageID == passageID)
}
