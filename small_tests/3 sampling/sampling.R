library(igraph)
library(igraphdata)
library(dplyr)
colorbar <- c("#fdbb84","#de2d26","#FFFFFF")


# 1. Load network and log ----------------------------------------------------------------
wd = getwd()
data <- read.csv(paste(wd,"small_tests","3 sampling","log.csv",sep = "/"))
load("model")
g = network$g

# 2. Sampling -------------------------------------------------------------

Sampling <- function(passageID = sample(1:300,1), log, )


