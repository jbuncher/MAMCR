library(readr)
library(igraph)

# import the csv file which has the bipartite network in matrix form
TestBipartiteMatrix <- read.csv("/media/jbuncher/Research/PER/REU/2017/R/MAMCR/TestBipartiteMatrix.csv", header=T, as.is=T,row.names=1)

# convert the matrix we imported into an igraph object
testNetwork <- graph_from_incidence_matrix(TestBipartiteMatrix, weighted = T)
