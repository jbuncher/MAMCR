library(readr)
library(igraph)

# import the csv file which has the bipartite network in matrix form
TestBipartiteMatrix <- read.csv("/media/jbuncher/Research/PER/REU/2017/R/MAMCR/TestBipartiteMatrix.csv", header = T, as.is = T,row.names = 1)

# convert the matrix we imported into an igraph object
testNetwork <- graph_from_incidence_matrix(TestBipartiteMatrix, weighted = T)

# create the two bipartite projections from the network
testNetwork.bp <- bipartite.projection(testNetwork)

# Store the response network as something shorter
RN <- testNetwork.bp$proj2

LANS <- function(RN, alpha) {
  # Make the widths of the edges equal to the weights (for plotting)
  E(RN)$width <- E(RN)$weight
  
  # Remove all nodes not connected to the rest of the network, store as a new graph
  RN_C <- RN - V(RN)[degree(RN) == 0]
  
  # Add a "backbone" attribute to the edges in the network, with 1 indicating
  # that the edge is part of the backbone, and 0 otherwise.  Intialize to 0
  RN_C_BB <- set_edge_attr(graph = RN_C, name = "backbone", value = "0")
  # RN_C_BB <- set_vertex_attr(graph = RN_C_BB, name = "frequency", value = "0")
  
  # makes a list of nodes and edges for convenience
  listOfNodes <- V(RN_C_BB)
  listOfEdges <- E(RN_C_BB)
  
  # for each node in the network....
  for (i in listOfNodes) {

    
    # grab the edges connected to that node and....
    edgesForNode <- listOfEdges[from(listOfNodes[i])]
    
    # set frequency of node (total edge weight)
    # V(RN_C_BB)[i]$frequency <- sum(edgesForNode$weight)
    
    # find the value that 95% of the weights are >= and....
    cutoff <- quantile(edgesForNode$weight,alpha)
    
    # mark those edges and ....
    edgesToKeep <- edgesForNode[edgesForNode$weight >= cutoff]
    
    # indicate that they are part of the backbone
    E(RN_C_BB)[edgesToKeep]$backbone <- 1
  }
  
  # remove all edges not part of the backbone
  final_backbone <- RN_C_BB - E(RN_C_BB)[backbone == 0]
  
  #return the backbone as output
  return(final_backbone)
}
my_backbone <- LANS(RN, 0.99)
# plot the backbone!
plot(my_backbone)
