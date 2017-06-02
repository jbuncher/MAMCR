library(readr)
library(igraph)

# import the csv file which has the bipartite network in matrix form
TestBipartiteMatrix <- read.csv("/media/jbuncher/Research/PER/REU/2017/R/MAMCR/TestBipartiteMatrix.csv", header=T, as.is=T,row.names=1)

# convert the matrix we imported into an igraph object
testNetwork <- graph_from_incidence_matrix(TestBipartiteMatrix, weighted = T)

# create the two bipartite projections from the network
testNetwork.bp <- bipartite.projection(testNetwork)

# plots the network of answer selections where the edge width is given by the edge weight
plot(testNetwork.bp$proj2, edge.width =E(testNetwork.bp$proj2)$weight)

# extract the weights of the response x response network
E(testNetwork.bp$proj2)$weight

# put the edgelist into its own variable
edgetest <- E(testNetwork.bp$proj2)

# select edges that include vertex X3C
edgetest[inc('X3C')]

## Apparently putting the edgelist in a different variable
## like above causes issues when trying to modify the acutal
## list.  I think it has to do with the fact that we have 
## separated it from the actual igraph object

## A workaround to shorten the code lines is to use shorter
## variable names, I guess?

# put the response x response network into its own variable
RR <- testNetwork.bp$proj2

# print the weights
E(RR)$weight

# extract the weights of any edges with a weight larger than 2
originalweight <- E(RR)[weight > 2]

# any weights that are greater than 2, set to 7
E(RR)[weight >2]$weight <- 7
E(RR)$weight

# restore the original weight
E(RR)[weight == 7]$weight <- originalweight

# list vertices
V(RR)

# list vertices along with attributes
V(RR)[[]]

# list vertex names
V(RR)$name

# add new empty column named X1A to a matrix TestResponse
TestResponse$X1A <- ""

# Create a '1' in the X1A column in any row that chose 'A' in column 'X1'
TestResponse$X1A[TestResponse$X1 == "A"] <- 1

# Create a '0' in the X1A column in any row that did not choose 'A' in column 'X1'
TestResponse$X1A[TestResponse$X1 != "A"] <- 0