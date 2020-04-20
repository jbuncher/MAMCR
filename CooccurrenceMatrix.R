library(igraph)
# proof of concept for generating co-occurence matrix from
# communities groups returned by clustering algorithms such as
# cluster_infomap

# takes in the network backbone and runs the first clustering attempt
group_loop <- cluster_infomap(BackboneNetwork, nb.trials = 1)

# lists the communities each node belongs to as a matrix (unnecessary?)
memtable <- as.table(membership(group_loop))

# create a matrix where each node is a row and each group is a column
# there *has* to be a better way to do this
memb_matrix <- xtabs(~rownames(memtable) + memtable, data = memtable)

# transpose that matrix so when we do the matrix multiplication, we get a
# "node by node" matrix, rather than a "group by group" matrix, and then
# find the co-occurence matrix
tpose <- t(memb_matrix)
coc <- crossprod(tpose, tpose)

# loop the above
for (i in 1:10000) {
  group_loop <- cluster_infomap(BackboneNetwork, nb.trials = 1)
  memtable <- as.table(membership(group_loop))
  memb_matrix <- xtabs(~rownames(memtable) + memtable, data = memtable)
  tpose <- t(memb_matrix)
  coc_loop <- crossprod(tpose, tpose)
  
  # add the co-occurence matrix to our total co-occurrence matrix
  coc <- coc + coc_loop
  
  # this just helps visualize how the loop is coming along
  print(i)
}
print(coc)

# generate the heatmap
heatmap(coc)