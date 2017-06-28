#Automating Column Names
library(readxl)
X1D_Data_QA_Only_2_ <- read.csv("~/Desktop/REU_2017/1D_Data_QA_Only(2).csv",row.names = 1)

#Number of Questions
NumberOfQuestions <- 6
#Number of response option students have to choice from 
NumberOfResponses <- 8

let1 <- c("ppp","ppn","pmp","pmn","npp","npn","nmp","nmn")
#Creating a Vector with length of number of questions
vect1 <- (1:NumberOfQuestions)
#Reapting the vector to correspond to the number of responses
vect2 <- rep(vect1,times=2)
vect3 <- rep("P", times=NumberOfResponses)
vect4 <- rep("S", times=NumberOfResponses)
vect5 <- c(vect3,vect4)
vect2 <- rep(vect2,times=NumberOfResponses)
#Organizing the vector
vect2 <- sort(vect2, decreasing = FALSE)
#Since we want the column names to be Q1A, Q1B, etc. 
Ques <- rep("Q", times =length(vect2))
#Putting it all together to create the Column names
ColumnNames <- paste(Ques, vect2, vect5, let1, sep ="")

#Removing excess infomation 
rm(Ques, vect1,vect2,vect3,vect4,vect5)

#Where the data is coming from (easier for following code to be copied and adapted)
Data <- X1D_Data_QA_Only_2_

#Creating a QA_Matrix the same size as TestResponse Matrix
QA_Matrix_1D <- matrix(data = NA,nrow=nrow(Data),ncol = (length(ColumnNames)),
                     byrow = FALSE, dimnames = NULL)

#Convert Matrix to a data frame so we can manipulate the names of the columns
QA_Matrix_1D <- data.frame(QA_Matrix_1D)

#Renaming the columns of TestMatrix to appropriate Students, Q1A, Q1B, etc.
QA_Matrix_1D <- setNames(QA_Matrix_1D, ColumnNames)

#Number of questions doubled (p/s verisons) for iteration i
QuestionLength <- (NumberOfQuestions*2)

#Adding row names from data set
row.names(QA_Matrix_1D) <- row.names(Data)

#loop for generating matrix
let1 <- c("ppp","ppn","pmp","pmn","npp","npn","nmp","nmn")
for (i in 1:QuestionLength)
  {for (j in 1:nrow(Data))
  {for (k in 1:length(let1))
  {for (l in 1) if (Data[j,i]==let1[k]) QA_Matrix_1D[j,(i-1)*8+k] <- 1 
        else QA_Matrix_1D[j,(i-1)*8+k] <- 0 }}}

rm(i,j,k,l)

library(igraph)
QA_1D_Network <- graph_from_incidence_matrix(QA_Matrix_1D, weighted = TRUE)
QA_1D_Network.bp <- bipartite.projection(QA_1D_Network)

#Following line of code no longer needed as long as the original matrix has both row 
    # and column names
#V(QA_1D_Network.bp$proj2)$label <- ColumnNames

E(QA_1D_Network.bp$proj2)$width <- E(QA_1D_Network.bp$proj2)$weight
plot(QA_1D_Network.bp$proj2)

#QR: Question Responses
QR_1D_Network <- QA_1D_Network.bp$proj2

degree(QR_1D_Network, mode = "all")

#Getting edge list from QR_1D_Network and creating a edge list 
QR_1D_Edge <- get.edgelist(QR_1D_Network)


#Creating graph from edge list
QR_1D_Edge_graph <- graph_from_edgelist(QR_1D_Edge, directed = FALSE)

#Plotting Edge graph for 1D responses
plot(QR_1D_Edge_graph, edge.width = E(QR_1D_Network)$weight)

#running infomap with the 1D responses network
###QR_1D_group <- cluster_infomap(QR_1D_Edge_graph, e.weights = NULL, v.weights = NULL, 
 ###                              nb.trials = 10000, modularity = TRUE)

#This creates a network where all the weights less than five are deleted
#QR_1D_Edge_lessthan5 <- delete_edges(QA_1D_Network.bp$proj2, E(QA_1D_Network.bp$proj2)[weight<5]) 
#QR_1D_Edge5 <-get.edgelist(QR_1D_Edge_lessthan5)
#QR_1D_Edge5_graph <- graph_from_edgelist(QR_1D_Edge5, directed = FALSE)
#plot(QR_1D_Edge5_graph)
#rm(QR_1D_Edge_lessthan5,QR_1D_Edge5,QR_1D_Edge5_graph)


#This returns weights for node [1] in the edge_graph list
E(QR_1D_Network)[E(QR_1D_Edge_graph)[from(V(QR_1D_Edge_graph)[1])]]$weight

#This returns all the edges connected to node [1]
E(QR_1D_Edge_graph)[from(V(QR_1D_Edge_graph)[1])]

#This gives me the weights and sorting them and grabbing specific data if using weighted_1_1D$ix
#weight_1_1D<-E(QR_1D_Network)[E(QR_1D_Edge_graph)[from(V(QR_1D_Edge_graph)[1])]]$weight
#weight_1_1D <- sort(weight_1_1D, decreasing=TRUE, index.return = TRUE)
#weight_1_1D

#This returns indices for top two weights. Will use in order to obtain specific nodes
#weight_1_1D$ix[1];weight_1_1D$ix[2]



QR_1D_Weights <- c()
QR_1D_Weights_Indices <- list()

for (i in 1:length(V(QR_1D_Edge_graph))) 
{QR_1D_Weights[i] <- quantile(E(QR_1D_Network)[E(QR_1D_Edge_graph)[from(V(QR_1D_Edge_graph)[i])]]$weight, 0.95)
QR_1D_Weights_Indices[i] <- which(E(QR_1D_Network)[E(QR_1D_Edge_graph)[from(V(QR_1D_Edge_graph)[i])]]$weight > QR_1D_Weights[i], arr.ind = TRUE)}

#for (i in 1:length(V(QR_1D_Edge_graph))) V(QR_1D_Edge_graph)[i]

#quantile will return the number within vector (x) that is less than %
#quantile(x, %) produces #

#which will tell you the indices of numbers in vector (x) that are greater than #
#which(x > # )

#quantile(E(QR_1D_Network)[E(QR_1D_Edge_graph)[from(V(QR_1D_Edge_graph)[1])]]$weight, 0.95)





