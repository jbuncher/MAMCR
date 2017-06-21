#Automating Column Names
library(readxl)
X1D_Data_QA_Only_1_ <- read_excel("1D_Data_QA_Only(1).xls")

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

#Creating a QA_Matrix the same size as TestResponse Matrix
QA_Matrix_1D <- matrix(data = NA,nrow=nrow(X1D_Data_QA_Only_1_),ncol = (length(ColumnNames)),
                     byrow = FALSE, dimnames = as.list(c(ColumnNames,ColumnNames)))

#Convert Matrix to a data frame so we can manipulate the names of the columns
QA_Matrix_1D <- data.frame(QA_Matrix_1D)

#Renaming the columns of TestMatrix to appropriate Students, Q1A, Q1B, etc.
QA_Matrix_1D <- setNames(QA_Matrix_1D, ColumnNames)

#Number of questions doubled (p/s verisons) for iteration i
QuestionLength <- (NumberOfQuestions*2)

let1 <- c("ppp","ppn","pmp","pmn","npp","npn","nmp","nmn")
for (i in 1:QuestionLength)
  {for (j in 1:nrow(X1D_Data_QA_Only_1_))
  {for (k in 1:length(let1))
  {for (l in 1) if (X1D_Data_QA_Only_1_[j,i]==let1[k]) QA_Matrix_1D[j,(i-1)*8+k] <- 1 
        else QA_Matrix_1D[j,(i-1)*8+k] <- 0 }}}

library(igraph)
QA_1D_Network <- graph_from_incidence_matrix(QA_Matrix_1D, add.names = NULL)
QA_1D_Network.bp <- bipartite.projection(QA_1D_Network)
V(QA_1D_Network.bp$proj2)$label <- ColumnNames

E(QA_1D_Network.bp$proj2)$width <- E(QA_1D_Network.bp$proj2)$weight
plot(QA_1D_Network.bp$proj2)

#QR: Question Responses
QR_1D_Network <- QA_1D_Network.bp$proj2

degree(QR_1D_Network, modes = "all")
igraph::degree(QR_1D_Network, modes = "all")