#Automating Column Names

#Number of Questions
NumberOfQuestions <- 4
#Number of response option students have to choice from 
NumberOfResponses <- 4

let1 <- LETTERS[1:NumberOfResponses]
#Creating a Vector with length of number of questions
vect1 <- (1:NumberOfQuestions)
#Reapting the vector to correspond to the number of responses
vect2 <- rep(vect1,times=NumberOfResponses)
#Organizing the vector
vect2 <- sort(vect2, decreasing = FALSE)
#Since we want the column names to be Q1A, Q1B, etc. 
Ques <- rep("Q", times =length(vect2))
#Putting it all together to create the Column names
ColumnNames <- paste(Ques, vect2, let1, sep ="")
#Adding student to the vector
#removing the excess info
rm(let1,vect1,vect2,Ques)

TestResponse <- read.csv("/media/jbuncher/Research/PER/REU/2017/R/MAMCR/TestResponse.csv",row.names=1)

TestMatrix = matrix(data=NA, nrow = nrow(TestResponse), ncol = (ncol(TestResponse))*4, byrow = FALSE, dimnames = NULL)
TestMatrix <- data.frame(TestMatrix)

TestMatrix <- setNames(TestMatrix, ColumnNames)