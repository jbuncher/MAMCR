# Script to clean the "test" data file, converting choices (a, b, c, d) to "True/False"

# Import the File of Responses
TestResponse <- read.csv("/media/jbuncher/Research/PER/REU/2017/R/MAMCR/TestResponse.csv",row.names=1)

# Extract the number of questions and students from original data file
NofQs = ncol(TestResponse)
NofSs = nrow(TestResponse)

# State the number of possible responses and create the letter choices
NofPRs = 4
choices = LETTERS[1:NofPRs]

# Create empty Matrix
ActualMatrix <- matrix(data = NA, nrow  = NofSs, ncol = NofQs*4)

# Convert our matrix to a data frame
TestMatrix <- data.frame(TestMatrix)

# create list of column names
ColumnNames <- character()
for (q in 1:NofQs) ColumnNames <- c(ColumnNames,paste0(rep(q,NofPRs),choices))

# Give the columns in our data frame actual names
TestMatrix <- setNames(TestMatrix, ColumnNames)

for (q in 1:NofQs){
  for (choice in choices){ 
    # Select the column we need
    selcol <- paste0(q,choice)
    # Put a "1" in the cell for choices that match, 0 for those that don't
    TestMatrix[selcol][TestResponse[q]==choice] <- 1
    TestMatrix[selcol][TestResponse[q]!=choice] <- 0
  }
}



