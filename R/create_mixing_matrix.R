library(tidyr)
library(readr)
library(dplyr)

x <- as.matrix(read_csv("data/mossong_GB_allcontacts.csv", col_names = FALSE))
nAges <- 75
monthlyages <- 60
mixing <- matrix(0, ncol = nAges, nrow = nAges)

#diagonalise mossong matrix first
x_diag <- matrix(0, ncol = nAges, nrow = nAges)

for (i in 1:15) {
  for (j in 1:15){
    x_diag[i,j] <- (x[i,j] + x[j,i]) / 2
  }
}

#lower right section
mixing[61:74,61:74] <- x_diag[2:15,2:15] #age groups 5-9y to 70-74y

#upper left section
mixing[1:monthlyages,1:monthlyages] <- x_diag[1,1]/monthlyages

#lower left section
for (i in 1:monthlyages){
  mixing[61:74,i] <- x_diag[2:15,1]
}

#upper right section
for (i in 61:74){
  mixing[1:60,i] <- x_diag[(i-59),1]/60
}

#add on row and column 75 by repeating row/column 74
mixing[,75] <- mixing[,74]
mixing[75,] <- mixing[74,]

mixing <- t(mixing)

write_csv(as.data.frame(mixing, col.names = NULL), 'mixing_75.csv')
