setwd("/home/oski/Work")

#To check we are in the correct working directory
list.files()

#All column
allcol <- read.csv("PS2data.bz2", nrow=1, header=FALSE, stringsAsFactors=FALSE)

#Specific for column
#Column we want
col_want <- c("ST", "NP", "BDSP", "BLD", "RMSP",
              "TEN", "FINCP", "FPARC", "HHL", "NOC",
              "MV", "VEH", "YBL")
#All column
coln_want <- which(allcol %in% col_want)
write.table(coln_want, "number.csv", row.name=FALSE, col.name=FALSE)

#Total row
total_row <- 0
datacon <- file("PS2data.gz", open="r")
#Initial
l <- 1
while(l > 0){
  total_row <- total_row + l
  l <- length(readLines(datacon, n=100000))
}
close(datacon); rm(datacon)
total_row <- total_row - 2

#my.read.csv
my.read.csv <- function(filename, n, total_row, maxrow=100000){
  upper <- (total_row %/% maxrow) + 1
  breaks <- seq(1, maxrow*upper + 1, by=maxrow)
  datacon <- file(filename, open='r')
  col_name <- read.csv(datacon, nrow=1, header=FALSE, stringsAsFactors=FALSE)
  data <- matrix(0, nrow=n, ncol=length(col_name))
  data <- as.data.frame(data)
  #Avoid miss match
  names(data) <- col_name
  #Sample
  n.row <- sample(1:total_row, n)
  #Index
  j <- 1
  for(i in 1:(length(breaks) - 1)){
    row_want <- which(n.row >= breaks[i] & n.row < breaks[i + 1])
    if(length(row_want) == 0){
      next
    }else{
      data[j:(j + length(row_want) - 1), ] <- read.csv(datacon, nrow=maxrow, header=FALSE)[n.row[row_want] - breaks[i] + 1, ]
      j <- j + length(row_want)  
    }
  }
  close(datacon)
  data
}
set.seed(1)
data <- my.read.csv("PS2data.gz", n=10000, total_row, maxrow=100000)
write.csv(data, "NewPS2data.csv", row.names=FALSE)
#Time comparison
system.time(my.read.csv("PS2data.gz", n=10000, total_row, maxrow=100000))

set.seed(1)
library(readr)

datacon <- file("PS2data.gz")
read_csv(datacon, n_max=1)
read_csv(datacon, n_max=1)
close(datacon)

#my.read_csv
my.read_csv <- function(filename, n, total_row, maxrow=100000){
  upper <- (total_row %/% maxrow) + 1
  breaks <- seq(1, maxrow*upper + 1, by=maxrow)
  col_name <- names(read_csv(filename, n_max=1))
  data <- matrix(0, nrow=n, ncol=length(col_name))
  data <- as.data.frame(data)
  #Avoid miss match
  names(data) <- col_name
  #Sample
  n.row <- sample(1:total_row, n)
  #Index
  j <- 1
  for(i in 1:(length(breaks) - 1)){
    row_want <- which(n.row >= breaks[i] & n.row < breaks[i + 1])
    if(length(row_want) == 0){
      next
    }else{
      data[j:(j + length(row_want) - 1), ] <- read_csv(filename, n_max=maxrow, skip=breaks[i] - 1)[n.row[row_want] - breaks[i] + 1, ]
      j <- j + length(row_want)  
    }
  }
  data
}
set.seed(1)
data <- my.read_csv("PS2data.gz", n=10000, total_row, maxrow=100000)

system.time(my.read_csv("PS2data.gz", n=10000, total_row, maxrow=100000))

#myreadLines
my.readLines <- function(filename, n, total_row, maxrow=100000){
  upper <- (total_row %/% maxrow) + 1
  breaks <- seq(1, maxrow*upper + 1, by=maxrow)
  datacon <- file(filename, open="r")
  col_name <- strsplit(readLines(datacon, n=1), split=",")[[1]]
  data <- matrix(0, nrow=n, ncol=length(col_name))
  data <- as.data.frame(data)
  names(data) <- col_name
  #Sample
  n.row <- sample(1:total_row, n)
  j <- 1
  for(i in 1:(length(breaks) - 1)){
    row_want <- which(n.row >= breaks[i] & n.row < breaks[i + 1])
    if(length(row_want) == 0){
      next
    }else{
      data_row <- readLines(datacon, n=maxrow)[n.row[row_want] - breaks[i] + 1]
      process <- as.numeric(unlist(strsplit(gsub(",", ", ", data_row), split=",")))
      process_matrix <- matrix(process, nrow=length(row_want), ncol=length(col_name), byrow=TRUE)
      data[j:(j + length(row_want) - 1), ] <- process_matrix 
      j <- j + length(row_want)   
    }
  }
  close(datacon)
  data
}
set.seed(1)
data <- my.readLines("PS2data.gz", n=10000, total_row)
system.time(my.readLines("PS2data.gz", n=10000, total_row))

my.read.mix <- function(filename, n, total_row, maxrow=100000){
  upper <- (total_row %/% maxrow) + 1
  breaks <- seq(1, maxrow*upper + 1, by=maxrow)
  datacon <- file(filename, open="r")
  data <- matrix(0, nrow=n + 1)
  data[1] <- readLines(datacon, n=1)
  #Sample
  n.row <- sample(1:total_row, n)
  #Index
  j <- 2
  for(i in 1:(length(breaks) - 1)){
    row_want <- which(n.row >= breaks[i] & n.row < breaks[i + 1])
    if(length(row_want) == 0){
      next
    }else{
      data[j:(j + length(row_want) - 1)] <- readLines(datacon, n=maxrow)[n.row[row_want] - breaks[i] + 1]
      j <- j + length(row_want)   
    }
  }
  close(datacon)
  writeLines(data, "data.csv")
  read_csv("data.csv")
}
set.seed(1)
data <- my.read.mix("PS2data.gz", n=10000, total_row)

system.time(my.read.mix("PS2data.gz", n=10000, total_row))

data <- read.csv("NewPS2data.csv")

#Check the field
summary(data)

with(data, boxplot(FINCP ~ TEN, xlab="Tenure type", ylab="Income"))

with(data, table(MV, YBL))

round(with(data, prop.table(table(HHL, ST), 2)), 2)
t <- as.data.frame(round(with(data, prop.table(table(HHL, ST), 2)), 2))

library(ggplot2)
#Visualize
ggplot(t) + geom_tile(aes(x=HHL, y=ST, z=Freq, fill=Freq))

pairs(data[c("NP", "BDSP", "RMSP", "VEH")])
