setwd("C:/Users/jason/Desktop/Work")
library(data.table)
library(RSQLite)
#Create database
db <- dbConnect(SQLite(), dbname="Flight.sqlite")

#Length to check that the function read the data correctly
l <- 0

#Record whether there is obersvation is 9999
nine4 <- 0 

#Start to create table in database
for(i in 1987:2008){
  #Construct the command to read data
  in_file <- paste("bunzip2 -c ", i, ".csv.bz2", sep="")
  
  #Read data
  data <- fread(in_file, data.table=FALSE)
  #Check 9999
  nine4 <- nine4 + sum(data$DepDelay[!is.na(data$DepDelay)] == 9999)
  #Replace NA with unmeaningful number for later filter
  data[which(is.na(data$DepDelay)), "DepDelay"] <- 9999
  #Check length
  l <- l + dim(data)[1]
  
  #Write data into the Flight table
  dbWriteTable(conn=db, name="Flight", value=data, append=TRUE)
  
  #Remove data
  rm(data)
}

dbListTables(db)

l
## [1] 123534969

nine4
##  [1] 0

#Query
query <- "select * from Flight where DepDelay < 9999"
data <- dbGetQuery(db, query)
                   
#Count time for filtering NA
system.time(data <- dbGetQuery(db, query))

library(dplyr)
#Add data.table class
class(data) <- c("data.table", "data.frame")

#Change the column coding in Month and DayOfWeek
myweek <- function(x){
  week <- c("Monday", "Tuesday", "Wednesday", "Thursday",
            "Friday", "Saturday", "Sunday")
  week[x]
}
mymonth <- function(x){
  month <- c("January", "February", "March", "April", "May",
             "June", "July", "August", "September", "October",
             "November", "December")
  month[x]
}

#Flight more than 30 mins late
data %>% 
  group_by(UniqueCarrier, Origin, Dest,
           Month=mymonth(Month), DayOfWeek=myweek(DayOfWeek), DepTime=DepTime %/% 100) %>%
  summarise(Prop30=mean(DepDelay > 30))

#Flight more than 60 mins late
data %>% 
  group_by(UniqueCarrier, Origin, Dest,
           Month=mymonth(Month), DayOfWeek=myweek(DayOfWeek), DepTime=DepTime %/% 100) %>%
  summarise(Prop60=mean(DepDelay > 60))

#Flight more than 180 mins late
data %>% 
  group_by(UniqueCarrier, Origin, Dest,
           Month=mymonth(Month), DayOfWeek=myweek(DayOfWeek), DepTime=DepTime %/% 100) %>%
  summarise(Prop180=mean(DepDelay > 180))


#Set the key by SQL
#Key columns
keycol <- c("UniqueCarrier", "Origin", "Dest", "Month",
            "DayOfWeek")
setkeyv(data, keycol)

#Flight more than 30 mins late
data %>% 
  group_by(UniqueCarrier, Origin, Dest,
           Month=mymonth(Month), DayOfWeek=myweek(DayOfWeek), DepTime=DepTime %/% 100) %>%
  summarise(Prop30=mean(DepDelay > 30))

#Flight more than 60 mins late
data %>% 
  group_by(UniqueCarrier, Origin, Dest,
           Month=mymonth(Month), DayOfWeek=myweek(DayOfWeek), DepTime=DepTime %/% 100) %>%
  summarise(Prop60=mean(DepDelay > 60))

#Flight more than 180 mins late
data %>% 
  group_by(UniqueCarrier, Origin, Dest,
           Month=mymonth(Month), DayOfWeek=myweek(DayOfWeek), DepTime=DepTime %/% 100) %>%
  summarise(Prop180=mean(DepDelay > 180))

#30 at least 150
data %>% 
  group_by(UniqueCarrier, Origin, Dest,
           Month=mymonth(Month), DayOfWeek=myweek(DayOfWeek), DepTime=DepTime %/% 100) %>%
  summarise(Prop30=mean(DepDelay > 30), Count=n()) %>%
  filter(Count >= 150) %>%
  ungroup() %>%
  arrange(Prop30)

#60 at least 150
data %>% 
  group_by(UniqueCarrier, Origin, Dest,
           Month=mymonth(Month), DayOfWeek=myweek(DayOfWeek), DepTime=DepTime %/% 100) %>%
  summarise(Prop60=mean(DepDelay > 60), Count=n()) %>%
  filter(Count >= 150) %>%
  ungroup() %>%
  arrange(Prop60)

#180 at least 150
data %>% 
  group_by(UniqueCarrier, Origin, Dest,
           Month=mymonth(Month), DayOfWeek=myweek(DayOfWeek), DepTime=DepTime %/% 100) %>%
  summarise(Prop180=mean(DepDelay > 180), Count=n()) %>%
  filter(Count > 150) %>%
  ungroup() %>%
  arrange(Prop180)

#Remove unwant column
head <- read.csv("1987.csv.bz2", nrows=1, header=FALSE, stringsAsFactors=FALSE)
col_need <- c("UniqueCarrier", "Origin", "Dest", "Month",
              "DayOfWeek", "DepTime", "DepDelay")

#Find out the position of columns we need
position <- which(head %in% col_need)
#Output it
write.csv(position, "header.csv")
