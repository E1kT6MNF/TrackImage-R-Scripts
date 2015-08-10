TRRconvert <- function(fileName){
#     this script reads .csv file from Track Report and rearranges from single column to a column for each metric
#
#
# Find header length (rows)

# trial stuff
#fileName <- choose.files()
#     Read header, find lines for STRINGS and SCALARS
header <- readLines(fileName,n=-1L)
infoRows <- grep("STRINGS|SCALARS",header) 
#     read STRINGS 
dat1 <- read.csv(fileName, skip = infoRows[1], nrows = infoRows[2]-infoRows[1]-2,header = F)
#     remove extra column
dat1 <- dat1[,1:2]
#     CONVERT TO CHARACTER
dat1[,1] <-as.character(dat1[,1])
#     Extract test number from character string and add as a column in dataframe
for (j in 1:nrow(dat1)) {
      #     find last blank space in string
      lastspace <- regexpr("\\ [^\\ ]*$", dat1[j,1])+1
      #     take characters from last blank space to end 
      dat1$Test_Num[j] <- substr(dat1[j,1],start = lastspace,stop = nchar(dat1[j,1]))
      }
#     drop unneeded column
dat1 <- dat1[,c(2:3)]
#     name and sort by test number
names(dat1) <- c("Test","Test_Num")
dat1 <-dat1[order(dat1$Test),]

#     read SCALARS
dat2 <- read.csv(fileName, skip = infoRows[2], header = F,colClasses = c("character","numeric","character"))
dat2 <-dat2[,1:3]
names(dat2) <- c("Metric", "Data","Units")
#     Make test Number column
for (i in 1:nrow(dat2)) {
      #     find last blank space in string
      lastspace <- regexpr("\\ [^\\ ]*$", dat2[i,1])+1
      #     take characters from last blank space to end 
      dat2$Test_Num[i] <- substr(dat2[i,1],start = lastspace,stop = nchar(dat2[i,1]))
      #     Extract units from character string
      dat2$Metric[i] <- substr(dat2[i,1],start = 1, stop = lastspace-1)
      #     convert units from m to mm if needed
      if(dat2$Units[i]== "m"){
            dat2$Data[i] <- 1000*dat2$Data[i]
            dat2$Units[i] <- "mm"
      }
      #     convert units from rad to deg if needed
      if(dat2$Units[i]== "rad"){
            dat2$Data[i] <- 57.2958*dat2$Data[i]
            dat2$Units[i] <- "deg"
      }  
}
#     rename metric with units
# concatenate metric and units
dat2$Metric <- paste0(dat2$Metric,"_",dat2$Units)
#     Drop units column
dat2 <-dat2[,c(1,2,4)]
dat2$Test_Num <- as.integer(dat2$Test_Num)
#     sort by metric, test
dat2 <-dat2[order(dat2$Metric, dat2$Test_Num),]
#     unstack data
library(tidyr)
dat <- spread(dat2, Metric, Data)
#     add test nubers to unstacked data
dat <- cbind(dat1$Test,dat)
names(dat)[names(dat)=="dat1$Test"] <- "Test"
#     write.table(dat2, file='clipboard', sep='\t',row.names = FALSE) # run for script testing
return(dat)

}
