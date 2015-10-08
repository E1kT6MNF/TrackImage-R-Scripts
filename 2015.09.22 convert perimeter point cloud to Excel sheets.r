#     this script converts TrackReport perimeter point cloud data from weird .csv
#     to Excel sheets for each time interval in x,y pairs
#
library(xlsx)
#     Choose TrackReport File
fileName <- choose.files(default = "*.csv",caption = "Select TrackReport file")

header <- readLines(fileName,n=1000)
row.x <- grep("HEADER|*.x",header) 
row.y <- grep("HEADER|*.y",header) 
row.z <- grep("HEADER|*.z",header) 
#Extact time steps from TrackImage file
times <-  read.delim(fileName, sep = ",", skip = row.x[2] , nrows = 1,header = F)
# drop extra column from trailing comma
times <- times[1:ncol(times)-1]
#     read x data
x <- read.csv(fileName, skip = row.x[2]+3,nrows = as.integer(row.y[2]-row.x[2]-4),header = F )
x <- x[,1:ncol(x)-1]
#     read y data
y <- read.csv(fileName, skip = row.y[2]+3,nrows = as.integer(row.z[2]-row.y[2]-4),header = F )
y <- y[,1:ncol(y)-1]
#     read z data
#z <- read.csv(fileName, skip = row.z[2]+3,header = F )
#z <- z[,1:ncol(z)-1]

#     create filename, set working directory
setwd( dirname(fileName))
#     step through time intervals
for(i in 1:length(times)){
    # make dataframe for write-out
      dat <- data.frame(X=x[,i], Y=y[,i])  
    #     create filename
        out.file <- paste0(sub(".csv","",basename(fileName)),"_",as.character(times[i]),".xlsx")
    write.xlsx2(dat,file=out.file,sheetName=as.character(times[i]),row.names=F)  
      
}