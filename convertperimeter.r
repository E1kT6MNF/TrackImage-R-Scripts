#     this script converts TrackReport perimeter point cloud data from weird .csv
#     to Excel sheets for each time interval in x,y pairs
#
library(xlsx)
#
# USER INPUT: LIST TIMES TO WRITE(SEPARATED BY COMMAS)
time_list = c(seq(from = 10,to = 100,by = 10), 142.5)
#

#     Choose TrackReport File
fileName <- choose.files(default = "*.csv",caption = "Select TrackReport file")
# read file extract field separator and find rows where x,y,z
# data start.
header <- readLines(fileName,n=-1)
sep_field <- header[grep("SEPARATOR:",header)]
sep = substr(sep_field,nchar(sep_field),nchar(sep_field))
row.x <- grep("*.x",header) 
row.y <- grep("*.y",header) 
row.z <- grep("*.z",header) 
#Extact time steps from TrackImage file
times <-  read.delim(fileName, sep = sep, skip = row.x , nrows = 1,header = F)
# drop extra column from trailing comma
times <- times[1:ncol(times)-1]
#     read x data
x <- read.delim(fileName, skip = row.x+3,nrows = as.integer(row.y-row.x-4),header = F, sep = sep)
x <- x[,1:ncol(x)-1]
#     read y data
y <- read.delim(fileName, skip = row.y+3,nrows = as.integer(row.z-row.y-4),header = F , sep = sep)
y <- y[,1:ncol(y)-1]
#     read z data
#z <- read.csv(fileName, skip = row.z+3,header = F )
#z <- z[,1:ncol(z)-1]

#  create directory path for output files
outPath <- paste0(dirname(fileName),"/",sub(".csv","",basename(fileName))," Output")
#  if directory already exists, don't make one otherwise...
if(!dir.exists(outPath)){dir.create(outPath, showWarnings = TRUE, recursive = FALSE, mode = "0777")}     
#  change working directory to out path
setwd(outPath)
# create vector of time elements for For loop
inds <- which(times %in% time_list)
#     step through time intervals
for(i in inds){
      # make dataframe for write-out
      dat <- data.frame(X=x[,i], Y=y[,i])  
      # create filename
      out.file <- paste0(sub(".csv","",basename(fileName)),"_",as.character(times[i]),".xlsx")
      #  write Excel file for time "i"
      write.xlsx2(dat,file=out.file,sheetName=as.character(times[i]),row.names=F)  
} # end for

