CombineFS <- function(){
#
#
library(tidyr)
source("S:\\OTCCommon\\AD_Reliability\\RAT\\Projects\\John\\R Scripts\\TrackImage Scripts\\ReadTI_Traj_2D_v 3.1.2.R")
basePath <- getwd()
# Set directory where TrackImage files are located.  
TI_dir <- choose.dir(default = basePath, caption = "Select folder containing TrackImage .csv files")
# make list of all .csv file in target directory, put in dataframe
# TI_List <- dir(path=TI_dir,pattern=glob2rx("*-*.csv"),full.names=T,recursive=F)
tests <- data.frame(FilePath = dir(path=TI_dir,pattern=glob2rx("*-*.csv"),full.names=T,recursive=F) )
#     extract simple file name
tests$File <- gsub(".csv","",gsub(".*/","",tests$FilePath))
# Extract video view orientation and test number
tests$Test <- as.factor(substr(tests$File,start = 3, stop = nchar(tests$File)))
tests$View <- as.factor(substr(tests$File,start = 1, stop = 1))
#     remove unneeded "File" column
tests <- tests[,c(1,3,4)]
#     unstack data
tests <- spread(tests, View, FilePath)
#     sort data
##
outPath <- paste0(TI_dir,"\\Output")

#     if directory already exists, don't make one otherwise...
if(!dir.exists(outPath)){dir.create(outPath, showWarnings = TRUE, recursive = FALSE, mode = "0777")}     

setwd(outPath)


# create empty dataframe for stacked data
dat2 <- data.frame()
# Start For loop to read and process files
for (i in 1:nrow(tests)) {
      #     read front and side files
      datF <- readTI_AB(tests$F[i])
      datS <- readTI_AB(tests$S[i])
  
      # append _F to end of all colun names except time
      names(datF)[-1] <- paste0(names(datF[2:ncol(datF)]),"_F")   
      # append _F to end of all colun names except time
      names(datS)[-1] <- paste0(names(datS[2:ncol(datS)]),"_S")  
      #     merge dataframes by Time
      dat <- merge(datF,datS,by = "Time")
      #     create column with test number
            Test <- rep(tests$Test[i], nrow(dat)) 
      #     Add test number column
      dat <- cbind(Test, dat)
      # Stack data set
      dat2 <- rbind(dat2, dat)
}
# Write csv file
write.csv(dat2, file=paste0(basename(TI_dir),".csv"),row.names=FALSE, na="") 
return(dat2)
}
