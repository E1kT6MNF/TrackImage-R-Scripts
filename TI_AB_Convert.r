TI_AB_Convert <- function(TI_dir){
#     This script reads TrackImage Airbag_2D .csv files, strips out the extraneous columns
#     renames the columns with units appended on end of name.
#     Save a stacked data set into a .csv or Excel file
# Rev history
#     2015.06.02 modifiy to auto create "Output" directory
#     2015.06.04 converted to a function with directory location as only arguement.
#     
source("S:\\OTCCommon\\AD_Reliability\\RAT\\Projects\\John\\R Scripts\\TrackImage Scripts\\ReadTI_Traj_2D_v 3.1.2.R")

# Set directory where TrackImage files are located.  
TI_dir <- choose.dir(default = basePath, caption = "Select folder containing TrackImage files")
# make list of all .csv file in target directory
TI_List <- dir(path=TI_dir,pattern="*.csv",full.names=T,recursive=F)

#     set directory for storage of output files
#     2015.02.02 rev: create "Output" folder within file folder
outPath <- paste0(TI_dir,"\\Output")

      #     if directory already exists, don't make one otherwise...
      if(!dir.exists(outPath)){dir.create(outPath, showWarnings = TRUE, recursive = FALSE, mode = "0777")}     
     
setwd(outPath)


# create empty dataframe for stacked data
      dat2 <- data.frame()
# Start For loop to read and process files
for (i in 1:length(TI_List)) {
      fileName <- TI_List[i]
      data <- readTI_AB(fileName)
#     extract test number from file name
      test <- gsub(".csv","",gsub(".*/","",fileName))
      Test_Number <- rep(test, nrow(data)) 
#     Add test number column
      data <- cbind(Test_Number, data)
# Stack data set
      dat2 <- rbind(dat2, data)
}
# Write csv file
write.csv(dat2, file=paste0(basename(TI_dir),".csv"),row.names=FALSE, na="") 
}