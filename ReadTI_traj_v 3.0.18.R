readTI <- function(fileName){
      # This function reads and formats data from TrackImage Trajectory files
      # returns a dataframe
      #     THIS WORKS WITH tRACKiMAGE vERSION 3.0.18
      
# Find header length (rows)
      header <- readLines(fileName,n=200)
      infoRows <- grep("HEADER|CHANNELS",header) 
#
#Extact file names from TrackImage file
      header_names <-  read.delim(fileName, sep = ";", skip = infoRows[2] , nrows = 1,header = F,stringsAsFactors =F)
# drop extra column from trailing semi-colon
      header_names <- header_names[1:ncol(header_names)-1]
# Rename 1st time column
      header_names[1] <- "Time"
#Extact column units from TrackImage file
      units <-  read.delim(fileName, sep = ";", skip = infoRows[2]+1 , nrows = 1,header = F,stringsAsFactors =F)
# drop extra column from trailing semi-colon
      units <- units[1:ncol(units)-1]
# identify columns with ".x" 
      dotx <- substr(header_names[1,], nchar(header_names[1,])-1,nchar(header_names[1,])) == ".x"
      world <- grepl("*world*",header_names)
# Remove the ".x" names
      keepers <- dotx == FALSE & world == TRUE | header_names== "Time"
# Read data
      dat2 <- read.delim(fileName, sep = ";", skip = infoRows[2]+2, header = F)
# drop extra column from trailing semi-colon
      dat2 <- dat2[1:ncol(dat2)-1]
      dat2 <-dat2[,keepers== TRUE]
      units <- units[keepers== TRUE]
      header_names <- header_names[keepers== TRUE]

# rename step 1
      newNames <- gsub("_world_position.y","", header_names)
# append units to file names
      newNames <- paste0(newNames,"_",units)
#     rename data columns
      names(dat2) <- newNames
# Convert to ms
      dat2$Time_ms <- dat2$Time_ms/1000

return(dat2)

}