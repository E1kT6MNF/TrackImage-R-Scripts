readTI_AB <- function(fileName){
#  This function reads and formats data from TrackImage Aibag files returns a cleaned dataframe
#  THIS WORKS WITH TRACKiMAGE VERSION: 3.1.2
#  Author: John Newkirk      
# Find header length (rows)
header <- readLines(fileName,n=200)
infoRows <- grep("HEADER|CHANNELS",header) 
#
#  Extact file names from TrackImage file
header_names <-  read.delim(fileName, sep = ";", skip = infoRows[2] , nrows = 1,header = F,stringsAsFactors =F)
#  drop extra column from trailing semi-colon
header_names <- header_names[1:ncol(header_names)-1]
#  Rename 1st time column
header_names[1] <- "Time"
#  Extact column units from TrackImage file
units <-  read.delim(fileName, sep = ";", skip = infoRows[2]+1 , nrows = 1,header = F,stringsAsFactors =F)
#  drop extra column from trailing semi-colon
units <- units[1:ncol(units)-1]
#  identify columns without ".x" 
not_dotx <- substr(header_names[1,], nchar(header_names[1,])-1,nchar(header_names[1,])) != ".x"
#  identify columns to keep
Airbag2D <- grepl("*Airbag2D*",header_names)
world <- grepl("*world_position*",header_names)
keepers <- as.logical(Airbag2D == TRUE | world == TRUE)
#  Set 1st column "Time" as a keeper
keepers[1] <- TRUE
#  combine logical vectors of keepers and not .x
keepers2 <- keepers == TRUE & not_dotx == TRUE
#  Read data
dat2 <- read.delim(fileName, sep = ";", skip = infoRows[2]+2, header = F)
#  drop extra column from trailing semi-colon
dat2 <- dat2[1:ncol(dat2)-1]
#  subset dataframe to "keepers"
dat2 <-dat2[,keepers2 == TRUE]
units <- units[keepers2 == TRUE]
header_names <- header_names[keepers2 == TRUE]
#  rename step 1: get rid of "Airbag2D_"
newNames <- gsub("Airbag2D_","", header_names)
# rename step 2: get rid of "world_position_"
newNames <- gsub("_world_position","",newNames)
#  rename step 3: Get rid of ".y"
newNames <- gsub(".y","", newNames, fixed = TRUE)
#  append units to file names 
#  newNames <- paste0(newNames,"_(",units,")") # un-comment this line for units in name for JMP, etc
#  rename data columns
names(dat2) <- newNames
#  clean up 001_ from column names
names(dat2) <- sub("001_","",names(dat2))
#  Convert to ms (uncomment next line as needed data is in seconds)
#  dat2$Time_ms <- dat2$Time_ms/1000
return(dat2)
}# end function