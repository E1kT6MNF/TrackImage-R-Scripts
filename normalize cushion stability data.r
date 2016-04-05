#  This script will normalizes TrackImage stability data against a baseline configuration
#  Author: John Newkirk
#  Rev. History:
#     2016.03.28 initial script 
#  Requirements: stacked data set from JMP saved as .csv file 
library(data.table)
#     read .csv file with data
filename <- choose.files(default = "*.csv")
setwd <- dirname(filename)
dat <- fread(filename)
#     remove missing data
dat <- na.omit(dat)
#  make time level as factor data type
dat$Time <- as.factor(dat$Time)
#  make array of Time levels
times <- unique(dat$Time)
#  make loop diffuser data as factor
dat$Condition <- as.factor(dat$Condition)
#  make array of conditions
condition <- unique(dat$Condition)
#  define directory to store results
outdir <- choose.dir(default = filedir, caption = "Select folder")
setwd(outdir)
#  define baseline configuration
BL <- as.character(condition[1])
#  define columns for normalization
metric_cols <- seq(4,ncol(dat))
# make empty dataframe for result
norm_dat <- data.frame()
# for loop to step through time level
for (i in 1:length(times)){
   # subset dat to time i
   dat_time <- subset(dat,Time == times[i])
   # step through response columns
   for (j in 1:length(metric_cols)){
      # calculate mean of baseline
      colnum <-  metric_cols[j];
      mean_j = mean(dat_time[[colnum]][dat$Condition == BL],na.rm = T)
      dat_time[[colnum]] <- dat_time[[colnum]]/mean_j
   }# end for j
   norm_dat <- rbind(norm_dat, dat_time)    
}# end for i 
#  write file into same directory as source file with 'Normalized' prepended to file name 
write.csv(norm_dat, file = paste("Normalized", basename(filename)), row.names = FALSE)