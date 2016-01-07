#  This script will read TrackImage .csv files and output cushion stabiliy metrics
#  Author: John Newkirk
#  Rev. History:
#     1/8/16   appended 2015.12.03 Cushion stability metrics.r and 
#              2015.09.02 combine front and side TI files.r to avoid finding external functions
#     
#  Requirements:
    #     1.    Be sure there are front and side .csv files for all test with no extras. Remove any
    #           other .csv files from the directory
    #     2.    Side view files must be named "S_test number.csv". (e.g., S_55A550-01.csv)
    #           Front view files must be named "F_test number.csv". (e.g., F_55A550-01.csv)
#  Instructions:
#   1. Run the following lines of code. Easiest way is Control-A to select everything, then
#      select "Run" button in upper-right corner of RStudio script editing window.
#   2. You will be prompted to select the directory where TrackImage .csv files are stored.
#      You might have to look for it on the Windows task bar if the file selection window
#      doesn't open to the front screen. You can navigate within the file selection window
#      but it is easier to use the "Copy address as text" function in Windows Explorer to
#      copy/paste the directory path into the R file window.
#   3. R will read the files, combine the front and side view data, and write a .csv file
#      stacked data set into a subdirectory named "Output".
#   4. The stability results will be written to a .csv file in the "Output" folder

# Read TrackImage .csv files
res <- CombineFS()
#
#  Optional line to read existing stacked .csv file: dat2 <- read.csv(choose.files())
#  Pass stacked TrackImage data through metrics function
#  arguments: 
#  dat2 = dataframe containing stacked TI data (should alway = dat2)
#  User provided arguements
offset<- 105  # offset = distance from origin to center of cushion (used for FCA spec., typ. 100-110)
LTL <- 20   # LTL= lower time limit for stability evaluation (For Toyota = 20)
UTL <- 75   # UTL = upper time limits for stability evaluation (For Toyota = 75)
ST1 <- 11   # ST1 is min time for slope calculation (need to graph data and make judgement)
ST2 <- 19.5 # ST2 is max time for slope calculation (need to graph data and make judgement)
Edge_side <- "L"  # Side of cushion leading edge(depends on camera position). 
                  # Options are "L" and "R". MUST BE IN QUOTES
#
#  Run metrics function
dat3 <- StabMetrics(dat2=res$data,offset=offset, LTL=LTL, UTL=UTL,ST1=ST1,ST2=ST2,
                    Src=res$Source,Qty=res$Quantity, Edge_side=Edge_side)
#
#  optional to write data table  to clipboard:
#  Select rest of line and hit "Run" button -> write.table(dat3, file='clipboard', sep='\t',row.names=FALSE)
#  End script
#
#  DON'T CHANGE ANYTHING BELOW THIS LINE!!
#    
#   ( )                 ( )_
# (_, |      __ __      | ,_)
#    \'\    /  ^  \    /'/
#     '\'\,/\      \,/'/'
#       '\| []   [] |/'
#         (_  /^\  _)
#           \  ~  /
#           /HHHHH\
#         /'/{^^^}\'\
#      _,/'/'  ^^^  '\'\,_
#     (_, |           | ,_)
#       (_)           (_)
#       
CombineFS <- function(){
   #  This script reads front and side view TrackImage .csv files and merges them into an single 
   #  combined data table.
   #  Author: John Newkirk
   #  Revision History
   # 09/02/15 changed output to an object containing the source directory, number of tests,
   #           results dataframe.
   library(tidyr)
   #  read function for reading and cleaning TrackImage .csv files
   basePath <- getwd()
   #  Set directory where TrackImage files are located.  
   TI_dir <- choose.dir(default = basePath, caption = "Select folder containing TrackImage .csv files")
   #  make list of all .csv file in target directory, put in dataframe
   #  TI_List <- dir(path=TI_dir,pattern=glob2rx("*-*.csv"),full.names=T,recursive=F)
   tests <- data.frame(FilePath = dir(path=TI_dir,pattern=glob2rx("*-*.csv"),full.names=T,recursive=F) )
   #  extract simple file name
   tests$File <- gsub(".csv","",gsub(".*/","",tests$FilePath))
   #  Extract video view orientation and test number
   tests$Test <- as.factor(substr(tests$File,start = 3, stop = nchar(tests$File)))
   tests$View <- as.factor(substr(tests$File,start = 1, stop = 1))
   #  remove unneeded "File" column
   tests <- tests[,c(1,3,4)]
   #  unstack data
   tests <- spread(tests, View, FilePath)
   #  sort data
   #  create directory path for output files
   outPath <- paste0(TI_dir,"\\Output")
   #  if directory already exists, don't make one otherwise...
   if(!dir.exists(outPath)){dir.create(outPath, showWarnings = TRUE, recursive = FALSE, mode = "0777")}     
   #  change working directory to out path
   setwd(outPath)
   #
   #  create empty dataframe for stacked data
   dat2 <- data.frame()
   #  Start For loop to read and process files
   for (i in 1:nrow(tests)) {
      #  read front and side files
      datF <- readTI_AB(tests$F[i])
      datS <- readTI_AB(tests$S[i])
      #  append _F to end of all column names except time
      names(datF)[-1] <- paste0(names(datF[2:ncol(datF)]),"_F")   
      #  append _F to end of all colun names except time
      names(datS)[-1] <- paste0(names(datS[2:ncol(datS)]),"_S")  
      #  merge dataframes by Time
      dat <- merge(datF,datS,by = "Time")
      #  create column with test number
      Test <- rep(tests$Test[i], nrow(dat)) 
      #  Add test number column
      dat <- cbind(Test, dat)
      # Stack data set
      dat2 <- rbind(dat2, dat)
   }#  end for
   #  Write csv file
   write.csv(dat2, file=paste0(basename(TI_dir),".csv"),row.names=FALSE, na="") 
   #  Make object with results and traceability data
   res <- list(Source = normalizePath(TI_dir,winslash = "/", mustWork = NA),Quantity = nrow(tests),data=dat2)
   class(res) <- "TI_Table"
   return(res)
} #  end function
#
####
readTI_AB <- function(fileName){
   #  This function reads and formats data from TrackImage Aibag files returns a cleaned dataframe
   #  THIS WORKS WITH TRACKiMAGE VERSION: 3.1.2
   #  Revision History:
   #     1/7/2016 added detection of field separator rather than hard-coding ";"
   #  Author: John Newkirk      
   # 
   # read header, find end of header, find field separator
   header <- readLines(fileName,n=-1)
   infoRows <- grep("HEADER|CHANNELS",header)
   sep_field <- header[grep("SEPARATOR:",header)]
   sep = substr(sep_field,nchar(sep_field),nchar(sep_field))
   rm(header)
   #
   #  Extact file names from TrackImage file
   header_names <-  read.delim(fileName, sep = sep, skip = infoRows[2] , nrows = 1,header = F,
                               stringsAsFactors =F)
   #  drop extra column from trailing semi-colon
   header_names <- header_names[1:ncol(header_names)-1]
   #  Rename 1st time column
   header_names[1] <- "Time"
   #  Extact column units from TrackImage file
   units <-  read.delim(fileName, sep = sep, skip = infoRows[2]+1 , nrows = 1,header = F,
                        stringsAsFactors =F)
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
   dat2 <- read.delim(fileName, sep = sep, skip = infoRows[2]+2, header = F)
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
####
StabMetrics <- function(dat2,offset, LTL, UTL,ST1,ST2,Src,Qty, Edge_side){
   #  This script uses the results of "2015.07.27 combine front and side TI files.r" to calculate 
   #  defined cushion stability metrics
   #  Author: John Newkirk  
   #  Rev. History:
   #  8/11/15 added arguments for offset, Slope Lower Time Limit, Slope Upper Time Limit
   #  dat2 <-  read.csv(choose.files())
   #  12/3/2015 appended FCA metrics function to this file for simplification
   #  added arguement for L/R camera side view.
   #  Appended function my.write for simplicity
   #  load libraries
   
   
   #  check for proper arguements
   #  
   #  if( nargs() !=9) stop("fIncorrect number of arguement.\n") 
   #  check for proper edge arguement
   
   if(Edge_side !="L" & Edge_side != "R") stop("Improper value for arguement Edge.\n")
   #
   #  set edge variable
   if (Edge_side == "L"){
      keepers <- c("Test","Time","Height_F","PT1_X_F",
                   "PT1_Y_F","PT2_X_F","PT2_Y_F",
                   "X_left_edge_S","Y_lower_edge_S")
      dat2 <- dat2[keepers]
      names(dat2) <- gsub("X_left_edge_S","Edge",names(dat2))
   } else {
      keepers <- c("Test","Time","Height_F","PT1_X_F",
                   "PT1_Y_F","PT2_X_F","PT2_Y_F",
                   "X_right_edge_S","Y_lower_edge_S")
      dat2 <- dat2[keepers]
      names(dat2) <- gsub("X_right_edge_S","Edge",names(dat2))
   }#end if
   
   #  calculate metrics by Test and put in dataframe 
   metrics1 <- tapply(1:nrow(dat2),dat2$Test,function(i) {
      dat <- dat2[i,]
      #  truncate data from LTL to UTL
      dat3 <- dat[dat$Time >= LTL & dat$Time <= UTL,]
      #  calculate the 20-75ms metrics
      Stability_X <- diff(range(dat3$PT1_X_F, na.rm=TRUE))
      Stability_Y <- diff(range(dat3$PT1_Y_F, na.rm=TRUE))
      Skew_Angle <- 57.2957795*atan((dat3$PT1_X_F-dat3$PT2_X_F)/(dat3$PT1_Y_F-dat3$PT2_Y_F))
      Skew_Angle_Range <-diff(range(Skew_Angle))
      #  calculate the untruncated time metrics
      Edge_X_Max <- max(abs(dat$Edge), na.rm = TRUE)
      Edge_X_Time <- dat$Time[which.max(abs(dat$Edge))]
      Edge_700X_Time <- min(dat$Time[abs(dat$Edge)>= 700])
      Y_Lower_Edge <- max(abs(dat$Y_lower_edge_S), na.rm = TRUE)
      Y_Lower_Edge_Time <- dat$Time[which.max(abs(dat$Y_lower_edge_S))]
      In_Pos_Time <- min(dat$Time[abs(dat$Y_lower_edge_S)>= 190])
      #     Calculate bottom drop slope
      #truncate data from ST1 to ST2
      dat4 <-dat[dat$Time >= ST1 & dat$Time <= ST2,]
      mod <- lm(Y_lower_edge_S~Edge, dat4)
      #      
      #  get FCA metrics
      FCA <- FCA_Metrics(dat=dat,offset=offset)
      #  compile metrics into dataframe
      metrics2 <- data.frame(Test=dat2$Test[i[1]],Stability_X_mm = Stability_X,Stability_Y_mm = Stability_Y,
                             Skew_Angle_Min_deg = min(Skew_Angle),Skew_Angle_Max_deg = max(Skew_Angle),
                             Skew_Angle_Range_deg = Skew_Angle_Range, Edge_Max_mm = Edge_X_Max,
                             Time_Edge_Max_ms = Edge_X_Time,Time_Edge700mm_ms = Edge_700X_Time,
                             Lower_Edge_Max_mm = Y_Lower_Edge, Time_Lower_Edge_Max_ms = Y_Lower_Edge_Time,
                             In_Position_Time_ms = In_Pos_Time, Slope=mod$coefficients[2], 
                             R_sqr=summary(mod)$r.squared)
      #  combine base metrics and FCA metrics
      data.frame(cbind(metrics2,FCA))
   })# end of tapply
   
   #  unlist tapply output 
   metrics3 <- as.data.frame(do.call("rbind",metrics1))
   
   #  rename columns to more presentable names
   row.names(metrics3)<-NULL
   #  .csv file of names is stored in S:\OTCCommon\AD_Reliability\RAT\Projects\John\R Scripts\TrackImage Scripts\Fold Analysis script stuff
   names(metrics3) <- c("Test ID","Stability X (mm)","Stability Y (mm)","Skew Angle Left Min (deg)",
                        "Skew Angle Right Max (deg)","Skew Angle Range (deg)","Left Edge X (mm)",
                        "Left Edge X (ms)","Left Edge X at -700mm (ms)","Lower Edge Y (mm)",
                        "Lower Edge Y (ms)","Lower Edge Y at 190mm (ms)","LftX vs LowY Slope",
                        "Slope R^2","FCA Trans Max (mm)","FCA Trans at LTL (mm)",
                        "FCA Trans at UTL (mm)","FCA Rot Max (mm)","FCA Rot at LTL (mm)",
                        "FCA Rot at UTL (mm)","FCA Specification (mm)","FCA Combined at LTL (mm)",
                        "FCA Combined at UTL (mm)")
   # 
   #
   #    create matrix of header information
   header <- matrix(c(paste("Source =",Src),
                      paste("Test Quantity =",Qty),
                      paste("Offset =", offset),
                      paste("LTL = ", LTL),
                      paste("UTL = ", UTL),
                      paste("ST1 = ", ST1),
                      paste("ST2 = ", ST2),
                      paste("Edge_side = ",Edge_side),
                      "End of Header"),
                    byrow = TRUE)
   #  write header and results table to .csv file
   my.write(dat= metrics3, file = basename(res$Source), header = header) 
   #     return dataframe
   return(metrics3)
}#  end function

####
#  function to process FCA stability metrics

FCA_Metrics <- function(dat, offset){
   #  This script uses the results of CombineFS to calculate 
   #  defined cushion stability metrics per Fiat Chrysler (FCA) definitions, passed to function as "dat"
   #  Author: John Newkirk
   #  Rev. history:
   # added argument for offset
   # truncate data from 30-40ms
   dat <- dat[dat$Time >= 30 & dat$Time <= 40,]
   # offset is from origin to theoretic center of cushion
   # following formulas are taken from John Sabin presentation
   # S:\AAMCommon\Engineering\Product Reliability\Sabin\Projects\TrackImage\TRI Projects\Chrysler Stability\FCA Stability Notes.pptx
   deltaT <- abs((dat$PT1_X_F+dat$PT2_X_F)/2-offset)
   L <- sqrt((dat$PT1_X_F-dat$PT2_X_F)^2+(dat$PT1_Y_F-dat$PT2_Y_F)^2)
   deltaX <- abs(dat$PT1_X_F-dat$PT2_X_F)
   Rotation <- (deltaX*dat$Height_F)/(2*L)
   Combined <- Rotation + deltaT
   
   #  Compile into dataframe
   FCA_Metrics <- data.frame(Trans_Max = max(deltaT), Trans30 = deltaT[1], Trans40 = deltaT[length(deltaT)],
                             Rot_Max = max(Rotation), Rot30 = Rotation[1], Rot40 = Rotation[length(Rotation)],
                             Comb_Max = max(Combined), Comb30 = Combined[1], Comb40 = Combined[length(Combined)])
   
   
   return(FCA_Metrics)
}# end function

##
#  function to write results to file
#  
my.write <- function(dat, file, header){
   #  this function writes a .csv file consisting of 
   #  1. a header section
   #  2. data table
   #  Author: John Newkirk 
   #  
   #  create and open the file connection
   datafile <- file(paste0(file," results table.csv"), open = 'wt')
   # close on exit
   on.exit(close(datafile))
   #  if a header is defined, write it to the file
   if(!missing(header)) writeLines(header,con=datafile)
   #  write the file using the defined function and required addition arguments  
   write.csv(dat, datafile,row.names = F)
}#  end function