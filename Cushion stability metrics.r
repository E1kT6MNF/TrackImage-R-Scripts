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
   #  This script uses the results of "2015.07.27 combine front and side TI files.r" to calculate 
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