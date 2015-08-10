StabMetrics <- function(dat2){
#     This script uses the results of "2015.07.27 combine front and side TI files.r" to calculate 
#     defined cushion stability metrics
#
#   dat2 <-  read.csv(choose.files())
source("S:\\OTCCommon\\AD_Reliability\\RAT\\Projects\\John\\R Scripts\\TrackImage Scripts\\2015.07.28 FCA metrics.r")
#
#     set truncation points for  (time in ms)
      u_tlimit <- 19.5
      l_tlimit <- 11
 
 metrics1 <- tapply(1:nrow(dat2),dat2$Test,function(i) {
       dat <- dat2[i,]
 dat3 <- dat[dat$Time >= 20 & dat$Time <= 75,]
 #    calculate the 20-75ms metrics
      Stability_X <- diff(range(dat3$PT1_X_F, na.rm=TRUE))
      Stability_Y <- diff(range(dat3$PT1_Y_F, na.rm=TRUE))
      Skew_Angle <- 57.2957795*atan((dat3$PT1_X_F-dat3$PT2_X_F)/(dat3$PT1_Y_F-dat3$PT2_Y_F))
      Skew_Angle_Range <-diff(range(Skew_Angle))
      #     calculate the untruncated time metrics
      Left_Edge_X_Max <- max(abs(dat$X_left_edge_S), na.rm = TRUE)
      Left_Edge_X_Time <- dat$Time[which.max(abs(dat$X_left_edge_S))]
      Left_Edge_700X_Time <- min(dat$Time[abs(dat$X_left_edge_S)>= 700])
      Y_Lower_Edge <- max(abs(dat$Y_lower_edge_S), na.rm = TRUE)
      Y_Lower_Edge_Time <- dat3$Time[which.max(abs(dat3$Y_lower_edge_S))]
      In_Pos_Time <- min(dat$Time[abs(dat$Y_lower_edge_S)>= 190])
#     Calculate bottom deop slope
      dat4 <-dat[dat$Time >= l_tlimit & dat$Time <= u_tlimit,]
      mod <- lm(Y_lower_edge_S~X_left_edge_S, dat4)
      
      # get FCA metrics
      FCA <- FCA_Metrics(dat)
      #     compile metrics into dataframe
      metrics2 <- data.frame(Test=dat2$Test[i[1]],Stability_X_mm = Stability_X,Stability_Y_mm = Stability_Y,
                             Skew_Angle_Min_deg = min(Skew_Angle),Skew_Angle_Max_deg = max(Skew_Angle),
                             Skew_Angle_Range_deg = Skew_Angle_Range, Left_Edge_Max_mm = Left_Edge_X_Max,
                             Time_Left_Edge_Max_ms = Left_Edge_X_Time,Time_Left_Edge700mm_ms = Left_Edge_700X_Time,
                             Lower_Edge_Max_mm = Y_Lower_Edge, Time_Lower_Edge_Max_ms = Y_Lower_Edge_Time,
                             In_Position_Time_ms = In_Pos_Time, Slope=mod$coefficients[2], 
                             R_sqr=summary(mod)$r.squared)
      data.frame(cbind(metrics2,FCA))
 })
 
 metrics3 <- as.data.frame(do.call("rbind",metrics1))
 row.names(metrics3)<-NULL
 
 #    rename columns to more presentable names
 #    .csv file of names is stored in S:\OTCCommon\AD_Reliability\RAT\Projects\John\R Scripts\TrackImage Scripts\Fold Analysis script stuff
 names(metrics3) <- c("Test ID","Stability X (mm)","Stability Y (mm)","Skew Angle Left Min (deg)",
                      "Skew Angle Right Max (deg)","Skew Angle Range (deg)","Left Edge X (mm)",
                      "Left Edge X (ms)","Left Edge X at -700mm (ms)","Lower Edge Y (mm)",
                      "Lower Edge Y (ms)","Lower Edge Y at 190mm (ms)","LftX vs LowY Slope",
                      "Slope R^2","FCA Trans Max (mm)","FCA Trans at LTL (mm)",
                      "FCA Trans at UTL (mm)","FCA Rot Max (mm)","FCA Rot at LTL (mm)",
                      "FCA Rot at UTL (mm)","FCA Specification (mm)","FCA Combined at LTL (mm)",
                      "FCA Combined at UTL (mm)")
 
 # write table  to clipboard
      write.table(metrics3, file='clipboard', sep='\t',row.names=FALSE)
#     return dataframe
      return(metrics3)
}    
 