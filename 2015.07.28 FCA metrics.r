FCA_Metrics <- function(dat){
#     This script uses the results of "2015.07.27 combine front and side TI files.r" to calculate 
#     defined cushion stability metrics per Fiat Chrysler (FCA) definitions, passed to function as "dat"
      
      #     truncate data from 30-40ms
      dat <- dat[dat$Time >= 30 & dat$Time <= 40,]
      # offset is 105mm to theoretic center of cushion
      deltaT <- abs((dat$PT1_X_F+dat$PT2_X_F)/2-105)
      L <- sqrt((dat$PT1_X_F-dat$PT2_X_F)^2+(dat$PT1_Y_F-dat$PT2_Y_F)^2)
      deltaX <- abs(dat$PT1_X_F-dat$PT2_X_F)
      Rotation <- (deltaX*dat$Height_F)/(2*L)
      Combined <- Rotation + deltaT
      
      #     Compile into dataframe
      FCA_Metrics <- data.frame(Trans_Max = max(deltaT), Trans30 = deltaT[1], Trans40 = deltaT[length(deltaT)],
                  Rot_Max = max(Rotation), Rot30 = Rotation[1], Rot40 = Rotation[length(Rotation)],
                  Comb_Max = max(Combined), Comb30 = Combined[1], Comb40 = Combined[length(Combined)])
                  
      
      return(FCA_Metrics)
}