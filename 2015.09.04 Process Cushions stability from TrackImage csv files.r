#     This script will read TrackImage .csv files and output cushion stabiliy metrics
#     Rev. History:
      #     08/11/15 added arguments for offset, Slope Lower Time Limit, Slope Upper Time Limit
      #     09/01/15 1. Moved arguement entry to line items instead of within function call
      #              2. changed output form of results table from clipboard to .csv file
      #              3. added a header section to results table listing file source and parameters used.
#     Requirements:
      #     1.    Be sure there are front and side .csv files for all test with no extras. Remove any
      #           other .csv files from the directory
      #     2.    Side view files must be named "S_test number.csv". (e.g., S_55A550-01.csv)
      #           Front view files must be named "F_test number.csv". (e.g., F_55A550-01.csv)
#     Instructions:
#           1.    Run the following lines of code. Easiest way is Control-A to select everything, then
#                 select "Run" button in upper-right corner of RStudio script editing window.
#           2.    You will be prompted to select the directory where TrackImage .csv files are stored.
#                 You might have to look for it on the Windows task bar if the file selection window
#                 doesn't open to the front screen. You can navigate within the file selection window
#                 but it is easier to use the "Copy address as text" function in Windows Explorer to
#                 copy/paste the directory path into the R file window.
#           3.    R will read the files, combine the front and side view data, and write a .csv file
#                 stacked data set into a subdirectory named "Output".
#           4.    The stability results will be written to a .csv file in the "Output" folder

#     load function scripts
source("S:\\OTCCommon\\AD_Reliability\\RAT\\Projects\\John\\R Scripts\\TrackImage Scripts\\2015.09.02 combine front and side TI files.r")
source("S:\\OTCCommon\\AD_Reliability\\RAT\\Projects\\John\\R Scripts\\TrackImage Scripts\\2015.09.04 Cushion stability metrics.r")
#
#     Read TrackImage .csv files, put in "dat2"

res <- CombineFS()
#
#     Optional line to read existing stacked .csv file: dat2 <- read.csv(choose.files())
#     Pass stacked TrackImage data through metrics function
#     arguments: 
#     dat2 = dataframe containing stacked TI data (should alway = dat2)
#     User provided arguements
offset<- 105  # offset = distance from origin to center of cushion (used for FCA spec., typ. 100-110)
LTL   <- 20   # LTL= lower time limit for stability evaluation (For Toyota = 20)
UTL   <- 75   # UTL = upper time limits for stability evaluation (For Toyota = 75)
ST1   <- 11   # ST1 is min time for slope calculation (need to graph data and make judgement)
ST2   <- 19.5 # ST2 is max time for slope calculation (need to graph data and make judgement)
#
#     Run metrics function
dat3 <- StabMetrics(dat2=res$data,offset=offset, LTL=LTL, UTL=UTL,ST1=ST1,ST2=ST2,Src=res$Source,Qty=res$Quantity)
#
#     optional to write data table  to clipboard:
#     Select rest of line and hit "Run" button -> write.table(dat3, file='clipboard', sep='\t',row.names=FALSE)
#     End