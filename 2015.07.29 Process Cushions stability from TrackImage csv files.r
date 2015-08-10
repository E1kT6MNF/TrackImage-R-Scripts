#     This script will read TrackImage .csv files and output cushion stabiliy metrics
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
#           4.    The stability results should be stored on your Windows clipboard. Open an Excel
#                 file, right mouse click, Paste.

#     load function scripts
source("S:\\OTCCommon\\AD_Reliability\\RAT\\Projects\\John\\R Scripts\\TrackImage Scripts\\2015.07.27 combine front and side TI files.r")
source("S:\\OTCCommon\\AD_Reliability\\RAT\\Projects\\John\\R Scripts\\TrackImage Scripts\\2017.07.28 Cushion stability metrics.r")
#
#     Read TrackImage .csv files, put in "dat2"
dat2 <- CombineFS()
#     pass stacked data to metrics function
dat3 <- StabMetrics(dat2)
#
#     End