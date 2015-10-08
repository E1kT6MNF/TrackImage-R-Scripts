my.write <- function(dat, file, header){
      # create and open the file connection
      datafile <- file(paste0(file," results table.csv"), open = 'wt')
      # close on exit
      on.exit(close(datafile))
      # if a header is defined, write it to the file (@CarlWitthoft's suggestion)
      if(!missing(header)) writeLines(header,con=datafile)
      # write the file using the defined function and required addition arguments  
      write.csv(dat, datafile,row.names = F)
}