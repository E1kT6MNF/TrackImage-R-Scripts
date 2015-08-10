dat <- read.delim('clipboard')
names(dat)[1] <- "Time"
library("reshape2")
library("tidyr")
dat2 <- melt(dat, id.vars = "Time", measure.vars = 2:ncol(dat))
names(dat2) <- c("Time", "ID", "Data")
dat2$ID <- as.character(dat2$ID)
#     make Axis and Test columns based on ID substring
dat2$Axis <- as.factor(substr(dat2$ID, nchar(dat2$ID), nchar(dat2$ID)))
dat2$Test <- as.factor(substr(dat2$ID, 1, nchar(dat2$ID)-2))
# drop unneeded columns
dat2 <- dat2[!names(dat2) %in% "ID"]
#     unstack data
dat3 <- spread(dat2, Axis, Data)
#     sort by test, time
dat3 <-dat3[order(dat3$Test, dat3$Time),]
#     calculate resultant
dat3$Resultant <- sqrt(dat3$X^2+dat3$Y^2)
#     add group info
dat3$Group <- as.factor("LAT")
dat3 <- dat3[c("Time", "Test","X","Y","Group","Resultant")]
write.table(dat3, file='clipboard', sep='\t',row.names = FALSE)
write.csv(dat3, file = "57X285 bracket movement data.csv",row.names = FALSE)
