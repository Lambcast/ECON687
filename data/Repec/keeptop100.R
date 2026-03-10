topunis <- read.csv("topunis.csv", header=TRUE)

topunis$X <- NULL
topunis2 <- read.csv("topuni_coded.csv", header=TRUE)

total <- merge(topunis,topunis2,by="instname")
total <- subset(total, uni == 1)

topunis3 <- read.csv("rename.csv", header=TRUE)
total <- merge(total,topunis3,by="instname")
total <- total[,c(ncol(total),1:(ncol(total)-1))]
total$instname <- NULL
total$uni <- NULL
total <- total[order(total$n,decreasing=TRUE),]
write.csv(total, "top100unis.csv", row.names=FALSE)
