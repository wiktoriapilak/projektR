importFile<- function(path){
  newTable = read.csv(path, skip=2)
  newTable = na.omit(newTable)
  return(newTable)
}

doDateTable <-function (rawFile){
  names(rawFile)[1] <- "week"
  names(rawFile)[2] <- "amount"
  rawFile[,1]=as.Date(rawFile[,1])
  dateTable=rawFile
  return(dateTable)
}

doSplitTable <- function (rawFile){
  rawFile$year = as.numeric(substring(rawFile[,1],1,4))
  rawFile$month = as.numeric(substring(rawFile[,1],6,7))
  rawFile$day = as.numeric(substring(rawFile[,1],9,10))
  rawFile = rawFile[, -1]
  names(rawFile)[1] <- "amount"
  splitTable <- rawFile[, c(2:ncol(rawFile), 1)]
  return(timeTable)
}
