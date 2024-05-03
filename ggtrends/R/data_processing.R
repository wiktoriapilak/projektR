importFile<- function(path){
  newTable = read.csv(path, skip=2)
  return(newTable)
}

doTimeTable <- function (rawFile){
  rawFile$year = as.numeric(substring(rawFile[,1],1,4))
  rawFile$month = as.numeric(substring(rawFile[,1],6,7))
  rawFile = rawFile[, -1]
  names(rawFile)[1] <- "amount"
  TimeTable = na.omit(rawFile)
  return(TimeTable)
}

doRegionTable <- function (rawFile){
  names(rawFile)[1] <- "country"
  names(rawFile)[2] <- "amount"
  RegionTable = na.omit(rawFile)
  return(RegionTable)
}
