#' Imports data from the BVA export peaks function.
#'
#' @param filename A string containing the filename of the file to be imported
#' @param pcode A string containing the participant code
#' @param conditions A list of strings containing the condition cell labels
#' @param labels A list of strings containing the condition names
#' @return A data frame containing properly formatted peak and area data from BVA

importPeakData <- function( filename, pcode, conditions, labels ) {

  require(reshape2)
  require(plyr)
  require(stringr)

  dataIn <- suppressMessages(melt(read.table(filename, header=T, quote="\"")))

  split <- colsplit(dataIn$variable, "[.]", c("labels", "d1", "datatype"))[,c(1,3)]
  peaklab <- ifelse(substr(split$labels, 2, 2) == "M", substr(split$labels, 1, 3), substr(split$labels, 1, 2))
  eleclab <- ifelse(substr(split$labels, 2, 2) == "M", substr(split$labels, 4, nchar(split$labels)-1), substr(split$labels, 3, nchar(split$labels)-1))

  partlab <- rep(pcode, length(dataIn[,1]))
  data <- data.frame(partlab)

  if(length(conditions) != length(labels))
    stop("importPeakData: Unequal number of condition labels and names")

  for(i in 1:length(conditions)) {
    tmplab <- data.frame(rep(conditions[[i]], length(dataIn[,1])))
    names(tmplab) <- labels[[i]]
    data <- data.frame(data, tmplab)
  }

  data <- data.frame(data, peaklab, eleclab, split$datatype, dataIn$value)
  return(data)
};
