#'Convert character vector to numeric, ignoring irrelevant characters.
#'
#'@description Code copied from https://github.com/gsk3/taRifx/blob/master/R/Rfunctions.R#L1161.
#'@param x A vector to be operated on
#'@param keep Characters to keep in, in bracket regular expression form.
#'Typically includes 0-9 as well as the decimal separator (. in the US and , in
#'Europe).
#'@return vector of type numeric
#'@export destring
#'@examples
#'
#'test <- "50,762.83a"
#'destring(test)
#'
destring <- function(x,keep="0-9.-") {
  return( as.numeric(gsub(paste("[^",keep,"]+",sep=""),"",x)) )
}
