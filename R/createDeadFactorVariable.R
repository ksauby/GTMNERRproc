#' Create Dead Variable (Factor Type)
#'
#' @param Data Dataset
#'
#' @export

createDeadFactorVariable <- function(Data) {
	Data$DeadFactor <- factor(
		Data$Dead, 
		levels=c("0","1"), 
		ordered=T
	)
	levels(Data$DeadFactor) <- c("No","Yes")
	return(Data)
}

