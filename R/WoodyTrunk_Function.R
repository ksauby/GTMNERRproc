#' Standardize "WoodyTrunk" values.
#'
#' @param x Vector of data.
#' @description Standardize "WoodyTrunk" values.
#'
#' @export

WoodyTrunk_Function <- function(x){	
	x[
		x == "woody trunk" | 
		x == "woody tunk" | 
		x == "Woody Trunk"
	] <- "WoodyTrunk"
	return(x)
}