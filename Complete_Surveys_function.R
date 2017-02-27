#' Determine if a survey was complete
#'
#' @param x Plant_Surveys Plant Survey Dataset
#'
#' @export

Complete_Surveys_function <- function(x){
	ifelse(rowSums(is.na(x))==0, 1, 0)
	return(x)
}
