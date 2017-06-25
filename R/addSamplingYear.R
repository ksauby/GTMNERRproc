#' Add Sampling Year Variable
#'
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

addFecundityYear <- function(Plant_Surveys) {
	Plant_Surveys$FecundityYear <- NA
	Plant_Surveys %<>% 
		group_by(Date) %>% 
		mutate(
			FecundityYear = replace(
				FecundityYear, 
				which(Date >= "2012-12-02" & Date < "2013-05-01"), 
				2012
			),
			FecundityYear = replace(
				FecundityYear, 
				which(Date >= "2013-05-01" & Date < "2014-05-01"), 
				2013
			),
			FecundityYear = replace(
				FecundityYear, 
				which(Date >= "2014-05-01" & Date < "2015-05-01"), 
				2014
			),
			FecundityYear = replace(
				FecundityYear, 
				which(Date >= "2015-05-01"), 
				2015
			)
		)
	return(Plant_Surveys)
}
