#' Add Sampling Year Variable
#'
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

addSamplingYear <- function(Plant_Surveys) {
	Plant_Surveys$SamplingYear <- NA
	Plant_Surveys %<>% 
		group_by(Date) %>% 
		mutate(
			SamplingYear = replace(
				SamplingYear, 
				which(Date >= "2012-12-02" & Date < "2013-05-01"), 
				2012
			),
			SamplingYear = replace(
				SamplingYear, 
				which(Date >= "2013-05-01" & Date < "2014-05-01"), 
				2013
			),
			SamplingYear = replace(
				SamplingYear, 
				which(Date >= "2014-05-01" & Date < "2015-05-01"), 
				2014
			),
			SamplingYear = replace(
				SamplingYear, 
				which(Date >= "2015-05-01"), 
				2015
			)
		)
	return(Plant_Surveys)
}
