#' Add Sampling Period Variable
#'
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

addSamplingPeriods <- function(Plant_Surveys) {
	Plant_Surveys$DemographicSurvey <- "NA"
	Plant_Surveys %<>% 
		group_by(Date) %>% 
		mutate(
			# SURVEY 0
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date < "2013-05-14"), "0"),
			# SURVEY 1 - SPRING/SUMMER 2013
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2013-05-14" & Date <= "2013-08-06"), "1"),
			# SURVEY 2 - FALL/WINTER 2013/2014
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2013-12-13" & Date <= "2014-01-28"), "2"),
			# SURVEY 3 - SPRING/SUMMER 2014
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2014-05-06" & Date <= "2014-09-24"), "3"),
			# SURVEY 4 - WINTER 2015
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2015-01-08" & Date <= "2015-02-21"), "4"),
			# SURVEY 5 - SPRING/SUMMER 2015
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2015-05-01"), "5")
		) %>%
		ungroup()
	return(Plant_Surveys)
}
