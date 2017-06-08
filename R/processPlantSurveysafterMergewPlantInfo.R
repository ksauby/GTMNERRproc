#' Process Plant Survey Data
#'
#' @description Steps:
#' \itemize{
#'  \item add column, "DemographicSurvey"
#'	\itemize{
#'		\item survey 1 - spring/summer 2013
#'		\item survey 2 - fall/winter 2013/2014
#'		\item survey 3 - spring/summer 2014
#'		\item survey 4 - winter 2015
#'		\item survey 5 - spring/summer 2015
#'	}
#'  \item addSamplingYear
#'	\itemize{
#'		\item 2012 - Date >= "2012-12-02" & Date < "2013-05-01"
#'		\item 2013 - Date >= "2013-05-01" & Date < "2014-05-01"
#'		\item 2014 - Date >= "2014-05-01" & Date < "2015-05-01"
#'		\item 2015 - Date >= "2015-05-01"
#'	}
#' }
#'
#' @export

processPlantSurveysafterMergewPlantInfo <- function(Plant_Surveys) {
	Plant_Surveys %<>%
		addSamplingPeriods %>%
		addSamplingYear %>%
		as.data.frame
	# ----------------------------------------------------------- ERROR MESSAGES
	# throw a warning if pusilla has flowers before summer 2015
	temp <- Plant_Surveys %>%
		filter(
			Species=="pusilla",
			SamplingYear!=2015,
			Fruit_Flowers_t > 0,
			!(month(Date) %in% c(5,6,7,8))
		)
	if (dim(temp)[1] > 0) {
		warning(paste(
			"O. pusilla plants ",
			paste(temp$PlotPlantID, collapse=", "),
			"are recorded as having fruit/flowers in a year other than 2015."
		))
	}
	# how many plants with less than 10 segments had fruit/flowers?
	# throw a warning if pusilla has flowers before summer 2015
	temp <- Plant_Surveys %>%
		filter(
			Size_t < 5,
			Fruit_Flowers_t > 0
		)
	if (dim(temp)[1] > 0) {
		write.csv(temp, "SmallPlantswFruitFlowers.csv")
		warning(paste(
			"Plants less than 5 segments in size observed with fruit/flowers. Records written to csv file."
		))
	}
	# --------------------------------------------------------------------------
	return(Plant_Surveys)	
}
	
