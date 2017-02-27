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
	Plant_Surveys %>%
		filter(InBigPlantStudy!="yes" & InBigPlantStudy!="Yes") %>%
		addSamplingPeriods %>%
		addSamplingYear %>%
		as.data.frame
}
	
