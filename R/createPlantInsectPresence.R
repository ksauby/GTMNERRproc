#' Create Moth Evidence and Insect Evidence Variables
#'
#' @description Create variables giving evidence of moths and insects, including moth presence or damage.
#' @param Plant_Surveys_by_Plant Plant Survey Dataset
#'
#' @export

createPlantInsectPresence <- function(Plant_Surveys_by_Plant) {
	Plant_Surveys_by_Plant %<>% 
	rowwise %>%
	mutate(
		Moth_Evidence_t = max(
			CA_t,
			ME_t,
			Old_Moth_Evidence_t,
			Unknown_Moth_t,
			na.rm=T
		),
		Insect_Evidence_t = max(
			CA_t,
			ME_t,
			Old_Moth_Evidence_t,
			Unknown_Moth_t,
			Gerstaeckeria_t,
			CH_t,
			DA_t,
			na.rm=T
		)
	) %>%
	ungroup()
	return(Plant_Surveys_by_Plant)
}