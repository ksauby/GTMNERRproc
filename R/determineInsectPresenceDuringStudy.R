#' Determine if an insect species was ever detected during the study period
#'
#' @description Determine if an insect species was ever detected during the study period.
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

determineInsectPresenceDuringStudy <- function(Plant_Surveys) {
	Plant_Surveys.present <- Plant_Surveys %>%
		group_by(PlantID) %>%
		summarise(
			CAPresent = ifelse(sum(CA_t, na.rm=T) > 0, 1, 0),
			MEPresent = ifelse(sum(ME_t, na.rm=T) > 0, 1, 0)
			) %>%
		as.data.frame()
	Plant_Surveys %>% merge(Plant_Surveys.present, by="PlantID")
}