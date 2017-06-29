#' Filter Plant Surveys by Plant and Keep Only the First Dead/Missing Observation
#'
#' @description Important for determining the first date the plant was marked dead or missing. Also, subsequent surveys recording death/missingness are not necessary for analysis.
#' @param Plant_Surveys_by_Plant
#' @export

findFirstDeadMissingObservation <- function(Plant_Surveys_by_Plant) {
	# figure out earliest Dead/Missing observations
	A <- Plant_Surveys_by_Plant %>% 
		filter(Dead == 1 | Missing == 1) %>%
		arrange(PlantID, Date) %>%
		group_by(PlantID) %>%
		dplyr::summarise(
			FirstDeadObservation		= Date[which(Dead==1)][1],
			FirstMissingObservation		= Date[which(Missing==1)][1],
			FirstDeadMissingObservation	= min(
				FirstDeadObservation,
				FirstMissingObservation,
				na.rm=T
			)
		) %>%
		dplyr::select(PlantID, FirstDeadMissingObservation)
	# this keeps only the first Dead/Missing observation in the Plant Surveys data
	B <- Plant_Surveys_by_Plant %>%
		merge(
			A,
			by.x = c("PlantID", "Date"),
			by.y = c("PlantID", "FirstDeadMissingObservation")
		)
	# rbind "Alive" surveys with first Dead/Missing observation
	C <- Plant_Surveys_by_Plant %>%
		filter(Dead == 0 & Missing == 0) %>%
		rbind.fill(B)
	# ------------------------------------------------------------------------ #
	# make sure that every plant observed missing/dead BEFORE processing still has observation of dead/missing AFTER processing
	D <- Plant_Surveys_by_Plant %>%
		filter(Dead == 1 | Missing == 1) %$%
		unique(PlantID)
	E <- C %$% unique(PlantID)
	if (all(D %in% E) == FALSE) {
		warning(
			"Some plants are no longer recorded as having died or gone missing after processing of the data."
		)
	}
	# ------------------------------------------------------------------------ #
	return(C)
} 