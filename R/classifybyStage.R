#' Classify plants by Stage
#'
#' @description Classify plants by stage. Individuals remain a seedling only for one year, then automatically transition to a "juvenile" stage. Once a juvenile plant reaches two segments in size, it is no longer a juvenile. Once a plant reaches the "Adult" stage, they remain "Adult", even if they retrogress to one segment.
#'
#' @param Data
#'
#' @export

classifybyStage <- function(Data) {
	# can only be seedling for one year
	#	thus can only consider plants surveyed in their first year/survey for this		
	Data %>%
		mutate(
			stage = "Adult",
			stage = replace(
				stage,
				which(
					FecundityYear == minFecundityYear &
					RecruitmentMode == "Seedling"
				),
				"Seedling"
			),
			stage = replace(
				stage,
				which(
					is.na(Size_t) &
					DeadMissingbyEndofYear == 1
				),
				"dead"
			)
		)
}