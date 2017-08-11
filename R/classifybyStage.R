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
			Stage = NA,
			Stage = replace(
				Stage,
				which(
					FecundityYear == minFecundityYear &
					RecruitmentMode == "Seedling"
				),
				"Seedling"
			),
			Stage = replace(
				Stage,
				which(
					Size_t == 1
				),
				"1"
			),
			Stage = replace(
				Stage,
				which(
					Size_t == 2
				),
				"2"
			),
			Stage = replace(
				Stage,
				which(
					Size_t >= 3 & Size_t <= 5
				),
				"3"
			),
			Stage = replace(
				Stage,
				which(
					Size_t >= 6 & Size_t <= 10
				),
				"4"
			),
			Stage = replace(
				Stage,
				which(
					Size_t >= 11
				),
				"5"
			)
			
		)
}