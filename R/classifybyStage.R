#' Classify plants by Stage
#'
#' @description Classify plants by stage. Individuals remain a seedling only for one year, then automatically transition to a "juvenile" stage. Once a juvenile plant reaches two segments in size, it is no longer a juvenile. Once a plant reaches the "Adult" stage, they remain "Adult", even if they retrogress to one segment.
#'
#' @param Data
#'
#' @export

classifybyStage <- function(Data) {
	Data$Stage <- Data$RecruitmentMode
	Data[which(Data$Stage!="Seedling"), ]$Stage <- "Adult"
	Data[which(is.na(Data$Stage)), ]$Stage <- "Adult"
	Data[which(Data$DeadbyEndofYear==1), ]$Stage <- "Dead"
	Data[which(Data$MissingbyEndofYear==1), ]$Stage <- "Dead"
	# fix Seedling category
	stages <- Data$Stage
	temp <- split(Data, stages)
	temp$Seedling %<>% 
		group_by(PlantID) %>%
		mutate(FirstObs = min(Date)) %>%
		as.data.frame %>%
		mutate(
			# individuals remain a seedling only for one year, then automatically transition to a "juvenile" stage
			Stage = replace(
				Stage,
				which(Date > FirstObs),
				"Juvenile"
			),
			# once the plant reaches two segments, it is no longer a juvenile
			Stage = replace(
				Stage,
				which(Size_t > 1),
				"Adult"
			)
		) %>%
		group_by(PlantID) %>%
		mutate(FirstAdult = ifelse(
			max(Size_t) > 1,
			min(Date[which(Size_t > 1)]),
			NA
		)) %>%
		# once plants reach "Adult", they remain "Adult", even if they retrogress to one segment
		mutate(
			Stage = replace(
				Stage,
				which(Date > FirstAdult),
				"Adult"
			)
		) %>%
		dplyr::select(-c(FirstObs, FirstAdult)) %>%
		as.data.frame
		temp$Seedling$FirstAdult %<>% as.Date
	Data <- do.call(rbind.fill, temp)
}