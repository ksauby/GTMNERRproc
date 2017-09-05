#' Process Create Habitat Type Variable
#'
#' @param data
#'
#' @export

createHabitatType <- function(dat) {
	dat %>%
		mutate(
			HabitatType = NA,
			HabitatType = replace(
				HabitatType,
				which(
					Island=="Roadway1" |
					Island=="Roadway2"
				),
				"Barrier Island Habitat"
			),
			HabitatType = replace(
				HabitatType,
				which(
					Island!="Roadway1" &
					Island!="Roadway2"
				),
				"Intracoastal Waterway Island"
			)
		) %>%
		as.data.frame
}