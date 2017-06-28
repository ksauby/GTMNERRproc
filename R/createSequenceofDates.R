#' Create sequence of dates, by fecundity year, to use as date intervals in mergePlantRecordsfromMultiplePlots
#' @param seasons
#' @param probs the quantiles to use to divide up the fecundity year (e.g., probs=seq(0,1,0.1125))
#' @export

createSequenceofDates <- function(seasons=seasons, probs=seq(0,1,0.1125)) {
	seq_of_date_windows <- NULL
	for (i in 1:(length(seasons$SE) - 1)) {
		seq_of_date_windows <- c(
			seq_of_date_windows,
				quantile(
					as.numeric(
						seq(
							as.Date(seasons$SE[i]), 
							as.Date(seasons$SE[i+1] - 1), 
							by = 1
						)
					),
					probs = probs
				)
		) 
	}
	seq_of_date_windows %>% as.Date(origin="1970-01-01")
}
