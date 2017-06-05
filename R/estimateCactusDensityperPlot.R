#' Estimate cactus density per plot
#'
#' @description Estimate cactus density as well as *O. stricta* and *O. pusilla* density (number of segments) per plot.
#'
#' @param Plant_Surveys_by_Year
#' @param Plot_Info
#'
#' @export

estimateCactusDensityperPlot <- function(Plant_Surveys_by_Year, Plot_Info) {
	# Warnings
	temp <- Plant_Surveys_by_Year %>% 
		filter(is.na(Size_t))
	if (dim(temp)[1] > 0) {
		warning("Some plant sizes = NA. Data written to csv file.")
		temp %>% write.csv("plants_w_no_size.csv")
	}
	# need to control for # of plots in network
	temp <- Plot_Info %>%
		group_by(Network) %>%
		summarise(n_plots = length(Tag_Number))
	# count plants per demographic plot per demographic survey
	temp_A <- Plant_Surveys_by_Year %>%
		filter(DeadbyEndofYear==0 & MissingbyEndofYear==0) %>%
		group_by(SamplingYear, Network, Species) %>%
		summarise(
			#N_plants = length(unique(PlantID)),
			N_segments = sum(Size_t, na.rm=T),
			Island = Island[1]
		) %>%
		as.data.frame %>%
		arrange(Network, SamplingYear) %>%
		dcast(
			., 
			SamplingYear + Network + Island ~ Species, 
			value.var="N_segments"
		) %>%
		setnames("Opuntia pusilla", "pusilla_density") %>%
		setnames("Opuntia stricta", "stricta_density") %>%
		merge(temp, by="Network")
	temp_A[which(is.na(temp_A$pusilla_density)), ]$pusilla_density <- 0
	temp_A[which(is.na(temp_A$stricta_density)), ]$stricta_density <- 0
	temp_A %<>%
		rowwise() %>%
		mutate(
			cactus_density = pusilla_density + stricta_density,
			pusilla_density_per_plot = pusilla_density/n_plots,
			stricta_density_per_plot = stricta_density/n_plots,
			cactus_density_per_plot = cactus_density/n_plots
		)
	Plant_Surveys_by_Year %<>% 
		merge(temp_A, by=c("SamplingYear", "Network", "Island"))
	return(Plant_Surveys_by_Year)
}