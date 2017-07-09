#' Estimate cactus density per plot
#'
#' @description Estimate cactus density as well as *O. stricta* and *O. pusilla* density (number of segments) per plot.
#'
#' @param Plant_Surveys_by_Year
#' @param Plot_Info
#'
#' @export

estimateCactusDensityperPlot <- function(Plant.Surveys.by.Year, Plot.Info) {
	# need to control for # of plots in network
	temp <- Plot.Info %>%
		group_by(Network) %>%
		summarise(n_plots = length(Tag_Number))
	# count plants per demographic plot per demographic survey
	temp_A <- Plant.Surveys.by.Year %>%
		# remove records of plants outside of the plot
		filter(OutsideOfPlot!="Yes") %>%
		group_by(Island, FecundityYear, Network, Species) %>%
		summarise(
			N_segments = sum(Size_t, na.rm=T)
		) %>%
		as.data.frame %>%
		arrange(Network, FecundityYear) %>%
		dcast(
			., 
			FecundityYear + Network + Island ~ Species, 
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
			cactus_density = pusilla_density + stricta_density
		)
	temp_B <- Plant.Surveys.by.Year %>% 
		merge(temp_A, by=c("FecundityYear", "Network", "Island")) %>%
	# subtract focal plant's biomass
		rowwise %>%
		mutate(
			cactus_density_per_plot = 
				sum(cactus_density,-(Size_t), na.rm=T)/n_plots,
			# pusilla
			pusilla_density_per_plot = NA,
			#		if focal plant is pusilla
			pusilla_density_per_plot = replace(
				pusilla_density_per_plot,
				which(Species=="Opuntia pusilla"),
				sum(pusilla_density,-(Size_t), na.rm=T)/n_plots
			),
			# 		if focal plant is stricta
			pusilla_density_per_plot = replace(
				pusilla_density_per_plot,
				which(Species=="Opuntia stricta"),
				pusilla_density/n_plots
			),
			# stricta
			stricta_density_per_plot = NA,
			# 		if focal plant is stricta
			stricta_density_per_plot = replace(
				stricta_density_per_plot,
				which(Species=="Opuntia stricta"),
				sum(stricta_density,-(Size_t), na.rm=T)/n_plots
			),
			#		if focal plant is pusilla
			stricta_density_per_plot = replace(
				stricta_density_per_plot,
				which(Species=="Opuntia pusilla"),
				stricta_density/n_plots
			)
		) %>%
		as.data.frame
	return(Plant_Surveys_by_Year)
}