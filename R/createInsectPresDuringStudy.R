#' Make Insect Factor Variables
#'
#' @description Make new insect variables with "yes"/"no" values instead of 0/1.
#' @param Plant_Surveys_by_Year Plant Survey Dataset
#'
#' @export

createInsectPresDuringStudy <- function(Plant_Surveys_by_Year) {
	network_summary_time_t <- Plant_Surveys_by_Year %>%
		group_by(Network, Year) %>%
		summarise(
			OldMothNetworkPres_t = max(Old_Moth_Evidence_t, na.rm=T),
			#InsectNetworkPres = max(Insect_t, na.rm=T),
			MENetworkPres_t = max(ME_t, na.rm=T),
			CANetworkPres_t = max(CA_t, na.rm=T),
			MothNetworkPres_t = max(Moth_Evidence_t, na.rm=T)
		) %>%
		calculateLagGroupedDF(
			vars=c(
				"OldMothNetworkPres_t", 
				"MENetworkPres_t", 
				"CANetworkPres_t", 
				"MothNetworkPres_t"
			), 
			arrange.variable="Year", 
			grouping.variable=c("Network")
		)
	network_summary <- Plant_Surveys_by_Year %>%
		group_by(Network) %>%
		summarise(
			OldMothNetworkPres = max(Old_Moth_Evidence_t, na.rm=T),
			#InsectNetworkPres = max(Insect_t, na.rm=T),
			MENetworkPres = max(ME_t, na.rm=T),
			CANetworkPres = max(CA_t, na.rm=T),
			MothNetworkPres = max(Moth_Evidence_t, na.rm=T)
		)
	Plant_summary <- Plant_Surveys_by_Year %>%
		group_by(PlantID) %>%
		summarise(
			OldMothPlantPres = max(Old_Moth_Evidence_t, na.rm=T),
			#InsectPlantPres = max(Insect_t, na.rm=T),
			MEPlantPres = max(ME_t, na.rm=T),
			CAPlantPres = max(CA_t, na.rm=T),
			MothPlantPres = max(Moth_Evidence_t, na.rm=T)
		)
	Plant_Surveys_by_Year %>% 
		merge(network_summary, by="Network") %>% 
		merge(Plant_summary, by="PlantID") %>%
		merge(network_summary_time_t, by=c("Year", "Network"))
}