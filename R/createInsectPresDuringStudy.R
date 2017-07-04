#' Make Insect Factor Variables
#'
#' @description Make new insect variables with "yes"/"no" values instead of 0/1.
#' @param Plant_Surveys_by_Year Plant Survey Dataset
#'
#' @export

createInsectPresDuringStudy <- function(Plant_Surveys_by_Year) {
	network_summary_time_t <- Plant_Surveys_by_Year %>%
		group_by(Network, FecundityYear) %>%
		dplyr::summarise(
			OldMothNetworkPres_t 	= Maximum(Old_Moth_Evidence_t),
			MENetworkPres_t 		= Maximum(ME_t),
			CANetworkPres_t 		= Maximum(CA_t),
			MothNetworkPres_t 		= Maximum(Moth_Evidence_t)
		) %>%
		arrange(FecundityYear) %>%
		group_by(Network) %>%
		mutate_at(
			.funs=funs("1" = "lag"),
			.vars=c("OldMothNetworkPres_t", 
				"MENetworkPres_t", 
				"CANetworkPres_t", 
				"MothNetworkPres_t"
			)
		) 
	network_summary <- Plant_Surveys_by_Year %>%
		group_by(Network) %>%
		dplyr::summarise(
			OldMothNetworkPres 		= Maximum(Old_Moth_Evidence_t),
			MENetworkPres 			= Maximum(ME_t),
			CANetworkPres 			= Maximum(CA_t),
			MothNetworkPres 		= Maximum(Moth_Evidence_t)
		)
	Plant_Surveys_by_Year %>% 
		merge(network_summary, by="Network") %>% 
		merge(network_summary_time_t, by=c("FecundityYear", "Network"))
}