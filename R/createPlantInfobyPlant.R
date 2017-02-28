#' Create Plant Info by Plant
#'
#' @description Create variables giving evidence of moths and insects, including moth presence or damage.
#' @param Plant_Surveys_by_Plant Plant Survey Dataset
#'
#' @export

createPlantInfobyPlant <- function(Plant_Info) {
	# one record per plant
	Plant_Info_Analysis <- Plant_Info %>%
		group_by(PlantID) %>%
		summarise(
			Island = Island[1],
			Network = Network[1],
			Species = Species[1],
			RecruitmentMode = RecruitmentMode[1],
			Parent = Parent[1],
			DaysAlive = DaysAlive[1],
			Dead = min(ConfirmedDeadMissing),
			First.Survey.Date = min(First.Survey.Date)
		) %>%
		filter(!is.na(Network))
	Plant_Info_Analysis$DaysAlive %<>% as.numeric
	# Parent
	Plant_Info_Analysis %<>% mutate(
		Parent = replace(
			Parent,
			which(is.na(Parent)),
			"Unknown"
		)
	)	
	Plant_Info_Analysis$Parent %<>% NA_Function
	Plant_Info_Analysis$Parent[which(Plant_Info_Analysis$Parent=="")] <- NA
	# order
	Plant_Info_Analysis %<>% arrange(desc(Island), desc(Parent))
	# summarise insect presence on plants and in networks
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
	Plant_Info_Analysis %<>% 
		merge(network_summary, by="Network") %>%
		merge(Plant_summary, by="PlantID")
	# get size at first survey
	First_Size <- Plant_Surveys_by_Year %>%
		group_by(PlantID) %>%
		summarise(
			First.Survey.Date = min(Date),
			First_Size = Size_t[which(Date==First.Survey.Date)],
			min.Size = min(Size_t, na.rm=T),
			max.Size = max(Size_t, na.rm=T)
		)
	Plant_Info_Analysis %<>% 
		merge(First_Size, by=c("First.Survey.Date", "PlantID"))
	return(Plant_Info_Analysis)
}createPlantInfobyPlant