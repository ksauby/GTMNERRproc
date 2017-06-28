#' Create Plant Info by Plant
#'
#' @description Create variables giving evidence of moths and insects, including moth presence or damage.
#' @param Plant_Surveys_by_Plant Plant Survey Dataset
#'
#' @export
#' @importFrom dataproc Unique

createPlantInfobyPlant <- function(Plant_Info, Plant_Surveys_by_Year) {
	# one record per plant
	# 2849 records
	Plant_Info_Analysis <- Plant_Info %>%
		mutate(
			Parent = replace(
				Parent,
				which(Parent=="Unkn"),
				NA
			)
		) %>%
		renameSpecies
	Plant_Info_Analysis %<>%
		arrange(First.Survey.Date.Alive) %>%
		group_by(PlantID) %>%
		dplyr::summarise(
			Island 				= Island[1],
			Network 			= Network[1],
			# make sure they are all the same species
			Species 			= Species[1],
			# fix/verify
			RecruitmentMode 	= paste(Unique(RecruitmentMode), collapse=","),
			Parent 				= paste(Unique(Parent), collapse=","),
			# should figure out the longest span of time a plotplantid was alive
			DaysAlive 			= Maximum(DaysAlive),
			# plant may have died in one plot but is alive in another
			DeadMissing 		= Minimum(ConfirmedDeadMissing),
			Dead 				= Minimum(ConfirmedDead),
			Missing 			= Minimum(ConfirmedMissing),
			First.Survey.Date.Alive 	= First.Survey.Date.Alive[1]
		) %>%
		filter(!is.na(Network))
	# save to figure out if some plants were lost during processing
	temp1 <- Plant_Info_Analysis$PlantID
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
	# network_summary <- Plant_Surveys_by_Year %>%
	#	group_by(Network) %>%
	#	dplyr::summarise(
	#		OldMothNetworkPres 	= Maximum(Old_Moth_Evidence_t),
	#		MENetworkPres 		= Maximum(ME_t),
	#		CANetworkPres 		= Maximum(CA_t),
	#		MothNetworkPres 	= Maximum(Moth_Evidence_t)
	#	)
	Plant_summary <- Plant_Surveys_by_Year %>%
		group_by(PlantID) %>%
		dplyr::summarise(
			OldMothPlantPres 	= Maximum(Old_Moth_Evidence_t),
			MEPlantPres 		= Maximum(ME_t),
			CAPlantPres 		= Maximum(CA_t),
			MothPlantPres 		= Maximum(Moth_Evidence_t)
		)
	Plant_Info_Analysis %<>% 
		merge(network_summary, by="Network") %>%
		merge(Plant_summary, by="PlantID")
	# get size at first survey
	First_Size <- Plant_Surveys_by_Plant %>%
		arrange(Date) %>%
		group_by(PlantID) %>%
		dplyr::summarise(
			First.Survey.Date 		= Date[1],
			minFecundityYear 		= Minimum(FecundityYear),
			First_Size 				= Size_t[!(is.na(Size_t))][1],
			First.Measurement.Date 	= Date[!(is.na(Size_t))][1],
			min.Size 				= Minimum(Size_t),
			max.Size 				= Maximum(Size_t)
		)
	Plant_Info_Analysis %<>% 
		merge(First_Size, by=c("PlantID"))
	# WARNINGS	
	temp <- Plant_Info_Analysis %>% 
		filter(grepl(",", RecruitmentMode)==TRUE)
	if (dim(temp)[1] > 0) {
		write.csv(temp, "InconsistentRecruitmentModePlantInfo.csv")
		warning(paste(
			"Inconsistent recruitment mode recorded for at least one plant spanning multiple plots. These plant info records have been written to csv."
		))
	}	
	temp <- Plant_Info_Analysis %>% 
		filter(grepl(",", Parent)==TRUE)
	if (dim(temp)[1] > 0) {
		write.csv(temp, "InconsistentParentPlantInfo.csv")
		warning(paste(
			"Inconsistent Parent recorded for at least one plant spanning multiple plots. These plant info records have been written to csv."
		))
	}	
	return(Plant_Info_Analysis)
}