#' Create Plant Info by Plant
#'
#' @description Create variables giving evidence of moths and insects, including moth presence or damage.
#' @param Plant_Surveys_by_Plant Plant Survey Dataset
#'
#' @export
#' @importFrom dataproc Unique

createPlantInfobyPlant <- function(Plant.Info, Plant.Surveys.by.Year, Plant.Surveys.by.Plant) {
	# one record per plant
	# 2849 records
	Plant.Info.Analysis <- Plant.Info %>%
		mutate(
			Parent = replace(
				Parent,
				which(Parent=="Unkn"),
				NA
			),
			AliveatEndofStudy = abs(ConfirmedDeadMissing - 1)
		) %>%
		renameSpecies
	Plant.Info.Analysis %<>%
		arrange(PlantID.First.Alive) %>%
		group_by(PlantID) %>%
		dplyr::summarise(
			Island 				= Island[1],
			Network 			= Network[1],
			# make sure they are all the same species
			Species 			= Species[1],
			# fix/verify
			RecruitmentMode 	= paste(Unique(RecruitmentMode), collapse=","),
			Parent 				= paste(Unique(Parent), collapse=","),
			First.Survey.Date.Alive = PlantID.First.Alive[1],
			AliveatEndofStudy	= Maximum(AliveatEndofStudy),
			Tag_Number			= Tag_Number[1]
		) %>%
		filter(!is.na(Network)) %>%
		mutate(
			RecruitmentMode = replace(
				RecruitmentMode,
				which(RecruitmentMode==""),
				"Unknown"
			),
			Parent = replace(
				Parent,
				which(Parent=="NA" | Parent==""),
				"Unknown"
			)
		)
	# save to figure out if some plants were lost during processing
	temp1 <- Plant.Info.Analysis$PlantID
	# order
	Plant.Info.Analysis %<>% arrange(desc(Island), desc(Parent))
	# summarise insect presence on plants and in networks
	# network_summary <- Plant_Surveys_by_Year %>%
	#	group_by(Network) %>%
	#	dplyr::summarise(
	#		OldMothNetworkPres 	= Maximum(Old_Moth_Evidence_t),
	#		MENetworkPres 		= Maximum(ME_t),
	#		CANetworkPres 		= Maximum(CA_t),
	#		MothNetworkPres 	= Maximum(Moth_Evidence_t)
	#	)
	Plant_summary <- Plant.Surveys.by.Year %>%
		group_by(PlantID) %>%
		dplyr::summarise(
			OldMothPlantPres 	= Maximum(Old_Moth_Evidence_t),
			MEPlantPres 		= Maximum(ME_t),
			CAPlantPres 		= Maximum(CA_t),
			MothPlantPres 		= Maximum(Moth_Evidence_t)
		)
	Plant.Info.Analysis %<>% 
		# merge(network_summary, by="Network") %>%
		merge(Plant_summary, by="PlantID")
	# get size at first survey
	First_Size <- Plant.Surveys.by.Plant %>%
		arrange(Date) %>%
		group_by(PlantID) %>%
		dplyr::summarise(
			First.Survey.Date 		= Date[1],
			First.DemographicSurvey = Minimum(DemographicSurvey),
			minFecundityYear 		= Minimum(FecundityYear),
			First_Size 				= Size_t[!(is.na(Size_t))][1],
			First.Measurement.Date 	= Date[!(is.na(Size_t))][1],
			min.Size 				= Minimum(Size_t),
			max.Size 				= Maximum(Size_t),
			LastDateAlive =
				Maximum(Date[which(Dead==0 & Missing==0)]) %>% as.Date(origin="1970-01-01"),
			FirstDeadObservation	= Date[which(Dead==1)][1],
			FirstMissingObservation	= Date[which(Missing==1)][1],
			FirstDeadMissingObservation = Minimum(
				c(
					FirstDeadObservation,
					FirstMissingObservation
				)
			) %>% as.Date(origin="1970-01-01"),
			# assume alive day before first observed: First.Survey.Date - 1
			# also alive day of last survey: LastDateAlive + 1
			minDaysAlive	= 
				(LastDateAlive + 1) - (First.Survey.Date - 1),
			# assume last day alive was day before death observation: FirstDeadMissingObservation - 1
			# assume alive day before first observed: First.Survey.Date - 1
			# the 1s cancel out
			maxDaysAlive	= 
				(FirstDeadMissingObservation - 1) -	(First.Survey.Date - 1)
		)
		First_Size$minDaysAlive %<>% as.numeric
		First_Size$maxDaysAlive %<>% as.numeric %<>%
			Replace_NA_w_Period_Function

	Plant.Info.Analysis %<>% 
		merge(First_Size, by=c("PlantID")) %>%
	   	renameSpecies %>%
		renamePatches %>%
		createHabitatType
		
	# Alternative Plant IDs
	# Plant_Info_Analysis %<>% mutate(
	# 	PlantIDb = paste(
	# 		IslandFullNames,
	# 		PlantID,
	# 		sep=", "
	# 	),
	# 	PlantIDc = paste(
	# 		IslandFullNames,
	# 		PlantID,
	# 		sep="\n"
	# 	)
	# )
	# ------------------------------------------------------ WARNING MESSAGES #
	temp <- Plant.Info.Analysis %>% 
		filter(maxDaysAlive!=".")
	temp$maxDaysAlive %<>% as.numeric
	temp %<>%
		filter(minDaysAlive > maxDaysAlive)
	if (dim(temp)[1] > 0) {
		write.csv(temp, "minDaysAlive_greater_maxDaysAlive.csv")
		warning(paste(
			"minDaysAlive > maxDaysAlive for some plants. Records written to csv."
		))
	}	
	temp <- Plant.Info.Analysis %>% 
		filter(grepl(",", RecruitmentMode)==TRUE)
	if (dim(temp)[1] > 0) {
		write.csv(temp, "InconsistentRecruitmentModePlantInfo.csv")
		warning(paste(
			"Inconsistent recruitment mode recorded for at least one plant spanning multiple plots. These plant info records have been written to csv."
		))
	}	
	temp <- Plant.Info.Analysis %>% 
		filter(grepl(",", Parent)==TRUE)
	if (dim(temp)[1] > 0) {
		write.csv(temp, "InconsistentParentPlantInfo.csv")
		warning(paste(
			"Inconsistent Parent recorded for at least one plant spanning multiple plots. These plant info records have been written to csv."
		))
	}	
	temp <- Plant.Info.Analysis %>% 
		dplyr::select(-c(
			FirstDeadObservation, 
			FirstMissingObservation, 
			FirstDeadMissingObservation
		)) %>% 
		.[complete.cases(.),]
	temp2 <- Plant.Info.Analysis[which(!(Plant.Info.Analysis$PlantID %in% temp$PlantID)),] %>% 
		filter(minDaysAlive > 2,
		PlantID != 7228 &
		PlantID != 7435 &
		PlantID != 7548 &
		PlantID != 8653
	)
	if (dim(temp2)[1] > 0) {
		write.csv(temp, "PlantInfoMissingInfo.csv")
		warning(paste(
			"Some information missing from plant info for some plants. Data written to csv."
		))
	}
	# No plant should be recorded as alive less than 2 days
	temp <- Plant.Info.Analysis %>% filter(minDaysAlive < 2)
	if (dim(temp)[1] > 0) {
		write.csv(temp,"PlantsAliveLessThan2Days.csv")
		warning(paste(
			"Some plants are recorded as alive for less than 2 days. Records written to csv."
		))
	}	
	# ------------------------------------------------------------------------ #
	return(Plant.Info.Analysis)
}