#' Create Plant Info by Plant
#'
#' @description Create variables giving evidence of moths and insects, including moth presence or damage.
#' @param Plant_Surveys_by_Plant Plant Survey Dataset
#'
#' @export
#' @importFrom dataproc Unique

createPlantInfobyPlant <- function(Plant_Info, Plant_Surveys_by_Year) {
	# one record per plant
	# 2887 records
	Plant_Info_Analysis <- Plant_Info %>%
		group_by(PlantID) %>%
		summarise(
			Island 				= Island[1],
			Network 			= Network[1],
			# make sure they are all the same species
			Species 			= Species[1],
			# fix/verify
			RecruitmentMode 	= paste(Unique(RecruitmentMode), collapse=","),
			Parent 				= Parent[1],
			# should figure out the longest span of time a plotplantid was alive
			DaysAlive 			= max(DaysAlive, na.rm=T),
			# plant may have died in one plot but is alive in another
			DeadMissing 		= min(ConfirmedDeadMissing),
			Dead 				= min(ConfirmedDead),
			Missing 			= min(ConfirmedMissing),
			First.Survey.Date.Alive 	= min(First.Survey.Date.Alive)
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
		
		
	################################
	# I lose 100 plant IDs at this step
	################################
	
	
	
	# 2693 plants 
	Plant_Info_Analysis %<>% 
		merge(network_summary, by="Network") %>%
		merge(Plant_summary, by="PlantID")
	# get size at first survey
	First_Size <- Plant_Surveys_by_Year %>%
		group_by(PlantID) %>%
		summarise(
			minFecundityYear = min(FecundityYear, na.rm=T),
			First_Size = 
				min(Size_t[FecundityYear == min(FecundityYear)]),
			min.Size = min(Size_t, na.rm=T),
			max.Size = max(Size_t, na.rm=T)
		)
	Plant_Info_Analysis %<>% 
		merge(First_Size, by=c("PlantID"))
	# WARNINGS	
	temp <- Plant_Surveys %>% 
		filter(grepl(",", RecruitmentMode)==TRUE)
	if (dim(temp)[1] > 0) {
		write.csv(temp, "InconsistentRecruitmentModePlantInfo.csv")
		warning(paste(
			"Inconsistent recruitment mode recorded for at least one plant spanning multiple plots. These plant info records have been written to csv."
		))
	}	
	return(Plant_Info_Analysis)
}