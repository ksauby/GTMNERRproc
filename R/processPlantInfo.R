#' Process Plant Info
#'
#' @description Process Plant Info
#' @param Plant.Info Dataset with Plant Information
#' @param Plot.Info Dataset with Plot Information
#'
#' @importFrom dplyr select summarise group_by arrange

#' @export

processPlantInfo <- function(Plant.Info, Plot.Info, Plant.Surveys) {
	# --------------------------------------------------- remove InBigPlantStudy
	Plant.Info %<>% filter(InBigPlantStudy=="")
	# ----------------------------------------------------------- ERROR MESSAGES
	# Plants listed as species Not Recorded
	dups <- Plant.Info[which(Plant.Info$HostSpecies=="Not Recorded"), ]
	if (dim(dups)[1] > 0) {
		warning(paste(
			"Species identity not recorded for plantID: ",
			paste(dups$PlantID, collapse=", ")
		))
	}
	# are any plant IDs in Plant Info not in the surveys?
	dups <- Plant.Info[which(Plant.Info$OutsideOfCluster!="Yes"),] %>%
		filter(!(PlantID %in% Plant.Surveys$PlotPlantID)) %>% 
		.[,2:5]
	if (dim(dups)[1] > 0) {
		warning(paste(
			"Some plant IDs are not in Plant Surveys data: ",
			paste(dups$PlantID, collapse=", ")
		))
	}
	# make sure that there is plot info for each tag number in plant info
	dups <- Plant.Info %>%
		filter(!(Tag_Number %in% Plot.Info$Tag_Number))
	if (dim(dups)[1] > 0) {
		warning(paste(
			"Plants (",
			paste(unique(dups$PlantID), collapse=", "),
			") are listed as being in plots not in Plot Info data (",
			paste(unique(dups$Tag_Number), collapse=", "),
			")"
		))
	}
	# ----------------- ADD INFO FROM Plot.Info (Cluster, Network, Island, etc.)
	Plant.Info <- Plot.Info %>%
		dplyr::select(
			Island, 
			Tag_Number, 
			Cluster, 
			Cluster2, 
			Network, 
			Sampling, 
			SurveyOrder
		) %>%
		merge(Plant.Info, by = "Tag_Number", all.y=TRUE) %>%
		as.data.table %>%
		setnames("HostSpecies", "Species") %>%
		as.data.frame
	#---------------------------------------------------------- FORMAT PLANT IDs
	Plant.Info %<>% Format_PlantIDs_Function
	#------------------------------------------------------------- FORMAT PARENT
	Plant.Info$Parent %<>% str_replace_all(fixed(" "), "")
	# remove 5th digit from plant ID
	Plant.Info$Parent %<>% substr(1,4) 
	#---------------------- CALCULATE AND ADD NUMBER OF PlotPlantIDs PER PlantID
	Plant.Info <- Plant.Info %>%
		group_by(PlantID) %>%
		dplyr::summarise(
			N.PlotPlantIDs = length(unique(PlotPlantID))
		) %>%
		merge(Plant.Info, by="PlantID")
	#-------------------------- ADD FIRST and LAST DATE PlotPlantID WAS SURVEYED
	# particularly relevant for plants that grew into plots over the course of the study (and thus the number of PlotPlantIDs for a given PlantID changed over time)
	# also helps calculate the number of days a plant was known to have survived
	Plant.Info <- Plant.Surveys %>%
		filter(
			Dead != 1,
			Missing != 1
		) %>%
		group_by(PlotPlantID) %>%
		dplyr::summarise(
			# simply the first survey date
			PlotPlantID.First.Survey.Date.Alive = min(Date),
			# should be max date the plant was alive
			PlotPlantID.Last.Survey.Date.Alive = max(Date)
		) %>%
		merge(Plant.Info, by="PlotPlantID")
	# ----------------------------------------------------------- PLANT SURVIVAL
	# indicate whether plant was listed as dead or missing
	A <- Plant.Surveys %>%
		rowwise() %>%
		mutate(DeadMissing = sum(Dead,Missing,na.rm=T)) %>%
		arrange(Date) %>%
		group_by(PlotPlantID) %>%
		dplyr::summarise(
			sequenceDeadobs 		= paste(Dead, collapse=""),
			sequenceMissingobs 		= paste(Missing, collapse=""),
			sequenceDeadMissingobs 	= paste(DeadMissing, collapse=""),
			ConfirmedDead 			= ifelse(
				grepl("11", sequenceDeadobs) == TRUE,
				1,
				0
			),
			ConfirmedMissing 		= ifelse(
				grepl("11", sequenceDeadobs) == TRUE,
				1,
				0
			),
			ConfirmedDeadMissing 	= ifelse(
				grepl("11", sequenceDeadMissingobs) == TRUE,
				1,
				0
			),
			InconsistentDeadMissing = ifelse(
				grepl("10", sequenceDeadMissingobs) == TRUE,
				1,
				0
			)
		)
	# ERROR MESSAGES
	Z <- A %>% 
		filter(InconsistentDeadMissing==1) %>%
		filter(!(
			PlotPlantID %in% c("8999b", "9113b", "9164a", "9341a", "9917a", 
			"8873b", "9892b", "9573b", "9810b", "9852b")
		)) 
	if (dim(Z)[1] > 0) {
		write.csv(Z, "inconsistentDeadMissing.csv")
		warning(paste(
			"Plants with inconsistent records of dead/missing/alive present in dataset. These plants have been saved to a csv."
		))
	}
	# info for plants NOT observed in summer 2015
	B <- Plant.Surveys %>% 
		filter(Date >= "2015-05-01")
	C <- A %>% filter(!(PlotPlantID %in% B$PlotPlantID))
	# info for plants observed in summer 2015 - these do not need 2 consecutive obs. of dead/missing to be confirmed dead/missing
	 B %<>%
		rowwise() %>%
		mutate(DeadMissing = sum(Dead,Missing,na.rm=T)) %>%
		arrange(Date) %>%
		group_by(PlotPlantID) %>%
		dplyr::summarise(
			ConfirmedDead 			= Dead_Missing_Function(Dead),
			ConfirmedMissing 		= Dead_Missing_Function(Missing),
			ConfirmedDeadMissing 	= Dead_Missing_Function(c(Dead,Missing))
		)
	# paste together
	#		info for plants NOT observed in summer 2015 &
	#		info for plants observed in summer 2015
	Plant.Info <- rbind.fill(B, C) %>%
		merge(Plant.Info, ., by="PlotPlantID")
	#---------------- ADD FIRST DATE PlotPlantID WAS RECORDED AS DEAD OR MISSING
	# earliest date PlotPlantID was recorded as dead
	temp_dead_obs <- filter(Plant.Surveys, Dead=="1") %>%
		group_by(PlotPlantID) %>%
		dplyr::summarise(FirstDeadObservation = min(Date))
	# earliest date PlotPlantID was recorded as missing
	temp_missing_obs <- filter(Plant.Surveys, Missing=="1") %>%
		group_by(PlotPlantID) %>%
		dplyr::summarise(FirstMissingObservation = min(Date))
	# earliest date PlotPlantID was recorded as missing or dead
	temp_dead_missing <- merge(
		temp_dead_obs, 
		temp_missing_obs, 
		by="PlotPlantID", 
		all=T
	) 
	temp_dead_missing$FirstDeadMissingObservation <- temp_dead_missing %>%
		dplyr::select(
			FirstDeadObservation,
			FirstMissingObservation
		) %>% 
		apply(., 1, min, na.rm=T) %>%
		as.Date
	# merge with Plant.Info
	Plant.Info <- merge(Plant.Info, temp_dead_missing, by="PlotPlantID", all=T)
	
# ------------------------------------------------------------------------------
# calculate first/last day alive for PlantID (not individual Plot Plant IDs)
	Plant.Info %<>% group_by(PlantID) %>%
		mutate(
			PlantID.First.Alive = min(PlotPlantID.First.Survey.Date.Alive, na.rm=T),
			PlantID.Last.Alive = max(PlotPlantID.Last.Survey.Date.Alive, na.rm=T)
		)
		
	# -------------------------------------------------- CLEANUP FOR CONSISTENCY
	Plant.Info[,c(
		"Quadrant",
		"ReproductiveMode",
		"Parent")] %<>%
		apply(., 2, as.character
	)
	Plant.Info[,c(
		"Quadrant",
		"ReproductiveMode",
		"Parent")] %<>%
		apply(., 2, NA_Function
	)
	Plant.Info[,c(
		"Quadrant",
		"ReproductiveMode",
		"Parent")] %<>%
		apply(., 2, as.factor
	)
	# ------------------------------------------------- modify Reproductive Mode
	Plant.Info$ReproductiveMode %<>% WoodyTrunk_Function()
	Plant.Info %<>% setnames("ReproductiveMode", "RecruitmentMode")
	Plant.Info %<>% mutate(
		RecruitmentMode = replace(
			RecruitmentMode,
			which(RecruitmentMode=="WoodyTrunk"),
			NA
		),
		RecruitmentMode = replace(
			RecruitmentMode,
			which(is.na(RecruitmentMode)),
			NA
		)
	)
	# ------------------------------------------------- ADD InDemomographicStudy
	# save all Plant.Info
	Plant.Info_All <- Plant.Info
	Plant.Info %<>% merge(
		., 
		ClustersInDemographicStudy, 
		by = "Cluster"
	) %>%
	filter(InDemographicStudy == "yes")
	# ------------------------------------------------------------ ADD ClusterID
	#	do this because some clusters share plots
	Plot.Info_Cluster <- Plot.Info %>%
		dplyr::select(Tag_Number, Cluster, Cluster2) %>%
		reshape2:::melt.data.frame(., id.vars=c("Tag_Number"), 
			value.name="ClusterID") %>%
		filter(ClusterID!=0) %>%
		.[, -2] %>%
		arrange(Tag_Number)
	Plot.Info_Cluster %<>%
		group_by(Tag_Number) %>%
		dplyr::summarise(ClusterID = paste(ClusterID, collapse=", "))
	# CLUSTER ID FOR PLOTS *NOT* IN CLUSTERS
	temp_A = Plot.Info %>%
		dplyr::select(Tag_Number, Cluster) %>%
		filter(Cluster==0)
	temp_A$ClusterID <- temp_A$Tag_Number
	temp_A %<>% .[, -2]
	Plot.Info_Cluster %<>% rbind.fill(temp_A) %>% 
		merge(Plot.Info, by="Tag_Number") %>%
		dplyr::select(ClusterID, Tag_Number)
	Plant.Info %<>% merge(Plot.Info_Cluster, by="Tag_Number", all.x=T)
	# ----------------------------------------------------------- ERROR MESSAGES
	temp <- Plant.Info %>% 
		filter(RecruitmentMode=="Seedling") %>% 
		filter(Species=="pusilla")
	if (dim(temp)[1] > 0) {
		write.csv(temp,"Pusillaseedlings.csv")
		warning(
			"O. pusilla plants marked as seedlings. Information written to csv file."
		)
	}
	# ------------------------------------------------------------------------ #
	return(Plant.Info)
}