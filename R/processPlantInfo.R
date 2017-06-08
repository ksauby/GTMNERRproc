#' Process Plant Info
#'
#' @description Process Plant Info
#' @param Plant_Info Dataset with Plant Information
#' @param Plot_Info Dataset with Plot Information
#'
#' @importFrom dplyr select summarise group_by arrange

#' @export

processPlantInfo <- function(Plant_Info, Plot_Info) {
	# --------------------------------------------------- remove InBigPlantStudy
	Plant_Info %<>% filter(InBigPlantStudy=="")
	# ----------------------------------------------------------- ERROR MESSAGES
	# Plants listed as species Not Recorded
	dups <- Plant_Info[which(Plant_Info$HostSpecies=="Not Recorded"), ]
	if (dim(dups)[1] > 0) {
		warning(paste(
			"Species identity not recorded for plantID: ",
			paste(dups$PlantID, collapse=", ")
		))
	}
	# are any plant IDs in Plant Info not in the surveys?
	dups <- Plant_Info[which(Plant_Info$OutsideOfCluster!="Yes"),] %>%
		filter(!(PlantID %in% Plant_Surveys$PlotPlantID)) %>% 
		.[,2:5]
	if (dim(dups)[1] > 0) {
		warning(paste(
			"Some plant IDs are not in Plant Surveys data: ",
			paste(dups$PlantID, collapse=", ")
		))
	}
	# make sure that there is plot info for each tag number in plant info
	dups <- Plant_Info %>%
		filter(!(Tag_Number %in% Plot_Info$Tag_Number))
	if (dim(dups)[1] > 0) {
		warning(paste(
			"Plants (",
			paste(unique(dups$PlantID), collapse=", "),
			") are listed as being in plots not in Plot Info data (",
			paste(unique(dups$Tag_Number), collapse=", "),
			")"
		))
	}
	# ----------------- ADD INFO FROM Plot_Info (Cluster, Network, Island, etc.)
	Plant_Info <- Plot_Info %>%
		select(
			Island, 
			Tag_Number, 
			Cluster, 
			Cluster2, 
			Network, 
			Sampling, 
			SurveyOrder
		) %>%
		merge(Plant_Info, by = "Tag_Number", all.y=TRUE) %>%
		as.data.table %>%
		setnames("HostSpecies", "Species") %>%
		as.data.frame
	#---------------------------------------------------------- FORMAT PLANT IDs
	Plant_Info %<>% Format_PlantIDs_Function
	#---------------------- CALCULATE AND ADD NUMBER OF PlotPlantIDs PER PlantID
	Plant_Info <- Plant_Info %>%
		group_by(PlantID) %>%
		summarise(
			N.PlotPlantIDs = length(unique(PlotPlantID))
		) %>%
		merge(Plant_Info, by="PlantID")
	#-------------------------- ADD FIRST and LAST DATE PlotPlantID WAS SURVEYED
	# particularly relevant for plants that grew into plots over the course of the study (and thus the number of PlotPlantIDs for a given PlantID changed over time)
	# also helps calculate the number of days a plant was known to have survived
	Plant_Info <- Plant_Surveys %>%
		filter(
			Dead != 1,
			Missing != 1
		) %>%
		group_by(PlotPlantID) %>%
		summarise(
			# simply the first survey date
			First.Survey.Date.Alive = min(Date),
			# should be max date the plant was alive
			Last.Survey.Date.Alive = max(Date)
		) %>%
		merge(Plant_Info, by="PlotPlantID")
	# ----------------------------------------------------------- PLANT SURVIVAL
	# indicate whether plant was listed as dead or missing
	A <- Plant_Surveys %>%
		rowwise() %>%
		mutate(DeadMissing = sum(Dead,Missing,na.rm=T)) %>%
		arrange(Date) %>%
		group_by(PlotPlantID) %>%
		summarise(
			sequenceDeadobs 		= paste(Dead, collapse=""),
			sequenceMissingobs 		= paste(Missing, collapse=""),
			sequenceDeadMissingobs 	= paste(DeadMissing, collapse=""),
			confirmedDead 			= ifelse(
				grepl("11", sequenceDeadobs) == TRUE,
				1,
				0
			),
			confirmedMissing 		= ifelse(
				grepl("11", sequenceDeadobs) == TRUE,
				1,
				0
			),
			confirmedDeadMissing 	= ifelse(
				grepl("11", sequenceDeadMissingobs) == TRUE,
				1,
				0
			),
			inconsistentDeadMissing = ifelse(
				grepl("10", sequenceDeadMissingobs) == TRUE,
				1,
				0
			)
		)
	# ERROR MESSAGES
	Z <- A %>% filter(inconsistentDeadMissing==1)
	if (dim(Z)[1] > 0) {
		write.csv(Z, "inconsistentDeadMissing.csv")
		warning(paste(
			"Plants with inconsistent records of dead/missing/alive present in dataset. These plants have been saved to a csv."
		))
	}
	# info for plants NOT observed in summer 2015
	B <- Plant_Surveys %>% 
		filter(Date >= "2015-05-01")
	C <- A %>% filter(!(PlotPlantID %in% B$PlotPlantID))
	# info for plants observed in summer 2015 - these do not need 2 consecutive obs. of dead/missing to be confirmed dead/missing
	 B %<>%
		rowwise() %>%
		mutate(DeadMissing = sum(Dead,Missing,na.rm=T)) %>%
		arrange(Date) %>%
		group_by(PlotPlantID) %>%
		summarise(
			ConfirmedDead 			= Dead_Missing_Function(Dead),
			ConfirmedMissing 		= Dead_Missing_Function(Missing),
			ConfirmedDeadMissing 	= Dead_Missing_Function(c(Dead,Missing))
		)
	# paste together
	#		info for plants NOT observed in summer 2015 &
	#		info for plants observed in summer 2015
	Plant_Info <- rbind.fill(B, C) %>%
		merge(Plant_Info, ., by="PlotPlantID")
	#---------------- ADD FIRST DATE PlotPlantID WAS RECORDED AS DEAD OR MISSING
	# earliest date PlotPlantID was recorded as dead
	temp_dead_obs <- filter(Plant_Surveys, Dead=="1") %>%
		group_by(PlotPlantID) %>%
		summarise(FirstDeadObservation = min(Date))
	# earliest date PlotPlantID was recorded as missing
	temp_missing_obs <- filter(Plant_Surveys, Missing=="1") %>%
		group_by(PlotPlantID) %>%
		summarise(FirstMissingObservation = min(Date))
	# earliest date PlotPlantID was recorded as missing or dead
	temp_dead_missing <- merge(
		temp_dead_obs, 
		temp_missing_obs, 
		by="PlotPlantID", 
		all=T
	) 
	temp_dead_missing$FirstDeadMissingObservation = temp_dead_missing %>%
		select(
			FirstDeadObservation,
			FirstMissingObservation
		) %>% 
		apply(., 1, min, na.rm=T) %>%
		as.Date
	# merge with Plant_Info
	Plant_Info <- merge(Plant_Info, temp_dead_missing, by="PlotPlantID", all=T)
	# calculate first/last day alive for PlantID (not individual Plot Plant IDs)
	Plant_Info %<>% group_by(PlantID) %>%
	mutate(
		PlantID.First.Alive = min(First.Survey.Date.Alive, na.rm=T),
		PlantID.Last.Alive = max(Last.Survey.Date.Alive, na.rm=T)
	)
	# ---------------------- CALCULATE HOW MANY DAYS PLANT WAS KNOWN TO BE ALIVE
	Plant_Info %<>% 
		group_by(PlantID) %>%
		mutate(DaysAlive = PlantID.Last.Alive - PlantID.First.Alive)
	# -------------------------------------------------- CLEANUP FOR CONSISTENCY
	Plant_Info[,c(
		"Quadrant",
		"ReproductiveMode",
		"Parent")] %<>%
		apply(., 2, as.character
	)
	Plant_Info[,c(
		"Quadrant",
		"ReproductiveMode",
		"Parent")] %<>%
		apply(., 2, NA_Function
	)
	Plant_Info[,c(
		"Quadrant",
		"ReproductiveMode",
		"Parent")] %<>%
		apply(., 2, as.factor
	)
	# ------------------------------------------------- modify Reproductive Mode
	Plant_Info$ReproductiveMode %<>% WoodyTrunk_Function()
	Plant_Info %<>% setnames("ReproductiveMode", "RecruitmentMode")
	Plant_Info %<>% mutate(
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
	# save all Plant_Info
	Plant_Info_All <- Plant_Info
	Plant_Info %<>% merge(
		., 
		ClustersInDemographicStudy, 
		by = "Cluster"
	) 
	# ------------------------------------------------------------ ADD ClusterID
	#	do this because some clusters share plots
	Plot_Info_Cluster <- Plot_Info %>%
		select(Tag_Number, Cluster, Cluster2) %>%
		reshape2:::melt.data.frame(., id.vars=c("Tag_Number"), 
			value.name="ClusterID") %>%
		filter(ClusterID!=0) %>%
		.[, -2] %>%
		arrange(Tag_Number)
	Plot_Info_Cluster %<>%
		group_by(Tag_Number) %>%
		summarise(ClusterID = paste(ClusterID, collapse=", "))
	# CLUSTER ID FOR PLOTS *NOT* IN CLUSTERS
	temp_A = Plot_Info %>%
		select(Tag_Number, Cluster) %>%
		filter(Cluster==0)
	temp_A$ClusterID <- temp_A$Tag_Number
	temp_A %<>% .[, -2]
	Plot_Info_Cluster %<>% rbind.fill(temp_A) %>% 
		merge(Plot_Info, by="Tag_Number") %>%
		select(ClusterID, Tag_Number)
	Plant_Info %<>% merge(Plot_Info_Cluster, by="Tag_Number", all.x=T)
	# ------------------------------------------------------------------------ #
	return(Plant_Info)
}