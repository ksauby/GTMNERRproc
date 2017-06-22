#' Merge Plants from Multiple Plots
#' 
#' @param Plant_Surveys Plant survey dataset
#' @description Only merge data collected on the same day. Merge survey data for individual plants in more than one plot. Then combine back into one file with surveys of plants in only one plot.
#' 
#' @export

mergePlantRecordsfromMultiplePlots <- function(Plant_Surveys) {
	# restrict to plants that span multiple plots
	temp_A <- filter(Plant_Surveys, N.PlotPlantIDs > 1)
	Z = list()
	for (i in 1:length(unique(temp_A$PlantID))) {
		# pull all records for this PlantID from the plant surveys
		L = filter(temp_A, PlantID==unique(temp_A$PlantID)[i])
		# Use the last date the plant was surveyed during each Demographic Survey
		Dates <- L %>% group_by(DemographicSurvey) %>% summarise(Date=max(Date))
		Z[[i]] 	<- data.frame(Dates)
		Z[[i]][, "PlantID"] 			<- L$PlantID[1]
		Z[[i]][, "ClusterID"] 			<- L$ClusterID[1]
		Z[[i]][, "Network"] 			<- L$Network[1]
		Z[[i]][, "Island"] 				<- L$Island[1]
		Z[[i]][, "Species"] 			<- L$Species[1]
		Z[[i]][, "Easting"] 			<- L$Easting[1]
		Z[[i]][, "Northing"] 			<- L$Northing[1]
		Z[[i]][, "RecruitmentMode"]	<- L$RecruitmentMode %>%
												.[which(. != "NA")] %>%
												.[which(!is.na(.))] %>%
												unique(.) %>%
												paste(collapse="")
												
		# for each date - how do I do it for a set of dates
		# do first date with window
		# then do for other dates as long as they do not fall within window of previous dates
		
		# can group by DemographicSurvey if no duplicate surveys present
		uniqSurveys <- unique(Z[[i]]$DemographicSurvey)
		for (j in 1:length(uniqSurveys)) {
		
			# pull all plant survey records for this date from plant surveys
			# allow surveys to occur within a two week window around this date
			M = L %>% 
				filter(
					DemographicSurvey == uniqSurveys[j],
					Dead != 1,
					Missing != 1
				)
			# get list of PlotPlantIDs alive at this time
			# plant would be dead if no PlantID records showed up in N
			N = Plant_Info %>%
				filter( 
					PlantID==L$PlantID[1], 
					# only include plants that are listed as having been added to Plant_Info on or after Date
					First.Survey.Date.Alive <= max(M$Date),
					# exclude dead plants (including date plant was first recorded as dead)
					FirstDeadMissingObservation > max(M$Date) | 
						is.na(FirstDeadMissingObservation)==T
				)
			# if all PlotPlantIDs were surveyed for a given date:
			if (identical(M$PlotPlantID[order(M$PlotPlantID)], 
				N$PlotPlantID[order(N$PlotPlantID)])==T) {
				Z[[i]][j, "CA_t"] 					<- mysum2(M$CA_t)
				Z[[i]][j, "ME_t"] 					<- mysum2(M$ME_t)
				Z[[i]][j, "CH_t"] 					<- mysum2(M$CH_t)
				Z[[i]][j, "DA_t"] 					<- mysum2(M$DA_t)
				Z[[i]][j, "Unknown_Moth_t"] 		<- mysum2(M$Unknown_Moth_t)
				Z[[i]][j, "Gerstaeckeria_t"] 		<- mysum2(M$Gerstaeckeria_t)
				Z[[i]][j, "Old_Moth_Evidence_t"]<- mysum2(M$Old_Moth_Evidence_t)
				# Dead or missing - has to be dead or missing in all plots
				# (1) if the sum of Dead = # of PlotPlantIDs, the plant is dead in all plots
				if (sum(M$Dead, na.rm=T)==dim(N)[1]) 
					{Z[[i]][j, "Dead"] <- 1} else {Z[[i]][j, "Dead"] <- 0}
				# Missing
				if (sum(M$Missing, na.rm=T)==dim(N)[1]) 
					{Z[[i]][j, "Missing"] <- 1} else {Z[[i]][j, "Missing"] <- 0}
				# all surveyed = TRUE
				Z[[i]][j, "AllSurveyed"] 			<- "TRUE"
			}
			else {
				# if all PlotPlantIDs were NOT surveyed on this date consider the insect to be detected if the sum is greater than zero
				Z[[i]][j, "CA_t"] 					<- mysum1(M$CA_t)
				Z[[i]][j, "ME_t"] 					<- mysum1(M$ME_t)
				Z[[i]][j, "CH_t"] 					<- mysum1(M$CH_t)
				Z[[i]][j, "DA_t"] 					<- mysum1(M$DA_t)
				Z[[i]][j, "Unknown_Moth_t"] 		<- mysum1(M$Unknown_Moth_t)
				Z[[i]][j, "Gerstaeckeria_t"] 		<- mysum1(M$Gerstaeckeria_t)
				Z[[i]][j, "Old_Moth_Evidence_t"]<- mysum1(M$Old_Moth_Evidence_t)
				# Dead or missing - alive if at least one observation of alive
				# 	if the sum of Dead/Missing == # of PlotPlantIDs, the plant is dead in all plots
				if (sum(M$Dead, M$Missing, na.rm=T) < dim(N)[1])
					{Z[[i]][j, "Dead"] <- 0} else
						if (dim(N)[1] == 0)
							{Z[[i]][j, "Dead"] <- 1} else
								{Z[[i]][j, "Dead"] <- NA}
				# Missing
				# 	if the sum of Missing == # of PlotPlantIDs, the plant is missing in all plots
				O = Plant_Info %>% filter(PlantID==L$PlantID[1])
				if (sum(M$Missing, na.rm=T) < dim(N)[1])
					{Z[[i]][j, "Missing"] <- 0}  else
						if (dim(N)[1] == 0 & sum(M$Missing, na.rm=T)==dim(O)[1])
							{Z[[i]][j, "Missing"] <- 1} else
								{Z[[i]][j, "Missing"] <- NA}
				# all surveyed = FALSE
				Z[[i]][j, "AllSurveyed"] 			<- "FALSE"
			}
			# Number of segments
			Z[[i]][j, "Size_t"] 					<- mysum(M$Size_t)
			Z[[i]][j, "Plant_Segments_w_leaves"] <- 
				mysum(M$Plant_Segments_w_leaves)
			Z[[i]][j, "Plant_Segments_wo_leaves"] <-
			 	mysum(M$Plant_Segments_wo_leaves)
			Z[[i]][j, "Plant_Segments_woody"] <- mysum(M$Plant_Segments_woody)
			# Size
			Z[[i]][j, "Height_t"] 				<- max(M$Height_t, na.rm=T)
			Z[[i]][j, "Width_t"] 				<- max(M$Width_t, na.rm=T)
			Z[[i]][j, "Perpen_Width"] 			<- max(M$Perpen_Width, na.rm=T)
			# Fruit	and Flowers
			Z[[i]][j, "Num_FlowerBuds"] 			<- mysum(M$Num_FlowerBuds)
			Z[[i]][j, "Num_Fruit_red"] 				<- mysum(M$Num_Fruit_red)
			Z[[i]][j, "Num_Fruit_green"] 			<- mysum(M$Num_Fruit_green)
			Z[[i]][j, "Num_Flowers"] 				<- mysum(M$Num_Flowers)
			Z[[i]][j, "Fruit_t"] 					<- mysum(M$Fruit_t)
			Z[[i]][j, "Fruit_Flowers_t"] 			<- mysum(M$Fruit_Flowers_t)
			Z[[i]][j, "DemographicSurvey"] 			<- M$DemographicSurvey[1]
			Z[[i]][j, "SamplingYear"] 				<- M$SamplingYear[1]
			# Paste PlotPlantIDs together to know which plants were surveyed on this date
			Z[[i]][j, "PlantsSurveyed"] <- paste(M$PlotPlantID, collapse=",")
		}
	}
	# Field indicating all plants that were merged
	# Field indicating the fraction of all plants surveying during this time period
	# Field indicating the dates used for the merge
	# n times surveyed
	# n times insect was found
	# fraction of time that the insect was found
	# field indicating when plant was measured
	temp_B <- do.call(rbind.data.frame, Z)
	# max(NA, NA, na.rm=T) returns "-Inf"
	temp_B[,c(
		"Perpen_Width",
		"Width_t",
		"Height_t")] %<>% 
		apply(., 2, NA_Function
	)
	# - Merge plant survey data into one file -------------------------------- #
	# create new file with those plants in only one plot
	temp_C <- filter(Plant_Surveys, N.PlotPlantIDs == 1)
	temp_C %<>% select( 
		PlantID, 
		Date, 
		ClusterID, 
		Network, 
		Island,
		Easting,
		Northing, 
		Species, 
		RecruitmentMode,
		# insects
		CA_t, 
		ME_t, 
		CH_t,
		DA_t,
		Unknown_Moth_t, 
		Gerstaeckeria_t,
		Old_Moth_Evidence_t, 
		# size
		Size_t, 
		Plant_Segments_w_leaves, 
		Plant_Segments_wo_leaves, 
		Plant_Segments_woody, 
		Height_t, 
		Width_t, 
		Perpen_Width, 
		# fruit
		Num_FlowerBuds, 
		Num_Fruit_red, 
		Num_Fruit_green, 
		Num_Flowers,
		Fruit_t, 
		Fruit_Flowers_t,
		Dead, 
		Missing, 
		DemographicSurvey,
		SamplingYear
	)
	temp_C$AllSurveyed <- "TRUE"
	temp_C$PlantsSurveyed <- "NA"
	# merge plants in multiple plots and plants in one plot
	temp_D <- rbind.fill(temp_B, temp_C)
	temp_D %<>% arrange(PlantID, Date)
	# ----------------------------------------------------------- ERROR MESSAGES
	temp <- temp_D %>% 
		filter(PlantID=="7101", Date=="2015-05-23")
	if (temp$Dead != 0 | temp$Missing != 0) {
		warning(paste(
			"Death of part of plant (but not whole plant) not recorded 
				correctly for plant 7101."
		))
	}
	
	# why dead=NA for DemographicSurvey=5 when both were recorded dead?
	temp <- temp_D %>% 
		filter(PlantID=="7185", Date=="2015-05-26")
	if (temp$Dead != 1) {
		warning(paste(
			"Dead/Alive/Missing status not recorded correctly for plant 7185."
		))
	}
	temp <- temp_D %>%
		filter(
			Dead == 1,
			!(
				is.na(Size_t) |
				is.na(Plant_Segments_w_leaves) |
				is.na(Plant_Segments_wo_leaves) |
				is.na(Plant_Segments_woody) |
				is.na(Height_t) |
				is.na(Width_t) |
				is.na(Perpen_Width)
			) |
			Num_FlowerBuds > 0 |
			Num_Fruit_red > 0 |
			Num_Fruit_green > 0 |
			Num_Flowers > 0 |
			Fruit_t > 0 |
			Fruit_Flowers_t
		)
	if (dim(temp)[1] > 0) {
		write.csv(temp,"Deadplantswsizemeasurements.csv")
		warning(paste(
			"Plant ",
			paste(temp$PlantID, collapse=","),
			"Marked dead but has size/fruit measurements. Information written to csv file."
		))
	}
	temp <- temp_D %>%
		filter(
			Missing == 1,
			!(
				is.na(Size_t) |
				is.na(Plant_Segments_w_leaves) |
				is.na(Plant_Segments_wo_leaves) |
				is.na(Plant_Segments_woody) |
				is.na(Height_t) |
				is.na(Width_t) |
				is.na(Perpen_Width)
			) |
			Num_FlowerBuds > 0 |
			Num_Fruit_red > 0 |
			Num_Fruit_green > 0 |
			Num_Flowers > 0 |
			Fruit_t > 0 |
			Fruit_Flowers_t
		)
	if (dim(temp)[1] > 0) {
		write.csv(temp,"Missingplantswsizemeasurements.csv")
		warning(paste(
			"Plant ",
			paste(temp$PlantID, collapse=","),
			"Marked missing but has size/fruit measurements. Information written to csv file."
		))
	}
	# how many plants with less than 10 segments had fruit/flowers?
	# throw a warning if pusilla has flowers before summer 2015
	temp <- temp_D %>%
		filter(
			Size_t < 5,
			Fruit_Flowers_t > 0
		) %>%
		filter(
			!(
				# these records in database were verified with original datasheets
				PlantID == 9329	& Date == "2013-02-25" |
				PlantID == 9972	& Date == "2013-04-01" |
				PlantID == 9893	& Date == "2013-04-13" |
				PlantID == 9893	& Date == "2013-05-16" |
				PlantID == 8159	& Date == "2013-05-23" |
				PlantID == 8780	& Date == "2014-05-26" |
				PlantID == 7569	& Date == "2014-09-21" |
				PlantID == 7813	& Date == "2015-01-20" |
				PlantID == 7814	& Date == "2015-01-20"
				)
			)
	if (dim(temp)[1] > 0) {
		write.csv(temp, "SmallPlantswFruitFlowers.csv")
		warning(paste(
			"Plants less than 5 segments in size observed with fruit/flowers. Records written to csv file."
		))
	}
	# ------------------------- CHANGE SURVEY INFO TO NA FOR DEAD/MISSING PLANTS
	temp_D %<>%
		rowwise() %>%
		mutate(
			CA_t = replace(
				CA_t,
				which(Dead == 1 | Missing == 1),
				NA
			),
			ME_t = replace(
				ME_t,
				which(Dead == 1 | Missing == 1),
				NA
			),
			CH_t = replace(
				CH_t,
				which(Dead == 1 | Missing == 1),
				NA
			),
			DA_t = replace(
				DA_t,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Unknown_Moth_t = replace(
				Unknown_Moth_t,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Gerstaeckeria_t = replace(
				Gerstaeckeria_t,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Old_Moth_Evidence_t = replace(
				Old_Moth_Evidence_t,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Size_t = replace(
				Size_t,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Plant_Segments_w_leaves = replace(
				Plant_Segments_w_leaves,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Plant_Segments_wo_leaves = replace(
				Plant_Segments_wo_leaves,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Plant_Segments_woody = replace(
				Plant_Segments_woody,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Height_t = replace(
				Height_t,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Width_t = replace(
				Width_t,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Perpen_Width = replace(
				Perpen_Width,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Num_FlowerBuds = replace(
				Num_FlowerBuds,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Num_Fruit_red = replace(
				Num_Fruit_red,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Num_Fruit_green = replace(
				Num_Fruit_green,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Num_Flowers = replace(
				Num_Flowers,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Fruit_t = replace(
				Fruit_t,
				which(Dead == 1 | Missing == 1),
				NA
			),
			Fruit_Flowers_t = replace(
				Fruit_Flowers_t,
				which(Dead == 1 | Missing == 1),
				NA
			)
		) %>%
		ungroup()
	# --------------------------------------------------------------------------
	return(temp_D)
}
