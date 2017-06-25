#' Merge Plants from Multiple Plots
#' 
#' @param Plant_Surveys Plant survey dataset
#' @description Only merge data collected on the same day. Merge survey data for individual plants in more than one plot. Then combine back into one file with surveys of plants in only one plot.
#' 
#' @export

mergePlantRecordsfromMultiplePlots <- function(Plant_Surveys, date_window=48) {
	h <- function(w) if( any( grepl( "no non-missing arguments to max", w) ) ) invokeRestart( "muffleWarning" )
	# restrict to plants that span multiple plots
	temp_A <- filter(Plant_Surveys, N.PlotPlantIDs > 1)
	Z = list()
	for (i in 1:length(unique(temp_A$PlantID))) {
		# pull all records for this PlantID from the plant surveys
		L = filter(temp_A, PlantID==unique(temp_A$PlantID)[i])
		# group by window of dates
		if (max(L$Date) - min(L$Date) > date_window) {
			L.list <- L %>%
				split(
				.,
				cut(
					L$Date,
					seq(
						min(L$Date), 
						max(L$Date) + date_window, 
						by = date_window
					)
				)
			)
			L.list %<>% .[sapply(., function(x) dim(x)[1]) > 0]
			L.list %<>% lapply(., function(x) split(x, x$DemographicSurvey))
			L.list %<>% unlist(recursive=F)
			list.names <- gsub("\\..*","", names(L.list))
			demography.surveys <- gsub("^.*\\.","", names(L.list))
			
			Z[[i]] 	<- data.frame(
				Date = list.names,
				DemographicSurvey = demography.surveys
			)
		} else {
			L.list <- list(L)
			names(L.list) <- max(L.list[[1]]$Date)
			Z[[i]] <- data.frame(Date = max(L.list[[1]]$Date))
		}
		Z[[i]][, "PlantID"] 			<- L$PlantID[1]
		Z[[i]][, "ClusterID"] 			<- L$ClusterID[1]
		Z[[i]][, "Network"] 			<- L$Network[1]
		Z[[i]][, "Island"] 				<- L$Island[1]
		Z[[i]][, "Species"] 			<- L$Species[1]
		Z[[i]][, "Easting"] 			<- L$Easting[1]
		Z[[i]][, "Northing"] 			<- L$Northing[1]
		Z[[i]][, "RecruitmentMode"]		<- L$RecruitmentMode %>%
			.[which(. != "NA")] %>%
			.[which(!is.na(.))] %>%
			unique(.) %>%
			paste(collapse="")
		# for each window of dates
		for (j in 1:length(names(L.list))) {
			K <- eval(parse(text=paste(
				"L.list$", 
				'"', 
				names(L.list)[j], 
				'"', 
				sep=""
			)))
			# save range of dates used to create whole plant survey
			if (length(unique(K$Date)) > 1) {
				Z[[i]][j, "RangeofDates"] <- paste(
					min(K$Date), 
					" - ", 
					max(K$Date),
					sep=""
				)
				Z[[i]][j, "SizeofDateRange"] <- max(K$Date) - min(K$Date)
				Z[[i]][j, "maxDate"] <- as.character(max(K$Date))
				
			} else {
				Z[[i]][j, "RangeofDates"] <- unique(K$Date)
				Z[[i]][j, "SizeofDateRange"] <- 0
				Z[[i]][j, "maxDate"] <- as.character(max(K$Date))
			}
			# pull all plant survey records for this date from plant surveys within the window of dates, excluding dead/missing
			M <- K %>% filter(Dead != 1, Missing != 1)
			# --------------------------------------------------------- WARNINGS
			# throw error if a plotplantID is surveyed multiple times within this window and multiple records have size measurements
			temp <- M %>% filter(
					Plant_Segments_w_leaves > 0 |
					Plant_Segments_wo_leaves > 0 |
					Plant_Segments_woody > 0 | 
					Height_t > 0 | 
					Width_t > 0 | 
					Perpen_Width > 0 | 
					Num_FlowerBuds > 0 | 
					Num_Fruit_red > 0 | 
					Num_Fruit_green > 0 | 
					Num_Flowers > 0 | 
					Num_Fruit > 0
			) %>%
				group_by(PlotPlantID) %>%
				mutate(n.records = n()) %>%
				ungroup() %>%
				filter(n.records > 1)
			if (dim(temp)[1] > 0) {
				warning(paste(
					"Multiple size records for PlotPlantID", 
					M$PlotPlantID[1], 
					"around date", 
					paste(M$Date, collapse=",")
				))
			}
			# ---------------------------------------------------------------- #
			# get list of PlotPlantIDs alive at this time
			# plant would be dead if no PlantID records showed up in N
			N = Plant_Info %>%
				filter( 
					PlantID==L$PlantID[1], 
					# only include plants that are listed as having been added to Plant_Info on or after Date
					First.Survey.Date.Alive <= 
						as.Date(Z[[i]]$Date[j]) + date_window,
					# exclude dead plants (including date plant was first recorded as dead)
					FirstDeadMissingObservation > 
						as.Date(Z[[i]]$Date[j]) + date_window | 
						is.na(FirstDeadMissingObservation)==T
				)	
			# pull all surveys where plant was marked dead
			O <- K %>% filter(Dead == 1 | Missing == 1)
			# the plant can only be marked dead if no live part was surveyed and no parts remained unsurveyed
			if (dim(M)[1] == 0 & dim(N)[1] == 0 & dim(O)[1] > 0) {
				Z[[i]][j, "Dead"] <- withCallingHandlers(
					max(O$Dead, na.rm=T),
					warning = h
				)
				Z[[i]][j, "Missing"] <- withCallingHandlers(
					max(O$Missing, na.rm=T),
					warning = h
				)
			} else
			# if all PlotPlantIDs were surveyed for a given date:
			if (dim(M)[1] > 0 & all(M$PlotPlantID %in% N$PlotPlantID)) {
				Z[[i]][j, "CA_t"] 					<- mysum2(M$CA_t)
				Z[[i]][j, "ME_t"] 					<- mysum2(M$ME_t)
				Z[[i]][j, "CH_t"] 					<- mysum2(M$CH_t)
				Z[[i]][j, "DA_t"] 					<- mysum2(M$DA_t)
				Z[[i]][j, "Unknown_Moth_t"] 		<- mysum2(M$Unknown_Moth_t)
				Z[[i]][j, "Gerstaeckeria_t"] 		<- mysum2(M$Gerstaeckeria_t)
				Z[[i]][j, "Old_Moth_Evidence_t"]<- mysum2(M$Old_Moth_Evidence_t)
				Z[[i]][j, "AllSurveyed"] 			<- "TRUE"
				Z[[i]][j, "Dead"] <- 0
				Z[[i]][j, "Missing"] <- 0
			} else {
				# if all PlotPlantIDs were NOT surveyed on this date consider the insect to be detected if the sum is greater than zero
				Z[[i]][j, "CA_t"] 					<- mysum1(M$CA_t)
				Z[[i]][j, "ME_t"] 					<- mysum1(M$ME_t)
				Z[[i]][j, "CH_t"] 					<- mysum1(M$CH_t)
				Z[[i]][j, "DA_t"] 					<- mysum1(M$DA_t)
				Z[[i]][j, "Unknown_Moth_t"] 		<- mysum1(M$Unknown_Moth_t)
				Z[[i]][j, "Gerstaeckeria_t"] 		<- mysum1(M$Gerstaeckeria_t)
				Z[[i]][j, "Old_Moth_Evidence_t"]<- mysum1(M$Old_Moth_Evidence_t)
				Z[[i]][j, "AllSurveyed"] 			<- "FALSE"
				Z[[i]][j, "Dead"] <- 0
				Z[[i]][j, "Missing"] <- 0
			}
			# Number of segments
			Z[[i]][j, "Size_t"] 					<- mysum(M$Size_t)
			Z[[i]][j, "Plant_Segments_w_leaves"] <- 
				mysum(M$Plant_Segments_w_leaves)
			Z[[i]][j, "Plant_Segments_wo_leaves"] <-
			 	mysum(M$Plant_Segments_wo_leaves)
			Z[[i]][j, "Plant_Segments_woody"] <- mysum(M$Plant_Segments_woody)
			# Size
			Z[[i]][j, "Height_t"] 				<- withCallingHandlers(
				max(M$Height_t, na.rm=T),
				warning = h
			)
			Z[[i]][j, "Width_t"] 				<- withCallingHandlers(
				max(M$Width_t, na.rm=T),
				warning = h
			)
			Z[[i]][j, "Perpen_Width"] 			<- withCallingHandlers(
				max(M$Perpen_Width, na.rm=T),
				warning = h
			)
			# Fruit	and Flowers
			Z[[i]][j, "Num_FlowerBuds"] 			<- mysum(M$Num_FlowerBuds)
			Z[[i]][j, "Num_Fruit_red"] 				<- mysum(M$Num_Fruit_red)
			Z[[i]][j, "Num_Fruit_green"] 			<- mysum(M$Num_Fruit_green)
			Z[[i]][j, "Num_Flowers"] 				<- mysum(M$Num_Flowers)
			Z[[i]][j, "Fruit_t"] 					<- mysum(M$Fruit_t)
			Z[[i]][j, "Fruit_Flowers_t"] 			<- mysum(M$Fruit_Flowers_t)
			Z[[i]][j, "DemographicSurvey"] 			<- M$DemographicSurvey[1]
			Z[[i]][j, "FecundityYear"] 				<- M$FecundityYear[1]
			# Paste PlotPlantIDs together to know which plants were surveyed on this date
			Z[[i]][j, "PlantsSurveyed"] <- paste(M$PlotPlantID, collapse=",")
		}
		Z[[i]] %<>%
			select(-(Date)) %>%
			setnames("maxDate", "Date")
	}
	temp_B <- do.call(rbind.data.frame, Z)
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
		FecundityYear
	)
	temp_C$AllSurveyed <- "TRUE"
	temp_C$PlantsSurveyed <- "NA"
	# merge plants in multiple plots and plants in one plot
	temp_D <- rbind.fill(temp_B, temp_C)
	temp_D %<>% arrange(PlantID, Date)
	# ----------------------------------------------------------------- WARNINGS
	# WHICH PLANTS WERE RECORDED AS ALIVE IN THE SPRING/SUMMER BUT HAD NO SIZE RECORDS?
	temp <- temp_D %>%
		filter(
			Dead != 1 & Missing != 1,
			DemographicSurvey == 1 |
			DemographicSurvey == 3 |
			DemographicSurvey == 5
		) %>%
		group_by(DemographicSurvey) %>%
		summarise(
			maxSize 		= max(Size_t, na.rm=T),
			maxFruit 		= max(Fruit_Flowers_t, na.rm=T),
			maxHeight_t 	= max(Height_t, na.rm=T),
			maxWidth_t 		= max(Width_t, na.rm=T),
			maxPerpen_Width = max(Perpen_Width, na.rm=T)
		) %>%
		filter(
			is.na(maxSize) |
			maxSize < 0 |
			is.na(maxFruit) |
			maxFruit < 0 |
			is.na(maxHeight_t) |
			maxHeight_t < 0 |
			is.na(maxWidth_t) |
			maxWidth_t < 0 |
			is.na(maxPerpen_Width) |
			maxPerpen_Width < 0 
		)	
	if (dim(temp)[1] > 0) {
		write.csv(temp3,"PlantsNotMeasuredinSpringSummer.csv")
		warning(paste(
			"Plants marked as alive but no size records during the spring/summer."
		))
	}
	# WHICH PLANTS COMPLETELY DIED BUT DO NOT HAVE A SURVEY INDICATING SO IN THE MERGED SURVEYS?
	# Dead/missing observations from plant surveys before merge
	temp1 <- temp_A %>% filter(Dead == 1 | Missing == 1)
	# Dead/missing observations from plant surveys after merge
	temp2 <- temp_D %>% filter(Dead == 1 | Missing == 1)
	temp <- temp1 %>% filter(!(PlantID %in% temp2$PlantID))
	# which of these plants completely died?
	temp3 <- Plant_Info %>%
		filter(PlantID %in% temp$PlantID) %>%
		group_by(PlantID) %>%
		summarise(
			Dead = sum(ConfirmedDeadMissing, na.rm=T)/
				length(ConfirmedDeadMissing)
		) %>%
		filter(Dead >= 1)
	if (dim(temp3)[1] > 0) {
		write.csv(temp3,"PlantsNotSurveyedasDead.csv")
		warning(paste(
			"Plant that is missing/dead is not indicated as such in merged surveys. Date written to csv file."
		))
	}
	# WARNING IF PLANTS MARKED DEAD HAVE NON-ZERO SIZE/FRUIT COUNT MEASUREMENTS
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
	# WARNING IF PLANTS MARKED DEAD HAVE NON-ZERO SIZE/FRUIT COUNT MEASUREMENTS
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
	# HOW MANY PLANTS WITH LESS THAN 5 SEGMENTS HAD FRUIT/FLOWERS?
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
	# THROW A WARNING IF PUSILLA HAS FLOWERS BEFORE SUMMER 2015 (THEY WERE ONLY OBSERVED SPRING/SUMMER 2015)
	temp <- temp_D %>%
		filter(
			Species == "pusilla",
			FecundityYear < "2015",
			Num_FlowerBuds > 0 |
			Num_Fruit_red > 0 |
			Num_Fruit_green > 0 |
			Num_Flowers > 0 |
			Fruit_t > 0 |
			Fruit_Flowers_t > 0
		)
	if (dim(temp)[1] > 0) {
		write.csv(temp, "OpusillaPlantsWithFruitPriorTo2015.csv")
		warning(paste(
			"O. pusilla plants recorded with fruit/flowers prior to 2015."
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
	temp_D$Date %<>% as.Date
	return(temp_D)
}
