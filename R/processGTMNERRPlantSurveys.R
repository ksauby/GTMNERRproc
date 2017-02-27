

#' Merge Plant Info and Plant Surveys data files
#'
#' @description Add Island, Cluster, and Host Species information to the Plant_Surveys dataset.
#' @param Plant_Surveys Plant Survey Dataset
#' @param Plant_Info  Plant Information Dataset
#'
#' @export

mergePlantSurveysPlantInfo <- function(Plant_Surveys, Plant_Info) {
	Plant_Surveys <- Plant_Info %>%
		dplyr::select(
			ClusterID, 
			InDemographicStudy, 
			PlotPlantID, 
			Tag_Number, 
			Island, 
			Cluster2, 
			Network, 
			Species, 
			InBigPlantStudy, 
			N.PlotPlantIDs, 
			Easting, 
			Northing,
			RecruitmentMode
		) %>%
		merge(Plant_Surveys, by = "PlotPlantID")
	return(Plant_Surveys)
}

#' Add Sampling Period Variable
#'
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

addSamplingPeriods <- function(Plant_Surveys) {
	Plant_Surveys$DemographicSurvey <- "NA"
	Plant_Surveys %<>% 
		group_by(Date) %>% 
		mutate(
			# SURVEY 0
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date < "2013-05-14"), "0"),
			# SURVEY 1 - SPRING/SUMMER 2013
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2013-05-14" & Date <= "2013-08-06"), "1"),
			# SURVEY 2 - FALL/WINTER 2013/2014
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2013-12-13" & Date <= "2014-01-28"), "2"),
			# SURVEY 3 - SPRING/SUMMER 2014
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2014-05-06" & Date <= "2014-09-24"), "3"),
			# SURVEY 4 - WINTER 2015
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2015-01-08" & Date <= "2015-02-21"), "4"),
			# SURVEY 5 - SPRING/SUMMER 2015
			DemographicSurvey = replace(DemographicSurvey, 
				which(Date >= "2015-05-01"), "5"))
	return(Plant_Surveys)
}

#' Add Sampling Year Variable
#'
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

addSamplingYear <- function(Plant_Surveys) {
	Plant_Surveys$SamplingYear <- NA
	Plant_Surveys %<>% 
		group_by(Date) %>% 
		mutate(
			SamplingYear = replace(
				SamplingYear, 
				which(Date >= "2012-12-02" & Date < "2013-05-01"), 
				2012
			),
			SamplingYear = replace(
				SamplingYear, 
				which(Date >= "2013-05-01" & Date < "2014-05-01"), 
				2013
			),
			SamplingYear = replace(
				SamplingYear, 
				which(Date >= "2014-05-01" & Date < "2015-05-01"), 
				2014
			),
			SamplingYear = replace(
				SamplingYear, 
				which(Date >= "2015-05-01"), 
				2015
			)
		)
	return(Plant_Surveys)
}

#' Determine Fruit and Flower Presence
#'
#' @description Create separate variables indicating whether fruit and fruit and flowers were present.
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

calculateFruitPresence <- function(Plant_Surveys) {
	Plant_Surveys %>% 
		mutate(
			FruitPres_t 		= ifelse(Fruit_t > 0, 1, 0),
			FruitFlowerPres_t 	= ifelse(Fruit_Flowers_t > 0, 1, 0)
		) %>%
		as.data.table %>%
		# add NAs
		.[is.na(Fruit_t), FruitPres_t := NA] %>%
		.[is.na(Fruit_Flowers_t), FruitPres_t := NA] %>%
		as.data.frame
}

#' Calculate Plant Volume
#'
#' @description Calculate plant volume as a cone, cylinder, and elliptic cylinder.
#' @param Plant_Surveys Plant_Surveys Plant Survey Dataset
#'
#' @export

calculatePlantVolume <- function(Plant_Surveys) {
	Plant_Surveys %>% 
		mutate(
			Cone_t 				= pi * (((Width_t + Perpen_Width)/4)^2) * 
									Height_t / 3,
			Cylinder_t 			= pi * ((Perpen_Width/2)^2) * Width_t,
			Elliptic_Cylinder_t = pi * Height_t/2 * Perpen_Width/2 * Width_t
		)
}

#' Determine if a survey was complete
#'
#' @param x Plant_Surveys Plant Survey Dataset
#'
#' @export

Complete_Surveys_function <- function(x){
	ifelse(rowSums(is.na(x))==0, 1, 0)
	return(x)
}

#' Determine whether a survey was complete
#'
#' @description Determine whether all data was collected during a survey.
#' Determine complete surveys for:
#' \itemize{
#'  \item insect surveys
#'  \item plant size
#'  \item size measurements (cm)
#'  \item fruit, flowers, and flower buds
#'  \item all data
#' 	}
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

determineCompleteSurveys <- function(Plant_Surveys) {
	Plant_Surveys$complete_insect_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
			"CA_t",
			"ME_t",
			"Unknown_Moth_t",
			"Old_Moth_Evidence_t"
		)]))==0, 1, 0
	)
	Plant_Surveys$complete_segments_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
			"Plant_Segments_w_leaves",
			"Plant_Segments_wo_leaves",
			"Plant_Segments_woody"
		)]))==0, 1, 0
	)
	Plant_Surveys$complete_size_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
			"Height_t",
			"Width_t",
			"Perpen_Width"
		)]))==0, 1, 0
	)
	Plant_Surveys$complete_fruit_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
			"Num_FlowerBuds",
			"Num_Fruit_red",
			"Num_Fruit_green",
			"Num_Flowers"
		)]))==0, 1, 0
	)
	Plant_Surveys$complete_surveys <- ifelse(rowSums(is.na(
		Plant_Surveys[, 
		c(
		# insects
		"CA_t",
		"ME_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t",
		# number of segments
		"Size_t",
		"Plant_Segments_w_leaves",
		"Plant_Segments_wo_leaves",
		"Plant_Segments_woody",
		# size in cm
		"Height_t",
		"Width_t",
		"Perpen_Width",
		# flowers and fruit
		"Fruit_t" 
		)]))==0, 1, 0
	)
	return(Plant_Surveys)
}

#' Determine if an insect species was ever detected during the study period
#'
#' @description Determine if an insect species was ever detected during the study period.
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

determineInsectPresenceDuringStudy <- function(Plant_Surveys) {
	Plant_Surveys.present <- Plant_Surveys %>%
		group_by(PlantID) %>%
		summarise(
			CAPresent = ifelse(sum(CA_t, na.rm=T) > 0, 1, 0),
			MEPresent = ifelse(sum(ME_t, na.rm=T) > 0, 1, 0)
			) %>%
		as.data.frame()
	Plant_Surveys %>% merge(Plant_Surveys.present, by="PlantID")
}

#' Rename Species levels
#'
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

renameSpecies <- function(Plant_Surveys) {
	Plant_Surveys %>% 
		as.data.table %>%
		.[Species == "pusilla", Species := "Opuntia pusilla"] %>%
		.[Species == "stricta", Species := "Opuntia stricta"] %>%
		as.data.frame
}

#' Rename Convert 1/0 to Yes/No
#'
#' @param x Vector of 0/1 data
#'
#' @export

Yes_No_from_1_0_Function <- function(x){
	x[x > 0] <- "Yes"
	x[x == 0] <- "No"
	return(x)
}

#' Make Insect Factor Variables
#'
#' @description Make new insect variables with "yes"/"no" values instead of 0/1.
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

createInsectFactorVariables <- function(Plant_Surveys) {
	Plant_Surveys$C_cactorum 	<- Plant_Surveys$CA_t
	Plant_Surveys$M_prodenialis <- Plant_Surveys$ME_t
	Plant_Surveys[,c(
		"C_cactorum",
		"M_prodenialis")] %<>%
		apply(., 2, Yes_No_from_1_0_Function) %>%
		apply(., 2, as.factor
	)
	return(Plant_Surveys)
}

#' Format as Factors
#'
#' @description Format variables as factors.
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

formatasFactors <- function(x) {
	factors <- c("ClusterID", "Network", "Island", "Species", 
		"DemographicSurvey", "Year", "Season", "C_cactorum", 
		"M_prodenialis")
	for (i in 1:length(factors)) {
		if (factors[i] %in% names(x)) {
			x[, factors[i]] %<>% as.factor
		}
	}
	return(x)
}

#' Process Plant Survey Data
#'
#' @description Steps:
#' \itemize{
#'  \item add column, "DemographicSurvey"
#'	\itemize{
#'		\item survey 1 - spring/summer 2013
#'		\item survey 2 - fall/winter 2013/2014
#'		\item survey 3 - spring/summer 2014
#'		\item survey 4 - winter 2015
#'		\item survey 5 - spring/summer 2015
#'	}
#'  \item addSamplingYear
#'	\itemize{
#'		\item 2012 - Date >= "2012-12-02" & Date < "2013-05-01"
#'		\item 2013 - Date >= "2013-05-01" & Date < "2014-05-01"
#'		\item 2014 - Date >= "2014-05-01" & Date < "2015-05-01"
#'		\item 2015 - Date >= "2015-05-01"
#'	}
#' }
#'
#' @export

processPlantSurveysafterMergewPlantInfo <- function(Plant_Surveys) {
	Plant_Surveys %>%
		filter(InBigPlantStudy!="yes" & InBigPlantStudy!="Yes") %>%
		addSamplingPeriods %>%
		addSamplingYear %>%
		as.data.frame
}
	
#' Process Plant Survey Data, per plant
#'
#' @description Steps:
#' \itemize{
#'  	\item merge records for the same plant from multiple plots; the data to be merged must have been collected on the same day.
#'  	\item Create separate variables indicating whether fruit and fruit and flowers were present
#'  	\item Calculate plant volume as a cone, cylinder, and elliptic cylinder
#'  	\item Determine if an insect species was ever detected during the study period
#'  	\item Rename species levels ("Opuntia stricta" instead of "stricta" and "Opuntia pusilla" instead of "pusilla")
#' 	 	\item Make new insect variables with "yes"/"no" values instead of 0/1, named "C_cactorum" and "M_prodenialis"
#'  	\item format the variables "ClusterID", "Network", "Island", "Species", "DemographicSurvey", "SamplingYear", "Year", "Season", "C_cactorum", "M_prodenialis" as factors
#' 	}
#'
#' @export
	
processSurveysMergedbyPlant <- function(Plant_Surveys) {
	Plant_Surveys %>%
		mergePlantRecordsfromMultiplePlots %>%
		calculateFruitPresence %>%
		calculatePlantVolume %>%
		determineInsectPresenceDuringStudy %>%
		renameSpecies %>%
		createInsectFactorVariables %>%
		formatasFactors
}