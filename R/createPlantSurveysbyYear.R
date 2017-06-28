#' Create Dataset by Year
#'
#' @description Create variabiles indicating whether an insect was present during the study. Useful in particular for Life Table Response Experiments.

#' @param Plant_Surveys_by_Plant Plant Survey Dataset
#'
#' @export

createPlantSurveysbyYear <- function(Plant_Surveys_by_Plant) {
	# 2849

	Plant_Surveys_by_Year <- Plant_Surveys_by_Plant %>% 
		group_by(FecundityYear, PlantID) %>%
		dplyr::summarise(
			FruitPres_t 				= Maximum(FruitPres_t),
			Fruit_Flowers_t 			= Maximum(Fruit_Flowers_t),
			FruitFlowerPres_t 			= Maximum(FruitFlowerPres_t),
			Size_t 						= Maximum(Size_t),
			Height_t 					= Maximum(Height_t),
			Fruit_t 					= Maximum(Fruit_t),
			FruitPres_t 				= Maximum(FruitPres_t),
			CA_t 						= Maximum(CA_t),
			ME_t 						= Maximum(ME_t),
			DA_t 						= Maximum(DA_t),
			CH_t 						= Maximum(CH_t),
			Gerstaeckeria_t 			= Maximum(Gerstaeckeria_t),
			Unknown_Moth_t 				= Maximum(Unknown_Moth_t),
			Old_Moth_Evidence_t 		= Maximum(Old_Moth_Evidence_t),
			Moth_Evidence_t 			= Maximum(Moth_Evidence_t),
			Insect_Evidence_t 			= Maximum(Insect_Evidence_t),
			DeadbyEndofYear 			= Maximum(Dead),
			MissingbyEndofYear 			= Maximum(Missing),
			DeadMissingbyEndofYear = sum(
				c(DeadbyEndofYear, MissingbyEndofYear),
				na.rm=T
			)
		) %>%
		ungroup() %>%
		rowwise() %>%
		mutate(
			PrevFecundityYear = FecundityYear - 1,
			Alive = abs(DeadMissingbyEndofYear - 1)
		)
	# figure out spring/summer survey dates	
	temp <- Plant_Surveys_by_Plant %>%
		filter(
			month(Date) == 5 |
			month(Date) == 6 |
			month(Date) == 7 |
			month(Date) == 8 |
			month(Date) == 9,
		    !(
				is.na(Fruit_Flowers_t) |
				is.na(Size_t)
			)
		) %>%
		dplyr::select(
			Date,
			PlantID,
			FecundityYear
		) %>%
		setnames("Date", "SurveyDate_SpringSummer")
	# merge
	########################
	# DO I LOSE PLANTS HERE?
	########################	
	Plant_Surveys_by_Year %<>% merge(temp, by=c("PlantID", "FecundityYear"), all.x=T)
	return(Plant_Surveys_by_Year)
}