#' Create Dataset by Year
#'
#' @description Create variabiles indicating whether an insect was present during the study. Useful in particular for Life Table Response Experiments.

#' @param Plant_Surveys_by_Plant Plant Survey Dataset
#'
#' @export

createPlantSurveysbyYear <- function(Plant_Surveys_by_Plant) {
	Plant_Surveys_by_Year <- Plant_Surveys_by_Plant %>% 
		group_by(FecundityYear, PlantID) %>%
		dplyr::summarise(
			Species = Species[1],
			Network = Network[1],
			Island = Island[1],
			RecruitmentMode = RecruitmentMode[1],
			#Date = min(Date),
			FruitPres_t = max(FruitPres_t, na.rm=T),
			Fruit_Flowers_t = max(Fruit_Flowers_t, na.rm=T),
			FruitFlowerPres_t = max(FruitFlowerPres_t, na.rm=T),
			Size_t = max(Size_t, na.rm=T),
			Height_t = max(Height_t, na.rm=T),
			Fruit_t = max(Fruit_t, na.rm=T),
			FruitPres_t = max(FruitPres_t, na.rm=T),
			CA_t = max(CA_t, na.rm=T),
			ME_t = max(ME_t, na.rm=T),
			DA_t = max(DA_t, na.rm=T),
			CH_t = max(CH_t, na.rm=T),
			Gerstaeckeria_t = max(Gerstaeckeria_t, na.rm=T),
			Unknown_Moth_t = max(Unknown_Moth_t, na.rm=T),
			Old_Moth_Evidence_t = max(Old_Moth_Evidence_t, na.rm=T),
			Moth_Evidence_t = max(Moth_Evidence_t, na.rm=T),
			Insect_Evidence_t = max(Insect_Evidence_t, na.rm=T),
			DeadbyEndofYear = max(Dead, na.rm=T),
			MissingbyEndofYear = max(Missing, na.rm=T),
			DeadMissingbyEndofYear = sum(
				c(DeadbyEndofYear, MissingbyEndofYear),
				na.rm=T
			)
		) %>% 
		rowwise() %>%
		mutate(
			PrevFecundityYear = FecundityYear - 1,
			Alive = abs(DeadMissingbyEndofYear - 1)
		)
	# create PrevYear
	Plant_Surveys_by_Year %<>% createInsectPresDuringStudy
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