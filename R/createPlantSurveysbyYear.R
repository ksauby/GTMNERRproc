#' Create Dataset by Year
#'
#' @description Create variabiles indicating whether an insect was present during the study. Useful in particular for Life Table Response Experiments.

#' @param Plant_Surveys_by_Plant Plant Survey Dataset
#'
#' @export

createPlantSurveysbyYear <- function(Plant_Surveys_by_Plant) {
	Plant_Surveys_by_Year <- Plant_Surveys_by_Plant %>% 
		group_by(SamplingYear, PlantID) %>%
		summarise(
			Species = Species[1],
			Network = Network[1],
			Island = Island[1],
			RecruitmentMode = RecruitmentMode[1],
			Date = min(Date),
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
			Missing = max(Missing, na.rm=T)
		) %>% 
		mutate(PrevSamplingYear = SamplingYear - 1)
	# create PrevYear
	Plant_Surveys_by_Year$Year <- year(Plant_Surveys_by_Year$Date)
	Plant_Surveys_by_Year$PrevYear <- year(Plant_Surveys_by_Year$Date) - 1
	Plant_Surveys_by_Year %<>% createInsectPresDuringStudy
	return(Plant_Surveys_by_Year)
}