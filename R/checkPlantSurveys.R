#' Check Plant Surveys for Errors
#' @param Plant_Surveys
#' @export

checkPlantSurveys <- function(Plant_Surveys) {
	# WHICH PLANTS WERE RECORDED AS ALIVE IN THE SPRING/SUMMER BUT HAD NO SIZE RECORDS?
	temp <- Plant_Surveys %>%
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
	# HOW MANY PLANTS WITH LESS THAN 5 SEGMENTS HAD FRUIT/FLOWERS?
	temp <- Plant_Surveys %>%
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
	temp <- Plant_Surveys %>%
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
}