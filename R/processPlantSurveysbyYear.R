#' Process Plant Surveys by Year data
#'
#' @param Plant.Surveys.by.Year
#'
#' @export

processPlantSurveysbyYear <- function(Plant.Surveys.by.Year) {
	Plant.Surveys.by.Year %<>% calculateDateLags
	Plant.Surveys.by.Year %<>%
		arrange(FecundityYear) %>% 
		group_by(PlantID) %>%
		mutate_at(
			.funs=funs("1" = "lag"),
			.vars=c("Size_t", "SizewClones_t", "CA_t", "ME_t", "DA_t", "CH_t", "Unknown_Moth_t", "Old_Moth_Evidence_t", "Moth_Evidence_t", "Insect_Evidence_t")
			) %>%
		createNewInsectVariables(
			arrange.variable="FecundityYear", 
			grouping.variable="PlantID"
		) %>% 			
		mutate(
			RGR_Size = (Size_t - Size_t_1) / (DaysSincePrevSurvey*Size_t_1),
			max_RGR_Size = (SizewClones_t - Size_t_1) / 
				(DaysSincePrevSurvey*Size_t_1),
			RGR_Size365 = RGR_Size*365,
			max_RGR_Size365 = max_RGR_Size*365						
		)
	# ------------------------------------------------------------------------ #
	########################## ERROR MESSAGE
		# check for PlantID = NA
		dups <- Plant.Surveys.by.Year %>% 
			filter(is.na(PlantID))

		if (dim(dups)[1] > 0) {
				stop("NA values for PlantID.")
		}
	# ------------------------------------------------------------------------ #
	# fix variable formats
	# ------------------------------------------------------------------------ #
	Plant.Surveys.by.Year[,c(
		"Insect_Evidence_t",
		"Moth_Evidence_t",
		"Gerstaeckeria_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t",
		"NatInsect_t",
		"Insect_t",
		"DA_t",
		"CH_t",
		"ME_t",
		"CA_t",                
		"Size_t",
		"FruitPres_t",
		"FruitFlowerPres_t",
		"Fruit_t",
		"Size_t_1",
		"DaysSincePrevSurvey",
		"CA_t_1",
		"ME_t_1",
		"CH_t_1",
		"DA_t_1",
		"Old_Moth_Evidence_t_1",
	    "Unknown_Moth_t_1",
		"Moth_Evidence_t_1",
		"Insect_Evidence_t_1",
		"NClones_t",
		"NSegLosttoClones_t"
	)] %<>% apply(., 2, as.integer)
	Plant.Surveys.by.Year[,c(
		"Height_t",
	 	"RGR_Size",
	 	"RGR_Size365",
		"max_RGR_Size365"
	)] %<>% apply(., 2, as.numeric)
	Plant.Surveys.by.Year[,c(
		"Size_t",
		"CA_t",
		"ME_t",
		"Old_Moth_Evidence_t",
		"Fruit_Flowers_t"
	)] %<>% apply(., 2, NA_Function)
	# ------------------------------------------------------------------------ #
	Plant.Surveys.by.Year %<>% createHabitatType
	# ------------------------------------------------------------------------ #
	Plant.Surveys.by.Year %<>% classifybyStage
	return(Plant.Surveys.by.Year)
}
