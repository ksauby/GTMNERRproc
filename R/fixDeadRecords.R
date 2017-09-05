#' Fix Dead Records for Transition Matrix Data
#'

#' @param Plant.Surveys.TM.largest.parent Transition matrix data.

#' @export

fixDeadRecords <- function(Plant.Surveys.TM.largest.parent) {
	plants_measured_dead_year <- Plant.Surveys.TM.largest.parent %>% 
		filter(DeadMissingbyEndofYear==1, !(is.na(SizewClones_t)))
	# make sure for plants that were measured in the spring/summer but dead by fall/winter there is another year where the plant is in the dead stage
	plants_dead_year <- Plant.Surveys.TM.largest.parent %>% 
		filter(DeadMissingbyEndofYear==1, is.na(SizewClones_t))
	# make extra year surveys for these plants
	need_dead_survey <- plants_measured_dead_year %>%
		filter(!(PlantID %in% plants_dead_year$PlantID)) %$%
		PlantID
	Plant.Surveys.TM.largest.parent %<>% 
		filter(PlantID %in% need_dead_survey) %>% 
		group_by(PlantID) %>% 
		filter(FecundityYear == max(FecundityYear)) %>%
		mutate(
			FecundityYear = FecundityYear + 1,
			SizewClones_t = NA,
			Fruit_Flowers_t = NA,
			CA_t = NA,
			ME_t = NA,
			Old_Moth_Evidence_t = NA,
			stage = "dead"
		) %>%
		rbind.fill(Plant.Surveys.TM.largest.parent)
	# remove extra dead records
	Plant.Surveys.TM.largest.parent %<>%
		arrange(PlantID, FecundityYear) %>%
		group_by(PlantID) %>%
		mutate(prev_stage = lag(stage, default="NA")) %>%
		filter(prev_stage!="dead")
	Plant.Surveys.TM.largest.parent %>% dplyr::select(-prev_stage)
}


