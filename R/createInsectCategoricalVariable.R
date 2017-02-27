#' Create Moth Occupancy Variable
#'
#' @param Plant_Surveys_by_Year Dataset
#'
#' @export

createMothOccupancytVariable <- function() {
	Plant_Surveys_by_Year$MothOccupancy_t <- with(
		Plant_Surveys_by_Year, 
		paste(CA_t, ME_t, Old_Moth_Evidence_t, Unknown_Moth_t, sep="")
	)

	# No insects
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="NANANANA"), ]$MothOccupancy_t <- "No Insects Observed"
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="0000"), ]$MothOccupancy_t <- "No Insects Observed"

	# Insect damage only
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="0010"), ]$MothOccupancy_t <- "Moth Damage Only,\nNo Insects Observed"
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="NANA1NA"), ]$MothOccupancy_t <- "Moth Damage Only,\nNo Insects Observed"

	# Invasive Moth only
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="1NANANA"), ]$MothOccupancy_t <- "Invasive Moth"
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="1NA1NA"), ]$MothOccupancy_t <- "Invasive Moth"
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="1000"), ]$MothOccupancy_t <- "Invasive Moth"
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="1010"), ]$MothOccupancy_t <- "Invasive Moth"

	# Native Moth Only
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="0110"), ]$MothOccupancy_t <- "Native Moth"
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="0100"), ]$MothOccupancy_t <- "Native Moth"
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="NA1NANA"), ]$MothOccupancy_t <- "Native Moth"

	# Native and Invasive Moths
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="1100"), ]$MothOccupancy_t <- "Invasive and Native Moths"
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy_t=="1110"), ]$MothOccupancy_t <- "Invasive and Native Moths"

	Plant_Surveys_by_Year$MothOccupancy_t %<>% factor(levels=c(
		"No Insects Observed",             
		"Moth Damage Only,\nNo Insects Observed",
		"Native Moth",                           
		"Invasive Moth",                         
		"Invasive and Native Moths"
	))
	
	return(Plant_Surveys_by_Year)
}

#' Create Moth Occupancy During Study Variable
#'
#' @param Plant_Surveys_by_Year Dataset
#'
#' @export

createMothOccupancyVariable <- function(Plant_Surveys_by_Year) {
	Plant_Surveys_by_Year$MothOccupancy <- with(
		Plant_Surveys_by_Year, 
		paste(CAPlantPres, MEPlantPres, OldMothPlantPres, sep="")
	)

	# No insects
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy=="NANANA"), ]$MothOccupancy <- "No Insects Observed"
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy=="000"), ]$MothOccupancy <- "No Insects Observed"

	# Insect damage only
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy=="001"), ]$MothOccupancy <- "Moth Damage Only,\nNo Insects Observed"

	# Invasive Moth only
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy=="100"), ]$MothOccupancy <- "Invasive Moth"
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy=="101"), ]$MothOccupancy <- "Invasive Moth"

	# Native Moth Only
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy=="011"), ]$MothOccupancy <- "Native Moth"
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy=="010"), ]$MothOccupancy <- "Native Moth"

	# Native and Invasive Moths
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy=="110"), ]$MothOccupancy <- "Invasive and Native Moths"
	Plant_Surveys_by_Year[which(Plant_Surveys_by_Year$MothOccupancy=="111"), ]$MothOccupancy <- "Invasive and Native Moths"

	Plant_Surveys_by_Year$MothOccupancy %<>% factor(levels=c(
		"No Insects Observed",             
		"Moth Damage Only,\nNo Insects Observed",
		"Native Moth",                           
		"Invasive Moth",                         
		"Invasive and Native Moths"
	))
	return(Plant_Surveys_by_Year)
}