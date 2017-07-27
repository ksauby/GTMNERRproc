#' Process GTMNERR Plant Survey Data
#'
#' @description Steps:
#' \itemize{
#'  \item Fix column names
#'  \item format dates
#'  \item format PlantIDs
#'  \item format convert "999" values to NA
#'  \item format change "yes"/"no" values of the insect survey, missing, and dead columns to 0/1
#'  \item format numeric columns
#'  \item add total segment column
#'  \item add fruit count column
#'  \item add fruit/flower count column
#'  \item check for observations of 0 pads, 0 height, or 0 width; if there are observations, stop
#'  \item add column, "DemographicSurvey"
#'	\itemize{
#'		\item survey 1 - spring/summer 2013
#'		\item survey 2 - fall/winter 2013/2014
#'		\item survey 3 - spring/summer 2014
#'		\item survey 4 - winter 2015
#'		\item survey 5 - spring/summer 2015
#'	}
#'  \item addFecundityYear
#'	\itemize{
#'		\item 2012 - Date >= "2012-12-02" & Date < "2013-05-01"
#'		\item 2013 - Date >= "2013-05-01" & Date < "2014-05-01"
#'		\item 2014 - Date >= "2014-05-01" & Date < "2015-05-01"
#'		\item 2015 - Date >= "2015-05-01"
#'	}
#' 	}
#' Column Names:
#' \itemize{
#'  \item PlantMeasureID Record number if Microsoft Access database
#'  \item First_Observer_Initials Initials of primary observer (should always be KS, for Kristen Sauby)
#'  \item Second_Observer_Initials Initials of secondary observer; CJP - Cory Penca; YP - Yani Paulay; KS - Kristen Sauby; JW: Juliana Welch; CW: Cedric Worman; AP: Adam Payton
#'  \item Date Date
#'  \item PlantID Unique number for the individual plant
#'  \item Plant_collected Were plant samples collected?
#'  \item Time
#'  \item PlantPictures identifying numbers of photos taken
#'  \item PlantPictures_Direction direction that the photo(s) was(were) taken

#'  \item CA_t Presence/absence (1/0) of Cactoblastis cactorum
#'  \item CACA_collected Were samples of Cactoblastis cactorum collected?
#'  \item CACA_quantity Number of Cactoblastis cactorum samples collected

#'  \item ME_t Presence/absence (1/0) of Melitara prodenialis
#'  \item MEPR_collected Were samples of Melitara prodenialis collected?
#'  \item MEPR_quantity Number of Melitara prodenialis samples collected

#'  \item CH_t Presence/absence (1/0) of Chelinidea vittiger
#'  \item CHVI_collected Were samples of Chelinidea vittiger collected?
#'  \item CHVI_quantity Number of Chelinidea vittiger samples collected

#'  \item DA_t Presence/absence (1/0) of Dactylopius species
#'  \item DACT_collected Were samples of Dactylopius species collected?
#'  \item DACT_quantity Number of Dactylopius species samples collected

#'  \item Unknown_Moth_t Presence/absence (1/0) of unknown moth
#'  \item UnknownMoth_collected Were samples of unknown moth collected?
#'  \item UnknownMoth_quantity Number of unknown moth samples collected

#'  \item Old_Moth_Evidence_t Evidence of past moth infestation
#'  \item Old_Moth_Evidence_recent Whether evidence of past moth infestation appears recent or old

#'  \item Fungus Presence/absence (1/0) of plant fungal infection
#'  \item Gerstaeckeria Presence/absence (1/0) of Gerstaeckeria
#'  \item Plant_Segments_total Number of segments
#'  \item Plant_Segments_w_leaves New, green segments with leaves
#'  \item Plant_Segments_wo_leaves Green segments without leaves
#'  \item Plant_Segments_woody number of woody segments/trunks; these segments are entirely brown on the outside      
#'  \item Height_t maximum height in cm
#'  \item Width_t maximum width in cm
#'  \item Perpen_Width width, perpendicular to max width, in cm
#'  \item Num_FlowerBuds Number of flower buds
#'  \item Num_Fruit_red Number of red fruit
#'  \item Num_Fruit_green Number of green fruit
#'  \item Num_Flowers Number of flowers
#'  \item Num_Fruit use this when number is recorded but distinction by color is not made
#'  \item Pollinators
#'  \item Spiders
#'  \item Ants
#'  \item Other_collected_quantity
#'  \item Plant_Notes
#'  \item Insect_Notes        
#'  \item Other_Notes
#'  \item Dead Whether the plant is observed to be dead; 0 or 1
#'  \item Missing
#'  \item OutsideOfPlot "Yes" if plant is no longer in plot
#'  \item PlotPlantID Unique number for the individual plant; if a plant is counted in multiple plots, a letter is appended to the plant ID here (e.g., 9606a) and then removed for analysis
#'  \item Size_t The sum of Plant_Segments_total, Plant_Segments_w_leaves, Plant_Segments_wo_leaves, and Plant_Segments_woody

#'  \item Fruit_t The sum of Num_Fruit_red, Num_Fruit_green, and Num_Fruit
#'  \item Fruit_Flowers_t The sum of Num_FlowerBuds, Num_Flowers, Num_Fruit_red, Num_Fruit_green, and Num_Fruit
#' 	}
#'
#' @export

processPlantSurveys <- function(Plant.Surveys, Plant_Info) {
	# ----------------------------------------------------------------- WARNINGS
	# check first duplicate data entries
	dups <- Plant.Surveys %>% 
		group_by(PlantID, DateSurveyed) %>%
		summarise(n.obs = length(Plant_collected)) %>%
		filter(n.obs > 1)
	if (dim(dups)[1] > 0) {stop("Duplicates observations for a PlantID, Date combination are present in the dataset.")}
	# check for PlantID = NA
	dups <- Plant.Surveys %>% filter(is.na(PlantID))
	if (dim(dups)[1] > 0) {stop("NA values for PlantID.")}
	# are all Plant IDs from the Plant Surveys data in Plant Info?
	dups <- filter(Plant.Surveys, !(PlantID %in% Plant_Info$PlantID))[, 4:5]
	if (dim(
		filter(Plant.Surveys, !(PlantID %in% Plant_Info$PlantID))[, 4:5]
	)[1] > 0) {
		warning(paste(
			"These Plant IDs from Plant Surveys are not in Plant Info:",
			paste(unique(dups$PlantID), collapse=", ")
		))
	}
	# Duplicates in Plant Surveys
	# duplicates for 1795 on 2013-02-10 are okay - I accidentally surveyed the plot twice; keep it for detectability
	dups = Plant.Surveys %>%
		group_by(PlantID, DateSurveyed) %>%
		dplyr::summarise(Nrecords = length(First_Observer_Initials)) %>%
		as.data.frame %>% 
		arrange(PlantID) %>%
		filter(Nrecords > 1)
	if (dim(dups)[1] > 0) {
		warning(paste(
			"Duplicate surveys on the same date for the following plants:",
			paste(unique(dups$PlantID), collapse=", ")
		))
	}
	# check for size = 0
	dups <- Plant.Surveys %>% filter(Max_Height==0)
		if (dim(dups)[1] > 0) {stop("Max. height values = 0.")}
	dups <- Plant.Surveys %>% filter(Max_Width==0)
		if (dim(dups)[1] > 0) {stop("Max. width values = 0.")}
	dups <- Plant.Surveys %>% filter(Perpen_Width==0)
		if (dim(dups)[1] > 0) {stop("Perpendicular width values = 0.")}
	# ------------------------------------------------------------- CHANGE NAMES
	# remame size and height
	Plant.Surveys %<>%	as.data.table %>%
		setnames("Max_Height", 				"Height_t") %>%
		setnames("Max_Width", 				"Width_t") %>%
		setnames("CACA_Larvae", 			"CA_t") %>%
		setnames("MEPR_Larvae", 			"ME_t") %>%
		setnames("CHVI_Evidence", 			"CH_t") %>%
		setnames("DACT_Evidence", 			"DA_t") %>%
		setnames("UnknownMoth_Evidence", 	"Unknown_Moth_t") %>%
		setnames("Old_Moth_Evidence", 		"Old_Moth_Evidence_t") %>%
		setnames("DateSurveyed", 			"Date") %>%
		setnames("Gerstaeckeria", 			"Gerstaeckeria_t") %>%
		as.data.frame
	# formatting/preparation necessary for prepping Demographic Plant Info
	Plant.Surveys$Date %<>% Format_Date_Function
	Plant.Surveys %<>% arrange(Date)
	Plant.Surveys %<>% Format_PlantIDs_Function
	# ------------------------------------------------ CONVERT ALL "999s" to NAs
	Plant.Surveys[,c(
		"Plant_Segments_total", 
		"Plant_Segments_w_leaves",
		"Plant_Segments_wo_leaves",
		"Plant_Segments_woody",
		"Perpen_Width",
		"Width_t",
		"Height_t",
		"Num_FlowerBuds",
		"Num_Fruit_red",
		"Num_Fruit_green",
		"Num_Flowers",
		"Num_Fruit")] %<>% 
		apply(2, NA_Function
	)
	# ------------------- INSECT SURVEYS, MISSING, DEAD - CHANGE YES, NO to 0, 1
	Plant.Surveys[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Gerstaeckeria_t",
		"Old_Moth_Evidence_t",
		"Dead",
		"Missing")] %<>% 
		apply(2, Yes_Function
	)
	Plant.Surveys[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Gerstaeckeria_t",
		"Old_Moth_Evidence_t",
		"Dead",
		"Missing")] %<>%
		apply(2, No_Function
	)
	Plant.Surveys[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Gerstaeckeria_t",
		"Old_Moth_Evidence_t",
		"Dead",
		"Missing")] %<>% 
		apply(2, NA_Function
	)
	# ------------------------------------------------------------- MAKE NUMERIC
	Plant.Surveys[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Gerstaeckeria_t",
		"Old_Moth_Evidence_t",
		"Plant_Segments_total",
		"Plant_Segments_w_leaves",
		"Plant_Segments_wo_leaves",
		"Plant_Segments_woody",
		"Height_t",
		"Width_t",
		"Perpen_Width",
		"Num_FlowerBuds",
		"Num_Fruit_red",
		"Num_Fruit_green",
		"Num_Flowers",
		"Num_Fruit",
		"Dead",
		"Missing")] %<>% 
		apply(2, destring
	)
	# ------------------------------------------------------------ GERSTAECKERIA
	# Gerstaeckeria was not reliably surveyed so can only take values of NA or 1
	Plant.Surveys$Gerstaeckeria_t %<>% Zero_is_NA_Function
	# ---------------------------------------------- Change Missing = NA to zero
	Plant.Surveys$Missing %<>% NA_is_Zero_Function
	# ------------------------------------------------- ADD TOTAL SEGMENT COLUMN
	# do this so that plants that have no segments recorded (all NAs) have a total segment count = NA
	# for those plants that have fewer than four NAs (at least one segment column has a number), sum the segments
	Plant.Surveys$Size_t <- Plant.Surveys %>%
		dplyr::select(
			Plant_Segments_total,
			Plant_Segments_w_leaves,
			Plant_Segments_wo_leaves,
			Plant_Segments_woody
		) %>%
		apply(1, mysum)
	Plant.Surveys$Size_t %<>% Zero_is_NA_Function
	# --------------------------------------------------------- ADD FRUIT COLUMN
	Plant.Surveys$Fruit_t <- Plant.Surveys %>%
		dplyr::select(
			Num_Fruit_red,
			Num_Fruit_green,
			Num_Fruit
		) %>%
		apply(1, mysum)
	Plant.Surveys$Fruit_Flowers_t <- Plant.Surveys %>%
		dplyr::select(
			Num_FlowerBuds,
			Num_Flowers,
			Num_Fruit_red,
			Num_Fruit_green,
			Num_Fruit
		) %>%
		apply(1, mysum)
	# -------- variable indicating whether size/fruit measured during the survey
	Plant.Surveys$SegmentsMeasured <- Plant.Surveys %>%
		dplyr::select(
			Plant_Segments_w_leaves,
			Plant_Segments_wo_leaves,
			Plant_Segments_woody,
			Plant_Segments_total
		) %>%
		apply(1, mysum3)
	Plant.Surveys$FruitMeasured <- Plant.Surveys %>%
		dplyr::select(
			Num_FlowerBuds,
			Num_Flowers,
			Num_Fruit_red,
			Num_Fruit_green,
			Num_Fruit
		) %>%
		apply(1, mysum3)
	Plant.Surveys$SizeMeasured <- Plant.Surveys %>%
		dplyr::select(
			Height_t, 
			Width_t, 
			Perpen_Width
		) %>%
		apply(1, mysum3)
	# --------------------------------------------------------------------------
	Plant.Surveys$Date %<>%
		strptime("%Y-%m-%d") %>%
		as.POSIXct(format="%Y-%m-%d", tz="")
	Plant.Surveys %<>%
		addSamplingPeriods %>%
		assignSeason %>%
		createFecundityYear %>%
		as.data.frame
	Plant.Surveys$Date %<>% as.Date
	# ----------------------------------------------------------------- WARNINGS
	dups <- Plant.Surveys %>% filter(Size_t==0)
		if (dim(dups)[1] > 0) {stop("Size values = 0.")}
	# throw a warning if a plant has a recorded size but is also marked either dead or missing
	temp <- Plant.Surveys %>%
		filter(!(is.na(Size_t)) & Dead == 1)
	if (dim(temp)[1] > 0) {
		warning(paste(
			"PlantMeasureID Records ",
			paste(temp$PlantMeasureID, collapse=", "),
			"have size measurements but are also marked dead"
		))
	}
	temp <- Plant.Surveys %>%
		filter(!(is.na(Size_t)) & Missing == 1)
	if (dim(temp)[1] > 0) {
		warning(paste(
			"PlantMeasureID Records ",
			paste(temp$PlantMeasureID, collapse=", "),
			"have size measurements but are also marked missing"
		))
	}
	# ------------------------------------------------------------------------ #
	return(Plant.Surveys)
}