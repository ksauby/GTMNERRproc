

#' Process Original Plant Survey Data
#'
#' @description I collected this data in December 2012, then gave the plants new Plant IDs. This data cannot be used for the demography study but can be used to determine plot occupancy.
#' Steps:
#' \itemize{
#'  \item Fix column names
#'  \item format dates
#'  \item format PlantIDs
#'  \item format convert "999" values to NA
#'  \item format change "yes"/"no" values of the insect survey, missing, and dead columns to 0/1
#'  \item format numeric columns
#'  \item check for observations of 0 pads, 0 height, or 0 width; if there are observations, stop
#' 	}

#' 	}
#'
#' @export

processOriginalPlantData <- function(Original_Plant_Data) {
	# ------------------------------------------------------------- CHANGE NAMES
	# remame size and height
	Original_Plant_Data %<>% as.data.table %>%
		setnames("Max_Height", 				"Height_t") %>%
		setnames("Max_Width", 				"Width_t") %>%
		setnames("CACA_Larvae", 			"CA_t") %>%
		setnames("MEPR_Larvae", 			"ME_t") %>%
		setnames("CHVI_Evidence", 			"CH_t") %>%
		setnames("DACT_Evidence", 			"DA_t") %>%
		setnames("UnknownMoth_Evidence", 	"Unknown_Moth_t") %>%
		setnames("Old_Moth_Evidence", 		"Old_Moth_Evidence_t") %>%
		as.data.frame
	# formatting/preparation necessary for prepping Demographic Plant Info
	Original_Plant_Data$Date %<>% Format_Date_Function
	Original_Plant_Data %<>% arrange(Date)
	# ------------------------------------------------ CONVERT ALL "999s" to NAs
	Original_Plant_Data[,c(
		"Plant_Segments_w_leaves",
		"Plant_Segments_wo_Leaves",
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
	Original_Plant_Data[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t")] %<>% 
		apply(2, Yes_Function
	)
	Original_Plant_Data[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t")] %<>%
		apply(2, No_Function
	)
	Original_Plant_Data[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t")] %<>% 
		apply(2, NA_Function
	)
	# ------------------------------------------------------------- MAKE NUMERIC
	Original_Plant_Data[,c(
		"CA_t",
		"ME_t",
		"CH_t",
		"DA_t",
		"Unknown_Moth_t",
		"Old_Moth_Evidence_t",
		"Plant_Segments_w_leaves",
		"Plant_Segments_wo_Leaves",
		"Height_t",
		"Width_t",
		"Perpen_Width",
		"Num_FlowerBuds",
		"Num_Fruit_red",
		"Num_Fruit_green",
		"Num_Flowers",
		"Num_Fruit")] %<>% 
		apply(2, as.numeric
	)
	# ----------------------------------------------------------- ERROR MESSAGES
	# There should never be observations of 0 pads, 0 height, or 0 width
	# replace 0 with NA
	dups <- Original_Plant_Data %>% filter(Height_t==0)
	if (dim(dups)[1] > 0) {stop("Values for Height_t equal 0.")}

	dups <- Original_Plant_Data %>% filter(Width_t==0)
	if (dim(dups)[1] > 0) {stop("Values for Width_t equal 0.")}

	dups <- Original_Plant_Data %>% filter(Perpen_Width==0)
	if (dim(dups)[1] > 0) {stop("Values for Perpen_Width equal 0.")}
	return(Original_Plant_Data)
}
