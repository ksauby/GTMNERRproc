#' Process Plot Survey Data
#'
#' @description Steps:
#' \enumerate{
	#'  \item Warnings and Errors:
	#' \itemize{
		#'  \item Duplicate plot surveys for plot(s)
	#' }	
	#'  \item limit to only plots still in study
	#'  \item format date
	#'  \item Change Yes/No to 1/0
	#'  \item make variables numeric
	#'  \item If \emph{O. pusilla} is not present, change all \emph{O. pusilla}-dependent data to NA
	#'  \item If O. stricta is not present, change all O. stricta-dependent data to NA
#'	}
#'
#' @export

processOccupancyPlotSurveys <- function(Plot_Surveys, Plot_Info) {
	# ----------------------------------------------------------- ERROR MESSAGES
	# Duplicates in Plot Surveys
	dups = Plot_Surveys %>%
		group_by(Tag_Number, Date) %>%
		summarise(Nrecords = length(CACA_on_Pusilla)) %>%
		as.data.frame %>%
		filter(Nrecords > 1)
	if (dim(dups)[1] > 0) {
		warning(paste(
			"Duplicate plot surveys for plot(s)",
			paste(unique(dups$Tag_Number), collapse=", "),
			"on date(s)",
			paste(unique(dups$Date), collapse=", ")
		))
	}
	# duplicates for 1795 on 2013-02-10 are okay - I accidentally surveyed the plot twice; keep it for detectability
	# ---------------------------- LIMIT TO ONLY SURVEYS OF PLOTS STILL IN STUDY
	# Plot_Surveys_All <- Plot_Surveys
	Plot_Surveys %<>% .[(.$Tag_Number %in% Plot_Info$Tag_Number), ]
	# -------------------------------------------------------------- FORMAT DATE
	Plot_Surveys$Date %<>% Format_Date_Function
	# -------------------------------------------------- CHANGE YES, NO to 0, 1 
	Plot_Surveys[,c(
		"Ammophila",
		"CACA_on_Ammo",
		"MEPR_on_Ammo",
		"CHVI_on_Ammo",
		"DACT_on_Ammo",
		"UnknwnMoth_on_Ammo",
		"Old_Moth_Evidence_Ammophila",
		"Pusilla",
		"CACA_on_Pusilla",
		"MEPR_on_Pusilla",
		"CHVI_on_Pusilla",
		"DACT_on_Pusilla",
		"UnknwnMoth_on_Pusilla",
		"Old_Moth_Evidence_Pusilla",
		"Stricta",
		"CACA_on_Stricta",
		"MEPR_on_Stricta",
		"CHVI_on_Stricta",
		"DACT_on_Stricta",
		"UnknwnMoth_on_Stricta",
		"Old_Moth_Evidence_Stricta")] %<>% 
		apply(., 2, Yes_Function
	)
	Plot_Surveys[,c(
		"Ammophila",
		"CACA_on_Ammo",
		"MEPR_on_Ammo",
		"CHVI_on_Ammo",
		"DACT_on_Ammo",
		"UnknwnMoth_on_Ammo",
		"Old_Moth_Evidence_Ammophila",
		"Pusilla",
		"CACA_on_Pusilla",
		"MEPR_on_Pusilla",
		"CHVI_on_Pusilla",
		"DACT_on_Pusilla",
		"UnknwnMoth_on_Pusilla",
		"Old_Moth_Evidence_Pusilla",
		"Stricta",
		"CACA_on_Stricta",
		"MEPR_on_Stricta",
		"CHVI_on_Stricta",
		"DACT_on_Stricta",
		"UnknwnMoth_on_Stricta",
		"Old_Moth_Evidence_Stricta")] %<>%
		apply(., 2, No_Function
	)
	Plot_Surveys[,c(
		"Ammophila",
		"CACA_on_Ammo",
		"MEPR_on_Ammo",
		"CHVI_on_Ammo",
		"DACT_on_Ammo",
		"UnknwnMoth_on_Ammo",
		"Old_Moth_Evidence_Ammophila",
		"Pusilla",
		"CACA_on_Pusilla",
		"MEPR_on_Pusilla",
		"CHVI_on_Pusilla",
		"DACT_on_Pusilla",
		"UnknwnMoth_on_Pusilla",
		"Old_Moth_Evidence_Pusilla",
		"Stricta",
		"CACA_on_Stricta",
		"MEPR_on_Stricta",
		"CHVI_on_Stricta",
		"DACT_on_Stricta",
		"UnknwnMoth_on_Stricta",
		"Old_Moth_Evidence_Stricta")] %<>% 
		apply(., 2, NA_Function
	)
	# ------------------------------------------------------------- MAKE NUMERIC
	Plot_Surveys[,c(
		"Ammophila",
		"CACA_on_Ammo",
		"MEPR_on_Ammo",
		"CHVI_on_Ammo",
		"DACT_on_Ammo",
		"UnknwnMoth_on_Ammo",
		"Old_Moth_Evidence_Ammophila",
		"Pusilla",
		"CACA_on_Pusilla",
		"MEPR_on_Pusilla",
		"CHVI_on_Pusilla",
		"DACT_on_Pusilla",
		"UnknwnMoth_on_Pusilla",
		"Old_Moth_Evidence_Pusilla",
		"Stricta",
		"CACA_on_Stricta",
		"MEPR_on_Stricta",
		"CHVI_on_Stricta",
		"DACT_on_Stricta",
		"UnknwnMoth_on_Stricta",
		"Old_Moth_Evidence_Stricta")] %<>% 
		apply(., 2, as.numeric
	)
	# ------------------ CHANGE CACTUS DEPENDENT DATA TO NA IF CACTI NOT PRESENT
	# If pusilla is not present, change all pusilla-dependent data to NA
	Plot_Surveys_Pusilla_1 <- Plot_Surveys %>% filter(Pusilla==1)
	Plot_Surveys_Pusilla_0 <- Plot_Surveys %>% 
		filter(Pusilla==0 | is.na(Pusilla))
	Plot_Surveys_Pusilla_0[,c(
		"CACA_on_Pusilla",
		"MEPR_on_Pusilla",
		"CHVI_on_Pusilla",
		"DACT_on_Pusilla",
		"UnknwnMoth_on_Pusilla",
		"Old_Moth_Evidence_Pusilla",
		"Old_Moth_Evidence_recent_Pusilla",
		"Percent_Cover_Pusilla",
		"Height_Pusilla")] %<>% 
		apply(2, NA_Function
	)
	Plot_Surveys <- rbind(Plot_Surveys_Pusilla_1, Plot_Surveys_Pusilla_0)
	# If stricta is not present, change all stricta-dependent data to NA
	Plot_Surveys_Stricta_1 <- Plot_Surveys %>% filter(Stricta==1)
	Plot_Surveys_Stricta_0 <- Plot_Surveys %>% 
		filter(Stricta==0 | is.na(Stricta))
	Plot_Surveys_Stricta_0[,c(
		"CACA_on_Stricta",
		"MEPR_on_Stricta",
		"CHVI_on_Stricta",
		"DACT_on_Stricta",
		"UnknwnMoth_on_Stricta",
		"Old_Moth_Evidence_Stricta",
		"Old_Moth_Evidence_recent_Stricta",
		"Percent_Cover_Stricta",
		"Height_Stricta")] %<>% 
		apply(2, NA_Function
	)
	Plot_Surveys <- rbind(Plot_Surveys_Stricta_1, Plot_Surveys_Stricta_0)
	# ------------------------------------------------------------------------ #
	return(Plot_Surveys)
}