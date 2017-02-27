#' Abbreviate Variable Names
#'
#' @description Steps:
#' \itemize{
#'  	\item Replace "CACA_on_Pusilla" with "P_Ca"
#'  	\item Replace "MEPR_on_Pusilla" with "P_Me"
#'  	\item Replace "CHVI_on_Pusilla" with "P_Ch"
#'  	\item Replace "UnknwnMoth_on_Pusilla" with "P_Umoth"
#'  	\item Replace "Old_Moth_Evidence_Pusilla" with "P_Omoth"
#'  	\item Replace "Height_Pusilla" with "P_H"
#'  	\item Replace "Percent_Cover_Pusilla" with "P_PC"
#'  	\item Replace "CACA_on_Stricta" with "S_Ca"
#'  	\item Replace "MEPR_on_Stricta" with "S_Me"
#'  	\item Replace "CHVI_on_Stricta" with "S_Ch"
#'  	\item Replace "UnknwnMoth_on_Stricta" with "S_Umoth"
#'  	\item Replace "Old_Moth_Evidence_Stricta" with "S_Omoth"
#'  	\item Replace "Height_Stricta" with "S_H"
#'  	\item Replace "Percent_Cover_Stricta" with "S_PC"
#' 	}
#'
#' @export

changeVariableNames <- function(Plot_Surveys) {
	Plot_Surveys %<>% 
		as.data.table %<>%
		# pusilla
		setnames("CACA_on_Pusilla", 			"P_Ca") %>%
		setnames("MEPR_on_Pusilla", 			"P_Me") %>%
		setnames("CHVI_on_Pusilla", 			"P_Ch") %>%
		setnames("UnknwnMoth_on_Pusilla", 		"P_Umoth") %>%
		setnames("Old_Moth_Evidence_Pusilla", 	"P_Omoth") %>%
		setnames("Height_Pusilla", 				"P_H") %>%
		setnames("Percent_Cover_Pusilla",		"P_PC")  %>%
		# stricta
		setnames("CACA_on_Stricta", 			"S_Ca") %>%
		setnames("MEPR_on_Stricta", 			"S_Me") %>%
		setnames("CHVI_on_Stricta", 			"S_Ch") %>%
		setnames("UnknwnMoth_on_Stricta", 		"S_Umoth") %>%
		setnames("Old_Moth_Evidence_Stricta", 	"S_Omoth") %>%
		setnames("Height_Stricta",				"S_H")  %>%
		setnames("Percent_Cover_Stricta",		"S_PC") %>%
		as.data.frame	
}