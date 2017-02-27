#' Make Insect Factor Variables
#'
#' @description Make new insect variables with "yes"/"no" values instead of 0/1.
#' @param Plant_Surveys Plant Survey Dataset
#'
#' @export

createInsectFactorVariables <- function(Plant_Surveys) {
	Plant_Surveys$C_cactorum 	<- Plant_Surveys$CA_t
	Plant_Surveys$M_prodenialis <- Plant_Surveys$ME_t
	Plant_Surveys[,c(
		"C_cactorum",
		"M_prodenialis")] %<>%
		apply(., 2, Yes_No_from_1_0_Function) %>%
		apply(., 2, as.factor
	)
	return(Plant_Surveys)
}

