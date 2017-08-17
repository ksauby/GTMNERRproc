#' Merge Duplicate Surveys
#' @description Merge duplicate surveys (two or more surveys on the same date) and add columns for CACA presence and MEPR presence (regardless of which cactus the insects were found).
#' @param E
#' @export

mergeDuplicateSurveys <- function(E) {
	Z = list()
	# for each tag number
	for (i in 1:length(unique(E$Tag_Number))) {
		# pull all records for this Tag Number from temp.plant.surveys
		L = E %>% dplyr::filter(Tag_Number==unique(E$Tag_Number)[i])
		Z[[i]] 			<- as.data.frame(matrix(NA,length(unique(L$Date)),1))
		Z[[i]][, 1] 	<- L$Tag_Number[1]
		Z[[i]][, "Date"] <- unique(L$Date)
		# for each date
		for (j in 1:length(unique(L$Date))) {
			# pull all plant survey records for this Tag Number and date from plant surveys
			M = filter(L, Date==unique(L$Date)[j])
			# pusilla		
			Z[[i]][j, "P_plot_survey"] 		<- Maximum(M$P_plot_survey)
			Z[[i]][j, "P_plant_survey"] 	<- Maximum(M$P_plant_survey) 
			Z[[i]][j, "P_Ca"] 				<- Maximum(M$P_Ca)
			Z[[i]][j, "P_Me"] 				<- Maximum(M$P_Me)
			Z[[i]][j, "P_Ch"] 				<- Maximum(M$P_Ch)
			Z[[i]][j, "P_Umoth"] 			<- Maximum(M$P_Umoth)
			Z[[i]][j, "P_Omoth"] 			<- Maximum(M$P_Omoth)
			Z[[i]][j, "P_PC"] 				<- Maximum(M$P_PC)
			Z[[i]][j, "P_H"] 				<- Maximum(M$P_H)
			# stricta		
			Z[[i]][j, "S_plot_survey"] 		<- Maximum(M$S_plot_survey)
			Z[[i]][j, "S_plant_survey"] 	<- Maximum(M$S_plant_survey)
			Z[[i]][j, "S_Ca"] 				<- Maximum(M$S_Ca)
			Z[[i]][j, "S_Me"] 				<- Maximum(M$S_Me)
			Z[[i]][j, "S_Ch"] 				<- Maximum(M$S_Ch)
			Z[[i]][j, "S_Umoth"] 			<- Maximum(M$S_Umoth)
			Z[[i]][j, "S_Omoth"] 			<- Maximum(M$S_Omoth)
			Z[[i]][j, "S_PC"] 				<- Maximum(M$S_PC)
			Z[[i]][j, "S_H"] 				<- Maximum(M$S_H)
		}
	}
	return(Z)
}

