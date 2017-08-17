#' Create occupancy plot survey data from original plant survey data
#' @description This data is from when I was surveying plants only, not also doing occupancy plot surveys. After IDing and surveying these plants, I deleted these IDs and renamed the plants so the IDs here DO NOT match the surveys and IDs in the demography study data.
#' Specific rules for filling the plot surveys:
#' \itemize{
#'  \item for these dates I am not certain that I surveyed all plants in the plots therefore either something is present or an NA
#' }
#' @param Original.Plant.Data
#' @export

createPlotSurveysfromOrigPlantSurveys <- function(Original.Plant.Data) {
	Z = list()
	# for each tag number in the demography plot survey data
	for (i in 1:length(unique(Original.Plant.Data$Tag_Number))) {
		# pull all records for this Tag Number from temp.plant.surveys
		L = filter(
			Original.Plant.Data, 
			Tag_Number==unique(Original.Plant.Data$Tag_Number)[i]
		)
		Z[[i]] 			<- as.data.frame(matrix(NA,length(unique(L$Date)),1))	
		Z[[i]][, 1] 	<- L$Tag_Number[1]
		Z[[i]][, "Date"] <- unique(L$Date)
		# for each date
		for (j in 1:length(unique(L$Date))) {
			# pull all plant survey records for this Tag Number and date from plant surveys
			M = filter(L, Date==unique(L$Date)[j])
			P = filter(M, HostSpecies=="pusilla")
			Z[[i]][j, "P_plant_survey"] 	<-  ifelse(dim(P)[1] > 0, 1, NA) 
			Z[[i]][j, "P_Ca"] 				<- mysum1(P$CA_t)
			Z[[i]][j, "P_Me"] 				<- mysum1(P$ME_t)
			Z[[i]][j, "P_Ch"] 				<- mysum1(P$CH_t)
			Z[[i]][j, "P_Umoth"] 			<- mysum1(P$Unknown_Moth_t)
			Z[[i]][j, "P_Omoth"] 	<- mysum1(P$Old_Moth_Evidence_t)			
			S = filter(M, HostSpecies=="stricta")
			Z[[i]][j, "S_plant_survey"] 	<-  ifelse(dim(S)[1] > 0, 1, NA) 
			Z[[i]][j, "S_Ca"] 				<- mysum1(S$CA_t)
			Z[[i]][j, "S_Me"] 				<- mysum1(S$ME_t)
			Z[[i]][j, "S_Ch"] 				<- mysum1(S$CH_t)
			Z[[i]][j, "S_Umoth"]	 		<- mysum1(S$Unknown_Moth_t)
			Z[[i]][j, "S_Omoth"] 			<- mysum1(S$Old_Moth_Evidence_t)
		}
	}	
	D <- do.call(rbind.data.frame, Z)
	names(D)[1] <- "Tag_Number"
	#  FIX DATA FORMAT
	D[, column_list] %<>% apply(., 2, as.numeric)
	D[, column_list] %<>% apply(., 2, NA_Function)
	D$Date %<>% Format_Date_Function
	return(D)	
}