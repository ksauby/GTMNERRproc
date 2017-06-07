#' Create occupancy plot survey data from plant surveys
#' @description This data is from when I was surveying plants only, not also doing plot surveys.
#' Specific rules for filling the plot surveys:
#' \itemize{
#'  \item I can only add absence data when I know that I surveyed all the plants that day; this requires creating a list of possible plants alive for each survey day
#'  \item filter plant surveys by dates that are not in plot surveys and also not in demographic plot surveys (I don't want to replicate plot, date combos)
#' }
#' @param Plant_Surveys
#' @param Plot_Surveys
#' @param D_Plot_Surveys
#' @export

createPlotSurveysfromPlantSurveys <- function(Plant_Surveys, Plot_Surveys, D_Plot_Surveys) {
	# create Tag/Date Combo Field
	Plant_Surveys 	%<>% mutate(Tag_Date=paste(Tag_Number, Date))
	Plot_Surveys 	%<>% mutate(Tag_Date=paste(Tag_Number, Date))
	D_Plot_Surveys 	%<>% mutate(Tag_Date=paste(Tag_Number, Date))
	# keep records of Tag Numbers not surveyed on particular dates
	temp.plant.surveys <- filter(
		Plant_Surveys, 
		!(Tag_Date %in% Plot_Surveys$Tag_Date),
		!(Tag_Date %in% D_Plot_Surveys$Tag_Date)
	)
	# print plants with Tag_Number==NA as warning
	TagNumbNA <- temp.plant.surveys %>% 
		filter(is.na(temp.plant.surveys$Tag_Number)) %$%
		PlantID
	if (length(TagNumbNA) > 0) {
		warning("Plants with Tag_Number=NA:", paste(TagNumbNA, collapse=","))
	}
	# remove plants with Tag_Number==NA
	temp.plant.surveys <- filter(
		temp.plant.surveys, 
		is.na(temp.plant.surveys$Tag_Number)==FALSE
	)
	Z = list()
	# for each tag number in the demography plot survey data
	for (i in 1:length(unique(temp.plant.surveys$Tag_Number))) {
		# pull all records for this Tag Number from temp.plant.surveys
		L = filter(
			temp.plant.surveys, 
			Tag_Number==unique(temp.plant.surveys$Tag_Number)[i]
		)
		Z[[i]] 			<- as.data.frame(matrix(NA,length(unique(L$Date)),1))	
		Z[[i]][, 1] 	<- L$Tag_Number[1]
		Z[[i]][, "Date"] <- unique(L$Date)
		# for each date
		for (j in 1:length(unique(L$Date))) {
			# pull all plant survey records for this Tag Number and date from plant surveys remove plants marked as missing or dead	
			M = filter(L, Date==unique(L$Date)[j], Dead!=1)
			M = M[which(M$Missing!=1 | is.na(M$Missing)==T), ]
			# get list of PlantIDs for this plot
			N = filter(
				Plant_Info, 
				Tag_Number==L$Tag_Number[1], 
				# only include plants that are listed as having been added to Plant.Info on or after Date
				First.Survey.Date.Alive <= unique(L$Date)[j],
				# exclude dead plants (including date plant was first recorded as dead)
				FirstDeadMissingObservation > unique(L$Date)[j] | 
					is.na(FirstDeadMissingObservation)==T
			)
			# if all PlotPlantIDs were surveyed for a given date:
			if (identical(
				M$PlotPlantID[order(M$PlotPlantID)], 
				N$PlotPlantID[order(N$PlotPlantID)]
			)==T) {
				P = filter(M, Species=="pusilla")
				Z[[i]][j, "P_plant_survey"] <- ifelse(dim(P)[1] > 0, 1, 0) 
				Z[[i]][j, "P_Ca"] 			<- Maximum(P$CA_t) 
				Z[[i]][j, "P_Me"] 			<- Maximum(P$ME_t) 
				Z[[i]][j, "P_Ch"] 			<- Maximum(P$CH_t) 
				Z[[i]][j, "P_Umoth"] 		<- Maximum(P$Unknown_Moth_t) 
				Z[[i]][j, "P_Omoth"] 		<- Maximum(P$Old_Moth_Evidence_t) 
				S = filter(M, Species=="stricta")
				Z[[i]][j, "S_plant_survey"] <- ifelse(dim(S)[1] > 0, 1, 0) 
				Z[[i]][j, "S_Ca"] 			<- Maximum(S$CA_t) 
				Z[[i]][j, "S_Me"] 			<- Maximum(S$ME_t) 
				Z[[i]][j, "S_Ch"] 			<- Maximum(S$CH_t) 
				Z[[i]][j, "S_Umoth"] 		<- Maximum(S$Unknown_Moth_t) 
				Z[[i]][j, "S_Omoth"] 		<- Maximum(S$Old_Moth_Evidence_t)
				Z[[i]][j, "all_surveyed"] 	<- "Yes"
			} 
			# if all PlotPlantIDs were NOT surveyed on this date
			else {
				P = filter(M, Species=="pusilla")
				Z[[i]][j, "P_plant_survey"] <- ifelse(dim(P)[1] > 0, 1, NA) 
				Z[[i]][j, "P_Ca"] 			<- mysum1(P$CA_t)
				Z[[i]][j, "P_Me"] 			<- mysum1(P$ME_t)
				Z[[i]][j, "P_Ch"] 			<- mysum1(P$CH_t)
				Z[[i]][j, "P_Umoth"] 		<- mysum1(P$Unknown_Moth_t)
				Z[[i]][j, "P_Omoth"] 		<- mysum1(P$Old_Moth_Evidence_t)
				S = filter(M, Species=="stricta")
				Z[[i]][j, "S_plant_survey"] <- ifelse(dim(S)[1] > 0, 1, NA) 
				Z[[i]][j, "S_Ca"] 			<- mysum1(S$CA_t)
				Z[[i]][j, "S_Me"] 			<- mysum1(S$ME_t)
				Z[[i]][j, "S_Ch"] 			<- mysum1(S$CH_t)
				Z[[i]][j, "S_Umoth"] 		<- mysum1(S$Unknown_Moth_t)
				Z[[i]][j, "S_Omoth"] 		<- mysum1(S$Old_Moth_Evidence_t)
				Z[[i]][j, "all_surveyed"] 	<- "No"
			}
		}
	}	
	C <- do.call(rbind.data.frame, Z)
	names(C)[1] <- "Tag_Number"
	# FIX DATA FORMAT
	C[, column_list] %<>% apply(., 2, as.numeric)
	C[, column_list] %<>% apply(., 2, NA_Function)
	return(C)	
}
