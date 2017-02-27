#' fill demography plot surveys with info from plant surveys
#' @description I only entered a plot as surveyed in this dataset if I had completely marked/surveyed all plants in the plot. I entered data in this dataset near the beginning of the demography study, when I was setting the study up.
#' Specific rules for filling the plot surveys:
#' \itemize{
#'  \item only use demography plot surveys that are not a duplicate of the occupancy plot surveys
#' }
#' @param D_Plot_Surveys
#' @param Plant_Surveys
#' @export

mergePlotSurveysDemoPlotSurveys <- function(D_Plot_Surveys, Plant_Surveys) {
	Z = list()
	# for each tag number in the demography plot survey data
	for (i in 1:length(unique(D_Plot_Surveys$Tag_Number))) {
		# pull all records for this Tag Number from plot surveys
		L = filter(
			D_Plot_Surveys, 
			Tag_Number==unique(D_Plot_Surveys$Tag_Number)[i]
		)
		Z[[i]] 			<- as.data.frame(matrix(NA,length(unique(L$Date)),1))
		Z[[i]][, 1] 	<- L$Tag_Number[1]
		Z[[i]][, "Date"] <- unique(L$Date)
		# for each date
		for (j in 1:length(unique(L$Date))) {
			# pull all plant survey records for this Tag Number and date from plant surveys
			M = filter(
				Plant_Surveys, 
				Tag_Number==L$Tag_Number[1], 
				Date==unique(L$Date)[j],
				# remove plants marked as dead
				Dead!=1
			)
			# remove plants marked as missing	
			M = M[which(M$Missing!=1 | is.na(M$Missing)==T), ]
			# if there is plant survey data for this date, update the occupancy survey data
			# if the M dataframe has at least one row of data:
			if (dim(M)[1] > 0) {
				P = filter(M, Species=="pusilla")
				Z[[i]][j, "P_plant_survey"] <-  ifelse(dim(P)[1] > 0, 1, 0) 
				Z[[i]][j, "P_Ca"] 			<-  Maximum(P$CA_t) 
				Z[[i]][j, "P_Me"] 			<-  Maximum(P$ME_t) 
				Z[[i]][j, "P_Ch"] 			<-  Maximum(P$CH_t) 
				Z[[i]][j, "P_Umoth"] 		<-  Maximum(P$Unknown_Moth_t) 
				Z[[i]][j, "P_Omoth"] 		<-  Maximum(P$Old_Moth_Evidence_t) 
				Z[[i]][j, "P_H"] 			<- Maximum(P$Height_t)
				S = filter(M, Species=="stricta")
				Z[[i]][j, "S_plant_survey"] <- ifelse(dim(S)[1] > 0, 1, 0) 
				Z[[i]][j, "S_Ca"] 			<- Maximum(S$CA_t) 
				Z[[i]][j, "S_Me"] 			<- Maximum(S$ME_t) 
				Z[[i]][j, "S_Ch"] 			<- Maximum(S$CH_t) 
				Z[[i]][j, "S_Umoth"] 		<- Maximum(S$Unknown_Moth_t) 
				Z[[i]][j, "S_Omoth"] 		<- Maximum(S$Old_Moth_Evidence_t) 
				Z[[i]][j, "S_H"] 			<- Maximum(S$Height_t)
			}
		}
	}
	B <- do.call(rbind.fill, Z)
	names(B)[1] <- "Tag_Number"
	#  FIX DATA FORMAT
	B[,c(
		column_list,
		"P_H", 
		"S_H")] %<>%
		apply(., 2, as.numeric
	)
	B[,c(
		column_list,
		"P_H",  
		"S_H")] %<>%
		apply(., 2, NA_Function
	)
	return(B)	
}