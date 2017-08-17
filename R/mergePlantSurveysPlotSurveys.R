#' fill occupancy plot surveys with info from plant surveys
#' @description This data is from when I was specifically doing occupancy plot surveys.
#' Specific rules for filling the plot surveys:
#' \itemize{
#'  \item percent cover: get from plot survey data; if no plot survey data for that tag number and date, fill with NA
#'  \item height: get from plot survey data; if it was not recorded in the plot survey then use max. plant height from the plant survey data
#'  \item insect presence/absence: get from plot survey data; if it was not recorded in the plot survey then use max. plant height from the plant survey data
#'  \item limit to plot surveys after Dec. 2, 2012 (I wasn't surveying individual plants before this)
#'  \item limit to Tag_Numbers that have marked plants
#' }
#' @param Plot.Surveys
#' @param Plant.Surveys
#' @export

mergePlantSurveysPlotSurveys <- function(Plot.Surveys, Plant.Surveys, date.window=48) {
	temp_A = Plot.Surveys %>% filter(
		Date > "2012-12-01", Tag_Number %in% Plant.Surveys$Tag_Number
	)
	Z = list()
	# for each tag number in the plot surveys data
	for (i in 1:length(unique(temp_A$Tag_Number))) {
		# pull all records for this Tag Number from plot surveys
		L = filter(temp_A, Tag_Number==unique(temp_A$Tag_Number)[i])
		Z[[i]] <- as.data.frame(matrix(NA,length(L$Date),1))
		Z[[i]][, 1] 					<- L$Tag_Number[1]
		Z[[i]][, "Date"] 				<- L$Date
		Z[[i]][, "Pictures"] 			<- L$Pictures
		Z[[i]][, "PictureDirection"] 	<- L$PictureDirection
		Z[[i]][, "Notes"] 				<- L$Notes
		# for each date
		for (j in 1:dim(L)[1]) {
			# fill with occupancy survey data
			Z[[i]][j, "P_plot_survey"] 	<- L$Pusilla[j]
			Z[[i]][j, "P_plant_survey"] <-  "NA"
			Z[[i]][j, "P_Ca"] 			<- L$P_Ca[j]
			Z[[i]][j, "P_Me"] 			<- L$P_Me[j]
			Z[[i]][j, "P_Ch"] 			<- L$P_Ch[j]
			Z[[i]][j, "P_Umoth"] 		<- L$P_Umoth[j]
			Z[[i]][j, "P_Omoth"] 		<- L$P_Omoth[j]
			Z[[i]][j, "P_PC"] 			<- L$P_PC[j]
			Z[[i]][j, "P_H"] 			<- L$P_H[j]
			Z[[i]][j, "S_plot_survey"] 	<- L$Stricta[j]
			Z[[i]][j, "S_plant_survey"] <- "NA"
			Z[[i]][j, "S_Ca"] 			<- L$S_Ca[j]
			Z[[i]][j, "S_Me"] 			<- L$S_Me[j]
			Z[[i]][j, "S_Ch"] 			<- L$S_Ch[j]
			Z[[i]][j, "S_Umoth"] 		<- L$S_Umoth[j]
			Z[[i]][j, "S_Omoth"] 		<- L$S_Omoth[j]
			Z[[i]][j, "S_PC"] 			<- L$S_PC[j]
			Z[[i]][j, "S_H"] 			<- L$S_H[j]
			# pull all plant survey records for this Tag Number and date from plant surveys
			
			# WINDOW OF DATES?
			
			M = filter(Plant.Surveys, 
				Tag_Number==L$Tag_Number[1],
				Date > (as.Date(unique(L$Date)[j]) - date.window),
				Date <= unique(L$Date)[j],
				# remove plants marked as dead
				Dead!=1)
			# remove plants marked as missing	
			M = M[which(M$Missing!=1 | is.na(M$Missing)==T), ]
			# if there is plant survey data for this date, update the occupancy survey data
			# if the M dataframe has at least one row of data:
			if (dim(M)[1] > 0) {
				P = filter(M, Species=="pusilla")
				Z[[i]][j, "P_plot_survey"] 	<- L$Pusilla[j]
				Z[[i]][j, "P_plant_survey"] <- ifelse(dim(P)[1] > 0, 1, 0)
				Z[[i]][j, "P_Ca"] 			<- Maximum(c(P$CA_t, L[j,]$P_Ca))
				Z[[i]][j, "P_Me"] 			<- Maximum(c(P$ME_t, L[j,]$P_Me))
				Z[[i]][j, "P_Ch"] 			<- Maximum(c(P$CH_t, L[j,]$P_Ch))
				Z[[i]][j, "P_Umoth"] 		<- Maximum(c(
												P$Unknown_Moth_t, 
												L[j,]$P_Umoth
											))
				Z[[i]][j, "P_Omoth"] 		<- Maximum(c(
												P$Old_Moth_Evidence_t, 
												L[j,]$P_Omoth
											))
				Z[[i]][j, "P_PC"] 			<- L$P_PC[j]
				# Height - if plot survey height is NA, then replace with max height from plant surveys
				Z[[i]][j, "P_H"] 			<- ifelse(
												is.na(L$P_H[j]) & dim(P)[1] > 0,
												Maximum(P$Height_t), 
												L$P_H[j]
											) 
				S = filter(M, Species=="stricta")
				Z[[i]][j, "S_plot_survey"] 	<- L$Stricta[j]
				Z[[i]][j, "S_plant_survey"] <- ifelse(dim(S)[1] > 0, 1, 0)
				Z[[i]][j, "S_Ca"] 			<- Maximum(c(S$CA_t, L[j,]$S_Ca))
				Z[[i]][j, "S_Me"] 			<- Maximum(c(S$ME_t, L[j,]$S_Me))
				Z[[i]][j, "S_Ch"] 			<- Maximum(c(S$CH_t, L[j,]$S_Ch))
				Z[[i]][j, "S_Umoth"] 		<- Maximum(c(
												S$Unknown_Moth_t, 
												L[j,]$S_Umoth
											))
				Z[[i]][j, "S_Omoth"] 		<- Maximum(c(
												S$Old_Moth_Evidence_t, 
												L[j,]$S_Omoth
											))
				Z[[i]][j, "S_PC"] 			<- L$S_PC[j]
				# Height - if plot survey height is NA, then replace with max height from plant surveys
				Z[[i]][j, "S_H"] <- ifelse(
					is.na(L$S_H[j]) & dim(S)[1] > 0, 
					Maximum(S$Height_t), 
					L$S_H[j]
				)
			}
		}	
	}
	A <- do.call(rbind.data.frame, Z)
	names(A)[1] <- "Tag_Number"
	#  FIX DATA FORMAT
	A[,c(column_list,
		"P_plot_survey", 
		"P_PC", 
		"P_H", 
		"S_plot_survey", 
		"S_PC", 
		"S_H")] %<>%
		apply(., 2, as.numeric
	)
	A[,c(column_list,
		"P_plot_survey", 
		"P_PC", 
		"P_H", 
		"S_plot_survey", 
		"S_PC", 
		"S_H")] %<>%
		apply(., 2, NA_Function
	)
	return(A)
}