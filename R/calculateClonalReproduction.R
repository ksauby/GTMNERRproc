#' Calculate Clonal Reproduction
#'
#' @description Determine parent plant size before segments were lost to offspring, the number of segments lost of offspring, the number of clonal offspring produced, and the first size observed for each clonal offspring.

#' @return Returns the following variables:
#' \itemize{
#'  	\item first size of offspring
#'  	\item Size_t_w_clone: calculate Parent size as observed parent size at time t plus sizes of all offspring observed at time t+1 (this ensures that a parent cannot have size equal to or less than offspring)
#'  	\item Clones_t: number of clones produced per parent
#'  	\item Loss_to_Offspring: number of segments lost to clonal offspring
#' 	}
#'
#' @param Plant_Surveys_by_Year
#' @param Plant_Surveys_by_Plant
#' @param Plant_Info_Analysis
#'
#' @export

calculateClonalReproduction <- function(
	Plant_Surveys_by_Year, 
	Plant_Surveys_by_Plant, 
	Plant_Info_Analysis
) {
	# PARENT SURVEY DATA
	# use this dataset to use parent size that is consistent for all offpsring
	A <- Plant_Surveys_by_Year %>% 
		dplyr::select(
			PlantID, 
			FecundityYear, 
			SurveyDate_SpringSummer, 
			Size_t
		) %>% 
		setnames("SurveyDate_SpringSummer", "Parent.Obs.Date") %>%
		setnames("Size_t", "Parent.Size_t") %>%
		setnames("FecundityYear", "Parent.FecundityYear") %>%
		setnames("PlantID", "Parent.ID")
	# OFFSPRING SURVEY DATA
	# use this dataset to figure out first size of offspring
	B <- Plant_Surveys_by_Plant %>% 
		dplyr::select(PlantID, Date, Size_t) %>% 
		setnames("Date", "Offspring.Obs.Date") %>%
		setnames("Size_t", "Offspring.Size_t") %>%
		setnames("PlantID", "Offspring.ID")
	# merge parent data (from A) with offspring info (from Plant_Info_Analysis)
	C <- Plant_Info_Analysis %>%
		setnames("PlantID", "Offspring.ID") %>%
		setnames("Parent", "Parent.ID") %>%
		dplyr::select(
			Parent.ID, 
			Offspring.ID, 
			First_Size,
			First.Survey.Date.Alive
		) %>%
		merge(
			A, 
			by = "Parent.ID", 
			all=T
		) %>%
		# merge first offspring size
		filter(!is.na(Offspring.ID)) %>%
		merge(
			B,
			by="Offspring.ID", 
			all.x=T
			) %>%
			filter(!is.na(Parent.ID))
	
	D <- C %>%
		group_by(Parent.ID) %>%
		# remove NAs
		filter(!is.na(Parent.Size_t)) %>%
		filter(!is.na(Offspring.Size_t)) %>%
		filter(!is.na(Parent.Obs.Date)) %>%
		# keep only parent surveys that are before or on the same date as the first offspring obs. date
		filter(
			Parent.Obs.Date <= First.Survey.Date.Alive
		) %>%
		# find Parent observation date (WITH a size obs) closest to (or equal to) date that the offspring was observed
		filter(Parent.Obs.Date==max(Parent.Obs.Date)) %>%
		# find Offspring observation date (WITH a size obs) closest to (or equal to) date it was first observed
		filter(Offspring.Obs.Date==min(Offspring.Obs.Date)) %>%
		setnames("First.Survey.Date.Alive", "Date")	%>%
		as.data.frame
		
	# figure out parent size by adding observed size + offspring segments
	parent_size <- D %>% 
		as.data.frame %>%
		group_by(Parent.ID, Parent.FecundityYear) %>%
		# calculate Parent size as observed parent size plus all offspring; this ensures that a parent cannot have size equal to or less than offspring
		dplyr::summarise(
			Parent_Size_w_clones_t = Parent.Size_t[1] + sum(Offspring.Size_t)
		)
	
	# merge offspring info with parent size info
	D %<>% 
		as.data.frame %>%
	   	select(Parent, FecundityYear, PlantID, Offspring.Size_t) %>%
		merge(parent_size, by=c("Parent", "FecundityYear"))


	# need to know number of segments produced per size class - WHY?
	loss_to_clones <- D %>% 
		group_by(Parent, FecundityYear) %>%
		dplyr::summarise(
			Clones_t = length(unique(PlantID)),
			Size_t_w_clone = paste(unique(Parent_Size_t), collapse=","),
			Loss_to_Offspring = sum(Offspring.Size_t)
		)

	Plant_Surveys_by_Yearw_clones <- merge(
			Plant_Surveys_by_Year, 
			loss_to_clones, 
			by.x=c("PlantID", "FecundityYear"), 
			by.y=c("Parent", "FecundityYear"),
			all=T
		) %>% 
		as.data.frame
	Plant_Surveys_by_Yearw_clones %<>%
		rowwise %>%
		mutate(
			Clones_t = replace(
				Clones_t,
				which(is.na(Clones_t)),
				0
			),
			Size_t_w_clone = replace(
				Size_t_w_clone,
				which(is.na(Size_t_w_clone)),
				Size_t
			),
			Loss_to_Offspring = replace(
				Loss_to_Offspring,
				which(is.na(Loss_to_Offspring)),
				0
			)
		)
	Plant_Surveys_by_Yearw_clones$Size_t_w_clone %<>% as.numeric
	# presence of clonal reproduction
	Plant_Surveys_by_Yearw_clones$ClonePres_t <- ifelse(
		Plant_Surveys_by_Yearw_clones$Loss_to_Offspring > 0,
		1,
		0
	)	
	return(Plant_Surveys_by_Yearw_clones)
}