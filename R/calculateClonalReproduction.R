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
	# prep parent survey data
	# use this dataset to use parent size that is consistent for all offpsring
	A <- Plant_Surveys_by_Year %>% 
		#filter(Species=="Opuntia stricta") %>%
		select(PlantID, SurveyDate_SpringSummer, Size_t) %>% 
		setnames("SurveyDate_SpringSummer", "Parent.Obs.Date") %>%
		setnames("Size_t", "Parent.Size_t")
	# prep offspring survey data
	# use this dataset to figure out first size of offspring
	B <- Plant_Surveys_by_Plant %>% 
		#filter(Species=="Opuntia stricta") %>%
		select(PlantID, Date, Size_t) %>% 
		setnames("Date", "Offspring.Obs.Date") %>%
		setnames("Size_t", "Offspring.Size_t")
	# merge parent data with offspring info
	C <- Plant_Info_Analysis %>%
		merge(
			A, 
			by.x="Parent", 
			by.y="PlantID", 
			all=T
		) %>%
		# merge first offspring size
		filter(!is.na(PlantID)) %>%
		merge(
			B,
			by="PlantID", 
			all.x=T
			) %>%
			filter(!is.na(Parent))
	
	D <- C %>%
		group_by(PlantID) %>%
		# remove NAs
		filter(!is.na(Parent.Size_t)) %>%
		filter(!is.na(Offspring.Size_t)) %>%
		filter(!is.na(Parent.Obs.Date)) %>%
		# remove Parent survey dates that are after offspring was observed
		filter(
			Parent.Obs.Date <= First.Survey.Date.Alive
		) %>%
		# find Parent observation date (WITH a size obs) closest to (or equal to) date that the offspring was observed
		filter(Parent.Obs.Date==max(Parent.Obs.Date)) %>%
		# find Offspring observation date (WITH a size obs) closest to (or equal to) date it was first observed
		filter(Offspring.Obs.Date==min(Offspring.Obs.Date)) %>%
		setnames("First.Survey.Date.Alive", "Date")	
	
	D %<>% addFecundityYear
	
	# figure out parent size
	parent_size <- D %>% 
		group_by(Parent, FecundityYear, Parent.Size_t, Offspring.Size_t) %>%
		# still need to count number of clones per size class
		summarise(n.clones=length(unique(PlantID))) %>%
		group_by(Parent, FecundityYear, Parent.Size_t, Offspring.Size_t) %>%
		summarise(
			n.clones = n.clones[1],
			n.offspring.segments = Offspring.Size_t * n.clones
			) %>%
		as.data.frame %>% 
		group_by(Parent, FecundityYear) %>%
		# calculate Parent size as observed parent size plus all offspring; this ensures that a parent cannot have size equal to or less than offspring
		mutate(Parent_Size_t = Parent.Size_t[1] + sum(n.offspring.segments)) %>%
		ungroup %>%
		select(Parent, FecundityYear, Parent_Size_t) %>%
		unique
	
	# merge offspring info with parent size info
	D %<>% 
		as.data.frame %>%
	   	select(Parent, FecundityYear, PlantID, Offspring.Size_t) %>%
		merge(parent_size, by=c("Parent", "FecundityYear"))


	# need to know number of segments produced per size class
	loss_to_clones <- D %>% 
		group_by(Parent, FecundityYear) %>%
		summarise(
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