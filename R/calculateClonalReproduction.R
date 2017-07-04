#' Calculate Clonal Reproduction
#'
#' @description Determine parent plant size before segments were lost to offspring, the number of segments lost of offspring, the number of clonal offspring produced, and the first size observed for each clonal offspring.
# calculate Parent size as observed parent size plus all offspring; this ensures that a parent cannot have size equal to or less than offspring

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
		group_by(PlantID) %>%
		mutate(
			Parent.SpringSummer.Obs.Date_t_1 = lag(SurveyDate_SpringSummer)
		) %>%
		dplyr::select(
			PlantID, 
			FecundityYear, 
			SurveyDate_SpringSummer, 
			Parent.SpringSummer.Obs.Date_t_1,
			Size_t
		) %>% 
		setnames("SurveyDate_SpringSummer", "Parent.SpringSummer.Obs.Date_t") %>%
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
	C <- Plant_Info_Analysis 
	C %<>%
		mutate(Parent = substr(x=Parent,start=1,stop=4)) %>%
		setnames("PlantID", "Offspring.ID") %>%
		setnames("Parent", "Parent.ID") %>%
		setnames("First_Size", "Offspring.First_Size") %>%
		setnames(
			"First.Survey.Date.Alive", 
			"Offspring.First.Survey.Date.Alive"
		) %>%
		setnames(
			"minFecundityYear", 
			"Offspring.minFecundityYear"
		) %>%
		dplyr::select(
			Parent.ID, 
			Offspring.ID, 
			Offspring.First_Size,
			Offspring.First.Survey.Date.Alive,
			Offspring.minFecundityYear
		) %>%
		# join all plant records with each offspring record
		merge(
			A, 
			by = "Parent.ID",
			all=T
		) %>%
		# remove plants that had no offspring
		filter(!is.na(Offspring.ID)) %>%
		# remove plants without identified parents
		filter(Parent.ID!="Unkn" & Parent.ID!="unkn")
		
	
	# keep only offspring records where first survey occurred after the previous parent spring/summer survey but before or on the same day as the current spring/summer survey
	D <- C %>% filter(
		(
			Offspring.First.Survey.Date.Alive>Parent.SpringSummer.Obs.Date_t_1 &
			Offspring.First.Survey.Date.Alive <= Parent.SpringSummer.Obs.Date_t
		) |
		# for plants surveyed during the first year, which do not have a previous parent size
		(
			is.na(Parent.SpringSummer.Obs.Date_t_1) &
			Offspring.First.Survey.Date.Alive <= Parent.SpringSummer.Obs.Date_t
		)
	)

	# ------------------------------------------------------------- WARNINGS ---
	# for which parents are there not size measurements?
	temp <- D %>% filter(is.na(Parent.Size_t))
	if (dim(temp)[1] > 0) {
		warning("Some parents have no recorded size.")
	}
	# for which offspring are there no first size measurements?
	temp <- D %>% 
		filter(is.na(Offspring.First_Size)) %>%
		# records for these plants were checked
		filter(!(
			Offspring.ID %in% c(8653, 8842, 7548)))
	if (dim(temp)[1] > 0) {
		warning("Some offspring have no recorded first size.")
	}	
	# for which parents are there no spring/summer observation dates?
	temp <- D %>% filter(is.na(Parent.SpringSummer.Obs.Date_t))
	if (dim(temp)[1] > 0) {
		warning("Some parents have no spring/summer observation dates.")
	}
	# --------------------------------------------------------------------------
	E <- D %>%
		filter(!is.na(Parent.Size_t)) %>%
		filter(!is.na(Offspring.First_Size)) %>%
		filter(!is.na(Parent.SpringSummer.Obs.Date_t))
		
	# figure out parent size by adding observed size + offspring segments
	parent_size <- E %>% 
		as.data.frame %>%
		group_by(Parent.ID, Parent.FecundityYear) %>%
		# calculate Parent size as observed parent size plus all offspring; this ensures that a parent cannot have size equal to or less than offspring
		dplyr::summarise(
			SizewClones_t = Parent.Size_t[1] + sum(Offspring.First_Size),
			NClones_t = length(unique(Offspring.ID)),
			NSegLosttoClones_t = sum(Offspring.First_Size)
		)
	# merge parent size with clones with plant surveys
	Plant_Surveys_by_Yearw_clones <- Plant_Surveys_by_Year %>%
		merge(
			parent_size, 
			by.x=c("PlantID", "FecundityYear"), 
			by.y=c("Parent.ID", "Parent.FecundityYear"),
			all=T
		) %>% 
		as.data.frame
	Plant_Surveys_by_Yearw_clones %<>%
		rowwise %>%
		# replace values for plants that did not produce clones that year
		mutate(
			NClones_t = replace(
				NClones_t,
				which(is.na(NClones_t)),
				0
			),
			SizewClones_t = replace(
				SizewClones_t,
				which(is.na(SizewClones_t)),
				Size_t
			),
			NSegLosttoClones_t = replace(
				NSegLosttoClones_t,
				which(is.na(NSegLosttoClones_t)),
				0
			)
		)
	Plant_Surveys_by_Yearw_clones %<>% as.data.frame
	Plant_Surveys_by_Yearw_clones$SizewClones_t %<>% as.numeric
	# presence of clonal reproduction
	Plant_Surveys_by_Yearw_clones$ClonePres_t <- ifelse(
		Plant_Surveys_by_Yearw_clones$NSegLosttoClones_t > 0,
		1,
		0
	)	
	# ------------------------------------------------------------- WARNINGS ---
	# for which parents of offspring are there no NClones_t records?
	PlanIntoParents <- Plant_Info_Analysis %>% 
		filter(Parent!="Unknown") %$% 
		unique(Parent)
	PlantSurveyswClones <- Plant_Surveys_by_Yearw_clones %>% 
		filter(ClonePres_t==1) %$% 
		unique(PlantID)
	missingOffspringCounts <- (PlanIntoParents[which(
		!(PlanIntoParents %in% PlantSurveyswClones) &
		# the offspring were surveyed before the parents
		PlanIntoParents != 8692 & PlanIntoParents != 9076
		)])
	temp <- D %>% 
		filter(Parent.ID %in% missingOffspringCounts) %>%
		filter(Offspring.ID!=7435 & Offspring.ID!=8842)
	if (dim(temp)[1] > 0) {
		warning("Some parents for which offspring were observed have no records of having clones after processing of the data.")
	}		
	# --------------------------------------------------------------------------
	return(Plant_Surveys_by_Yearw_clones)
}