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
#' @param Plant.Surveys.by.Year
#' @param Plant.Surveys.by.Plant
#' @param Plant.Info.Analysis
#' @param Parent.Choice
#'
#' @export

calculateClonalReproduction <- function(
	Plant.Surveys.by.Year, 
	Plant.Surveys.by.Plant, 
	Plant.Info.Analysis,
	ParentChoice
) {
	if (ParentChoice == "largest" | ParentChoice == "random" | ParentChoice == "random.largest") {
		# FIRST, ASSIGN PARENTS FOR PARENT-LESS PLANTS
		Plants.wo.parents <- Plant.Info.Analysis %>% filter(Parent=="Unknown")
		Plant.Surveys.by.Plant$DemographicSurvey %<>% as.numeric
		Plants.wo.parents$First.DemographicSurvey %<>% as.numeric
		for (i in 1:length(Plants.wo.parents$PlantID)) {
			#	1) find surveys of plants in that plot from survey preceeding that when plant was discovered
			A <- Plant.Surveys.by.Year %>% 
				filter(
					# cannot be its own parent
					PlantID != Plants.wo.parents$PlantID[i],
					# parent must be of same species
					Species == Plants.wo.parents$Species[i],
					# parent must have been surveyed in previous fecundity year
					FecundityYear == 
						Plants.wo.parents$minFecundityYear[i] - 1,
					# parent must be in same plot as offspring
					grepl(Plants.wo.parents$Tag_Number[i], Tag_Numbers_Surveyed)==T,
					# parent cannot be a seedling
					!(FecundityYear == minFecundityYear &
					RecruitmentMode == "Seedling"),
					!(is.na(Size_t))
				)
			if (dim(A)[1] > 0) {
				if (ParentChoice == "random") {
					Plants.wo.parents[i, "Parent"] <- 
						A[sample(dim(A)[1], 1), "PlantID"]
				}
				if (ParentChoice == "largest") {
					A %<>% arrange(desc(Size_t))
					Plants.wo.parents[i, "Parent"] <- A[1, "PlantID"]
				}
				if (ParentChoice == "random.largest") {
					Plants.wo.parents[i, "Parent"] <- 
						A[sample(
							x=dim(A)[1], 
							size=1, 
							prob=A$Size_t/sum(A$Size_t)
						), "PlantID"]
				}
			}
			# expand to a plant in the same network
			if (dim(A)[1] == 0) {
				A <- Plant.Surveys.by.Year %>% 
					filter(
						# cannot be its own parent
						PlantID != Plants.wo.parents$PlantID[i],
						# parent must be of same species
						Species == Plants.wo.parents$Species[i],
						# parent must have been surveyed in previous fecundity year
						FecundityYear == 
							Plants.wo.parents$minFecundityYear[i] - 1,
						# parent must be in same plot as offspring
						Network == Plants.wo.parents$Network[i],
						# parent cannot be a seedling
						!(FecundityYear == minFecundityYear &
						RecruitmentMode == "Seedling"),
						!(is.na(Size_t))
					)
			}
			# expand to a plant in the same patch
			if (dim(A)[1] == 0) {
				A <- Plant.Surveys.by.Year %>% 
					filter(
						# cannot be its own parent
						PlantID != Plants.wo.parents$PlantID[i],
						# parent must be of same species
						Species == Plants.wo.parents$Species[i],
						# parent must have been surveyed in previous fecundity year
						FecundityYear == 
							Plants.wo.parents$minFecundityYear[i] - 1,
						# parent must be in same plot as offspring
						Island == Plants.wo.parents$Island[i],
						# parent cannot be a seedling
						!(FecundityYear == minFecundityYear &
						RecruitmentMode == "Seedling"),
						!(is.na(Size_t))
					)
			}
			if (dim(A)[1] > 0) {
				if (ParentChoice == "random") {
					Plants.wo.parents[i, "Parent"] <- 
						A[sample(dim(A)[1], 1), "PlantID"]
				}
				if (ParentChoice == "largest") {
					A %<>% arrange(desc(Size_t))
					Plants.wo.parents[i, "Parent"] <- A[1, "PlantID"]
				}
				if (ParentChoice == "random.largest") {
					Plants.wo.parents[i, "Parent"] <- 
						A[sample(
							x=dim(A)[1], 
							size=1, 
							prob=A$Size_t/sum(A$Size_t)
						), "PlantID"]
				}
			}
			cat(paste(i,",",sep=""))
		}
		Plants.w.parents <- Plant.Info.Analysis %>% filter(Parent!="Unknown")
		Plant.Info.Analysis <- rbind.fill(Plants.wo.parents, Plants.w.parents)
		Plant.Surveys.by.Year %<>% 
			dplyr::select(-c(
				Island,
				Network,
				Species,                
				RecruitmentMode,
				Parent,
				First.Survey.Date.Alive,
				AliveatEndofStudy,
				Tag_Number,
				OldMothPlantPres,
				MEPlantPres,
				CAPlantPres,
				MothPlantPres,
				First.Survey.Date,
				First.DemographicSurvey,
				minFecundityYear,
				First_Size,
				First.Measurement.Date,
				min.Size,
				max.Size,
				LastDateAlive,
				FirstDeadObservation,
				FirstMissingObservation,
				FirstDeadMissingObservation,
				minDaysAlive,
				maxDaysAlive,
				IslandFullNames,
				HabitatType
			))
		Plant.Surveys.by.Plant %<>% 
			dplyr::select(-c(
				Island,
				Network,
				Species,                
				RecruitmentMode,
				Parent,
				First.Survey.Date.Alive,
				AliveatEndofStudy,
				Tag_Number,
				OldMothPlantPres,
				MEPlantPres,
				CAPlantPres,
				MothPlantPres,
				First.Survey.Date,
				First.DemographicSurvey,
				minFecundityYear,
				First_Size,
				First.Measurement.Date,
				min.Size,
				max.Size,
				LastDateAlive,
				FirstDeadObservation,
				FirstMissingObservation,
				FirstDeadMissingObservation,
				minDaysAlive,
				maxDaysAlive,
				IslandFullNames,
				HabitatType
			))
		Plant.Surveys.by.Year %<>% merge(Plant.Info.Analysis, by="PlantID")
		Plant.Surveys.by.Plant %<>% merge(Plant.Info.Analysis, by="PlantID")
	}
	# PARENT SURVEY DATA
	# use this dataset to use parent size that is consistent for all offpsring
	A <- Plant.Surveys.by.Year %>% 
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
		)
	colnames(A)[which(names(A) == "SurveyDate_SpringSummer")] <- 
		"Parent.SpringSummer.Obs.Date_t"
	colnames(A)[which(names(A) == "Size_t")] <- "Parent.Size_t"
	colnames(A)[which(names(A) == "FecundityYear")] <- "Parent.FecundityYear"
	colnames(A)[which(names(A) == "PlantID")] <- "Parent.ID"
	# OFFSPRING SURVEY DATA
	# use this dataset to figure out first size of offspring
	B <- Plant.Surveys.by.Plant %>% 
		dplyr::select(PlantID, Date, Size_t)
	colnames(B)[which(names(B) == "Date")] <- "Offspring.Obs.Date"
	colnames(B)[which(names(B) == "Size_t")] <- "Offspring.Size_t"
	colnames(B)[which(names(B) == "PlantID")] <- "Offspring.ID"
	# merge parent data (from A) with offspring info (from Plant.Info.Analysis)
	C <- Plant.Info.Analysis 
	C %<>%
		mutate(Parent = substr(x=Parent,start=1,stop=4))
	colnames(C)[which(names(C) == "PlantID")] <- "Offspring.ID"
	colnames(C)[which(names(C) == "Parent")] <- "Parent.ID"
	colnames(C)[which(names(C) == "First_Size")] <- "Offspring.First_Size"
	colnames(C)[which(names(C) == "First.Survey.Date.Alive")] <- 
		"Offspring.First.Survey.Date.Alive"
	colnames(C)[which(names(C) == "minFecundityYear")] <- 
		"Offspring.minFecundityYear"
	C %<>%
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
	Plant.Surveys.by.Yearw_clones <- Plant.Surveys.by.Year %>%
		merge(
			parent_size, 
			by.x=c("PlantID", "FecundityYear"), 
			by.y=c("Parent.ID", "Parent.FecundityYear"),
			all=T
		) %>% 
		as.data.frame
	Plant.Surveys.by.Yearw_clones %<>%
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
	Plant.Surveys.by.Yearw_clones %<>% as.data.frame
	Plant.Surveys.by.Yearw_clones$SizewClones_t %<>% as.numeric
	# presence of clonal reproduction
	Plant.Surveys.by.Yearw_clones$ClonePres_t <- ifelse(
		Plant.Surveys.by.Yearw_clones$NSegLosttoClones_t > 0,
		1,
		0
	)	
	# ------------------------------------------------------------- WARNINGS ---
	# for which parents of offspring are there no NClones_t records?
	PlanIntoParents <- Plant.Info.Analysis %>% 
		filter(Parent!="Unknown") %$% 
		unique(Parent)
	PlantSurveyswClones <- Plant.Surveys.by.Yearw_clones %>% 
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
	# create dataset for building the clonal reproduction transition matrix
	clonal_reproduction_info_for_transition_matrix <- merge(
		E %>% 
			dplyr::select(
				Parent.ID, 
				Parent.FecundityYear, 
				Offspring.ID, 
				Offspring.First_Size
			),
		parent_size %>% 
			dplyr::select(
				Parent.ID, 
				Parent.FecundityYear, 
				SizewClones_t
			),
		by=c("Parent.ID", "Parent.FecundityYear")
	)
	
	
	# --------------------------------------------------------------------------
	return(
		list(
			Plant.Surveys.by.Yearw_clones,
			clonal_reproduction_info_for_transition_matrix
		)
	)
}