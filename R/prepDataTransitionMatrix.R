#' Prepare Data For Transition Matrix Function
#'
#' @param Dat Dataset
#' @param SizeClass Classes into which to divide individuals based on size
#' @param TransitionYear Year

#' @description Prepare Data For Transition Matrix Function
#'
#' @export

prepDataTransitionMatrix <- function(
	Dat,
	clonal_repro_dataset,
	SizeClass,
	TransitionYear
) {
	# Dat$stage <- Dat$Stage
	Dat_census <- Dat %>% 
	rowwise() %>%
	mutate(
		stage=replace(
			stage, 
			which(stage=="Adult"),
			cut(SizewClones_t, SizeClass, include.lowest=T, labels=FALSE)
		)
	)
	Dat_census %<>%
		dplyr::select(
			FecundityYear, 
			PlantID, 
			stage, 
			Fruit_Flowers_t
		)
	colnames(Dat_census)[which(names(Dat_census) == "FecundityYear")] <- "Year"
	# merge year with year - 1
	trans <- subset(
		merge(
			Dat_census, 
			Dat_census, 
			by = "PlantID", 
			sort = FALSE
		), 
		Year.x == Year.y - 1
	)
	# rename rows and columns
	rownames(trans) <- 1:nrow(trans)
	colnames(trans)[which(names(trans) == "Year.x")] 			<- "Year"
	colnames(trans)[which(names(trans) == "stage.x")] 			<- "stage"
	colnames(trans)[which(names(trans) == "Fruit_Flowers_t.x")] <- "Repro"
	colnames(trans)[which(names(trans) == "Year.y")] 			<- "Year2"
	colnames(trans)[which(names(trans) == "stage.y")] 			<- "fate"
	colnames(trans)[which(names(trans) == "Fruit_Flowers_t.y")] <- "Repro2"

	# year-specific transition matrix
	trans01 <- trans %>%
		filter(Year == TransitionYear) %>%
		dplyr::select(PlantID, stage, Repro, fate, Repro2)
	# create full set of stages
	stages <- c(unique(c(trans01$stage, trans01$fate))) %>% 
		as.numeric %>%
		na.omit %>% 
		as.vector %>%
		sort
	stages <- c("Seed", "Seedling", stages, "dead")
	trans01$stage %<>% factor(., levels=stages[-length(stages)], ordered=T)
	trans01$fate %<>% factor(., levels=stages, ordered=T)
	
	# remove last stage
	stages %<>% .[-length(.)]
	proj_matrix <- projection.matrix(
		transitions = trans01,
		stage = stage,
		fate = fate, 
		fertility = Repro,
		sort = stages
	)
	# create matrix of transition counts
	transition.counts <- table(trans01$fate, trans01$stage)
	# create clonal matrices
	clonal_repro_dat <- clonal_repro_dataset %>%
		filter(
			minFecundityYear == TransitionYear,
			Species == "Opuntia stricta"
		)
	clonal.matrices <- createClonalReproTransitionMatrix(
		clonal_repro_dat, 
		trans01,
		stages
	)
	return(list(
		clone_transition_counts = clonal.matrices[[1]],
		clone_transition_rates = clonal.matrices[[2]], 
		transition.counts = transition.counts, 
		proj_matrix = proj_matrix, 
		trans01 = trans01, 
		stages = stages,
		n_per_stage = clonal.matrices[[3]]
	))
}


