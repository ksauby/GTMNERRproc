#' Prepare Data For Transition Matrix Function
#'
#' @param Dat Dataset
#' @param SizeClass Classes into which to divide individuals based on size
#' @param TransitionYear Year
#' @param SeedSurvival Seed survival rate
#' @param SeedBankSize Number of seeds in the seed bank
#' @param SeedsPerFruit Number of seeds produced per fruit

#' @description Prepare Data For Transition Matrix Function
#'
#' @export

prepDataTransitionMatrix <- function(
	Dat,
	SizeClass,
	TransitionYear,
	SeedSurvival,
	SeedBankSize,
	SeedsPerFruit
) {
	# Dat$stage <- Dat$Stage
	Dat_census <- Dat %>% 
	rowwise() %>%
	mutate(
		stage=replace(
			stage, 
			which(stage=="Adult"),
			cut(Size_t, SizeClass, include.lowest=T, labels=FALSE)
		),
	)
	Dat_census %<>%
		dplyr::select(
			FecundityYear, 
			PlantID, 
			stage, 
			Size_t, 
			Fruit_Flowers_t,
			Size_t
		) %>%
		dplyr::select(-Size_t)
		colnames(Dat_census)[which(names(Dat_census) == "FecundityYear")] <- 
			"Year"
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
	colnames(trans)[which(names(trans) == "Year.x")] <- 
		"Year"
	colnames(trans)[which(names(trans) == "stage.x")] <- 
		"stage"
	colnames(trans)[which(names(trans) == "Fruit_Flowers_t.x")] <- 
		"Repro"
	colnames(trans)[which(names(trans) == "Year.y")] <- 
		"Year2"
	colnames(trans)[which(names(trans) == "stage.y")] <- 
		"fate"
	colnames(trans)[which(names(trans) == "Fruit_Flowers_t.y")] <- 
		"Repro2"


	# year-specific transition matrix
	trans01 <- subset(trans, Year == TransitionYear, c(PlantID, stage, Repro, fate, Repro2))
	seedlings <- nrow(subset(
		Dat_census, 
		Year == TransitionYear & stage =="Seedling"
	))
	# trans01 %<>% filter(stage != "Dead")
	# number of seedlings estimated to have been produced by each stage class
	trans01$Seedling <- trans01$Repro/sum(trans01$Repro, na.rm=T) * seedlings
	# estimate seed to seedling transition
	Seedlings <- nrow(subset(trans, Year == TransitionYear & stage =="Seedling"))
	seeds.from.plants <- sum(trans01$Repro) * SeedsPerFruit
	recruitment.rate <- Seedlings/(SeedBankSize + seeds.from.plants)
	trans01$Seedling <- trans01$Repro/sum(trans01$Repro) * 
		seeds.from.plants * recruitment.rate
	trans01$Seed <- trans01$Repro * SeedsPerFruit * SeedSurvival
	# Create ordered list of stages
	stages <- c(unique(c(trans01$stage, trans01$fate))) %>% 
		as.numeric %>%
		na.omit %>% 
		as.vector %>%
		sort
	stages <- c("Seed", "Seedling", stages, "dead")
	trans01$stage <- factor(trans01$stage, levels=stages, ordered=T)
	trans01$fate <- factor(trans01$fate, levels=stages, ordered=T)
	return(list(trans01, recruitment.rate, stages))
}