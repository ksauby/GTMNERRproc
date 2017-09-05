
addSeedstoTransitionMatrix <- function(
	TMdata,
	SeedSurvival,
	SeedBankSize,
	SeedsPerFruit
) {
	seeds.from.plants <- sum(trans01$Repro, na.rm=T) * SeedsPerFruit
	recruitment.rate <- Seedlings/(SeedBankSize + seeds.from.plants)
	trans01$Seedling <- trans01$Repro/sum(trans01$Repro, na.rm=T) * 
		seeds.from.plants * recruitment.rate
	trans01$Seed <- trans01$Repro * SeedsPerFruit * SeedSurvival
	# Create ordered list of stages

	return(list(trans01, recruitment.rate, stages))
	
}

