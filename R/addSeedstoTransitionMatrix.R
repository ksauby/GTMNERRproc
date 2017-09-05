#' Create Projection Matrix

#' Create projection matrix with growth, stasis, retrogression, survival, and sexual reproduction rates.

#' @param trans_data transition data
#' @param SeedSurvival seed survival estimate
#' @param SeedBankSize seed bank size estimate
#' @param SeedsPerFruit average number of seeds per fruit


#' @return Return a projection matrix

#' @export

createProjectionMatrix <- function(
	trans_data,
	SeedBankSize,
	SeedsPerFruit,
	SeedSurvival
) {
	seeds.from.plants 		<- sum(trans_data$Repro, na.rm=T) * SeedsPerFruit
	recruitment.rate 		<- Seedlings/(SeedBankSize + seeds.from.plants)
	trans_data$Seedling 	<- trans_data$Repro/sum(trans01$Repro, na.rm=T) * 
									seeds.from.plants * recruitment.rate
	trans_data$Seed 		<- trans_data$Repro * SeedsPerFruit * SeedSurvival
	# return projection matrix
	projection.matrix(
		trans_data, 
		add = c(
			# transition from seed to seedling
			2, 1, recruitment.rate
		),
		sort = levels(trans_data$stage)
	)
}

