#' Create Projection Matrix

#' @description Create projection matrix with growth, stasis, retrogression, survival, and sexual reproduction rates. Assumptions about the recruitment rate, seed bank, and fertilities follow Stubben (2007).

#' @param trans_data transition data
#' @param SeedSurvival seed survival estimate
#' @param SeedBankSize seed bank size estimate
#' @param SeedsPerFruit average number of seeds per fruit


#' @return Return a projection matrix
#' @reference Stubben, C., & Milligan, B. (2007). Estimating and analyzing demographic models using the popbio package in R. Journal of Statistical Software.
#' @export

createProjectionMatrix <- function(
	trans_data,
	SeedBankSize,
	SeedsPerFruit,
	SeedSurvival
) {
	Seedlings 			<- trans_data %>% filter(stage =="Seedling") %>% nrow
	seeds.from.plants 	<- sum(trans_data$Repro, na.rm=T) * SeedsPerFruit
	# so the recruitment rate declines as the seed bank size increases?
	recruitment.rate 	<- Seedlings/(SeedBankSize + seeds.from.plants)
	trans_data$Seedling <- trans_data$Repro/sum(trans_data$Repro, na.rm=T)* 
									seeds.from.plants * recruitment.rate
	trans_data$Seed 	<- trans_data$Repro * SeedsPerFruit * SeedSurvival
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