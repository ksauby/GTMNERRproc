#' Analyze Bootstrapped Matrix Population Models
#'
#' @description Analyze multiple matrix population models, allowing for a seed bank and clonal reproduction. If a boostrap population cannot be analyzed, it is not added to the dataset (or is listed as NAs for all values).

#' @return Returns a list including:
#' \itemize{
#'  	\item the growth/retrogression/survival/fecundity transition matrix and clonal reproduction transition matrix for each matrix population model analysis
#'  	\item the results of the matrix population model analysis, including repro. values for each stage, the stable stage distribution, sensitivities, elasticities, lambda, the damping ratio, the seed bank size, the number of seeds assumed per fruit, and the seed survival rate (the rate at which seeds stay in the seed stage from one year to the next).
#' 	}
#'
#' @param trans_data dataset from which to create growth/retrogression/survival/fecundity transition matrix
#' @param SizeClass A vector containing the size breaks with which to create stages based on size.
#' @param TransitionYear
#' @param SeedSurvival Rate at which seeds survive (i.e., rate at which seeds remain in seed stage). Can be a single value or vector of values.
#' @param SeedBankSize Number of seeds in the seed bank (can be a single value or vector of values).
#' @param SeedsPerFruit Average number of seeds produced per fruit (can be a single value or vector of values).
#' @param clone_transition clonal reproduction transition matrix
#' @param SizeClass
#' @param n.iter Number of iterations for pop.projection function. Default value is 1000.

#' @export


analyzeBootstrappedMatrixPopModels <- function(
	trans_data,
	clonal_repro_dataset,
	SizeClass,
	TransitionYear,
	SeedSurvival,
	SeedBankSize,
	SeedsPerFruit,
	n.iter=1000,
	n_bootstraps = 5
) {
	A <- vector("list", length(SeedBankSize))	
	for (i in 1:length(SeedBankSize)) {
		A[[i]] <- vector("list", length(SeedsPerFruit))	
		for (j in 1:length(SeedsPerFruit)) {
			A[[i]][[j]] <- vector("list", length(SeedSurvival))	
			for (k in 1:length(SeedSurvival)) {
				seeds <- runif(n_bootstraps)
				for (l in 1:n_bootstraps) {
					temp_seed <- seeds[l]*100000
					analysis.results <- NULL
					# -------------------------------------------------------- #
					# Create growth/retrogression/survival/fecundity transition matrix
					# -------------------------------------------------------- #
					# bootstrap ldf
					set.seed(temp_seed)
					x <- sample(nrow(trans_data$trans01), replace=TRUE)
					ldf_bootstrap <- trans_data$trans01[x, ]
					ldf_bootstrap$bootstrap_indiv <- 1:dim(ldf_bootstrap)[1]
					# create transition matrix
					proj_mat <- createProjectionMatrix(
						ldf_bootstrap,
						SeedBankSize[i],
						SeedsPerFruit[j],
						SeedSurvival[k]
					)
					# -------------------------------------------------------- #
					# Create clonal reproduction transition matrix
					# -------------------------------------------------------- #
					# DONT RANDOMLY SAMPLE, MERGE WITH BOOTSTRAPPED LDF TO GET CLONES THAT THOSE PLANTS PRODUCED
					
					clonal_repro_dat <- merge(
						ldf_bootstrap, 
						clonal_repro_dataset,
						by.x="PlantID", 
						by.y="Parent.ID"
					)
					
					# STAGE AND PARENT STAGE ARE DIFFERENT FOR PARENTS WITH CLONES
				
					# AFTER I FIGURE OUT CLONES THEN I NEED TO FIGURE OUT PARENT SIZE AGAIN? AND THEN DIVIDE BY CORRECT NUMBER OF INDIVIDUALS PER STAGE?
				
					# create clonal transition matrix
					clonal.matrices <- createClonalReproTransitionMatrix(
						clonal_repro_dat, 
						ldf_bootstrap,
						trans_data$stages
					)
					# ------------------------------------------------------- #
					# add transition matrices together
					# ------------------------------------------------------- #
					all_proj_matrix <- proj_mat + clonal.matrices[[2]]
					# ------------------------------------------------------- #
					# starting numbers
					# ------------------------------------------------------- #
					n_per_stage <- NULL
					n_per_stage <- calculateNumberIndivperStage(
						trans_data$trans01, 
						trans_data$stages
					)
					# ------------------------------------------------------- #
					# dynamics
					# ------------------------------------------------------- #
					all_proj_matrix[1, 1] <- SeedSurvival[k]
					n_per_stage %<>% 
						mutate(
							n = replace(
								n, 
								which(stage=="Seed"),
								SeedBankSize[i]
							)
						)
					pr <- pop.projection(
						A = all_proj_matrix, 
						n = n_per_stage$n, 
						iterations = n.iter
					)
					analysis.results <- eigen.analysis(all_proj_matrix)
					if (!(is.null(analysis.results))) {
						# create table of results				
						#		repro.values
						repro.value.dat <- analysis.results$repro.value
						names(repro.value.dat) <- 
							paste("repro.value",names(repro.value.dat),sep=".")
						repro.value.dat %<>% t %>% as.data.frame
						#		stable stage distribution
						stable.stage.dat <- analysis.results$stable.stage
						names(stable.stage.dat) <- 
							paste("stable.stage",names(stable.stage.dat),sep=".")
						stable.stage.dat %<>% t %>% as.data.frame
						#		elasticities
						elasticities <- analysis.results$elasticities %>% 
							as.data.frame %>%
							mutate(
								Name = paste(
									"elasticities.", 
									Var2, "-", 
									Var1, 
									sep=""
								)
							) %>%
							as.data.frame(row.names=.$Name) %>%
							dplyr::select(Freq) %>%
							t %>%
							as.data.frame()
						#		sensitivities
						sensitivities <- analysis.results$sensitivities %>% 
							as.table %>% as.data.frame %>%
							mutate(
								Name = paste(
									"sensitivities.", 
									Var2, "-",
									Var1, 
									sep=""
								)
							) %>%
							as.data.frame(row.names=.$Name) %>%
							dplyr::select(Freq) %>%
							t %>%
							as.data.frame()
						A[[i]][[j]][[k]][[l]] <- data.frame(
							repro.value.dat,
							stable.stage.dat,
							sensitivities,
							elasticities,
							lambda1 		= analysis.results$lambda1,
							damping.ratio 	= analysis.results$damping.ratio,
							SeedBankSize 	= SeedBankSize[i],
							SeedsPerFruit 	= SeedsPerFruit[j],
							SeedSurvival 	= SeedSurvival[k],
							Bootstrap		= l,
							seed			= temp_seed
						)		
					}
				}
				A[[i]][[j]][[k]] <- do.call(rbind.data.frame, A[[i]][[j]][[k]])
			}
			A[[i]][[j]] <- do.call(rbind.data.frame, A[[i]][[j]])
		}
		A[[i]] <- do.call(rbind.data.frame, A[[i]])
	}
	A <- do.call(rbind.data.frame, A)
	return(A)
}
