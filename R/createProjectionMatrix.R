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













  if (missing(stage)) {
        stage <- "stage"
    }
    if (missing(fate)) {
        fate <- "fate"
    }
    nl <- as.list(1:ncol(transitions))
    names(nl) <- names(transitions)
    stage <- eval(substitute(stage), nl, parent.frame())
    fate <- eval(substitute(fate), nl, parent.frame())
    if (is.null(transitions[, stage])) {
        stop("No stage column matching ", stage)
    }
    if (is.null(transitions[, fate])) {
        stop("No fate column matching ", fate)
    }
    if (missing(sort)) {
        sort <- levels(transitions[, stage])
    }
    if (missing(fertility)) {
        fertility <- intersect(sort, names(transitions))
    }
    fertility <- eval(substitute(fertility), nl, parent.frame())
    tf <- table(transitions[, fate], transitions[, stage])
    T_matrix <- try(prop.table(tf, 2)[sort, sort], silent = TRUE)
    if (class(T_matrix) == "try-error") {
        warning(paste("Error sorting matrix.\n  Make sure that levels in stage and fate columns\n  match stages listed in sort option above.\n Printing unsorted matrix instead!\n"), 
            call. = FALSE)
        sort <- TRUE
        T_matrix <- prop.table(tf, 2)
    }
    T_matrix[is.nan(T_matrix)] <- 0
    if (length(add) > 0) {
        for (i in seq(1, length(add), 3)) {
            T_matrix[add[i + 0], add[i + 1]] <- as.numeric(add[i + 
                2])
        }
    }
    n <- length(fertility)
    F_matrix <- T_matrix * 0
    if (n == 0) {
        warning("Missing a fertility column with individual fertility rates\n", 
            call. = FALSE)
    }
    else {
        for (i in 1:n) {
            fert <- tapply(transitions[, fertility[i]], transitions[, 
                stage], mean, na.rm = TRUE)[sort]
            F_matrix[i, ] <- fert
        }
    }
    F_matrix[is.na(F_matrix)] <- 0
    if (TF) {
        list(T = T_matrix, F = F_matrix)
    }
    else {
        T_matrix + F_matrix
    }
}
