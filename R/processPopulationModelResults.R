#' Process Population Model Results
#'
#' @description Prepare population modeling results for plotting.
#'
#' @param MPMresults
#' @param LTRE_variable Life table response variable, Default value is NULL.
#'
#' @export

processPopulationModelingResults <- function(
	MPMresults,
	LTRE_variable = NULL
) {
	reshape.variables <- c(
		"SeedBankSize",
		"SeedsPerFruit",
		"SeedSurvival",
		"Parent Assignment",	
		"N.Stages",
		LTRE_variable
	)
	s.variables <- c(
		reshape.variables,
		"sensitivities"
	)
	sensitivity.results <- MPMresults %>% 
		dplyr::select(matches(paste(s.variables, collapse="|"))) %>% 
		reshape2::melt(id.vars=c(reshape.variables)) %>%
		rowwise() %>%
		mutate(
			variable = gsub(pattern="sensitivities.?","",variable),
			variable = gsub(pattern="[.]","->",variable)
		) %>%
		as.data.frame %>%
		setnames("variable", "transition") %>%
		setnames("value", "sensitivity")
	e.variables <- c(
		reshape.variables,
		"elasticities"
	)
	elasticity.results <- MPMresults %>% 
		dplyr::select(matches(paste(e.variables, collapse="|"))) %>% 
		reshape2::melt(id.vars=c(reshape.variables)) %>%
		rowwise() %>%
		mutate(
			variable = gsub(pattern="elasticities.?","",variable),
			variable = gsub(pattern="[.]","->",variable)
		) %>%
		as.data.frame %>%
		setnames("variable", "transition") %>%
		setnames("value", "elasticity")
	return(list(
		sensitivity.results = sensitivity.results,
		elasticity.results = elasticity.results
	))
}