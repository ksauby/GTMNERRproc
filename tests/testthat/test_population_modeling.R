test_that("calculateTransitionRates", {
	test_clone_table <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,13,0,0,0,0,0,
		122,32,14,0,0,0,443,162,84,12) %>%
		matrix(
			.,
			nrow=6,
			ncol=6,
			dimnames=list(
				c("Seed","Seedling","1","2","3","4"), 
				c("Seed","Seedling","1","2","3","4")
			)
		)
	test_n_per_stage <- c(0,2,202,174,280,82)
	expected_transition_rate_table <- c(
		rep(0, 20),
		0.07471264,
		rep(0, 5),
		0.43571429, 0.11428571, 0.05000000,
		rep(0, 3),
		5.40243902, 1.97560976, 1.02439024, 0.14634146
	) %>%
		matrix(
			.,
			nrow=6,
			ncol=6,
			dimnames=list(
				c("Seed","Seedling","1","2","3","4"), 
				c("Seed","Seedling","1","2","3","4")
			)
		)	
	# check that transition rates are calculated correctly
	test_transition_rate_table <- calculateTransitionRates(
		test_clone_table, 
		test_n_per_stage
	)
	expect_that(
		test_transition_rate_table,
		equals(expected_transition_rate_table)
	)
	# Stages should give rise to 0 seeds and 0 seedlings
	expect_that(
		as.vector(test_transition_rate_table[, "Seed"]),
		equals(rep(0, length(test_transition_rate_table[, "Seed"])))
	)
	expect_that(
		as.vector(test_transition_rate_table[, "Seedling"]),
		equals(rep(0, length(test_transition_rate_table[, "Seedling"])))
	)
	# seeds should give rise to 0 clones
	expect_that(
		as.vector(test_transition_rate_table["Seed", ]),
		equals(rep(0, length(test_transition_rate_table["Seed", ])))
	)
	# seedlings should give rise to 0 clones
	expect_that(
		as.vector(test_transition_rate_table["Seedling", ]),
		equals(rep(0, length(test_transition_rate_table["Seedling", ])))
	)
})