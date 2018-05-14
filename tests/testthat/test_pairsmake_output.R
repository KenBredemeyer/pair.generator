context("pairs_make output")

test_that("output of pairs_make has changed", {
	expect_equivalent(head(pairs_make(data_standard)),
		           data.frame(left = c("g", "g", "g", "g", "j", "b"),
			                    right = c("s", "m", "u", "k", "g", "j"),
		           	          combination = c(147, 141, 149, 139, 138, 33),
		           	          chain = c("g", "g", "g", "g", "j", "j"),
		           	          chain_number = c(1, 1, 1, 1, 2, 2),
		           	stringsAsFactors = FALSE)
)})

# save output to file to test equivalent.
	load("H:/packs/pair.generator/data/gp_standard_default.RData")
	expect_identical(pairs_make(data_standard), gp)

	load("H:/packs/pair.generator/data/gp_cnc_nci10.RData")
	expect_identical(pairs_make(data_cnc, type = "core_noncore", nc_include = 10),
		               gp_cnc_saved)
