## code to prepare `data` for testing examples

rADAE <- random.cdisc.data::cadae
usethis::use_data(rADAE, overwrite = TRUE)

rADLB <- random.cdisc.data::cadlb
usethis::use_data(rADLB, overwrite = TRUE)

rADRS <- random.cdisc.data::cadrs
usethis::use_data(rADRS, overwrite = TRUE)

rADSL <- random.cdisc.data::cadsl
usethis::use_data(rADSL, overwrite = TRUE)

rADTTE <- random.cdisc.data::cadtte
usethis::use_data(rADTTE, overwrite = TRUE)

# Use <pkg>:: prefix in examples/tests/vignettes when accessing rAD## data
# objects in case similar dataset is also exported exist in other packages.
