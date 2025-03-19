adsl <- teal.data::rADSL
adtte <- teal.data::rADTTE

data_list <- list(ADSL = reactive(adsl), ADTTE = reactive(adtte))
join_keys <- teal.data::default_cdisc_join_keys[c("ADSL", "ADTTE")]
primary_keys_list <- lapply(join_keys, function(keys) keys[[1]])
