## code to prepare `inst/extdata/pool.xlsx` dataset goes here
readxl::read_xlsx(system.file("extdata", "pool.xlsx", package = "multilex")) %>%
  usethis::use_data(overwrite = TRUE)
