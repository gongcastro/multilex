context("ml_vocabulary")

credentials <- jsonlite::read_json(file.path(paste0(.libPaths()[1], "/multilex/secrets/secrets.json")))
ml_connect(
  google_email = credentials$google_email,
  formr_email = credentials$formr_email,
  formr_password = credentials$formr_password
)

participants <- ml_participants()

responses <- ml_responses(participants, update = FALSE)

n_total <- studies %>%
  dplyr::distinct(version, language, .keep_all = TRUE) %>%
  dplyr::group_by(version) %>%
  dplyr::summarise(n_total = sum(n))

vocabulary <- ml_vocabulary(participants, responses, scale = c("count", "prop"), by = "code") %>%
  dplyr::left_join(dplyr::select(participants, id, code, version, randomisation)) %>%
  dplyr::mutate(
    version = case_when(
      stringr::str_detect(id, "cbc") ~ "CBC",
      stringr::str_detect(id, "devlex") ~ "DevLex",
      TRUE ~ paste(version, randomisation, sep = "-")
    )
  ) %>%
  dplyr::left_join(n_total)

test_that("vocabulary counts are plausible", {
  expect_true(all(vocabulary$vocab_count_total <= vocabulary$n_total))
})

test_that("vocabulary proportions are plausible", {
  expect_true(all(between(vocabulary$vocab_prop_total, 0, 1)))
  expect_true(all(between(vocabulary$vocab_prop_dominance_l1[!is.na(vocabulary$vocab_prop_dominance_l1)], 0, 1)))
  expect_true(all(between(vocabulary$vocab_prop_dominance_l2[!is.na(vocabulary$vocab_prop_dominance_l2)], 0, 1)))
  expect_true(all(between(vocabulary$vocab_prop_conceptual, 0, 1)))
  expect_true(all(between(vocabulary$vocab_prop_te, 0, 1)))

})
