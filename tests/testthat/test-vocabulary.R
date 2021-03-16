context("ml_vocabulary")
participants <- ml_participants()
responses <- ml_responses(participants, update = FALSE)
n_total <- studies %>%
  distinct(version, language, .keep_all = TRUE) %>%
  group_by(version) %>%
  summarise(n_total = sum(n))

vocabulary <- ml_vocabulary(participants, responses, scale = c("count", "prop"), by = "code") %>%
  left_join(select(participants, id, code, version, randomisation)) %>%
  mutate(version = paste(version, randomisation, sep = "-")) %>%
  left_join(n_total)

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
