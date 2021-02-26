ml_connect()

options(shiny.usecairo = TRUE)

participants <- ml_participants()
responses <- ml_responses(participants = participants, update = FALSE)

new_codes <- participants %>%
  filter(call=="Successful") %>%
  pull(code)
studies <- c("BiLexicon", "BiLexiconShort", "CognatePriming", "DevLex", "Lockdown", "PhoCross", "phoCross2")
cdi <- c("BL-Lockdown", "BL-Long-2", "CBC", "DevLex", "BL-Long-1", "BL-Short")
version <- c("A", "B", "C", "D")

vocabulary <- ml_vocabulary(participants, responses, scale = "prop")
logs <- ml_logs(participants, responses)
norms <- ml_norms(participants, responses) %>%
  mutate(age_num = as.numeric(factor(age_bin, ordered = TRUE)))

norms_processed <- norms %>%
  mutate(age_bin = factor(age_bin, ordered = TRUE)) %>%
  mutate_if(is.double, function(x) round(x*100, 0)) %>%
  mutate(value = paste0(proportion, "% [", ci_lower, "-", ci_upper, "]", " n=", n)) %>%
  select(te, label, language, ipa, age_bin, type, lp, item_dominance, value) %>%
  mutate(label = paste0(label, " /", ipa, "/")) %>%
  pivot_wider(
    names_from = c(type, item_dominance, language),
    values_from = c(value, label)
  ) %>%
  arrange(te, age_bin, lp) %>%
  select(
    te, age_bin, lp,
    label_understands_L1_Catalan,
    value_understands_L1_Catalan, value_understands_L2_Catalan,
    value_produces_L1_Catalan, value_produces_L2_Catalan,
    label_understands_L1_Spanish,
    value_understands_L1_Spanish, value_understands_L2_Spanish,
    value_produces_L1_Spanish, value_produces_L2_Spanish
  )

scale_colour_custom <- function() scale_color_manual(values = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"))
scale_fill_custom <- function() scale_fill_manual(values = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"))
scale_colour_custom_discrete <- function() scale_color_manual(values = c("#3B9AB2", "#F21A00", "#EBCC2A", "#78B7C5", "#E1AF00"))
scale_fill_custom_discrete <- function() scale_fill_manual(values = c("#3B9AB2", "#F21A00", "#EBCC2A", "#78B7C5", "#E1AF00"))
