#### import_formr-short: Import formr short ####################################

import_formr_short <- function(
  surveys = c(
    "bilexicon_short_01_log",
    "bilexicon_short_02_welcome",
    "bilexicon_short_03_consent",
    "bilexicon_short_04_demo",
    "bilexicon_short_05_language",
    "bilexicon_short_06_words_catalan",
    "bilexicon_short_06_words_spanish"
  )) {
  participants <- get("participants", parent.frame())
  participants <- filter(participants, cdi %in% "BL-Short")

  # import data
  raw <- purrr::map(surveys, formr::formr_raw_results) %>%
    purrr::set_names(surveys) %>%
    purrr::map(select, -any_of("language"))
  raw$bilexicon_short_06_words_spanish <- rename_all(
    raw$bilexicon_short_06_words_spanish,
    stringr::str_replace, "cat_", "spa_"
  )
  raw$bilexicon_short_01_log <- raw$bilexicon_short_01_log %>%
    mutate(created = lubridate::as_datetime(created)) %>%
    arrange(desc(created), code) %>%
    distinct(code, .keep_all = TRUE) %>%
    drop_na(code)

  # process data
  processed <- raw %>%
    purrr::map(select, -one_of(c("created", "modified", "ended", "expired"))) %>%
    purrr::reduce(left_join, by = "session") %>%
    mutate(code = fix_code(code)) %>%
    left_join(participants, by = "code") %>%
    filter(code %in% participants$code) %>%
    left_join(select(raw$bilexicon_short_06_words_cat, session, created_cat = created, ended_cat = ended), by = "session") %>%
    left_join(select(raw$bilexicon_short_06_words_spa, session, created_spa = created, ended_spa = ended), by = "session") %>%
    drop_na(created_cat, created_spa, ended_cat, ended_spa) %>%
    mutate_at(c("created_cat", "created_spa", "ended_cat", "ended_spa", "date_birth"), lubridate::as_datetime) %>%
    mutate_at(vars(starts_with("language_doe")), function(x) ifelse(is.na(x), 0, x)) %>%
    mutate(
      version = paste0("BL-Short-", version),
      time_stamp = lubridate::as_datetime(get_time_stamp(., c("ended_spa", "ended_cat"), "last")),
      age = as.numeric(time_stamp-date_birth)/30,
      age = ifelse(age %in% c(-Inf, Inf), NA_real_, age),
      language_doe_catalan = get_doe(., languages = c("catalan_barcelona", "catalan_majorca", "catalan_other")),
      language_doe_spanish = get_doe(., languages = c("spanish", "spanish_american"))
    ) %>%
    janitor::clean_names() %>%
    arrange(desc(time_stamp)) %>%
    distinct(session, .keep_all = TRUE) %>%
    rename(postcode = demo_postcode, edu_parent1 = demo_parent1, edu_parent2 = demo_parent2) %>%
    rename_all(stringr::str_remove, "language_") %>%
    drop_na(age) %>%
    select(
      starts_with("id"), time, code, study, version,
      time_stamp, date_birth, age, sex, postcode,
      starts_with("edu_"), doe_catalan, doe_spanish,
      matches("cat_|spa_")
    ) %>%
    # group_by(id, time, code) %>%
    # summarise_all(coalesce_by_column) %>%
    # ungroup() %>%
    pivot_longer(cols = matches("cat_|spa_"), names_to = "item", values_to = "response") %>%
    mutate(
      language = ifelse(grepl("cat_", item), "Catalan", "Spanish"),
      sex = ifelse(sex %in% 1, "Male", "Female"),
      postcode = as.character(ifelse(postcode %in% "", NA_character_, postcode)),
      edu_parent1 = ifelse(edu_parent1 %in% "", NA_character_, edu_parent1),
      edu_parent2 = ifelse(edu_parent2 %in% "", NA_character_, edu_parent2)
    ) %>%
    arrange(desc(time_stamp))

  message("BiLexicon-Short updated")

  return(processed)
}
