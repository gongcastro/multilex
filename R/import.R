#### lockdown ------------------------------------------------------------------
#' @import dplyr

import_formr_lockdown <- function(
  surveys = c(
    "bilexicon_lockdown_01_log",
    "bilexicon_lockdown_02_welcome",
    "bilexicon_lockdown_03_consent",
    "bilexicon_lockdown_04_demo",
    "bilexicon_lockdown_05_language",
    "bilexicon_lockdown_06_words_catalan",
    "bilexicon_lockdown_06_words_spanish"
  )) {
  participants <- get("participants", parent.frame())
  participants <- filter(participants, version %in% "BL-Lockdown") %>%
    select(-version)

  items_to_keep <- c("code", "bl_code", "consent_mail", "demo_parent1", "demo_parent2", "demo_postcode", "sex", "language_doe_spanish", "language_doe_spanish_american", "language_doe_catalan_barcelona", "language_doe_catalan_majorca", "language_doe_catalan_other")

  raw <- purrr::map(surveys, formr::formr_raw_results) %>%
    purrr::set_names(surveys) %>%
    purrr::map(select, -any_of("language"))

  raw$bilexicon_lockdown_01_log <- raw$bilexicon_lockdown_01_log %>%
    mutate(created = lubridate::as_datetime(created)) %>%
    arrange(desc(created), bl_code) %>%
    tidyr::drop_na(bl_code, ended) %>%
    filter(bl_code != "") %>%
    distinct(bl_code, .keep_all = TRUE)

  processed <- raw %>%
    purrr::map(select, -one_of(c("created", "modified", "ended", "expired"))) %>%
    purrr::reduce(left_join, by = "session") %>%
    rename(code = bl_code) %>%
    mutate(code = fix_code(code)) %>%
    left_join(., participants, by = "code") %>%
    filter(code %in% participants$code) %>%
    left_join(
      select(
        raw$bilexicon_lockdown_06_words_cat,
        session,
        created_cat = created,
        ended_cat = ended),
      by = "session"
    ) %>%
    left_join(
      select(
        raw$bilexicon_lockdown_06_words_spa,
        session,
        created_spa = created,
        ended_spa = ended),
      by = "session"
    ) %>%
    tidyr::drop_na(created_cat, created_spa) %>%
    mutate_at(
      c("created_cat", "created_spa", "ended_cat", "ended_spa", "date_birth"),
      lubridate::as_datetime
    ) %>%
    mutate_at(
      vars(starts_with("language_doe")),
      function(x) ifelse(is.na(x), 0, x)
    ) %>%
    mutate(
      version = paste0("BL-Lockdown-", version),
      time_stamp = lubridate::as_datetime(get_time_stamp(., c("ended_cat", "ended_spa"), "last")),
      age = as.numeric(time_stamp-date_birth)/30,
      age = ifelse(age %in% c(-Inf, Inf), NA_real_, age),
      language_doe_catalan = get_doe(., languages = languages_lockdown1[grep("catalan", languages_lockdown1)]),
      language_doe_spanish = get_doe(., languages = languages_lockdown1[grep("spanish", languages_lockdown1)]),
      language_doe_catalan_lockdown = get_doe(., languages = languages_lockdown2[grep("catalan", languages_lockdown2)]),
      language_doe_spanish_lockdown = get_doe(., languages = languages_lockdown2[grep("spanish", languages_lockdown2)]),
    ) %>%
    rowwise() %>%
    mutate(
      language_doe_others = 100-sum(language_doe_catalan, language_doe_spanish, na.rm = TRUE),
      language_doe_others_lockdown = 100-sum(language_doe_catalan_lockdown, language_doe_spanish_lockdown, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    janitor::clean_names() %>%
    arrange(desc(time_stamp)) %>%
    distinct(session, .keep_all = TRUE) %>%
    rename(
      postcode = demo_postcode,
      edu_parent1 = demo_parent1,
      edu_parent2 = demo_parent2
    ) %>%
    rename_all(str_remove, "language_") %>%
    tidyr::drop_na(age) %>%
    select(
      starts_with("id"), time, code, study, version, randomisation,
      time_stamp, date_birth, age, postcode, sex,
      starts_with("edu_"),
      doe_catalan, doe_spanish, doe_others,
      doe_catalan_lockdown, doe_spanish_lockdown, doe_others_lockdown,
      matches("cat_|spa_")
    ) %>%
    tidyr::pivot_longer(
      cols = matches("cat_|spa_"),
      names_to = "item",
      values_to = "response") %>%
    mutate(
      language = ifelse(str_detect(item, "cat_"), "Catalan", "Spanish"),
      sex = ifelse(sex==1, "Male", "Female"),
      postcode = as.integer(ifelse(postcode=="", NA_character_, postcode)),
      edu_parent1 = ifelse(edu_parent1 %in% "", NA_character_, edu_parent1),
      edu_parent2 = ifelse(edu_parent2 %in% "", NA_character_, edu_parent2),
      sex = NA_character_
    ) %>%
    arrange(desc(time_stamp))

  message("BL-Lockdown updated")

  return(processed)
}


#### short ---------------------------------------------------------------------
#' @import dplyr
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
  participants <- filter(participants, version %in% "BL-Short") %>%
    select(-version)

  items_to_keep <- c("code", "bl_code", "consent_mail", "demo_parent1", "demo_parent2", "demo_postcode", "sex", "language_doe_spanish", "language_doe_spanish_american", "language_doe_catalan_barcelona", "language_doe_catalan_majorca", "language_doe_catalan_other")

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
    tidyr::drop_na(code)

  # process data
  processed <- raw %>%
    purrr::map(select, -one_of(c("created", "modified", "ended", "expired"))) %>%
    purrr::reduce(left_join, by = "session") %>%
    mutate(code = fix_code(code)) %>%
    left_join(participants, by = "code") %>%
    filter(code %in% participants$code) %>%
    left_join(select(raw$bilexicon_short_06_words_cat, session, created_cat = created, ended_cat = ended), by = "session") %>%
    left_join(select(raw$bilexicon_short_06_words_spa, session, created_spa = created, ended_spa = ended), by = "session") %>%
    tidyr::drop_na(created_cat, created_spa) %>%
    mutate_at(c("created_cat", "created_spa", "ended_cat", "ended_spa", "date_birth"), lubridate::as_datetime) %>%
    mutate_at(vars(starts_with("language_doe")), function(x) ifelse(is.na(x), 0, x)) %>%
    mutate(
      version = paste0("BL-Short-", version),
      time_stamp = lubridate::as_datetime(get_time_stamp(., c("ended_cat", "ended_spa"), "last")),
      age = as.numeric(time_stamp-date_birth)/30,
      age = ifelse(age %in% c(-Inf, Inf), NA_real_, age),
      language_doe_catalan = get_doe(., languages = languages_short[grep("catalan", languages_short)]),
      language_doe_spanish = get_doe(., languages = languages_short[grep("spanish", languages_short)])
    ) %>%
    rowwise() %>%
    mutate(language_doe_others = 100-sum(language_doe_catalan, language_doe_spanish, na.rm = TRUE)) %>%
    ungroup() %>%
    janitor::clean_names() %>%
    arrange(desc(time_stamp)) %>%
    distinct(session, .keep_all = TRUE) %>%
    rename(postcode = demo_postcode, edu_parent1 = demo_parent1, edu_parent2 = demo_parent2) %>%
    rename_all(stringr::str_remove, "language_") %>%
    tidyr::drop_na(age) %>%
    select(
      starts_with("id"), time, code, study, version, randomisation,
      time_stamp, date_birth, age, sex, postcode,
      starts_with("edu_"), doe_catalan, doe_spanish,
      matches("cat_|spa_")
    ) %>%
    # group_by(id, time, code) %>%
    # summarise_all(coalesce_by_column) %>%
    # ungroup() %>%
    tidyr::pivot_longer(cols = matches("cat_|spa_"), names_to = "item", values_to = "response") %>%
    mutate(
      language = ifelse(grepl("cat_", item), "Catalan", "Spanish"),
      sex = ifelse(sex %in% 1, "Male", "Female"),
      postcode = as.integer(ifelse(postcode %in% "", NA_character_, postcode)),
      edu_parent1 = ifelse(edu_parent1 %in% "", NA_character_, edu_parent1),
      edu_parent2 = ifelse(edu_parent2 %in% "", NA_character_, edu_parent2)
    ) %>%
    arrange(desc(time_stamp))

  message("BL-Short updated")

  return(processed)
}


#### formr 2.0 -----------------------------------------------------------------
#' @import dplyr

import_formr2 <- function(
  surveys = c(
    "bilexicon_01_log",
    "bilexicon_02_welcome",
    "bilexicon_03_consent",
    "bilexicon_04_demo",
    "bilexicon_05_language",
    "bilexicon_06_words_cat",
    "bilexicon_06_words_spa"
  )) {
  participants <- get("participants", parent.frame())
  participants <- filter(participants, version %in% "BL-Long", randomisation %in% "2") %>%
    select(-version)

  items_to_keep <- c("code", "bl_code", "consent_mail", "demo_parent1", "demo_parent2", "demo_postcode", "sex", "language_doe_spanish", "language_doe_spanish_american", "language_doe_catalan_barcelona", "language_doe_catalan_majorca", "language_doe_catalan_other")

  # import data
  raw <- purrr::map(surveys, formr::formr_raw_results) %>%
    purrr::set_names(surveys) %>%
    purrr::map(select, -any_of("language"))
  raw$bilexicon_06_words_spa <- rename_all(
    raw$bilexicon_06_words_spa,
    stringr::str_replace, "cat_", "spa_"
  )
  raw$bilexicon_01_log <- raw$bilexicon_01_log %>%
    mutate(created = lubridate::as_datetime(created)) %>%
    arrange(desc(created), code) %>%
    tidyr::drop_na(code, ended) %>%
    filter(code != "") %>%
    distinct(code, .keep_all = TRUE)

  # process data
  processed <- raw %>%
    purrr::map(select, -any_of(c("created", "modified", "ended", "expired"))) %>%
    purrr::reduce(left_join, by = "session") %>%
    mutate(code = fix_code(code)) %>%
    left_join(
      select(participants, -comments),
      by = "code"
    ) %>%
    filter(code %in% participants$code) %>%
    left_join(
      select(raw$bilexicon_06_words_cat, session, created_cat = created, ended_cat = ended),
      by = "session"
    ) %>%
    left_join(
      select(raw$bilexicon_06_words_spa, session, created_spa = created, ended_spa = ended),
      by = "session") %>%
    tidyr::drop_na(created_cat, created_spa, ended_cat, ended_spa) %>%
    mutate_at(
      c("created_cat", "created_spa", "ended_cat", "ended_spa", "date_birth"),
      lubridate::as_datetime
    ) %>%
    mutate_at(
      vars(starts_with("language_doe")),
      function(x) ifelse(is.na(x), 0, x)
    ) %>%
    mutate(
      version = "BL-Long-2",
      time_stamp = lubridate::as_datetime(get_time_stamp(., c("ended_cat", "ended_spa"), "last")),
      age = as.numeric(time_stamp - date_birth) / 30,
      age = ifelse(age %in% c(-Inf, Inf), NA_real_, age),
      language_doe_catalan = get_doe(., languages = languages2[grep("catalan", languages2)]),
      language_doe_spanish = get_doe(., languages = languages2[grep("spanish", languages2)]),
    ) %>%
    rowwise() %>%
    mutate(language_doe_others = 100-sum(language_doe_catalan, language_doe_spanish, na.rm = TRUE)) %>%
    ungroup() %>%
    janitor::clean_names() %>%
    arrange(desc(time_stamp)) %>%
    distinct(session, .keep_all = TRUE) %>%
    rename(
      postcode = demo_postcode,
      edu_parent1 = demo_parent1,
      edu_parent2 = demo_parent2
    ) %>%
    rename_all(str_remove, "language_") %>%
    tidyr::drop_na(age) %>%
    select(
      starts_with("id"), time, code, study,version, randomisation,
      time_stamp, date_birth, age, sex, postcode,
      starts_with("edu_"), doe_catalan, doe_spanish, doe_others,
      matches("cat_|spa_")
    ) %>%
    tidyr::pivot_longer(cols = matches("cat_|spa_"), names_to = "item", values_to = "response") %>%
    mutate(
      language = ifelse(grepl("cat_", item), "Catalan", "Spanish"),
      sex = ifelse(sex %in% 1, "Male", "Female"),
      postcode = as.integer(ifelse(postcode %in% "", NA_character_, postcode)),
      edu_parent1 = ifelse(edu_parent1 %in% "", NA_character_, edu_parent1),
      edu_parent2 = ifelse(edu_parent2 %in% "", NA_character_, edu_parent2)
    ) %>%
    arrange(desc(time_stamp))

  message("BL-Long-2 updated")

  return(processed)
}
