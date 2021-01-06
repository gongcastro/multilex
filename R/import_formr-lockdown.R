#' Import lockdown
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr any_of
#' @importFrom dplyr rename_all
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr starts_with
#' @importFrom dplyr matches
#' @importFrom dplyr distinct
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr rowwise
#' @importFrom dplyr ungroup
#' @importFrom dplyr vars
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_longer
#' @importFrom janitor clean_names
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @importFrom purrr set_names
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove
#' @importFrom stringr str_detect
#' @importFrom formr formr_raw_results
#' @importFrom lubridate as_datetime
#'
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
  participants <- filter(participants, cdi %in% "BL-Lockdown") %>%
    select(-version)

  items_to_keep <- c("code", "bl_code", "consent_mail", "demo_parent1", "demo_parent2", "demo_postcode", "sex", "language_doe_spanish", "language_doe_spanish_american", "language_doe_catalan_barcelona", "language_doe_catalan_majorca", "language_doe_catalan_other")

  raw <- map(surveys, formr::formr_raw_results) %>%
    set_names(surveys) %>%
    map(select, -any_of("language"))

  raw$bilexicon_lockdown_01_log <- raw$bilexicon_lockdown_01_log %>%
    mutate(created = as_datetime(created)) %>%
    arrange(desc(created), bl_code) %>%
    distinct(bl_code, .keep_all = TRUE) %>%
    drop_na(bl_code)

  processed <- raw %>%
    map(select, -one_of(c("created", "modified", "ended", "expired"))) %>%
    reduce(left_join, by = "session") %>%
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
    drop_na(created_cat, created_spa) %>%
    mutate_at(
      c("created_cat", "created_spa", "ended_cat", "ended_spa", "date_birth"),
      as_datetime
    ) %>%
    mutate_at(
      vars(starts_with("language_doe")),
      function(x) ifelse(is.na(x), 0, x)
    ) %>%
    mutate(
      version = paste0("BL-Lockdown-", version),
      time_stamp = as_datetime(get_time_stamp(., c("ended_cat", "ended_spa"), "last")),
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
    clean_names() %>%
    arrange(desc(time_stamp)) %>%
    distinct(session, .keep_all = TRUE) %>%
    rename(
      postcode = demo_postcode,
      edu_parent1 = demo_parent1,
      edu_parent2 = demo_parent2
    ) %>%
    rename_all(str_remove, "language_") %>%
    drop_na(age) %>%
    select(
      starts_with("id"), time, code, study, version,
      time_stamp, date_birth, age, postcode, sex,
      starts_with("edu_"),
      doe_catalan, doe_spanish, doe_others,
      doe_catalan_lockdown, doe_spanish_lockdown, doe_others_lockdown,
      matches("cat_|spa_")
    ) %>%
    pivot_longer(
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
