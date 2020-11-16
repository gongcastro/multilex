#### bilexicon_update: Update the project ######################################

#' Retrieve and update local and/or remote data from formr
#'
#' @param email E-mail used in formr account.
#' @param bilingual_threshold Minimum degree of exposure to consider a participant as *monolingual*.
#' @param runs formr runs to update
#' @return A list with 3 elements: \code{responses} contains the each participant's response to each item, along with some session-specific metadata. \code{participants} contains participant-level data, such as demographic and linguitic information, and some metadata about their session. \code{summary} summarises the demographic and linguistic characteristics of the sample.
#' @examples
#' d <- bilexicon_update(email = "user@mail.com", bilingual_threshold = 95, runs = c("formr-short", "formr-2"))

bilexicon_update <- function(
  email = "gonzalo.garciadecastro@upf.edu", # email (formr and Google Drive)
  bilingual_threshold = 95,
  runs = c("formr-2", "formr-lockdown") # c("inhibition", "devlex", "cbc", "formr-short", "formr1", "formr2", "formr-lockdown")
) {

  {
    formr_password <- readline(prompt = "Enter formr password: ")
  }

  #### set up #############################################

  # load packages
  formr::formr_connect(email, password = formr_password) # get credentials
  googlesheets4::gs4_auth(email = email)
  message("Logged to platforms successfully")

  # define parameters
  items_to_keep <- c("code", "bl_code", "consent_mail", "demo_parent1", "demo_parent2", "demo_postcode", "sex", "language_doe_spanish", "language_doe_spanish_american", "language_doe_catalan_barcelona", "language_doe_catalan_majorca", "language_doe_catalan_other")
  bins <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
  bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30")
  breaks <- c(0, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 60)

  #### import data ########################################
  participants <- suppressMessages(googlesheets4::range_read("164DMKLRO0Xju0gdfkCS3evAq9ihTgEgFiuJopmqt7mo", sheet = "Participants")) %>%
    drop_na(code) %>%
    select(-version)

  total_items <- studies %>%
    distinct(version, language, n) %>%
    group_by(version) %>%
    summarise(total_items = sum(n),
              .groups = "drop")

  # make sure no columns are lists (probably due to inconsistent cell types)
  if (any(purrr::map_lgl(participants, is.list))) {
    col <- names(which(purrr::map_lgl(participants, is.list)))
    stop(paste0("Column(s) ", col, " is a list. Check that all its values are of the same format."))
  }

  # formr2
  formr2 <- import_formr2()

  # formr-lockdown
  formr_lockdown <- import_formr_lockdown()

  message("Data processed")

  #### merge data ###########################################################
  dat <- list(formr1, formr2, formr_short, formr_lockdown, cbc, inhibition, devlex) %>%
    bind_rows() %>%
    mutate(lp = case_when((doe_spanish >= bilingual_threshold | doe_catalan >= bilingual_threshold)  ~ "Monolingual",
                          between(doe_catalan, 100-bilingual_threshold, bilingual_threshold) &
                            between(doe_spanish, 100-bilingual_threshold, bilingual_threshold) ~ "Bilingual",
                          TRUE ~ "Other"),
           date_birth = lubridate::as_date(date_birth),
           time_stamp = lubridate::as_date(time_stamp),
           version = case_when(study %in% "DevLex" ~ "DevLex",
                               study %in% c("CBC", "Inhibition", "Signs", "Negation", "Inhibition") ~ "CBC",
                               TRUE ~ version),
           age_bin = cut(age, breaks = breaks, labels = bins, ordered_result = TRUE) %>%
             factor(levels = bins, ordered = TRUE),
           postcode = ifelse(nchar(postcode) < 5, paste0("0", postcode), postcode),
           postcode = ifelse(nchar(postcode) < 5, NA_character_, postcode),
           time = ifelse(is.na(time), 1, time)) %>%
    #distinct(id, id_db, time, language, item, .keep_all = TRUE) %>%
    drop_na(time_stamp) %>%
    group_by(id) %>%
    mutate(sex = sex[which(!is.na(sex))[1]]) %>%
    ungroup()

  message("Data merged")

  #### logs ############################
  logs <- dat %>%
    group_by(id_db, date_birth, time, age, age_bin, sex, postcode, edu_parent1, edu_parent2, lp, doe_spanish, doe_catalan, time_stamp, code, study, version) %>%
    summarise(complete_items = sum(!is.na(response)), .groups = "drop") %>%
    left_join(total_items, by = c("version")) %>%
    left_join(select(participants, -date_birth), by = c("id_db", "time", "code", "study")) %>%
    mutate_at(vars(time_stamp), lubridate::as_datetime) %>%
    rowwise() %>%
    mutate(progress = 100*complete_items/total_items) %>%
    ungroup() %>%
    mutate(completed = progress >= 95,
           date_sent = lubridate::as_date(date_sent),
           time_stamp = lubridate::as_date(time_stamp),
           days_from_sent = as.numeric((time_stamp-date_sent), units = "days"),
           age_today = as.numeric((lubridate::today()-lubridate::as_date(date_birth)))/30,
           months_from_last_response = as.numeric(lubridate::today()-time_stamp)/30) %>%
    select(id, id_db, time, first_contact, date_sent, days_from_sent, time_stamp, date_birth, age, age_bin, sex, postcode, edu_parent1, edu_parent2, lp, doe_spanish, doe_catalan, code, study, version, progress, completed, age_today, months_from_last_response) %>%
    arrange(time_stamp) %>%
    #mutate(completed = ifelse(study %!in% c("BiLexiconShort", "Lockdown"), TRUE, completed)) %>%
    group_by(id, id_db) %>%
    mutate(time = time-min(time)+1) %>%
    ungroup()

  dat <- dat %>%
    group_by(id_db) %>%
    mutate(time = time-min(time)+1) %>%
    ungroup()

  # summarise logs by age and language profile
  logs_summary <- logs %>%
    filter(completed) %>%
    group_by(age_bin, lp) %>%
    summarise(n = n(), id_db = list(id_db), .groups = "drop") %>%
    ungroup() %>%
    full_join(expand(., age_bin, lp), by = c("age_bin", "lp")) %>%
    rowwise() %>%
    mutate(id_db = paste0(unlist(id_db), collapse = ", "),
           n = ifelse(is.na(n), 0, n)) %>%
    ungroup() %>%
    filter(age_bin %in% bins_interest,
           lp %in% c("Monolingual", "Bilingual")) %>%
    janitor::clean_names()

  message("Logs generated")

  #### save objects ###################################
  bl_dat <- list(responses = dat,
                 participants = logs,
                 summary = logs_summary)
  return(bl_dat)

}
