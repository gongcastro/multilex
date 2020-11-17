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
  formr_email = "gonzalo.garciadecastro@upf.edu", # email (formr and Google Drive)
  google_email = NULL,
  runs = c("formr-2", "formr-lockdown") # c("inhibition", "devlex", "cbc", "formr-short", "formr1", "formr2", "formr-lockdown")
) {

  {
    formr_password <- readline(prompt = "Enter formr password: ")
  }

  #### set up #############################################

  # load packages
  formr::formr_connect(formr_email, password = formr_password) # get credentials
  message("Logged to platforms successfully")

  # define parameters
  items_to_keep <- c("code", "bl_code", "consent_mail", "demo_parent1", "demo_parent2", "demo_postcode", "sex", "language_doe_spanish", "language_doe_spanish_american", "language_doe_catalan_barcelona", "language_doe_catalan_majorca", "language_doe_catalan_other")
  bins <- c("< 10", "10-12", "12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30", "30-32", "32-34", "34-36", "36-38", "38-40", "> 40")
  bins_interest <- c("12-14", "14-16", "16-18", "18-20", "20-22", "22-24", "24-26", "26-28", "28-30")
  breaks <- c(0, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 60)

  #### import data ########################################
  participants <- bilexicon_participants(google_email = google_email)

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

  formr2 <- import_formr2() # formr2
  formr_lockdown <- import_formr_lockdown() # formr-lockdown

  message("Data processed")

  #### merge data ###########################################################
  dat <- list(formr1, formr2, formr_short, formr_lockdown, cbc, inhibition, devlex) %>%
    bind_rows() %>%
    mutate(date_birth = lubridate::as_date(date_birth),
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
    mutate(time = time-min(time)+1,
           sex = sex[which(!is.na(sex))[1]]) %>%
    ungroup()

  message("Data merged")

  return(dat)

}
