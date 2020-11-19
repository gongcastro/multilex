#### ml_participants: Update participant database ##############################

#' Retrieve and update local and/or remote data from formr
#' @param google_email E-mail used in Google Drive account.
#' @return A data frame with all participants that have participated or are candidates to participate in any of the versions of MultiLex
#' @examples
#' participants <- ml_participants(email = "user@mail.com")
#'


ml_participants <- function(
  google_email = NULL
) {
  suppressMessages(
    {
      googlesheets4::gs4_auth(email = google_email)
      participants <- googlesheets4::read_sheet("164DMKLRO0Xju0gdfkCS3evAq9ihTgEgFiuJopmqt7mo",
                                                sheet = "Participants") %>%
        drop_na(code) %>%
        select(-c(version, link))
      googlesheets4::gs4_deauth()
    }
  )
  # make sure no columns are lists (probably due to inconsistent cell types)
  if (any(purrr::map_lgl(participants, is.list))) {
    col <- names(which(purrr::map_lgl(participants, is.list)))
    stop(paste0("Column(s) ", col, " is a list. Check that all its values are of the same format."))
  }
  return(participants)
}

