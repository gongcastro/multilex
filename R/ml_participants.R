#### ml_participants: Update participant database ##############################

#' Retrieve and update local and/or remote data from formr
#' @import dplyr
#' @export ml_participants
#' @return A data frame with all participants that have participated or are candidates to participate in any of the versions of MultiLex

ml_participants <- function(
) {
  suppressMessages(
    {
      ml_connect() # get credentials to Google and formr

      participants <- googlesheets4::read_sheet(
        "164DMKLRO0Xju0gdfkCS3evAq9ihTgEgFiuJopmqt7mo",
        sheet = "Participants"
      ) %>%
        tidyr::drop_na(code) %>%
        mutate_at(
          vars(date_birth, date_test, date_sent),
          lubridate::as_date
        ) %>%
        select(-link) %>%
        arrange(desc(as.numeric(gsub("BL", "", code))))
    }
  )
  # make sure no columns are lists (probably due to inconsistent cell types)
  if (any(purrr::map_lgl(participants, is.list))) {
    col <- names(which(purrr::map_lgl(participants, is.list)))
    stop(paste0("Column(s) ", col, " is a list. Check that all its values are of the same format."))
  }
  return(participants)
}

