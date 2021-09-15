#### ml_participants: Update participant database ##############################

#' Retrieve and update local and/or remote data from formr
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom lubridate as_date
#' @importFrom googlesheets4 read_sheet
#' @importFrom purrr map_lgl
#' @importFrom rlang .data
#' @export ml_participants
#' @return A data frame with all participants that have participated or are candidates to participate in any of the versions of MultiLex

ml_participants <- function(
) {
  suppressMessages(
    {
      ml_connect() # get credentials to Google and formr

      participants <- read_sheet(
        "164DMKLRO0Xju0gdfkCS3evAq9ihTgEgFiuJopmqt7mo",
        sheet = "Participants"
      ) %>%
        drop_na(.data$code) %>%
        mutate_at(vars(.data$date_birth, .data$date_test, .data$date_sent), as_date) %>%
        select(-.data$link) %>%
        arrange(desc(as.numeric(gsub("BL", "", .data$code))))
    }
  )
  # make sure no columns are lists (probably due to inconsistent cell types)
  if (any(map_lgl(participants, is.list))) {
    col <- names(which(map_lgl(participants, is.list)))
    stop(paste0("Column(s) ", col, " is a list. Check that all its values are of the same format."))
  }
  return(participants)
}

