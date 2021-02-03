#### ml_participants: Update participant database ##############################

#' Retrieve and update local and/or remote data from formr
#' @importFrom googlesheets4 read_sheet
#' @importFrom googlesheets4 gs4_has_token
#' @importFrom purrr map_lgl
#' @importFrom lubridate as_date
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr mutate_at
#' @importFrom dplyr vars
#' @importFrom tidyr drop_na
#' @export ml_participants
#' @return A data frame with all participants that have participated or are candidates to participate in any of the versions of MultiLex
#' @examples
#' participants <- ml_participants()
#'


ml_participants <- function(
) {
  suppressMessages(
    {
      ml_connect()

      participants <- read_sheet(
        "164DMKLRO0Xju0gdfkCS3evAq9ihTgEgFiuJopmqt7mo",
        sheet = "Participants"
      ) %>%
        drop_na(code) %>%
        mutate_at(
          vars(date_birth, date_test, date_sent),
          as_date
        ) %>%
        select(-link) %>%
        arrange(desc(as.numeric(gsub("BL", "", code))))
    }
  )
  # make sure no columns are lists (probably due to inconsistent cell types)
  if (any(map_lgl(participants, is.list))) {
    col <- names(which(map_lgl(participants, is.list)))
    stop(paste0("Column(s) ", col, " is a list. Check that all its values are of the same format."))
  }
  return(participants)
}

