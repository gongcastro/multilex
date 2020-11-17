#### bilexicon_participants: Update participant database #######################

#' Retrieve and update local and/or remote data from formr
#' @param email Google Drive email
#' @return A data frame with all participants that have participated or are candidates to participate in any of the versions of BiLexicon.
#' @examples
#' participants <- bilexicon_participants(email = "user@mail.com")
#'


bilexicon_participants <- function(
  google_email = NULL
) {
  suppressMessages(
    {
      googlesheets4::gs4_auth(email = google_email)
      participants <- googlesheets4::read_sheet("164DMKLRO0Xju0gdfkCS3evAq9ihTgEgFiuJopmqt7mo",
                                                sheet = "Participants") %>%
        drop_na(code) %>%
        select(-version)
      googlesheets4::gs4_deauth()
    }
  )

  assign("participants", participants, envir = .GlobalEnv)
}

