#### ml_participants: Update participant database ##############################

#' Retrieve and update local and/or remote data from formr
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom lubridate as_date
#' @importFrom googlesheets4 read_sheet
#' @importFrom purrr map_lgl
#' @importFrom rlang .data
#' @export ml_participants
#' @details This function generates a data frame with the information of all
#'  participants that have participated or are candidates to participate in any of
#'  the versions of MultiLex.
#' @return A data frame (actually, a \code{\link[tibble]{tibble}}) with all participants that have participated or are candidates to participate in any of the versions of MultiLex. Each row corresponds to a questionnaire response and each column represents a variable. The output includes the following variables:
#'  \describe{
#'      \item{id}{a character string indicating a participant's identifier. This value is always the same for each participant, so that different responses from the same participant share the same \code{id}.}
#'      \item{id_exp}{a character string indicating a participant's identifier in the context of the particular study in which the participant was tested and invited to fill in the questionnaire. This value is always the same for each participant within the same study, so that different responses from the same participant in the same study share \code{id_exp} The same participant may have different \code{id_exp} across different studies.}
#'      \item{id_db}{a character string with five digits indicating a participant's identifier in the database from the \href{https://www.upf.edu/web/cbclab}{Laboratori de Recerca en Infància} at Universitat Pompeu Fabra. This value is always the same for each participant, so that different responses from the same participant share the same \code{id_db}.}
#'      \item{code}{a character string identifying a single response to the questionnaire. This value is always unique for each response to the questionnaire, even for responses from the same participant.}
#'      \item{time}{a numeric value indicating how many times a given participant has been sent the questionnaire, regardless of whether they completed it or not.}
#'      \item{date_birth}{a date value (see lubridate package) in \code{yyyy/mm/dd} format indicating participants birth date.}
#'      \item{age_now}{a numeric value indicating the number of months elapsed since participants' birth date until the present day, as indicated by \code{\link[lubridate]{now}.}}
#'      \item{study}{a character string indicating the study in which the participant was invited to fill in the questionnaire. Frequently, participants that filled in the questionnaire came to the lab to participant in a study, and were then invited to fill in the questionnaire later. This value indicates what study each participant was tested in before being sent the questionnaire.}
#'      \item{version}{a character string indicating what version of the questionnaire a given participant filled in. Different versions may contain a different subset of items, and the administration instructions might vary slightly (see formr questionnaire templates in the \href{https://github.com/gongcastro/multilex}{GitHub repository}). Also, different versions were designed, implemented, and administrated at different time points (e.g., before/during/after the COVID-related lockdown).}
#'      \item{randomisation}{a character string indicating the specific list of items a participant was assigned to. Only applies in the case of short versions of multilex, such as BL-Short, BL-Short-2 or BL-Lockdown, where the list of items was partinioned into several versions.}
#'      \item{date_test}{a date value (see lubridate package) in \code{yyyy/mm/dd} format indicating the date in which the participant was tested in the associated study, if any}
#'      \item{date_sent}{a date value (see lubridate package) in \code{yyyy/mm/dd} format indicating the date in which the participant was sent the questionnaire}
#'      \item{call}{a character string indicating the status of the participant's response: \emph{successful} (participant completed the questionnaire), \emph{sent} (participant has been sent the email but has not completed it yet), \emph{pending} (participant is still to be sent the questionnaire, \emph{reminded} (a week has elapsed since the participant was sent the questionnaire, and has been already reminded of it), or \emph{stop} (participant has not completed the questionnaire after two weeks since they were sent the questionnaire).}
#'      \item{comments}{a character string indicating useful information for database management}
#' }
#' @author Gonzalo Garcia-Castro

ml_participants <- function() {
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

