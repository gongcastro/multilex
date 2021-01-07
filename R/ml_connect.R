#' Authenticate in Google and formr
#' @importFrom formr formr_connect
#' @importFrom googlesheets4 gs4_auth
#' @export ml_connect
#' @param google_email E-mail used in Google Drive account.
#' @param formr_email E-mail used in formr account.
#' @return Authenticate Google and formr accounts to access the database.
#' @examples
#' ml_connect(google_email = "user@mail.com", formr_email = "user@email.com")
#'

ml_connect <- function(
  google_email = NULL,
  formr_email = "gonzalo.garciadecastro@upf.edu"
){

  if (!gs4_has_token()){
    if (is.null(google_email)){
      {google_email <- readline(prompt = "Enter Google email: ")}
    }
    if (is.null(formr_email)){
      {formr_email <- readline(prompt = "Enter formr email: ")}
    }
    {formr_password <- readline(prompt = "Enter formr password: ")}
    formr_connect(
      email = formr_email,
      password = formr_password
    )
    gs4_auth(email = google_email)

  }
}



