#' Authenticate in Google and formr
#' @export ml_connect
#' @importFrom googlesheets4 gs4_has_token
#' @importFrom googlesheets4 gs4_auth
#' @importFrom formr formr_connect
#' @param google_email E-mail used in Google Drive account.
#' @param formr_email E-mail used in formr account.
#' @param formr_password Password used in formr account. Do NOT hard-code (include it in a script) this password at any time. We suggest using the keyring R package to store your encrypted password to avoid introducing it every time. Run \code{keyring::key_set(service = "formr", username = "myusername")}, and introduce your password. Your password will be locally store and encrypted. You can retrieve you password running \code{keyring::key_get("formr", "myusername")}. This way, your password will not be hard-coded in your scripts.
#' @return Authenticate Google and formr accounts to access the database.
#'

ml_connect <- function(
  google_email = NULL,
  formr_email = "gonzalo.garciadecastro@upf.edu",
  formr_password = NULL
){

  if (!gs4_has_token()){
    if (is.null(google_email)){
      {google_email <- readline(prompt = "Enter Google email: ")}
    }
    if (is.null(formr_email)){
      {formr_email <- readline(prompt = "Enter formr email: ")}
    }
    if (is.null(formr_password)){
      {formr_password <- readline(prompt = "Enter formr password: ")}
    }

    # connect
    formr_connect(
      email = formr_email,
      password = formr_password
    )
    gs4_auth(email = google_email)

  }
}



