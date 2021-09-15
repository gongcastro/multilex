#' Authenticate in Google and formr
#' @export ml_connect
#' @importFrom googlesheets4 gs4_has_token
#' @importFrom googlesheets4 gs4_auth
#' @importFrom formr formr_connect
#' @importFrom keyring key_list
#' @importFrom keyring key_get
#' @importFrom keyring key_set
#' @importFrom keyring key_set_with_value
#' @param formr_email E-mail used in formr account
#' @param formr_password Password used in formr account. Do NOT hard-code (include it in a script) this password at any time. We suggest using the keyring R package to store your encrypted password to avoid introducing it every time. Run \code{keyring::key_set(service = "formr", username = "myusername")}, and introduce your password. Your password will be locally store and encrypted. You can retrieve you password running \code{keyring::key_get("formr", "myusername")}. This way, your password will not be hard-coded in your scripts.
#' @param google_email E-mail used in Google Drive account. If NULL (default), it is assumed to be the same as formr_email.
#' @return TRUE if Google and formr authentication was successful, FALSE if authentication of any of the two failed.
ml_connect <- function(
  google_email = NULL,
  formr_email = "gonzalo.garciadecastro@upf.edu",
  formr_password = NULL
){

  # ask for email in console is NULL
  if (is.null(formr_email)) {formr_email <- readline(prompt = "formr email: ")}
  if (is.null(google_email) & !is.null(formr_email)) {
    google_email <- formr_email
  }
  if (is.null(google_email) & !is.null(formr_email)) {
    {google_email <- readline(prompt = "Google email: ")}
    {formr_email <- readline(prompt = "formr email: ")}
  }

  tryCatch(
    {
      # check if formr key exists, set if not
      is_key_formr_missing <- !("formr" %in% key_list()$service)
      if (is_key_formr_missing){
        key_set("formr", formr_email)
      } else if (!is.null(formr_password)) {
        key_set_with_value("formr", formr_email, formr_password)
      } else {
        suppressWarnings(
          formr_connect(
            email = formr_email,
            password = key_get("formr", formr_email),
            host = "https://formr.org/admin/account/login"
          )
        )
      }

      # check if Google credentials exists, ask for them if not
      is_key_google_missing <- gs4_has_token()
      if (is_key_google_missing) gs4_auth(email = google_email)

      # return success code
      return(TRUE)
    },
    # in case log in failed, return failure code
    error = function(err){
      err$message <- paste("Failed to log into formr or Google", err, sep = " ")
      stop(err)
      return(FALSE)
    }
  )
}



