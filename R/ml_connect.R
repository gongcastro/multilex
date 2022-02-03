#' Authenticate in Google and formr
#' @export ml_connect
#' @importFrom googlesheets4 gs4_has_token
#' @importFrom googlesheets4 gs4_auth
#' @importFrom formr formr_connect
#' @importFrom keyring key_list
#' @importFrom keyring key_get
#' @importFrom keyring key_set
#' @importFrom keyring key_set_with_value
#' @param google_email E-mail used in Google Drive account. If NULL (default), it is assumed to be the same as admin email (gonzalo.garciadecastro@upf.edu)
#' @return TRUE if Google and formr authentication was successful, FALSE if authentication of any of the two failed.
#' @examples
#' my_email <- "gonzalo.garciadecastro@upf.edu"
#' ml_connect(google_email = my_email)
ml_connect <- function(
  google_email = NULL
){

  formr_email <- "gonzalo.garciadecastro@upf.edu"

  # wrapper function to establish connection with formr
  connect_fn <- function(){
    # try to log in and catch errors
    out <- tryCatch(
      {
        suppressWarnings({
          formr_connect(
            email = formr_email,
            password = key_get("multilex", formr_email),
            host = "https://formr.org/"
          )
        })
      },
      error = function(c){
        if (grepl("Incorrect credentials.", c)){
          msg <- "Wrong password. Try deleting your current password using keyring::key_delete() and running ml_connect again to reset your password."
        }
        if (grepl("Could not login for unknown reason.", c)){
          msg <- "Could not login for unknown reason. The server might be down. Try connecting later."
        }
        stop(msg)
      },
      finally = 'You can reveal the stored pasword by running keyring::key_get("multilex", "your@email.com") in your console.\nNever hard-code your password in your files."'
    )
    return(out)
  }

  # ask for email in console is everything is NULL
  if (is.null(google_email)) google_email <- formr_email
  if (!grepl("@", google_email)) stop("Email must be valid")

  # locate or set key using keyring
  is_key_formr_missing <- !("multilex" %in% key_list()$service)
  if (is_key_formr_missing) key_set("multilex", formr_email)
  connect_fn()

  # check if Google credentials exists, ask for them if not
  is_key_google <- gs4_has_token()
  if (!is_key_google) gs4_auth(email = google_email)

  # return success code
  is_connected_google <- gs4_has_token()
  if (is_connected_google) message("You are now connected!")
  return(is_connected_google)
}



