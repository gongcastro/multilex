#' Helper functions
#'
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_detect
#' @importFrom stringr str_trim
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom readxl read_xlsx
#'

# get timestamps
get_time_stamp <- function(data, cols, which) {
  d <- data[c(cols[1], cols[2])]
  if (which %in% "first") {
    x <- apply(d, 1, min, na.rm = TRUE)
  } else if (which %in% "last") {
    x <- apply(d, 1, max, na.rm = TRUE)
  }
  suppressMessages(return(x))
}

# summarise language profile
get_doe <- function(data, languages = languages) {
  apply(data[paste0("language_doe_", languages)], 1, sum, na.rm = TRUE)
}

# fix codes
fix_code <- function(x)
{
  x %>%
    str_trim() %>%
    str_to_upper() %>%
    str_remove_all(".*BL") %>%
    str_replace_all(c(
      "O" ="0",
      "l" = "L",
      "I" = "L",
      "BLBL" = "BL"
    )) %>%
    ifelse(!str_detect(., "BL"), paste0("BL", .), .)
}

# fix DoE
fix_doe <- function(x) {
  mutate(x,
         doe_catalan = case_when(
           id_db=="54469" & time==2 ~ 0,
           id_db=="57157" & time==1 ~ 80,
           id_db=="57046" & time==1 ~ 50,
           TRUE ~ doe_catalan
         ),
         doe_spanish = case_when(
           id_db=="57046" & time==1 ~ 50,
           TRUE ~ doe_spanish
         )
  )
}

# fix sex (missing in first responses to BL-Lockdown)
fix_sex <- function(x) {
  group_by(x, id) %>%
    mutate(sex = case_when(
      id %in% c("bilexicon_1097",
                "bilexicon_1441",
                "bilexicon_1124",
                "bilexicon_1448") ~ "Female",
      id %in% c("bilexicon_1447") ~ "Male",
      TRUE ~ sex[which(!is.na(sex))[1]])
    ) %>%
    ungroup()
}

# fix postcode
fix_postcode <- function(x) {
  mutate(
    x,
    postcode = ifelse(
      nchar(postcode) < 5,
      paste0("0", postcode),
      postcode
    ),
    postcode = ifelse(
      nchar(postcode) < 5,
      NA_character_,
      postcode
    )
  )
}

# fix item
fix_item <- function(x) {
  mutate(
    x,
    item = str_replace_all(
      item,
      c(
        "cat_parc" = "cat_parc1",
        "cat_eciam" = "cat_enciam",
        "cat_voler" = "cat_voler1",
        "cat_voler3" = "cat_voler2",
        "cat_despres1" = "cat_despres",
        "cat_peix" = "cat_peix1",
        "cat_estar" = "cat_estar1",
        "cat_querer" = "cat_querer1",
        "cat_estiguestequiet" = "cat_estiguesquiet",
        "spa_nibla" = "spa_niebla",
        "spa_ir" = "spa_ir1",
        "spa_querer" = "spa_querer1"
      )
    )
  )
}

# replace special characters
replace_special_characters <- function(x) {
  str_replace_all(
    x,
    c(
      "á" = "a",
      "é" = "e",
      "í" = "i",
      "é" = "e",
      "ú" = "u",
      "ñ" = "n",
      "ç" = "c",
      "à" = "a",
      "è" = "e",
      "ò" = "o",
      "ó" = "o",
      "ü" = "u",
      "ï" = "i"
    )
  )
}

# fill missing with previous row
coalesce_by_column <- function(x) {
  return(x[max(which(!is.na(x)))])
}


# evaluate if x is included in y
"%!in%" <- function(x, y) !(x %in% y)

# first non-non-missing value
first_non_na <- function(x) {
  ifelse(
    is.logical(first(x[!is.na(x)])),
    NA,
    first(x[!is.na(x)])
  )
}

# proportion adjusted from boundary values (Gelman, Hill & Vehtari, 2020)
prop_adj <- function(x, n){
  e <- (x+2)/(n+4)
  return(e)
}

# adjusted standard error of proportion (Gelman, Hill & Vehtari, 2020)
prop_adj_se <- function(x, n) {
  e <- (x+2)/(n+4)
  se <- sqrt(e*(1-e)/(n+4))
  return(se)
}

# adjusted standard error of proportion (Gelman, Hill & Vehtari, 2020)
prop_adj_ci <- function(x, n, .width = 0.95) {
  e <- (x+2)/(n+4)
  se <- sqrt(e*(1-e)/(n+4))
  ci <-  e + qnorm(c((1-.width)/2, (1-(1-.width)/2)))*se
  ci[1] <- ifelse(ci[1]<0, 0, ci[1]) # truncate at 0
  ci[2] <- ifelse(ci[2]>1, 1, ci[2]) # truncate at 1
  return(ci)
}

# confidence interval of proportion (Gelman, Hill & Vehtari, 2020)
proportion_se <- function(x, n) {
  sqrt(x*(1-x)/n)
}

# import pool
import_pool <- function(
  file = "pool.xlsx"
){
  x <- read_xlsx(file) %>%
    mutate_at(vars(te), as.integer) %>%
    mutate_at(
      vars(cognate, include),
      function(x) as.logical(as.integer(x))
    ) %>%
    mutate_at(vars(version), function(x) strsplit(x, split = ","))

  return(x)
}
