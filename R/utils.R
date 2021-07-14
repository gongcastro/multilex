#' Get timestamps
#' @param data Data frame containing a column for the first time stamp and the last time stamp of participants' resposes the word inventory in each language (Spanish and Catalan)
#' @param cols Character string vector indicating the name of the columns containing the first and the last time stamps (in that order) of participants' responses to a given language inventory.
#' @param which Which time stamp to consider: first (by default) or last?
#' @export get_time_stamp
get_time_stamp <- function(data, cols, which = "first") {
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

# fix variable version
fix_version <- function(x) {
  stringr::str_trim(x)
}

# fix codes
fix_code <- function(x) {
  x %>%
   stringr:: str_trim() %>%
    stringr::str_to_upper() %>%
    stringr::str_remove_all(".*BL") %>%
    stringr::str_replace_all(c(
      "O" ="0",
      "l" = "L",
      "I" = "L",
      "BLBL" = "BL"
    )) %>%
    ifelse(!stringr::str_detect(., "BL"), paste0("BL", .), .)
}

fix_code_raw <- function(x) {
  y <- dplyr::mutate(x, code = dplyr::case_when(
      session=="-OYU0wA9FPQ9-ugKUpyrz1A0usJZIuM5hb-cbV2yMgGBal5S9q3ReRgphBDDxFEY" ~ "BL1674",
      session=="ZZiRT3JN4AdKnXMxjEMtU3CzRkniH0hOSZzS-0kzquRt_Ls9PJzmKsY3qm8tQ7Z2" ~ "BL1671",
      session=="TW8vSEn7YTtbZoe9BaEtRgwNvryWTwSv49dLKb5W0_6bFL306Eiw0Ehg72Q9nqLx" ~ "BL1672",
      session=="DDjiYrPl-WD951rocaSKH9grkn2T4ZJKjhdCAPDzqNBWyhc8E8wwjOY0CcruNn1m" ~ "BL1673",
      session=="c9fPw4Lbm5WS0AbBRppv4NVHh4eETxvEitH8lUC1pkt2ulxxHCvXgAYopCGRQSa_" ~ "BL1569",
      TRUE ~ code
    ))
  return(y)
}

# fix DoE
fix_doe <- function(x) {
  x %>%
    dplyr::mutate(
      doe_catalan = dplyr::case_when(
        id_db=="54469" & time==2 ~ 0,
        id_db=="57157" & time==1 ~ 80,
        id_db=="57046" & time==1 ~ 50,
        TRUE ~ doe_catalan
      ),
      doe_spanish = dplyr::case_when(
        id_db=="57046" & time==1 ~ 50,
        TRUE ~ doe_spanish
      )
    )
}

# fix sex (missing in first responses to BL-Lockdown)
fix_sex <- function(x) {
  dplyr::group_by(x, id) %>%
    dplyr::mutate(sex = dplyr::case_when(
      id %in% c(
        "bilexicon_1097",
        "bilexicon_1441",
        "bilexicon_1124",
        "bilexicon_1448"
      ) ~ "Female",
      id %in% c(
        "bilexicon_1447"
      ) ~ "Male",
      TRUE ~ sex[which(!is.na(sex))[1]])
    ) %>%
    dplyr::ungroup()
}

# fix postcode
fix_postcode <- function(x) {
  dplyr::mutate(x,
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
  dplyr::mutate(
    x,
    item = stringr::str_replace_all(
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
  stringr::str_replace_all(
    x,
    c(
      "\u00e1" = "a",
      "\u00e9" = "e",
      "\u00ed" = "i",
      "\u00fa" = "u",
      "\u00f1" = "n",
      "\u00e7" = "c",
      "\u00e0" = "a",
      "\u00e9" = "e",
      "\u00f2" = "o",
      "\u00f3" = "o",
      "\u00fc" = "u",
      "\u00ef" = "i"
    )
  )
}

# fill missing with previous row
coalesce_by_column <- function(x) {
  return(x[max(which(!is.na(x)))])
}


# evaluate if x is included in y
`%nin%` <- function(x, y) !(x %in% y)

# first non-non-missing value
first_non_na <- function(x) {
  ifelse(
    is.logical(dplyr::first(x[!is.na(x)])),
    NA,
    dplyr::first(x[!is.na(x)])
  )
}

# select age bins flexibly
get_age_bins <- function(x, width = 2){
  min_age <- min(x)
  if (width==1){
    y <- factor(round(x), ordered = TRUE)
  } else {
    y <- ggplot2::cut_width(x, width = width, boundary = 1) %>%
      stringr::str_replace_all(",", "-") %>%
      stringr::str_remove_all(c("\\(|\\)|\\[|\\]")) %>%
      factor(levels =  unique(ggplot2::cut_width(x, width = width, boundary = 1)), ordered = TRUE)
  }
  return(y)
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
  x <- readxl::read_xlsx(file) %>%
    dplyr::mutate_at(dplyr::vars(te), as.integer) %>%
    dplyr::mutate_at(
      dplyr::vars(cognate, include),
      function(x) as.logical(as.integer(x))
    ) %>%
    dplyr::mutate_at(dplyr::vars(version), function(x) strsplit(x, split = ","))

  return(x)
}

# deal with repeated measures
get_longitudinal <- function(x, longitudinal = "all"){

  repeated <- dplyr::distinct(x, id, time) %>%
    dplyr::group_by(id) %>%
    dplyr::filter(dplyr::n()>1) %>%
    dplyr::ungroup()

  if (longitudinal=="no"){
    y <- dplyr::filter(x, id %nin% repeated$id)
  } else if (longitudinal=="first"){
    y <- dplyr::group_by(x, id) %>%
      dplyr::filter(time==min(time, na.rm = TRUE)) %>%
      dplyr::ungroup()
  } else if (longitudinal=="last"){
    y <- dplyr::group_by(x, id) %>%
      dplyr::filter(time==max(time, na.rm = TRUE)) %>%
      dplyr::ungroup()
  } else if (longitudinal=="only") {
    y <- dplyr::filter(x, id %in% repeated$id)
  } else {
    y <- x
  }
  return(y)
}

