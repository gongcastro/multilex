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
#' @importFrom dplyr distinct
#' @importFrom readxl read_xlsx
#' @importFrom stats frequency
#' @importFrom stats time
#' @importFrom stats frequency
#' @importFrom stats qnorm

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

# fix variable version
fix_version <- function(x) {
  str_trim(x)
}

# fix codes
fix_code <- function(x) {
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

fix_code_raw <- function(x) {
  y <- x %>%
    mutate(code = case_when(
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
    mutate(
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
      "\U00E1" = "a",
      "\U00E9" = "e",
      "\U00ED" = "i",
      "\U00FA" = "u",
      "\U00F1" = "n",
      "\U00E7" = "c",
      "\U00E0" = "a",
      "\U00E9" = "e",
      "\U00F2" = "o",
      "\U00F3" = "o",
      "\U00FC" = "u",
      "\U00EF" = "i"
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
    is.logical(first(x[!is.na(x)])),
    NA,
    first(x[!is.na(x)])
  )
}

# select age bins flexibly
get_age_bins <- function(x, width = 2){
  min_age <- min(x)
  if (width==1){
    y <- round(x) %>%
      factor(., ordered = TRUE)
  } else {
    y <- cut_width(x, width = width, boundary = 1) %>%
      str_replace_all(",", "-") %>%
      str_remove_all(c("\\(|\\)|\\[|\\]")) %>%
      factor(levels =  unique(cut_width(x, width = width, boundary = 1)), ordered = TRUE)
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
  x <- read_xlsx(file) %>%
    mutate_at(vars(te), as.integer) %>%
    mutate_at(
      vars(cognate, include),
      function(x) as.logical(as.integer(x))
    ) %>%
    mutate_at(vars(version), function(x) strsplit(x, split = ","))

  return(x)
}

# deal with repeated measures
get_longitudinal <- function(x, longitudinal = "all"){

  repeated <- distinct(x, id, time) %>%
    group_by(id) %>%
    filter(n()>1) %>%
    ungroup()

  if (longitudinal=="no"){
    y <- x %>%
      filter(id %!in% repeated$id)
  } else if (longitudinal=="first"){
    y <- x %>%
      group_by(id) %>%
      filter(time==min(time, na.rm = TRUE)) %>%
      ungroup()
  } else if (longitudinal=="last"){
    y <- x %>%
      group_by(id) %>%
      filter(time==max(time, na.rm = TRUE)) %>%
      ungroup()
  } else if (longitudinal=="only") {
    y <- x %>%
      filter(id %in% repeated$id)
  } else {
    y <- x
  }
  return(y)
}

