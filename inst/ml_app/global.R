library(dplyr)
library(stringr)
library(ggplot2)
library(janitor)
library(lubridate)
library(scales)
library(multilex)
library(googlesheets4)
library(Cairo)
library(tidyr)
library(utils)
library(htmltools)
library(shinyalert)

# prepare data for app
if (!gs4_has_token()){
  ml_connect()
}

options(shiny.usecairo = TRUE)

participants <- ml_participants()
responses <- ml_responses(participants = participants, update = FALSE)

new_codes <- participants %>%
  filter(call=="Successful") %>%
  pull(code)
studies <- c("BiLexicon", "BiLexiconShort", "CognatePriming", "DevLex", "Lockdown", "PhoCross", "phoCross2")
cdi <- c("BL-Lockdown", "BL-Long-2", "CBC", "DevLex", "BL-Long-1", "BL-Short")
version <- c("A", "B", "C", "D")

vocabulary <- ml_vocabulary(participants, responses)
logs <- ml_logs(participants, responses)
norms <- ml_norms(participants, responses) %>%
  mutate(age_num = as.numeric(factor(age_bin, ordered = TRUE)))

norms_processed <- norms %>%
  mutate(age_bin = factor(age_bin, ordered = TRUE)) %>%
  mutate_if(is.double, function(x) round(x*100, 0)) %>%
  mutate(value = paste0(proportion, "% [", ci_lower, "-", ci_upper, "]", " n=", n)) %>%
  select(te, label, language, ipa, age_bin, type, lp, item_dominance, value) %>%
  mutate(label = paste0(label, " /", ipa, "/")) %>%
  pivot_wider(
    names_from = c(type, item_dominance, language),
    values_from = c(value, label)
  ) %>%
  arrange(te, age_bin, lp) %>%
  select(
    te, age_bin, lp,
    label_understands_L1_Catalan,
    value_understands_L1_Catalan, value_understands_L2_Catalan,
    value_produces_L1_Catalan, value_produces_L2_Catalan,
    label_understands_L1_Spanish,
    value_understands_L1_Spanish, value_understands_L2_Spanish,
    value_produces_L1_Spanish, value_produces_L2_Spanish
  )

# utils ------------------------------------------------------------------------
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

