#' Get timestamps
#' @param .data Data frame containing a column for the first time stamp and the last time stamp of participants' resposes the word inventory in each language (Spanish and Catalan)
#' @param cols Character string vector indicating the name of the columns containing the first and the last time stamps (in that order) of participants' responses to a given language inventory.
#' @param which Which time stamp to consider: first (by default) or last?
#' @importFrom lubridate as_datetime
#' @importFrom rlang .data
#' @export get_time_stamp
get_time_stamp <- function(.data, cols, which = "first") {
  d <- .data[c(cols[1], cols[2])]
  if (which %in% "first") {
    x <- apply(d, 1, min, na.rm = TRUE)
  } else if (which %in% "last") {
    x <- apply(d, 1, max, na.rm = TRUE)
  }
  x <- as_datetime(x)
  suppressMessages(return(x))
}

#' Summarise language profile
#' @importFrom rlang .data
#' @param .data Data frame that contains each degree of exposure as columns, named \code{language_doe_*}
#' @param languages Character vector of languages to compute degree of exposure for (all others will be considered as doe_others)
get_doe <- function(.data, languages) {
  apply(.data[paste0("language_doe_", languages)], 1, sum, na.rm = TRUE)
}

#' Fix variable version
#' @param x Vector of \code{version} whose values should be fixed
fix_version <- function(x) {
  trimws(x)
}

#' Fix codes
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @param x Vector of \code{code} whose values should be fixed
fix_code <- function(x) {
  x <- toupper(trimws(x))
  x <- x %>%
    str_remove_all(".*BL") %>%
    str_replace_all(
      c(
        "O" ="0",
        "l" = "L",
        "I" = "L",
        "BLBL" = "BL"
      )
    )
  x <- ifelse(!grepl("BL", x), paste0("BL", x), x)
  return(x)
}


#' Fix raw codes
#' @param x Vector of \code{code} whose values should be fixed, based on \code{session}
fix_code_raw <- function(x) {
  x[x$session=="-OYU0wA9FPQ9-ugKUpyrz1A0usJZIuM5hb-cbV2yMgGBal5S9q3ReRgphBDDxFEY", "code"] <- "BL1674"
  x[x$session=="ZZiRT3JN4AdKnXMxjEMtU3CzRkniH0hOSZzS-0kzquRt_Ls9PJzmKsY3qm8tQ7Z2", "code"] <- "BL1671"
  x[x$session=="TW8vSEn7YTtbZoe9BaEtRgwNvryWTwSv49dLKb5W0_6bFL306Eiw0Ehg72Q9nqLx", "code"] <- "BL1672"
  x[x$session=="DDjiYrPl-WD951rocaSKH9grkn2T4ZJKjhdCAPDzqNBWyhc8E8wwjOY0CcruNn1m", "code"] <- "BL1673"
  x[x$session=="c9fPw4Lbm5WS0AbBRppv4NVHh4eETxvEitH8lUC1pkt2ulxxHCvXgAYopCGRQSa_", "code"] <- "BL1569"
  return(x)
}

#' Fix DOEs
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @param x Vector of \code{doe} whose values should be fixed
fix_doe <- function(x) {
  x %>%
    mutate(
      doe_catalan = case_when(
        id_db=="54469" & time==2 ~ 0,
        id_db=="57157" & time==1 ~ 80,
        id_db=="57046" & time==1 ~ 50,
        code=="BL1582" ~ 30,
        code=="BL1295" ~ 10,
        TRUE ~ doe_catalan
      ),
      doe_spanish = case_when(
        id_db=="57046" & time==1 ~ 50,
        code=="BL896" ~ 75,
        TRUE ~ doe_spanish
      ),
      doe_others = case_when(
        code=="BL1252" ~ 10,
        code=="BL1208" ~ 0,
        code=="BL896" ~ 0,
        code=="BL1582" ~ 0,
        code=="BL1295" ~ 0,
        TRUE ~ doe_others
      )
    )
}

#' Fix sex (missing in first responses to BL-Lockdown)
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr ungroup
#' @param x Vector of \code{sex} whose values should be fixed
fix_sex <- function(x) {
  group_by(x, id) %>%
    mutate(
      sex = case_when(
        id %in% c("bilexicon_1097", "bilexicon_1441", "bilexicon_1124", "bilexicon_1448") ~ "Female",
        id %in% c("bilexicon_1447") ~ "Male",
        TRUE ~ sex[which(!is.na(sex))[1]]
      )
    ) %>%
    ungroup()
}

#' Fix postcode
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom rlang .data
#' @param x Vector of \code{postcode} whose values should be fixed
fix_postcode <- function(x) {
  mutate(
    x,
    postcode = ifelse(nchar(.data$postcode) < 5, paste0("0", .data$postcode), .data$postcode),
    postcode = ifelse(nchar(.data$postcode) < 5, NA_character_, .data$postcode)
  )
}

#' Fix item
#' @importFrom stringr str_replace_all
#' @importFrom rlang .data
#' @param x Vector of \code{item} whose values should be fixed
fix_item <- function(x) {
  mutate(
    x,
    item = str_replace_all(
      .data$item,
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

#' Fix study
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @param x Vector of \code{study} whose values should be fixed
fix_study <- function(x) {
  mutate(
    x,
    study = ifelse(
      is.na(.data$study),
      "BiLexicon",
      .data$study
    )
  )
}

#' Fix id_exp
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @param x Vector of \code{id_exp} whose values should be fixed
fix_id_exp <- function(x) {
  mutate(
    x,
    id_exp = case_when(
      code=="BL547" ~ "bilexicon_189",
      TRUE ~ id_exp
    )
  )
}

#' Replace special characters
#' @importFrom stringr str_replace_all
#' @importFrom rlang chr_unserialise_unicode
#' @param x Character vector whose strings will be transformed
replace_special_characters <- function(x) {
  str_replace_all(
    chr_unserialise_unicode(x),
    c(
      "<U+00E1>" = "a",
      "<U+00E9>" = "e",
      "<U+00ED>" = "i",
      "<U+00FA>" = "u",
      "<U+00F1>" = "n",
      "<U+00E7>" = "c",
      "<U+00E0>" = "a",
      "<U+00E9>" = "e",
      "<U+00F2>" = "o",
      "<U+00F3>" = "o",
      "<U+00FC>" = "u",
      "<U+00EF>" = "i"
    )
  )
}

#' Fill missing with previous row
#' @param x Vector whose missing values will be filled with parallel non-missing values
coalesce_by_column <- function(x) {
  return(x[max(which(!is.na(x)))])
}


#' Evaluate if x is included in y
#' @export `%nin%`
#' @param x Vector whose elements will be evaluated
#' @param y Vector in which elements of \code{x} will be evaluated
`%nin%` <- function(x, y) !(x %in% y)

#' First non-non-missing value
#' @importFrom dplyr first
#' @param x Vector whose NAs will be replaced with first non-NA value
first_non_na <- function(x) {
  ifelse(is.logical(first(x[!is.na(x)])), NA, first(x[!is.na(x)]))
}

#' Select age bins flexibly
#' @importFrom ggplot2 cut_width
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove_all
#' @param x Vector of ages (in months)
#' @param width Width of the age bins (in months, defaults to 2)
get_age_bins <- function(x, width = 2){
  min_age <- min(x)
  if (width==1){
    y <- factor(round(x), ordered = TRUE)
  } else {
    y <- cut_width(x, width = width, boundary = 1) %>%
      str_replace_all(",", "-") %>%
      str_remove_all(c("\\(|\\)|\\[|\\]")) %>%
      factor(levels = unique(cut_width(x, width = width, boundary = 1)), ordered = TRUE)
  }
  return(y)
}


#' Adjusted proportion (Gelman, Hill & Vehtari, 2020)
#' @param x Number of successes
#' @param n Number of tries
prop_adj <- function(x, n){
  e <- (x+2)/(n+4)
  return(e)
}

#' Adjusted standard error of proportion (Gelman, Hill & Vehtari, 2020)
#' @param x Number of successes
#' @param n Number of tries
prop_adj_se <- function(x, n) {
  e <- (x+2)/(n+4)
  se <- sqrt(e*(1-e)/(n+4))
  return(se)
}

#' Adjusted confidence interval of proportion (Gelman, Hill & Vehtari, 2020)
#' @importFrom stats qnorm
#' @param x Number of successes
#' @param n Number of tries
#' @param .width Confidence level (defaults to .95)
prop_adj_ci <- function(x, n, .width = 0.95) {
  e <- (x+2)/(n+4)
  se <- sqrt(e*(1-e)/(n+4))
  ci <-  e + qnorm(c((1-.width)/2, (1-(1-.width)/2)))*se
  ci[1] <- ifelse(ci[1]<0, 0, ci[1]) # truncate at 0
  ci[2] <- ifelse(ci[2]>1, 1, ci[2]) # truncate at 1
  return(ci)
}


import_pool <- function(
  file = system.file("extdata", "pool.xlsx", package = "multilex")
){
  x <- readxl::read_xlsx(file) %>%
    dplyr::mutate_at(vars(.data$te), as.integer) %>%
    dplyr::mutate_at(
      dplyr::vars(.data$cognate, .data$include),
      function(x) as.logical(as.integer(x))
    ) %>%
    dplyr::mutate_at(dplyr::vars(.data$version), function(x) strsplit(x, split = ",")) %>%
    mutate(
      ipa_flat = gsub(
        pool$ipa %>%
          paste(collapse = "") %>%
          strsplit("") %>%
          unlist() %>%
          unique() %>%
          .data[c(3, 6, 37, 39, 44, 50)] %>%
          paste0("\\", .data, collapse = "|"),
        "",
        .data$ipa
      )) %>%
    dplyr::relocate(.data$ipa_flat, .after = .data$ipa)
  return(x)
}

#' Deal with repeated measures
#' @export get_longitudinal
#' @param x A data frame containing a column for participants (each participant gets a unique ID), and a column for times (a numeric value indicating how many times each participant appears in the data frame counting this one). One participant may appear several times in the data frame, with each time with a unique value of \code{time}.
#' @param longitudinal A character string indicating what subset of the participants should be returned: "all" (defult) returns all participants, "no" remove all participants with more than one response, only" returns only participants with more than one response in the dataset (i.e., longitudinal participants), "first" returns the first response of each participant (participants with only one appearance are included), and "last" returns the last response from each participant (participants with only one response are included).
#' @importFrom dplyr group_by
#' @importFrom dplyr distinct
#' @importFrom dplyr n
#' @importFrom dplyr filter
#' @importFrom dplyr ungroup
#' @return A subset of the data frame \code{x} with only the selected cases, according to \code{longitudinal}.
get_longitudinal <- function(x, longitudinal = "all"){
  repeated <- distinct(x, id, time) %>%
    group_by(id) %>%
    filter(n()>1) %>%
    ungroup()

  if (longitudinal=="no"){
    y <- filter(x, id %nin% repeated$id)
  } else if (longitudinal=="first"){
    y <- group_by(x, id) %>%
      filter(time==min(time, na.rm = TRUE)) %>%
      ungroup()
  } else if (longitudinal=="last"){
    y <- group_by(x, id) %>%
      filter(time==max(time, na.rm = TRUE)) %>%
      ungroup()
  } else if (longitudinal=="only") {
    y <- filter(x, id %in% repeated$id)
  } else {
    y <- x
  }
  return(y)
}


#' Extract lexical frequencies from CHILDES
#' @export get_childes_frequency
#' @param token Character string vector indicating the words-forms to look up
#' @param languages Languages in which to look up the word forms in ISO code (see ISOcodes::ISO_639_2). Default are c("cat", and "spa"), Catalan and Spanish.
#' @param ... Additional arguments passed to the \code{childesr::get_speaker_statistics} function. Use it to refine the search.
#' @importFrom childesr get_speaker_statistics
#' @importFrom childesr get_tokens
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr count
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr starts_with
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @return A datase
get_childes_frequency <- function(
  token,
  languages = c("cat", "spa"),
  ...
){

  # get total number of tokens in each language
  total_counts <- get_speaker_statistics(...) %>%
    filter(grepl(paste(languages, collapse = "|"), .data$language)) %>%
    group_by(.data$language) %>%
    summarise(num_tokens = sum(.data$num_tokens), .groups = "drop") %>%
    mutate(language = strsplit(.data$language, split = " ")) %>%
    unnest(cols = .data$language) %>%
    group_by(.data$language) %>%
    summarise(n = sum(.data$num_tokens, na.rm = TRUE), .groups = "drop")

  # absolute frequency (raw counts)
  freq_counts <- get_tokens(role = "target_child", token =  token, language = languages) %>%
    mutate(gloss = tolower(.data$gloss)) %>%
    filter(grepl(paste(languages, collapse = "|"), .data$language)) %>%
    count(.data$gloss, .data$language) %>%
    mutate(language = strsplit(.data$language, " ")) %>%
    unnest(.data$language) %>%
    group_by(.data$language, .data$gloss) %>%
    summarise(freq_counts = sum(.data$n), .groups = "drop") %>%
    filter(freq_counts>0)

  # relative frequency (counts per million)
  freq_million <- freq_counts %>%
    left_join(total_counts, by = "language") %>%
    mutate(
      freq_per_million = freq_counts/n*1e6,
      freq_zipf = log10(.data$freq_per_million)+3
    ) %>%
    rename(word = .data$gloss, test_language = .data$language) %>%
    select(.data$word, .data$test_language, starts_with("freq_"))

  return(freq_million)
}





