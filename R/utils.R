#### helper functions ##########################################################

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

# fix BiLexicon codes
fix_code <- function(x)
{
  x %>% # correct typos in code
    stringr::str_replace_all(., "O", "0") %>%
    stringr::str_replace_all(., "l", "L") %>%
    stringr::str_replace_all(., "I", "L") %>%
    stringr::str_to_upper(.) %>%
    stringr::str_replace_all(., "BL ", "BL") %>%
    stringr::str_replace_all(., "BLBL", "BL") %>%
    stringr::str_replace_all(., " ", "") %>%
    stringr::str_remove_all(., "BL$") %>%
    ifelse(!stringr::str_detect(., "BL"), paste0("BL", .), .)
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
  x %>%
    group_by(id) %>%
    mutate(sex = case_when(id %in% c("bilexicon_1097",
                                     "bilexicon_1441",
                                     "bilexicon_1124",
                                     "bilexicon_1448") ~ "Female",
                           id %in% c("bilexicon_1447") ~ "Male",
                           TRUE ~ sex[which(!is.na(sex))[1]])) %>%
    ungroup()
}

# fix postcode
fix_postcode <- function(x) {
  x %>%
    mutate(postcode = ifelse(nchar(postcode) < 5, paste0("0", postcode), postcode),
           postcode = ifelse(nchar(postcode) < 5, NA_character_, postcode))
}

# fix item
fix_item <- function(x) {
  x %>%
    mutate(item = case_when(
      item=="cat_parc" ~ "cat_parc1",
      item=="cat_eciam" ~ "cat_enciam",
      item=="cat_voler" ~ "cat_voler1",
      item=="cat_voler3" ~ "cat_voler2",
      item=="cat_despres1" ~ "cat_despres",
      item=="cat_peix" ~ "cat_peix1",
      item=="cat_estar" ~ "cat_estar1",
      item=="cat_querer" ~ "cat_querer1",
      item=="cat_estiguestequiet" ~ "cat_estiguesquiet",
      item=="spa_nibla" ~ "spa_niebla",
      item=="spa_ir" ~ "spa_ir1",
      item=="spa_querer" ~ "spa_querer1",
      TRUE ~ item
    ))
}

# replace special characters
replace_special_characters <- function(x) {
  # replace characters
  x %>%
    sub("á", "a", .) %>%
    sub("é", "e", .) %>%
    sub("í", "i", .) %>%
    sub("é", "e", .) %>%
    sub("ú", "u", .) %>%
    sub("ñ", "n", .) %>%
    sub("ç", "c", .) %>%
    sub("à", "a", .) %>%
    sub("è", "e", .) %>%
    sub("ò", "o", .) %>%
    sub("ó", "o", .) %>%
    sub("ü", "u", .) %>%
    sub("ï", "i", .)
}

# fill missing with previous row
coalesce_by_column <- function(x) {
  return(x[max(which(!is.na(x)))])
}


# evaluate if x is included in y
"%!in%" <- function(x, y) !(x %in% y)

# first non-non-missing value
first_non_na <- function(x) {
  ifelse(is.logical(first(x[!is.na(x)])), NA, first(x[!is.na(x)]))
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
