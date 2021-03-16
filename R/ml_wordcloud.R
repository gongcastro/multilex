#### ml_wordcloud: Create wordcloud for participant ############################

#' Retrieve and update local and/or remote data from formr
#' @importFrom wordcloud2 wordcloud2
#' @importFrom lubridate as_date
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr mutate_at
#' @importFrom dplyr vars
#' @importFrom tidyr drop_na
#' @export ml_wordcloud
#' @return A plot with the wordcloud of one participant across ages
#' @examples
#' words <- ml_wordcloud(responses = ml_responses(), participant = "bilexicon_000")

ml_wordcloud <- function(responses, participant) {
  d <- responses %>%
    drop_na(response) %>%
    filter(
      id %in% participant,
      response > 1
    ) %>%
    select(item, age) %>%
    left_join(select(pool, label, item)) %>%
    mutate(frequency = 1) %>%
    drop_na(age, label) %>%
    select(age, label, frequency) %>%
    group_split(age) %>%
    map(., function(x){
      x %>%
        select(-age) %>%
        wordcloud2::wordcloud2(
          size = 0.1,
          colors = brewer.pal(8, "Set1"))
    }) %>%


  wordcloud2::wordcloud2(d)

}

