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

ml_wordcloud <- function(responses = NULL, participant) {

  if (is.null(responses)) {
    responses <- ml_responses()
  }

  d <- responses %>%
    tidyr::drop_na(response) %>%
    dplyr::filter(
      id %in% participant,
      response > 1
    ) %>%
    dplyr::select(item, age) %>%
    dplyr::left_join(select(pool, label, item)) %>%
    dplyr::mutate(frequency = 1) %>%
    tidyr::drop_na(age, label) %>%
    dplyr::select(age, label, frequency) %>%
    dplyr::group_split(age) %>%
    purrr::map(., function(x){
      x %>%
        dplyr::select(-age) %>%
        wordcloud2::wordcloud2(
          size = 0.1
          #color = RColorBrewer::brewer.pal(8, "Set1")
          )
    })

  return(d)

}

