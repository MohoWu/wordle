#' Compare candidate letter with the answer
#'
#' @param candidate A candidate word
#' @param answer The answer word
#'
#' @return A numeric vector.
#' @details Returns a vector of values
#'  0 = "not in the answer"
#'  1 = "in the answer but incorrect position"
#'  2 = "correct position in the answer"
#'
#' @importFrom stringr str_length
#'
compare_word <- function(candidate, answer) {

  # return NA if candidate is empty string
  if (str_length(candidate) == 0) return(NA_integer_)

  # making sure candidate and answer have the same length
  if (str_length(candidate) != str_length(answer))
    stop("candidate and answer need to have the same length", call. = FALSE)

  # compare each letter
  answer_vec <- vectorise_word(answer)
  candidate_vec <- vectorise_word(candidate)

  exact <- 2L * (candidate_vec == answer_vec)
  inexact <- which(candidate_vec != answer_vec)

  for (i in inexact) {
    for (j in inexact) {
      wrong_spot = candidate_vec[i] == answer_vec[j]
      if (wrong_spot) {
        exact[i] <- 1L
        inexact <- inexact[inexact != j]
        break
      }
    }
  }
  # return
  exact

}

#' Vectorise a word
#'
#' @param word a word
#'
#' @return A character vector with each letter of the word as an element
#'
#' @examples vectorise("apple")
vectorise_word <- function(word) {

  word_len <- str_length(word)
  # return NA if it's empty string
  if (word_len == 0) return(NA_character_)
  # vectorise word
  vec <- purrr::map_chr(1:word_len, ~substr(word, .x, .x))

  # return
  vec
}

#' Pad a vector with a value
#'
#' @param vec A vector
#' @param n Total length to achieve
#' @param with Pad with
#'
#' @return A vector
#'
pad_vec <- function(vec, n, with) {

  c(vec, rep(with, (n - length(vec))))

}


#' Build a data frame for plotting the puzzle grid
#'
#' @param candidates A vector of candidate words
#' @param answer The answer word
#' @param total_try Total number of tries allowed.
#'
#' @return A data frame containing `word`, `letter`, `letter_idx` and `clue` columns
#'
build_puzzle_df <- function(candidates, answer, total_try = 6) {

  # build word column
  word_len <- str_length(answer)
  n_candidates <- length(candidates)
  word <- rep(1:total_try, each = word_len)

  # build letter column
  letter <- unlist(purrr::map(candidates, vectorise_word))
  letter <- pad_vec(letter, word_len*total_try, NA_character_)

  # build letter_idx column
  letter_idx <- rep(1:word_len, total_try)

  # build clue column
  clue <- unlist(purrr::map(candidates, compare_word, answer))
  clue <- pad_vec(clue, word_len*total_try, NA_integer_)

  # build df
  df <- data.frame(
    word = word,
    letter = toupper(letter),
    letter_idx = letter_idx,
    clue = clue,
    stringsAsFactors = FALSE
  )

  # return
  df

}


#' Build keyboard data frame
#'
#' @param candidates A vector of candidate words
#' @param answer The answer word
#'
#' @return
#'
#' @importFrom dplyr `%>%` desc
#'
build_keyboard_df <- function(candidates, answer) {

  # candidate letters
  letter <- unlist(purrr::map(candidates, vectorise_word))

  # candidate clues
  clue <- unlist(purrr::map(candidates, compare_word, answer))

  # candidate df
  can_df <- data.frame(
    letter = letter,
    clue = clue,
    stringsAsFactors = FALSE
  )

  # only keep unique letters according to highest clue
  can_df <- can_df %>%
    dplyr::group_by(letter) %>%
    dplyr::arrange(desc(clue)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # keyboard df
  key_df <- data.frame(
    letter = c("q","w","e","r","t","y","u","i","o","p",
               "a","s","d","f","g","h","j","k","l",
               "z", "x", "c", "v", "b", "n", "m"),
    x = c(1:10,
          1.5:9.5,
          2:8),
    y = c(rep(1, 10),
          rep(2, 9),
          rep(3, 7))
  )

  key_df <- dplyr::left_join(
    key_df, can_df,
    by = "letter"
  ) %>%
    dplyr::mutate(letter = toupper(letter))

  # return
  key_df

}
