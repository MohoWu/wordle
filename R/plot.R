

#' Plot the puzzle grid
#'
#' @param df Data frame built by `build_puzzle_df`
#'
#' @return ggplot
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_y_discrete scale_fill_manual coord_equal theme_void theme scale_x_continuous
#'
#'
plot_puzzle <- function(df) {

  ggplot(df, aes(x = letter_idx, y = as.character(word))) +
    geom_tile(aes(fill = as.character(clue)),
              color = "grey60",
              show.legend = FALSE) +
    geom_text(aes(label = letter),
              size = 12) +
    scale_fill_manual(
      breaks = c("0", "1", "2"),
      values = c("#787C7E", "#C9B458", "#6AAA64"),
      na.value = "#FFFFFF") +
    scale_y_discrete(limits = rev) +
    coord_equal() +
    theme_void()
}


#' Plot keyboard grid as hint
#'
#' @param df Data frame built by `build_keyboard_df`
#'
#' @return ggplot
#'
plot_keyboard <- function(df) {

  ggplot(df, aes(x, as.character(y))) +
    geom_tile(aes(fill = as.character(clue),
                  width = 0.9),
              color = "grey80",
              show.legend = FALSE) +
    geom_text(aes(label = letter),
              size = 12) +
    scale_fill_manual(
      breaks = c("0", "1", "2"),
      values = c("#787C7E", "#C9B458", "#6AAA64"),
      na.value = "#FFFFFF") +
    coord_equal() +
    scale_x_continuous(breaks = seq(from = 1, to = 10, by = 0.5)) +
    scale_y_discrete(limits = rev) +
    theme_void()

}








