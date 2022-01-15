


as.data.frame(list("a", "b", "c", "d", "e")) %>%
  ggplot()

df <- data.frame(
  word = rep(1:6, each = 5),
  letter_idx = rep(1:5, 6),
  letter = rep(sample(LETTERS, 5), 6),
  stringsAsFactors = FALSE
)

ggplot(df, aes(x = letter_idx, y = as.character(word))) +
  geom_tile(colour = "grey80",
            fill = "grey50") +
  geom_text(aes(label = letter)) +
  scale_y_discrete(limits = rev)



test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


