test_that("test compare_word", {

  # normal case
  expect_equal(c(2, 1, 0, 0, 2),
               compare_word("alcde", "apple"))

  # empty word
  expect_equal(NA_integer_,
               compare_word("", "apple"))

})


test_that("test vectorise_word", {

  # normal case
  expect_equal(c("a", "p", "p", "l", "e"),
               vectorise_word("apple"))

  # empty word
  expect_equal(NA_character_,
               vectorise_word(""))

})

test_that("test build_puzzle_df", {

  expect_df <- data.frame(
    word = rep(1:6, each = 5),
    letter = c("A", "B", "C", "D", "E",
               "C", "D", "E", "F", "G",
               rep(NA_character_, 20)),
    letter_idx = rep(1:5, 6),
    clue = c(2, 0, 0, 0, 2,
             0, 0, 1, 0, 0,
             rep(NA_integer_, 20)),
    stringsAsFactors = FALSE
  )

  expect_equal(
    expect_df,
    build_puzzle_df(c("abcde", "cdefg"), "apple")
  )
})

test_that("test build_keyboard_df", {

  expect_df <- data.frame(
    letter = toupper(c("q","w","e","r","t","y","u","i","o","p",
               "a","s","d","f","g","h","j","k","l",
               "z", "x", "c", "v", "b", "n", "m")),
    x = c(1:10,
          1.5:9.5,
          2:8),
    y = c(rep(1, 10),
          rep(2, 9),
          rep(3, 7)),
    clue = c(NA_integer_, 0, 1, 0, NA, 0, NA_integer_, NA_integer_, NA_integer_,
             1, 2, 0, rep(NA_integer_, 9), 0, NA_integer_, 0, 0, 0)
  )

  expect_equal(
    expect_df,
    build_keyboard_df(c("swamp", "carny", "abbey"),
                      "apple")
  )

})
