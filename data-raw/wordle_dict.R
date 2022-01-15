## code to prepare `wordle_dict` dataset goes here
# Thanks coolbutuseless for scraping the 'official' wordle dict
# remotes::install_github('coolbutuseless/wordle')

dict <- wordle::wordle_dict
usethis::use_data(dict, overwrite = TRUE)
