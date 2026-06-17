# Собираем три словаря тональности в один файл data/lexicons.rds.
# Делается один раз; в приложении уже лежит готовый файл.

library(tidyverse)

# bing идёт прямо в составе tidytext
bing <- tidytext::get_sentiments("bing")

# afinn качаем напрямую (AFINN-111) — это тот же словарь, что отдаёт textdata
afinn <- read_tsv("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-111.txt",
                  col_names = c("word", "value"), col_types = "ci")

# nrc берём из пакета syuzhet (тот же словарь Mohammad & Turney 2013)
nrc <- syuzhet::get_sentiment_dictionary("nrc", language = "english") |>
  as_tibble() |>
  filter(value > 0) |>
  transmute(word = as.character(word), sentiment = as.character(sentiment)) |>
  distinct()

lexicons <- list(bing = bing, afinn = afinn, nrc = nrc)
saveRDS(lexicons, "data/lexicons.rds")
cat("Словари собраны:", paste(names(lexicons), collapse = ", "), "\n")
