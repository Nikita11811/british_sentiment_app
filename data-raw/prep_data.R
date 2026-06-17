# Готовим данные для приложения: список книг и токены каждого романа.
# Запускать один раз из корня приложения. В самом приложении это не нужно —
# там уже лежат готовые .rds.

library(tidyverse)
library(tidytext)

corpus_dir <- "../british_fiction"   # папка с 27 txt-файлами

# --- 1. Список книг ---
# Метаданные забиваем руками: так ничего не теряется (в overview.tsv пара опечаток
# в названиях, из-за которых автоматический join по имени файла молча терял книги).
# gender: 1 — автор-женщина, 2 — автор-мужчина.
books <- tribble(
  ~file,                     ~author,             ~title,                        ~year, ~gender,
  "ABronte_Agnes.txt",       "Anne Brontë",       "Agnes Grey",                   1847, 1,
  "ABronte_Tenant.txt",      "Anne Brontë",       "The Tenant of Wildfell Hall",  1848, 1,
  "Austen_Emma.txt",         "Jane Austen",       "Emma",                         1815, 1,
  "Austen_Pride.txt",        "Jane Austen",       "Pride and Prejudice",          1813, 1,
  "Austen_Sense.txt",        "Jane Austen",       "Sense and Sensibility",        1811, 1,
  "CBronte_Jane.txt",        "Charlotte Brontë",  "Jane Eyre",                    1847, 1,
  "CBronte_Professor.txt",   "Charlotte Brontë",  "The Professor",                1857, 1,
  "CBronte_Villette.txt",    "Charlotte Brontë",  "Villette",                     1853, 1,
  "Dickens_Bleak.txt",       "Charles Dickens",   "Bleak House",                  1852, 2,
  "Dickens_David.txt",       "Charles Dickens",   "David Copperfield",            1849, 2,
  "Dickens_Hard.txt",        "Charles Dickens",   "Hard Times",                   1854, 2,
  "EBronte_Wuthering.txt",   "Emily Brontë",      "Wuthering Heights",            1847, 1,
  "Eliot_Adam.txt",          "George Eliot",      "Adam Bede",                    1859, 1,
  "Eliot_Middlemarch.txt",   "George Eliot",      "Middlemarch",                  1871, 1,
  "Eliot_Mill.txt",          "George Eliot",      "The Mill on the Floss",        1860, 1,
  "Fielding_Joseph.txt",     "Henry Fielding",    "Joseph Andrews",               1742, 2,
  "Fielding_Tom.txt",        "Henry Fielding",    "Tom Jones",                    1749, 2,
  "Richardson_Clarissa.txt", "Samuel Richardson", "Clarissa",                     1748, 2,
  "Richardson_Pamela.txt",   "Samuel Richardson", "Pamela",                       1740, 2,
  "Sterne_Sentimental.txt",  "Laurence Sterne",   "A Sentimental Journey",        1768, 2,
  "Sterne_Tristram.txt",     "Laurence Sterne",   "Tristram Shandy",              1759, 2,
  "Thackeray_Barry.txt",     "William Thackeray", "The Luck of Barry Lyndon",     1844, 2,
  "Thackeray_Pendennis.txt", "William Thackeray", "The History of Pendennis",     1848, 2,
  "Thackeray_Vanity.txt",    "William Thackeray", "Vanity Fair",                  1848, 2,
  "Trollope_Barchester.txt", "Anthony Trollope",  "Barchester Towers",            1857, 2,
  "Trollope_Phineas.txt",    "Anthony Trollope",  "Phineas Finn",                 1869, 2,
  "Trollope_Prime.txt",      "Anthony Trollope",  "The Prime Minister",           1876, 2
) |>
  mutate(id = row_number(),
         gender_label = if_else(gender == 1, "Женщина", "Мужчина"),
         label = paste0(author, " — ", title, " (", year, ")")) |>
  relocate(id)

# --- 2. Токены ---
# Для каждой книги сохраняем слова по порядку (порядок = позиция в тексте).
dir.create("data/tokens", showWarnings = FALSE, recursive = TRUE)

for (i in seq_len(nrow(books))) {
  words <- tibble(line = read_lines(file.path(corpus_dir, books$file[i]))) |>
    unnest_tokens(word, line) |>          # нижний регистр + чистка пунктуации
    filter(str_detect(word, "[a-z]")) |>  # выкидываем голые числа
    pull(word)
  saveRDS(words, paste0("data/tokens/", books$id[i], ".rds"))
  cat(i, books$file[i], "-", length(words), "слов\n")
}

saveRDS(books, "data/books.rds")
cat("Готово. Словари собираются отдельно — build_lexicons.R\n")
