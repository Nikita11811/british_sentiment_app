# Эмоциональная анатомия британского романа
# Shiny-приложение: смотрим, как меняется тональность текста по ходу романа.
# Можно выбрать книгу, словарь тональности и размер фрагмента.

library(shiny)
library(bslib)      # тема оформления
library(tidyverse)
library(tidytext)   # сам сентимент-анализ (inner_join со словарём)
library(DT)         # интерактивная таблица слов
library(wordcloud)  # облако слов

# --- 1. Данные ---
# Тяжёлую токенизацию сделали заранее (см. data-raw/), тут только читаем готовое.
books    <- readRDS("data/books.rds")     # 27 романов + автор, год
lexicons <- readRDS("data/lexicons.rds")  # три словаря: bing, afinn, nrc

github_url <- "https://github.com/doknikal/FinRBr"

# выпадающий список книг и общая палитра
book_choices <- setNames(books$id, books$label)
my_colors <- c("позитив" = "#2e7d32", "негатив" = "#c62828")

# русские подписи для восьми эмоций NRC
emo_ru <- c(anger = "гнев", anticipation = "предвкушение", disgust = "отвращение",
            fear = "страх", joy = "радость", sadness = "грусть",
            surprise = "удивление", trust = "доверие")

# --- 2. Готовим словарь к виду (word, score) ---
# Так тональность любого словаря считается одинаково — простой суммой по фрагменту.
# bing и nrc дают +1 / -1, afinn — свою оценку от -5 до +5.
get_lex <- function(name) {
  if (name == "afinn") {
    lexicons$afinn |> rename(score = value)
  } else if (name == "bing") {
    lexicons$bing |> transmute(word, score = if_else(sentiment == "positive", 1, -1))
  } else {
    lexicons$nrc |>
      filter(sentiment %in% c("positive", "negative")) |>
      transmute(word, score = if_else(sentiment == "positive", 1, -1))
  }
}

# =====================================================================
# UI
# =====================================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "litera", primary = "#3b5b7a"),
  titlePanel("Эмоциональная анатомия британского романа"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("book", "Роман:", choices = book_choices, selected = 4),
      radioButtons("lexicon", "Словарь тональности:",
                   choices = c("Bing (плюс / минус)" = "bing",
                               "AFINN (от -5 до +5)" = "afinn",
                               "NRC (эмоции)"        = "nrc")),
      sliderInput("chunk_size", "Размер фрагмента (слов):",
                  min = 500, max = 5000, value = 2000, step = 500),
      actionButton("go", "Анализировать", class = "btn-primary"),
      helpText("Анализ запускается только после нажатия кнопки.")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        # вкладка с описанием — она же главная страница
        tabPanel(
          "О проекте",
          br(),
          p("Приложение показывает эмоциональную тональность британских романов (1740-1876):
             как меняется настроение текста от начала к концу, какие эмоции преобладают
             и какие слова сильнее всего влияют на оценку."),
          p(strong("Как это работает."), "Текст делится на фрагменты по N слов,
             каждое слово сопоставляется со словарём тональности (inner_join — лишние
             стоп-слова отсеиваются сами), и для каждого фрагмента считается сумма оценок.
             Получается эмоциональная арка сюжета."),
          h4("Три словаря на выбор"),
          tags$ul(
            tags$li(strong("Bing"), " — слово либо позитивное, либо негативное."),
            tags$li(strong("AFINN"), " — у слова числовая оценка от -5 до +5."),
            tags$li(strong("NRC"), " — восемь эмоций (радость, страх, доверие...) плюс полярность.")
          ),
          p("Если переключать словари на одной книге, видно, что выбор словаря заметно
             меняет картину — это важно помнить при любом анализе тональности."),
          h4("Команда"),
          tags$ul(
            tags$li("Никита Докудовский")
          ),
          p(tags$a(href = github_url, target = "_blank", "Репозиторий проекта на GitHub"))
        ),

        tabPanel("Тональность текста",
                 br(),
                 plotOutput("arc", height = "330px"),
                 plotOutput("words", height = "420px")),

        tabPanel("Эмоции (NRC)",
                 br(),
                 plotOutput("emo", height = "380px"),
                 DTOutput("emo_tbl")),

        tabPanel("Сравнение словарей",
                 br(),
                 p("Одна и та же книга, посчитанная тремя словарями. Шкалы у словарей
                    разные, поэтому ось Y у панелей своя."),
                 plotOutput("compare", height = "540px")),

        tabPanel("Облако слов",
                 br(),
                 p("Самые частые тональные слова: зелёные — позитивные, красные — негативные."),
                 plotOutput("cloud", height = "450px"))
      )
    )
  )
)

# =====================================================================
# SERVER
# =====================================================================
server <- function(input, output) {

  # Главный расчёт. Грузим слова книги и режем на фрагменты.
  # Считается только по кнопке — bindEvent, как в учебнике.
  prepared <- reactive({
    req(input$book)
    words <- readRDS(paste0("data/tokens/", input$book, ".rds"))  # слова романа по порядку
    tokens <- tibble(word = words) |>
      mutate(chunk = (row_number() - 1) %/% input$chunk_size)     # нарезка кусками, как в курсовой
    list(tokens  = tokens,
         lexicon = input$lexicon,
         book    = filter(books, id == as.integer(input$book)))
  }) |>
    bindEvent(input$go)

  # --- Арка тональности ---
  output$arc <- renderPlot({
    res <- req(prepared())
    lex <- get_lex(res$lexicon)

    arc <- res$tokens |>
      inner_join(lex, by = "word") |>
      group_by(chunk) |>
      summarise(sum = sum(score), .groups = "drop") |>
      mutate(tone = if_else(sum >= 0, "позитив", "негатив"))

    ggplot(arc, aes(chunk, sum, fill = tone)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = my_colors) +
      labs(title = paste0("Тональность по тексту: ", res$book$title),
           x = "Фрагмент текста (по порядку)", y = "Тональность фрагмента") +
      theme_minimal(base_size = 14)
  })

  # --- Слова, которые влияют на тональность сильнее всего ---
  output$words <- renderPlot({
    res <- req(prepared())
    lex <- get_lex(res$lexicon)

    top <- res$tokens |>
      inner_join(lex, by = "word") |>
      count(word, score, name = "n") |>
      mutate(contribution = n * score,
             tone = if_else(contribution >= 0, "позитив", "негатив")) |>
      slice_max(abs(contribution), n = 15)

    ggplot(top, aes(contribution, reorder(word, contribution), fill = tone)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = my_colors) +
      labs(title = "Слова, которые сильнее всего влияют на тональность",
           x = "Вклад (частота × оценка)", y = NULL) +
      theme_minimal(base_size = 14)
  })

  # --- Профиль эмоций по NRC ---
  output$emo <- renderPlot({
    res <- req(prepared())

    emo <- res$tokens |>
      inner_join(filter(lexicons$nrc, sentiment %in% names(emo_ru)), by = "word") |>
      count(sentiment, name = "n") |>
      mutate(emotion = emo_ru[sentiment])

    ggplot(emo, aes(n, reorder(emotion, n), fill = emotion)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Какие эмоции преобладают (словарь NRC)",
           x = "Число эмоционально окрашенных слов", y = NULL) +
      theme_minimal(base_size = 14)
  })

  # таблица: топ слов по каждой эмоции (колонки подписаны по-русски через colnames)
  output$emo_tbl <- renderDT({
    res <- req(prepared())

    tab <- res$tokens |>
      inner_join(filter(lexicons$nrc, sentiment %in% names(emo_ru)), by = "word") |>
      mutate(emotion = emo_ru[sentiment]) |>
      count(emotion, word, name = "n", sort = TRUE)

    datatable(tab, rownames = FALSE,
              colnames = c("Эмоция", "Слово", "Частота"),
              options = list(pageLength = 10))
  })

  # --- Сравнение трёх словарей на одной книге ---
  output$compare <- renderPlot({
    res <- req(prepared())

    compare_df <- map_dfr(c("bing", "afinn", "nrc"), function(nm) {
      res$tokens |>
        inner_join(get_lex(nm), by = "word") |>
        group_by(chunk) |>
        summarise(sum = sum(score), .groups = "drop") |>
        mutate(lexicon = nm)
    })

    ggplot(compare_df, aes(chunk, sum, fill = sum >= 0)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ lexicon, ncol = 1, scales = "free_y") +
      scale_fill_manual(values = c("TRUE" = my_colors[["позитив"]],
                                   "FALSE" = my_colors[["негатив"]])) +
      labs(title = "Одна книга — три словаря", x = "Фрагмент текста", y = "Тональность") +
      theme_minimal(base_size = 13)
  })

  # --- Облако тональных слов ---
  output$cloud <- renderPlot({
    res <- req(prepared())
    lex <- get_lex(res$lexicon)

    cloud_df <- res$tokens |>
      inner_join(lex, by = "word") |>
      count(word, score, name = "freq", sort = TRUE) |>
      slice_max(freq, n = 120)

    cols <- if_else(cloud_df$score >= 0, my_colors[["позитив"]], my_colors[["негатив"]])
    wordcloud(cloud_df$word, cloud_df$freq, colors = cols, ordered.colors = TRUE,
              random.order = FALSE, scale = c(4, 0.4), max.words = 120)
  })

  # считаем графики и на скрытых вкладках, чтобы после кнопки они не были пустыми
  for (id in c("arc", "words", "emo", "emo_tbl", "compare", "cloud"))
    outputOptions(output, id, suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
