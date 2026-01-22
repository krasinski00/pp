library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(plotly)
library(DT)
library(tidyr)

# ====== DANE ======

path <- "Daily_Global_Stock_Market_Indicators.csv"


df <- read_csv(path, show_col_types = FALSE) %>%
  mutate(
    Date = suppressWarnings(as.Date(Date)),
    Open  = suppressWarnings(as.numeric(Open)),
    High  = suppressWarnings(as.numeric(High)),
    Low   = suppressWarnings(as.numeric(Low)),
    Close = suppressWarnings(as.numeric(Close)),
    Volume = suppressWarnings(as.numeric(Volume)),
    Daily_Change_Percent = suppressWarnings(as.numeric(Daily_Change_Percent))
  ) %>%
  filter(!is.na(Date), !is.na(Country), !is.na(Index_Name)) %>%
  arrange(Country, Index_Name, Date) %>%
  mutate(Series = paste(Country, Index_Name, sep = " | "))

countries <- sort(unique(df$Country))
indexes_all <- sort(unique(df$Index_Name))
min_date <- min(df$Date, na.rm = TRUE)
max_date <- max(df$Date, na.rm = TRUE)

# ====== UI ======
ui <- fluidPage(
  titlePanel("Daily Global Stock Market Indicators — Dashboard (Shiny)"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("country", "Country", choices = countries,
                     selected = head(countries, 2), multiple = TRUE),
      selectizeInput("index", "Index", choices = indexes_all,
                     selected = head(indexes_all, 2), multiple = TRUE),
      dateRangeInput("dates", "Date", start = min_date, end = max_date,
                     min = min_date, max = max_date),
      selectInput("chart", "Chart",
                  choices = c("Close (line)", "Close (line, normalized)", "Candlestick (OHLC)"),
                  selected = "Close (line)"),
      sliderInput("roll", "Rolling", min = 1, max = 30, value = 1, step = 1),
      sliderInput("topn", "Top N", min = 3, max = 30, value = 10, step = 1),
      checkboxInput("drop_na", "Drop NaN rows", value = TRUE)
    ),
    mainPanel(
      h4("KPI"),
      tableOutput("kpi"),
      br(),
      plotlyOutput("plot1", height = "420px"),
      br(),
      plotlyOutput("hist", height = "320px"),
      br(),
      h4("Top N (ostatni dzień z danymi po filtrach)"),
      h5("Gainers"),
      DTOutput("gainers"),
      br(),
      h5("Losers"),
      DTOutput("losers"),
      br(),
      h4("Podgląd (ostatnie 30 wierszy po filtrach)"),
      DTOutput("tail30")
    )
  )
)

# ====== SERVER ======
server <- function(input, output, session) {
  
  filtered <- reactive({
    req(input$dates)
    
    sel_countries <- if (length(input$country)) input$country else countries
    sel_indexes   <- if (length(input$index)) input$index else indexes_all
    
    d <- df %>%
      filter(
        Country %in% sel_countries,
        Index_Name %in% sel_indexes,
        Date >= input$dates[1],
        Date <= input$dates[2]
      )
    
    if (isTRUE(input$drop_na)) {
      d <- d %>% filter(!is.na(Open), !is.na(High), !is.na(Low), !is.na(Close), !is.na(Daily_Change_Percent))
    }
    
    d <- d %>% arrange(Country, Index_Name, Date)
    
    # Rolling mean po grupie
    roll <- as.integer(input$roll)
    if (roll > 1) {
      d <- d %>%
        group_by(Country, Index_Name) %>%
        arrange(Date, .by_group = TRUE) %>%
        mutate(
          Close_roll = zoo::rollapply(Close, width = roll, FUN = mean, align = "right",
                                      fill = NA, partial = TRUE)
        ) %>%
        ungroup()
    } else {
      d <- d %>% mutate(Close_roll = Close)
    }
    
    d
  })
  
  output$kpi <- renderTable({
    d <- filtered()
    tibble(
      Rows = nrow(d),
      Countries = n_distinct(d$Country),
      Indexes = n_distinct(d$Index_Name),
      `Avg daily change %` = if (nrow(d)) round(mean(d$Daily_Change_Percent, na.rm = TRUE), 3) else NA,
      `Median volume` = if (nrow(d) && any(!is.na(d$Volume))) as.integer(median(d$Volume, na.rm = TRUE)) else NA
    )
  })
  
  output$plot1 <- renderPlotly({
    d <- filtered()
    validate(need(nrow(d) > 0, "Brak danych dla wybranych filtrów."))
    
    chart_type <- input$chart
    
    if (chart_type == "Close (line)") {
      plot_ly(
        d, x = ~Date, y = ~Close_roll, color = ~Series,
        type = "scatter", mode = "lines",
        hoverinfo = "text",
        text = ~paste0(
          "Date: ", Date,
          "<br>Country: ", Country,
          "<br>Index: ", Index_Name,
          "<br>Open: ", Open,
          "<br>High: ", High,
          "<br>Low: ", Low,
          "<br>Close: ", Close,
          "<br>Volume: ", Volume,
          "<br>Daily %: ", Daily_Change_Percent
        )
      ) %>% layout(title = paste0("Close over time (rolling=", input$roll, ")"),
                   xaxis = list(title = "Date"), yaxis = list(title = "Close (rolled)"))
      
    } else if (chart_type == "Close (line, normalized)") {
      
      d_norm <- d %>%
        group_by(Country, Index_Name) %>%
        arrange(Date, .by_group = TRUE) %>%
        mutate(
          first_valid = first(na.omit(Close_roll)),
          Close_norm_100 = ifelse(!is.na(first_valid) & first_valid != 0, (Close_roll / first_valid) * 100, NA_real_)
        ) %>%
        ungroup()
      
      plot_ly(
        d_norm, x = ~Date, y = ~Close_norm_100, color = ~Series,
        type = "scatter", mode = "lines",
        hoverinfo = "text",
        text = ~paste0(
          "Date: ", Date,
          "<br>Country: ", Country,
          "<br>Index: ", Index_Name,
          "<br>Close: ", Close,
          "<br>Volume: ", Volume,
          "<br>Daily %: ", Daily_Change_Percent
        )
      ) %>% layout(title = paste0("Close normalized to 100 at start (rolling=", input$roll, ")"),
                   xaxis = list(title = "Date"), yaxis = list(title = "Index (100=start)"))
      
    } else {
      # Candlestick
      validate(need(n_distinct(d$Country) == 1 && n_distinct(d$Index_Name) == 1,
                    "Candlestick wymaga wyboru dokładnie 1 Country i 1 Index."))
      
      plot_ly(
        d, x = ~Date, type = "candlestick",
        open = ~Open, high = ~High, low = ~Low, close = ~Close,
        name = unique(d$Series)
      ) %>% layout(title = "Candlestick (OHLC)",
                   xaxis = list(title = "Date"), yaxis = list(title = "Price"))
    }
  })
  
  output$hist <- renderPlotly({
    d <- filtered()
    validate(need(nrow(d) > 0, "Brak danych dla wybranych filtrów."))
    
    plot_ly(d, x = ~Daily_Change_Percent, type = "histogram", nbinsx = 40) %>%
      layout(title = "Rozkład Daily_Change_Percent (po filtrach)",
             xaxis = list(title = "Daily_Change_Percent"),
             yaxis = list(title = "Count"))
  })
  
  last_day_data <- reactive({
    d <- filtered()
    validate(need(nrow(d) > 0, "Brak danych dla wybranych filtrów."))
    
    last_day <- max(d$Date, na.rm = TRUE)
    d_last <- d %>% filter(Date == last_day)
    list(last_day = last_day, d_last = d_last)
  })
  
  output$gainers <- renderDT({
    x <- last_day_data()
    d_last <- x$d_last
    topn <- as.integer(input$topn)
    
    gainers <- d_last %>%
      arrange(desc(Daily_Change_Percent)) %>%
      slice_head(n = topn) %>%
      select(Date, Country, Index_Name, Close, Daily_Change_Percent, Volume)
    
    datatable(gainers, options = list(pageLength = min(10, nrow(gainers))), rownames = FALSE,
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left;',
                paste0("Top ", topn, " gainers dla dnia: ", x$last_day)
              ))
  })
  
  output$losers <- renderDT({
    x <- last_day_data()
    d_last <- x$d_last
    topn <- as.integer(input$topn)
    
    losers <- d_last %>%
      arrange(Daily_Change_Percent) %>%
      slice_head(n = topn) %>%
      select(Date, Country, Index_Name, Close, Daily_Change_Percent, Volume)
    
    datatable(losers, options = list(pageLength = min(10, nrow(losers))), rownames = FALSE,
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left;',
                paste0("Top ", topn, " losers dla dnia: ", x$last_day)
              ))
  })
  
  output$tail30 <- renderDT({
    d <- filtered() %>%
      tail(30) %>%
      select(Date, Country, Index_Name, Open, High, Low, Close, Volume, Daily_Change_Percent)
    
    datatable(d, options = list(pageLength = 10), rownames = FALSE)
  })
}

if (!requireNamespace("zoo", quietly = TRUE)) {
  install.packages("zoo")
}
library(zoo)

shinyApp(ui, server)
