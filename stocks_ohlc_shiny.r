library(shiny)
library(httr)
library(tidyverse)
library(plotly)
library(jsonlite)

ui <- navbarPage("Historical Stock Market Data Analysis",
                 
                 tabPanel("Stock Search",
                          textInput("stock_symbol", "Enter Stock Symbol or Name of Company:", ""),
                          p("Enter any date ranging from 1st January 2000 to current day to get Open-High-Low-Close (OHLC) stock prices"),
                          dateInput("date", "Select Date:", format="yyyy-mm-dd"),
                          radioButtons("radio","Select Timeframe:", choices=list("Intra-Day" = "intraDay",
                                                                                 "Month" = "month",
                                                                                 "Year" = "year",
                                                                                 "Decades" = "decade")),
                          actionButton("analysis_button", "Submit"),
                          verbatimTextOutput("error")
                 ),
                 tabPanel("Stock Summary",
                          dataTableOutput("summary_table")
                 ),
                 tabPanel("Stock Close Plot",
                          plotlyOutput("close_plot")
                 ),
                 tabPanel("Stock OHLC Candlestick Plot",
                          plotlyOutput("candlestick_plot")
                 )
                 
)

server <- function(input, output) {
  observeEvent(input$analysis_button, {
    
    if (input$stock_symbol=="") {
      output$error <- renderText("No Symbol/Name entered.")
      return(NA)
    }
    else {output$error <- renderText("")}
    
    api_key <- "insert_your_api_key"   #limit: 25 API calls per day
    #examples used for debugging: https://www.alphavantage.co/documentation/
    stock_search <- content(GET(paste0("https://www.alphavantage.co/query?function=SYMBOL_SEARCH&keywords=",
                                       input$stock_symbol,
                                       "&apikey=",api_key)))
    stock_name <- stock_search$`bestMatches`[[1]]$`1. symbol`
    
    if(is.null(stock_name)){
      output$error <- renderText("Search Error. Check input or change API key")
    }
    
    else{
      output$error <- renderText("")
      
      if (input$radio == "intraDay"){
        
        api_call_intraday <- paste0("https://www.alphavantage.co/query",
                                    "?function=TIME_SERIES_INTRADAY",
                                    "&symbol=", 
                                    stock_name,
                                    "&interval=30min",
                                    "&month=", format(input$date,"%Y-%m"),
                                    "&outputsize=full",
                                    "&apikey=", api_key)
        
        response_intraday <- GET(api_call_intraday)
        
        stock_data_intraday <- content(response_intraday)
        
        dates_intraday <- as.Date(names(stock_data_intraday$`Time Series (30min)`))
        times_intraday <- format(as.POSIXct(names(stock_data_intraday$`Time Series (30min)`)), format = "%H:%M:%S")
        open_prices_intraday <- as.numeric(sapply(stock_data_intraday$`Time Series (30min)`, function(x) as.numeric(x$"1. open")))
        high_prices_intraday <- as.numeric(sapply(stock_data_intraday$`Time Series (30min)`, function(x) as.numeric(x$"2. high")))
        low_prices_intraday <- as.numeric(sapply(stock_data_intraday$`Time Series (30min)`, function(x) as.numeric(x$"3. low")))
        close_prices_intraday <- as.numeric(sapply(stock_data_intraday$`Time Series (30min)`, function(x) as.numeric(x$"4. close")))
        volume_intraday <- as.numeric(sapply(stock_data_intraday$`Time Series (30min)`, function(x) as.numeric(x$"5. volume")))
        
        data_intraday <- data.frame(`Date` = dates_intraday,
                                    `Time` = times_intraday,
                                    `OpenPrice` = open_prices_intraday,
                                    `HighPrice` = high_prices_intraday,
                                    `LowPrice` = low_prices_intraday,
                                    `ClosePrice` = close_prices_intraday,
                                    `Volume` = volume_intraday)
        
        new_data_intraday <- data_intraday %>%
          filter(`Date` == input$date) %>%
          arrange(`Date`) %>% 
          select(`Time`, `OpenPrice`, `HighPrice`, `LowPrice`, `ClosePrice`, `Volume`)
        
        shifted_close_intraday <- c(new_data_intraday$OpenPrice[1],new_data_intraday$ClosePrice[1:length(new_data_intraday$ClosePrice)-1])
        
        summary_data_intraday <- new_data_intraday %>% 
          mutate(`Price Range`= `HighPrice` - `LowPrice`, `PriceChange` = `ClosePrice` - shifted_close_intraday, `PercentPriceChange` = (`PriceChange`/`OpenPrice`)*100)
        
        output$close_plot <- renderPlotly({
          if(dim(new_data_intraday)[1]!=0){
            close_intraday <- ggplot(new_data_intraday, aes(x = Time, y = ClosePrice, group=1)) +
              geom_line(color="blue") +
              labs(title = paste("Close Price Plot for Stock: ", stock_name),
                   x = "Time",
                   y = "Close Price") +
              theme_classic() +
              theme(panel.background = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            ggplotly(close_intraday)
          }
          else{ggplot()}
        })
        
        output$candlestick_plot <- renderPlotly({
          if(dim(new_data_intraday)[1]!=0){
            candlestick_intraday <- ggplot(new_data_intraday, aes(x = Time)) +
              labs(x = "Time", y = "OHLC Candlestick", title = paste("Open-High-Low-Close Plot for Stock: ", stock_name)) +
              geom_linerange(aes(ymin = LowPrice, ymax = HighPrice)) +
              geom_linerange(aes(ymin = ClosePrice, ymax = OpenPrice, color = ifelse(ClosePrice > OpenPrice, "green", "red")), linewidth = 3.5, show.legend=FALSE) +
              theme_classic() +
              theme(panel.background = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position='none')
            ggplotly(candlestick_intraday)
          }
          else{ggplot()}
        })
        
        
        output$summary_table <- renderDataTable({
          summary_data_intraday
        })
        
        
      }
      
      else if (input$radio == "month"){
        api_call_month <- paste0("https://www.alphavantage.co/query",
                                 "?function=TIME_SERIES_INTRADAY",
                                 "&symbol=", 
                                 stock_name,
                                 "&interval=5min",
                                 "&month=", format(input$date,"%Y-%m"),
                                 "&outputsize=full",
                                 "&apikey=", api_key)
        
        response_month <- GET(api_call_month)
        
        
        stock_data_month <- content(response_month)
        
        dates_month <- as.Date(names(stock_data_month$`Time Series (5min)`))
        open_prices_month <- as.numeric(sapply(stock_data_month$`Time Series (5min)`, function(x) as.numeric(x$"1. open")))
        high_prices_month <- as.numeric(sapply(stock_data_month$`Time Series (5min)`, function(x) as.numeric(x$"2. high")))
        low_prices_month <- as.numeric(sapply(stock_data_month$`Time Series (5min)`, function(x) as.numeric(x$"3. low")))
        close_prices_month <- as.numeric(sapply(stock_data_month$`Time Series (5min)`, function(x) as.numeric(x$"4. close")))
        volume_month <- as.numeric(sapply(stock_data_month$`Time Series (5min)`, function(x) as.numeric(x$"5. volume")))
        
        data_month <- data.frame(`Date` = dates_month,
                                 `OpenPrice` = open_prices_month,
                                 `HighPrice` = high_prices_month,
                                 `LowPrice` = low_prices_month,
                                 `ClosePrice` = close_prices_month,
                                 `Volume` = volume_month)
        
        new_data_month <- data_month %>%
          group_by(Date) %>%
          summarize(first(OpenPrice), max(HighPrice), min(LowPrice), last(ClosePrice), sum(`Volume`)) %>%
          rename(`OpenPrice` = `first(OpenPrice)`, `HighPrice` = `max(HighPrice)`, `LowPrice` = `min(LowPrice)`, `ClosePrice` = `last(ClosePrice)`, `Volume` = `sum(Volume)`) %>%
          arrange(Date)
        
        shifted_close_month <- c(new_data_month$OpenPrice[1],new_data_month$ClosePrice[1:length(new_data_month$ClosePrice)-1])
        
        summary_data_month <- new_data_month %>% 
          mutate(`Price Range`= `HighPrice` - `LowPrice`, `PriceChange` = `ClosePrice` - shifted_close_month, `PercentChange` = (`PriceChange`/`OpenPrice`)*100)
        
        output$close_plot <- renderPlotly({
          if(dim(new_data_month)[1]!=0){
            close_month <- ggplot(new_data_month, aes(x = as.factor(day(Date)), y = ClosePrice, group=1)) +
              geom_line(color="blue") +
              labs(title = paste("Close Price Plot for Stock: ", stock_name),
                   x = "Day of Month",
                   y = "Close Price") +
              theme_classic() +
              theme(panel.background = element_blank(), panel.grid = element_blank())
            ggplotly(close_month)
          }
          else{ggplot()}
        })
        
        output$candlestick_plot <- renderPlotly({
          if(dim(new_data_month)[1]!=0){
            candlestick_month <- ggplot(new_data_month, aes(x = as.factor(day(Date)))) +
              labs(x="Day of Month", y="OHLC Candlestick", title=paste("Open-High-Low-Close Plot for Stock:", stock_name)) +
              geom_linerange(aes(ymin = LowPrice, ymax = HighPrice)) +
              geom_linerange(aes(ymin = ClosePrice, ymax = OpenPrice, color = ifelse(ClosePrice > OpenPrice, "green", "red")), linewidth = 3.5, show.legend=FALSE) +
              theme_classic() +
              theme(panel.background = element_blank(), panel.grid = element_blank(), legend.position='none')
            ggplotly(candlestick_month)
          }
          else{ggplot()}
        })
        
        
        output$summary_table <- renderDataTable({
          summary_data_month
        })
      }
      else if (input$radio == "year"){
        api_call_year <- paste0("https://www.alphavantage.co/query",
                                "?function=TIME_SERIES_MONTHLY_ADJUSTED",
                                "&symbol=", 
                                stock_name,
                                "&apikey=", api_key)
        
        response_year <- GET(api_call_year)
        
        
        stock_data_year <- content(response_year)
        
        dates_year <- as.Date(names(stock_data_year$`Monthly Adjusted Time Series`))
        open_prices_year <- as.numeric(sapply(stock_data_year$`Monthly Adjusted Time Series`, function(x) as.numeric(x$"1. open")))
        high_prices_year <- as.numeric(sapply(stock_data_year$`Monthly Adjusted Time Series`, function(x) as.numeric(x$"2. high")))
        low_prices_year <- as.numeric(sapply(stock_data_year$`Monthly Adjusted Time Series`, function(x) as.numeric(x$"3. low")))
        close_prices_year <- as.numeric(sapply(stock_data_year$`Monthly Adjusted Time Series`, function(x) as.numeric(x$"4. close")))
        volume_year <- as.numeric(sapply(stock_data_year$`Monthly Adjusted Time Series`, function(x) as.numeric(x$"6. volume")))
        
        data_year <- data.frame(`Date` = dates_year,
                                `OpenPrice` = open_prices_year,
                                `HighPrice` = high_prices_year,
                                `LowPrice` = low_prices_year,
                                `ClosePrice` = close_prices_year,
                                `Volume` = volume_year)
        
        new_data_year <- data_year %>%
          filter(format(Date,"%Y") == format(input$date,"%Y")) %>% 
          arrange(Date)
        
        shifted_close_year <- c(new_data_year$OpenPrice[1],new_data_year$ClosePrice[1:length(new_data_year$ClosePrice)-1])
        
        summary_data_year <- new_data_year %>% 
          mutate(`Price Range`= `HighPrice` - `LowPrice`, `PriceChange` = `ClosePrice` - shifted_close_year, `PercentChange` = (`PriceChange`/`OpenPrice`)*100)
        
        output$close_plot <- renderPlotly({
          if(dim(new_data_year)[1]!=0){
            close_year <- ggplot(new_data_year, aes(x = month(Date, label =TRUE), y = ClosePrice, group=1)) +
              geom_line(color="blue") +
              labs(title = paste("Close Price Plot for Stock:", stock_name),
                   x = "Month",
                   y = "Close Price") +
              theme_classic() +
              theme(panel.background = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            ggplotly(close_year)
          }
          else{ggplot()}
        })
        
        output$candlestick_plot <- renderPlotly({
          if(dim(new_data_year)[1]!=0){
            candlestick_year <- ggplot(new_data_year, aes(x = month(Date,label=TRUE))) +
              labs(x = "Month", y = "OHLC Candlestick", title = paste("Open-High-Low-Close Plot for Stock:", stock_name))+
              geom_linerange(aes(ymin = LowPrice, ymax = HighPrice))+
              geom_linerange(aes(ymin = ClosePrice, ymax = OpenPrice, color = ifelse(ClosePrice > OpenPrice, "green", "red")), linewidth = 3.5, show.legend=FALSE) +
              theme_classic() +
              theme(panel.background = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position='none')
            ggplotly(candlestick_year)
          }
          else{ggplot()}
        })
        
        
        output$summary_table <- renderDataTable({
          summary_data_year
        })
      }
      else if (input$radio == "decade"){
        api_call_decade <- paste0("https://www.alphavantage.co/query",
                                  "?function=TIME_SERIES_MONTHLY_ADJUSTED",
                                  "&symbol=", 
                                  stock_name,
                                  "&apikey=", api_key)
        
        response_decade <- GET(api_call_decade)
        
        
        stock_data_decade <- content(response_decade)
        
        dates_decade <- format(as.Date(names(stock_data_decade$`Monthly Adjusted Time Series`)),"%Y")
        open_prices_decade <- as.numeric(sapply(stock_data_decade$`Monthly Adjusted Time Series`, function(x) as.numeric(x$"1. open")))
        high_prices_decade <- as.numeric(sapply(stock_data_decade$`Monthly Adjusted Time Series`, function(x) as.numeric(x$"2. high")))
        low_prices_decade <- as.numeric(sapply(stock_data_decade$`Monthly Adjusted Time Series`, function(x) as.numeric(x$"3. low")))
        close_prices_decade <- as.numeric(sapply(stock_data_decade$`Monthly Adjusted Time Series`, function(x) as.numeric(x$"4. close")))
        volume_decade <- as.numeric(sapply(stock_data_decade$`Monthly Adjusted Time Series`, function(x) as.numeric(x$"6. volume")))
        
        data_decade <- data.frame(`Date` = dates_decade,
                                  `OpenPrice` = open_prices_decade,
                                  `HighPrice` = high_prices_decade,
                                  `LowPrice` = low_prices_decade,
                                  `ClosePrice` = close_prices_decade,
                                  `Volume` = volume_decade)
        
        new_data_decade <- data_decade %>%
          group_by(Date) %>%
          summarize(first(OpenPrice), max(HighPrice), min(LowPrice), last(ClosePrice), sum(`Volume`)) %>%
          rename(`OpenPrice` = `first(OpenPrice)`, `HighPrice` = `max(HighPrice)`, `LowPrice` = `min(LowPrice)`, `ClosePrice` = `last(ClosePrice)`, `Volume` = `sum(Volume)`) %>%
          arrange(Date)
        
        shifted_close_decade <- c(new_data_decade$OpenPrice[1],new_data_decade$ClosePrice[1:length(new_data_decade$ClosePrice)-1])
        
        summary_data_decade <- new_data_decade %>% 
          mutate(`Price Range`= `HighPrice` - `LowPrice`, `PriceChange` = `ClosePrice` - shifted_close_decade, `PercentChange` = (`PriceChange`/`OpenPrice`)*100)
        
        output$close_plot <- renderPlotly({
          if(dim(new_data_decade)[1]!=0){
            close_decade <- ggplot(new_data_decade, aes(x = Date, y = ClosePrice, group=1)) +
              geom_line(color="blue") +
              labs(title = paste("Close Price Plot for Stock:", stock_name),
                   x = "Year",
                   y = "Close Price") +
              theme_classic() +
              theme(panel.background = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            ggplotly(close_decade)
          }
          else{ggplot()}
        })
        
        output$candlestick_plot <- renderPlotly({
          if(dim(new_data_decade)[1]!=0){
            candlestick_decade <- ggplot(new_data_decade, aes(x = Date)) +
              labs(x="Year", y="OHLC Candlestick", title=paste("Open-High-Low-Close Plot for Stock:", stock_name)) +
              geom_linerange(aes(ymin = LowPrice, ymax = HighPrice))+
              geom_linerange(aes(ymin = ClosePrice, ymax = OpenPrice, color = ifelse(ClosePrice > OpenPrice, "green", "red")), linewidth = 3.5, show.legend=FALSE) +
              theme_classic() +
              theme(panel.background = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position='none')
            ggplotly(candlestick_decade)
          }
          else{ggplot()}
        })
        
        
        output$summary_table <- renderDataTable({
          summary_data_decade
        })
      }
      
    }
    
  })
}

shinyApp(ui = ui, server = server)
