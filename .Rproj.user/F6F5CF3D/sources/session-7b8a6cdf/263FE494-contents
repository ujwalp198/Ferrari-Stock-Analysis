# 1. LOAD LIBRARIES
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(zoo)
library(TTR)

# 2. DATA PREPARATION
# Ensure 'Ferrari.csv' is in your project folder
df_raw <- read.csv("Ferrari.csv", stringsAsFactors = FALSE)

# AUTO-FIX: Force Column Names to match code logic
names(df_raw) <- toupper(names(df_raw))
df <- df_raw %>% 
  rename(Date = DATE, Open = OPEN, High = HIGH, Low = LOW, Close = CLOSE, Volume = VOLUME)

# AUTO-FIX: Date Formats (Tries common Kaggle formats)
df$Date <- as.Date(df$Date, format = "%d-%m-%y") 
if(all(is.na(df$Date))) df$Date <- as.Date(df_raw$DATE, format = "%Y-%m-%d")
if(all(is.na(df$Date))) df$Date <- as.Date(df_raw$DATE, format = "%m/%d/%Y")

# Final Cleanup: Remove empty rows and sort by time
df <- df %>% filter(!is.na(Date), !is.na(Close)) %>% arrange(Date)

# --- DEBUG CONSOLE MESSAGES ---
message("--- DATA LOADING STATUS ---")
message(paste("Rows processed successfully:", nrow(df)))

# 3. UI - THE FRONTEND
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Ferrari Stock Analysis"),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Layout Reset for Full Width */
        .content-wrapper { background-color: white !important; padding: 0 !important; }
        .content { padding: 0 !important; }
        
        /* Hero Image - PASTE YOUR URL HERE */
        .hero-image {
          width: 100%;
          height: 450px;
          background-image: url('f2.jpeg');
          background-size: cover;
          background-position: center;
          margin-bottom: 30px;
          border-bottom: 5px solid #cc0000;
        }
        
        /* Box & Stat Styling */
        .box { border-top: 3px solid #cc0000; margin-left: 15px; margin-right: 15px; border-radius: 0; }
        .stat-header { font-size: 26px; font-weight: bold; color: #cc0000; text-align: center; }
        .stat-label { text-align: center; color: #666; font-weight: bold; }
      "))
    ),
    
    # Hero Header
    div(class = "hero-image"),
    
    fluidPage(
      # SECTION 1: KEY STATS
      fluidRow(
        box(width = 12, title = "Market Overview",
            column(4, div(class="stat-header", paste0("$", round(tail(df$Open,1), 2))), div(class="stat-label", "Current Open")),
            column(4, div(class="stat-header", paste0("$", round(max(df$High), 2))), div(class="stat-label", "All-Time High")),
            column(4, div(class="stat-header", paste0("$", round(min(df$Low), 2))), div(class="stat-label", "All-Time Low"))
        )
      ),
      
      # SECTION 2: THE INTERACTIVE GRAPHS
      fluidRow(
        # Main Line Chart
        box(width = 12, title = "Price History (Interactive Line)", 
            plotlyOutput("lineChart", height = "400px")),
        
        # Candlestick with Vertical Zoom Enabled
        box(width = 12, title = "Detailed Candlestick (Use Mouse to Zoom Vertically/Horizontally)", 
            plotlyOutput("candlestick", height = "500px")),
        
        # Technical Indicators
        box(width = 6, title = "Moving Averages (Trends)", plotlyOutput("movingAverage")),
        box(width = 6, title = "RSI (Momentum)", plotlyOutput("rsiPlot")),
        
        # Volume & Volatility
        box(width = 6, title = "Trading Volume", plotlyOutput("volumeChart")),
        box(width = 6, title = "Daily Price Spread", plotlyOutput("rangeBar"))
      )
    )
  )
)

# 4. SERVER - THE BACKEND
server <- function(input, output) {
  
  # A. Simple Line Chart
  output$lineChart <- renderPlotly({
    plot_ly(df, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines', line = list(color = '#cc0000')) %>%
      layout(plot_bgcolor='white', paper_bgcolor='white', xaxis = list(title = "Year"), yaxis = list(title = "Price ($)"))
  })
  
  # B. Candlestick with VERTICAL ZOOM ENABLED
  output$candlestick <- renderPlotly({
    plot_ly(df, x = ~Date, type = "candlestick", 
            open = ~Open, close = ~Close, high = ~High, low = ~Low) %>%
      layout(
        yaxis = list(fixedrange = FALSE, autorange = TRUE, title = "Price ($)"),
        xaxis = list(rangeslider = list(visible = TRUE)),
        dragmode = "zoom" # Default tool is zoom
      ) %>%
      config(scrollZoom = TRUE) # Enables Mouse Wheel Zoom
  })
  
  # C. Moving Averages
  output$movingAverage <- renderPlotly({
    df$sma50 <- rollmean(df$Close, k = 50, fill = NA, align = "right")
    df$sma200 <- rollmean(df$Close, k = 200, fill = NA, align = "right")
    plot_ly(df, x = ~Date) %>%
      add_lines(y = ~Close, name = "Price", line = list(color = "black", width = 0.5)) %>%
      add_lines(y = ~sma50, name = "50-SMA", line = list(color = "blue")) %>%
      add_lines(y = ~sma200, name = "200-SMA", line = list(color = "orange")) %>%
      layout(yaxis = list(fixedrange = FALSE))
  })
  
  # D. RSI
  output$rsiPlot <- renderPlotly({
    rsi_vals <- RSI(df$Close, n = 14)
    plot_ly(x = df$Date, y = rsi_vals, type = 'scatter', mode = 'lines', line = list(color = "purple")) %>%
      layout(yaxis = list(range = c(0, 100), title = "RSI"))
  })
  
  # E. Volume
  output$volumeChart <- renderPlotly({
    plot_ly(df, x = ~Date, y = ~Volume, type = "bar", marker = list(color = "steelblue"))
  })
  
  # F. Daily Spread
  output$rangeBar <- renderPlotly({
    plot_ly(df, x = ~Date, y = ~(High - Low), type = 'scatter', mode = 'lines', line = list(color = "green"))
  })
}

# 5. RUN
shinyApp(ui, server)