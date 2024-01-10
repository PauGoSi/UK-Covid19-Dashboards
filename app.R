# Load required libraries
library(shiny)
library(highcharter)
library(dplyr)
library(jsonlite)
library(httr)
endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","newCases":"newCasesByPublishDate"}'

httr::GET(
  url = endpoint,
  timeout(10)
) -> response


if (response$status_code >= 400) {
  err_msg = httr::http_status(response)
  stop(err_msg)
}

# Convert response from binary to JSON:
json_text <- content(response, "text")
data      <- jsonlite::fromJSON(json_text) 

dataset <- data$data %>% 
  dplyr::mutate(#date=as.Date(date),
    year=substr(date, start = 1, stop = 4),
    month=substr(date, start = 6, stop = 7)
  ) %>% 
  dplyr::select(newCases,year, month) %>% 
  dplyr::group_by(year,month) %>% 
  dplyr::summarise(newCases=sum(newCases)) %>% 
  dplyr::ungroup(year,month)


helper_yearmonth_bm <- dataset %>% 
  dplyr::mutate(year_bm=as.character(as.integer(year)-1)) %>% 
  dplyr::select(year_bm,month) %>% 
  dplyr::rename(year=year_bm)

dataset_bm <- dataset %>% 
  dplyr::inner_join(helper_yearmonth_bm,by=c("year","month")) %>% 
  dplyr::mutate(year=as.character(as.integer(year)+1))

dataset <- full_join(dataset,dataset_bm,by=c("year","month")) %>% 
  dplyr::rename(newCases=newCases.x,newCases_lastPeriod=newCases.y) %>% 
  mutate(year_month = as.Date(paste(year, month, "01", sep = "-")))

# Define UI
ui <- fluidPage(
  includeCSS("www/styles.css"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel("UK Covid Data Visualization"),
  # Include the custom JQuery code
  # tags$script(HTML('
  #   // Add hover effect and enlarge chart on mouseover
  #   $(document).ready(function() {
  #     $("#myChart").on("mouseenter", function() {
  #       $(this).css({"transform": "scale(1.1)", "transition": "transform 0.3s"});
  #     }).on("mouseleave", function() {
  #       $(this).css({"transform": "scale(1)", "transition": "transform 0.3s"});
  #     });
  #   });
  # ')),
  # Include the custom pure JavaScript code
  tags$script(HTML('
    // Add hover effect and enlarge chart on mouseover
    document.addEventListener("DOMContentLoaded", function() {
      var chart = document.getElementById("myChart");

      chart.addEventListener("mouseenter", function() {
        chart.style.transform = "scale(1.1)";
        chart.style.transition = "transform 0.3s";
      });

      chart.addEventListener("mouseleave", function() {
        chart.style.transform = "scale(1)";
        chart.style.transition = "transform 0.3s";
      });
    });
  ')),
  
  # tags$style(
  #   HTML('
  #     /* Add the custom CSS here */
  #     
  #   ')
  # ),
  
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("chartType", "Select Chart Type",
                    choices = c("line", "column", "bar"),
                    selected = "line"),
        checkboxInput("showBoth", "Show lastPeriod", FALSE),
        sliderInput(inputId="slider", label="Period", 
                    min = min(dataset$year_month),
                    max = max(dataset$year_month),
                    value = c(min(dataset$year_month), max(dataset$year_month)),
                    timeFormat="%b %Y")
      ),
      
      mainPanel(
          highchartOutput("myChart", width = "100%", height = "550px"),
          style = "max-width: 1000px;"  # Adjust the width of the main panel
      )
    )
  
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression for data based on slicer input and selected month range
  filteredData <- reactive({
    
    startdate = input$slider[1]
    enddate = input$slider[2]
    
    dataset %>%
      filter(year_month >= startdate,year_month<=enddate) %>%
      arrange(year) %>%
      mutate(month = month.name[as.numeric(month)])
  })
  
  observe({
    print("Print filteredData() ")
    print(filteredData())
  })
  
  output$myChart <- renderHighchart({
    hc <- highchart() %>%
      hc_chart(
        type = input$chartType,
        backgroundColor = "#fcfce8",
        zoomType = "xy"
      ) %>%
      hc_title(text = "UK Covid Data Visualization") %>%
      hc_xAxis(categories = unique(paste(filteredData()$year, filteredData()$month, sep = "-"))) %>%
      hc_yAxis(title = list(text = if (input$showBoth) "Count" else input$dataSlicer)) %>%
      hc_tooltip(
        crosshairs = TRUE,
        shared = TRUE,
        backgroundColor = "#000",
        style = list(color = "#fff"),
        useHTML = TRUE,  # Enable HTML in tooltip
        headerFormat = '<b>{point.x}</b><br>',
        pointFormat = '<span style="color:{point.color}">{series.name}</span>: <b>{point.y}</b><br/>'
      )
    
    if (input$showBoth) {
      hc <- hc %>%
        hc_add_series(
          name = "new cases",
          data = filteredData()$newCases,
          color = "#7cb5ec"  # Set color for the newCases series
        ) %>%
        hc_add_series(
          name = "new cases (last period)",
          data = filteredData()$newCases_lastPeriod,
          color = "#90ed7d",  # Set color for the newCases_lastPeriod series
          dashStyle = "LongDash"  # Set the dash pattern for the newCases_lastPeriod series
        )
    } else {
      hc <- hc %>%
        hc_add_series(
          name = "newCases",
          data = filteredData()$newCases,
          color = "#7cb5ec"  # Set color for the newCases series
        )
    }
    
    hc
  })
  
  
  
}


# Run the application
shinyApp(ui = ui, server = server)
