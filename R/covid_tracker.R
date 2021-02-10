#' Daily covid tracker shiny
#'
#' @return
#' @export
#'
#' @examples
covid_tracker <- function(){

 # require(shiny)
  #   require(shiny)

  #load packages
 library(shiny)
  library(shinythemes)
  library(tidyverse)
# library(lubridate)
  library(zoo) #calculate moving average
  library(tidyr)
  library(glue)
  library(ggthemes)
  library(bslib)

  #theme for app
  # my_theme <- bs_theme(
  #   bg = "blue", fg = "#B8BCC2", primary = "#EA80FC",
  #   base_font = font_google("Open Sans"),
  #   "font-size-base" = "1.1rem"
  # )


  # load data

  covid <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
    select(-UID, - iso2, -iso3, -code3, -Admin2, -Country_Region, -Lat, -Long_,
           -Combined_Key)

  # Define UI for application that draws a histogram
  ui <- fluidPage(

    #theme = my_theme,

    # Application title
    titlePanel("Daily New COVID-19 Cases by State"),

    sidebarLayout(
      sidebarPanel(

        #select date
        dateRangeInput("date", strong("Date range"),
                       start = "2020-03-01", end = lubridate::today()-1,
                       min = "2020-01-24", max = lubridate::today()-1),

        selectInput("state", strong("Choose a state:"),
                    choices = unique(covid$Province_State),
                    multiple = TRUE,
                    selected = c("North Carolina", "Ohio")),

        textInput("text1", "Noun","dog"),
        textInput("text2", "Verb", "runs"),

        actionButton("submit", label = "Update Text"),

      ),


      # Show a plot of the generated distribution
      mainPanel(
        plotOutput(outputId = "dailyplot"),
        br(),
        tags$h1("This is a big header"),
        br(),
        tags$p("This is a paragraph of text"),
        # uiOutput(outputId = "cdcmessage")
        #span(textOutput(outputId = "cdcmessage"), style = "color:blue", align = "center")
        br(),
        textOutput("value")
      )
    )
  )

  server <- function(input, output) {

    # data for selected sates and date range
    covid_long <- reactive({
      covid %>%
        filter(Province_State %in% input$state) %>%
        pivot_longer(cols = c(-FIPS, -Province_State),
                     names_to = "date",
                     values_to = "total_cases") %>%
        mutate(date = mdy(date)) %>%
        group_by(Province_State, FIPS) %>%
        mutate(new_cases = total_cases - lag(total_cases, n = 1),
               change_new_cases = new_cases - lag(new_cases, n = 1)) %>%
        group_by(Province_State, date) %>%
        summarise(state_new_cases = sum(new_cases)) %>%
        mutate(week_avg = rollmean(state_new_cases, k = 7, fill = NA, align = "right"),
               change_new_cases =  state_new_cases - lag(state_new_cases, n = 1))
    })

    covid_state_range <- reactive({
      covid_long() %>%
        filter(date >= as_date(input$date[1]) &
                 date <= as_date(input$date[2]))
    })

    ## make daily plot
    state_title <- reactive({
      str_c(input$state, collapse = ", ")
    })

    output$dailyplot <- renderPlot({
      ggplot(data = covid_state_range(), aes(x = date, y = state_new_cases,
                                             fill = Province_State)) +
        geom_bar(stat = "identity", position = "dodge", alpha= 0.5) +
        geom_line(aes(y = week_avg, color = Province_State), size = 0.5) +
        labs(x = "Date",
             y = "New Cases",
             fill = "State",
             title = glue("Daily New Cases in {state_title()}"),
             subtitle = "+ 7-day average") +
        theme_fivethirtyeight() +
        guides(color = FALSE)

    })

    text_reactive <- eventReactive( input$submit, {
      paste("The ", input$text1, input$text2, sep = " ")
    }, ignoreNULL= FALSE)

    output$value <- renderText({
      text_reactive()
    })



    ## Figure out if 14-day trend criteria is met
    # trend_table <- reactive({
    #     covid_long() %>%
    #         filter(date >= (as_date(input$date[2]) - days(13)) & date <= as_date(input$date[2]))
    # })
    #
    # output$cdcmessage <- renderUI({
    #     criteria_met <- trend_table() %>%
    #         group_by(Province_State) %>%
    #         summarise(mean_cases = mean(change_new_cases)) %>%
    #         mutate(message = if_else(mean_cases < 0, paste0("can open! ", emo::ji("white_check_mark")),
    #                                  paste0("should stick with stay at home! ",
    #                                         emo::ji("stop_sign"))))
    #
    #     HTML(paste(criteria_met$Province_State, criteria_met$message, "<br>"))
    # })

  }

  # Run the application
  shinyApp(ui = ui, server = server)

}
