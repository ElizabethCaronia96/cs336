library(shiny)
library(RMySQL)

options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root",
  "password" = "rootroot"
))
databaseName <- "agenda"
table1 <- "populate1"



saveData <- function(data) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table1, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

loadData <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table1)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}
# Define the fields we want to save from the form
fields <- c("`Food Group`", "Quantity", "Datetime")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    mainPanel(
      titlePanel("Hello Agenda!"),

                    textInput("`Food Group`", "Food Group", ""),
                  sliderInput("obs", "Number of observations:",
                  min = 0, max = 1000, value = 500),
                    dateInput("Datetime", label = "Date only rn", value = "2017-11-01", min =NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", width = NULL),
                    actionButton("submit", "Submit"),
                    DT::dataTableOutput("responses", width = 300), tags$hr()
                  )),
            
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    })   
    
    
    
    
  }
)