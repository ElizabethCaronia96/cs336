library(shiny)
library(RMySQL)
library(timevis)
#cs 336 git
options(mysql = list(
  "host" = "cs336.cnnbxidvx4vb.us-west-2.rds.amazonaws.com",#"agenda.c4tlyckz1zml.us-east-1.rds.amazonaws.com", #cs336.cnnbxidvx4vb.us-west-2.rds.amazonaws.com", #
  "port" = 3306,
  "user" = "root", #root
  "password" = "rootroot"
))
databaseName <- "agenda" #agenda
table1 <- "food"
table2 <- "exercise"
table3 <- "finsta"
table4 <- "Trains"
table5 <- "Shows"
table6 <- "Posts"
table7 <- "history"
Pid6 <- "4975M6"
Pid4 <- "Hamburger"


pattern1 <- function(table) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  
  # Construct the fetching query
  query <- sprintf("select avg(l1.Likes) > avg(l2.Likes)  from finsta l1, finsta l2 where l1.Sponsored = 1 and l2.Sponsored = 0")
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

groupChoice <- function(column, table){
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  query <- sprintf(
    "select distinct %s from %s",
    paste(column),
    paste(table)
  )
  choices <- dbGetQuery(db, query)
  print.AsIs(choices)
  dbDisconnect(db)
  return (choices)
}

loadData <- function(query) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    titlePanel("Hello Again!"),
    tabsetPanel(type="tabs",
                tabPanel("Entities", mainPanel(
                  uiOutput("food_expense"),
                  DT::dataTableOutput("responses1", width = 500), tags$hr(),
                  headerPanel("Find where people have skied!"),
                  DT::dataTableOutput("responses2", width = 300), tags$hr(),
                  headerPanel("Use the up down arrows to find the least liked post!"),
                  DT::dataTableOutput("responses3", width = 300), tags$hr()
                  
                )),
                tabPanel("Relationships", mainPanel(
                  uiOutput("pid_choices"),
                  textInput("Pid4", "After eating a Hamburger, what is a person's average heartrate when working out?"),
                  actionButton("submit4", "Submit"),
                  DT::dataTableOutput("responses4", width = 300), tags$hr(),
                  textInput("Pid5", "Given a finsta user, how much do they spend on food ($/g)"),
                  actionButton("submit5", "Submit"),
                  DT::dataTableOutput("responses5", width = 300), tags$hr(),
                  textInput("Pid6", "given a poster, find the average length of their workouts"),
                  actionButton("submit6", "Submit"),
                  DT::dataTableOutput("responses6", width = 300), tags$hr()
                  
                  
                )),
                tabPanel("Patterns/ Updates", mainPanel(
                  titlePanel("Patterns:"),
                  #headerPanel("sponsored posts should make more likes on average"),
                  textAreaInput("thisOne", "sponsored posts should make more likes on average than unsponsored","select * from finsta where exists(select avg(l1.Likes) < avg(l2.Likes)  from finsta l1, finsta l2 where l1.Sponsored = 1 and l2.Sponsored = 0)", cols = 10, rows = 5),
                  DT::dataTableOutput("responses7", width = 300), tags$hr(),
                  textAreaInput("thisOne", "Healthy food is more expensive than fatty food, so anything with less than 600 calories is more expensive than anything above 600 calories","select * from food where price < any( select price from food where Calories > 600) and Calories < 600;
)", cols = 10, rows = 5),
                  DT::dataTableOutput("responses11", width = 300), tags$hr(),
                  titlePanel("Updates:"),
                  #headerPanel("sponsored posts should make more likes on average"),
                  textAreaInput("thisOne", "Let's say it doesn't make sense to buy Choripan from club disco.  Change it to be pizza first, check the food table:"," select Name, Location from food where Name = 'Choripan';", cols = 10, rows = 5),
                  DT::dataTableOutput("responses8", width = 300), tags$hr(),
                  textAreaInput("thisOne", "Check what the original Trains table is ","select Name, Type from Trains where Name = 'Choripan';", cols = 1, rows = 5),
                  DT::dataTableOutput("responses9", width = 300), tags$hr(),
                  textAreaInput("thisOne", "Check what the original Shows table is","select Name, Pid from Shows where Name = 'Choripan';", cols = 1, rows = 5),
                  DT::dataTableOutput("responses10", width = 300), tags$hr(),
                  
                  
                  textAreaInput("thisOne", "Check that the food table changed"," update food set Name = 'Pizza' where Name = 'Choripan';04:24:22	select Name, Location from food where Name = 'Choripan'	1 row(s) returned	0.000 sec / 0.000 sec", cols = 10, rows = 5)
                 
                  
                ))
                
                       
   
  )),
  server = function(input, output, session) {
    output$food_expense <- renderUI({
      selectInput("food_expense", "See the pricest menu item for each location", as.list(test1()))
    })
    test1 <- reactive({
      
      groupChoice("Location", table1)
    })
    
   
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses1 <- DT::renderDataTable({
      input$submit
      query <- sprintf("SELECT Location, max(price) FROM %s group by Location", paste(table1)) #, paste(input$food_expense)
      loadData(query)
    })     
  
    output$responses2 <- DT::renderDataTable({
      input$submit
      query <- sprintf("SELECT * FROM %s where Type = 'Skiing'",paste(table2))
      loadData(query)
    })  
    
    output$responses3 <- DT::renderDataTable({
      input$submit
      query <- sprintf("SELECT Pid, sum(likes) FROM %s group by Pid order by sum(likes) asc limit 5", paste(table3))
      loadData(query)
    }) 
    
    output$responses4 <- DT::renderDataTable({
      input$submit
      query <- sprintf("select avg(HeartRate) from exercise e where e.Type in (select t.Type from Trains t where t.Name = '%s')", paste(Pid4))
      loadData(query)
    }) 
    
    output$responses5 <- DT::renderDataTable({
      input$submit
      query <- sprintf("select sum(Price)/ sum(Grams) from food f where f.Name in (select t.Name from Shows t where t.Pid = '%s')", paste(Pid6));
      
      loadData(query)
    })  
    
    output$responses6 <- DT::renderDataTable({
      input$submit6
      
      query <- sprintf("select Pid, avg(Duration) from Posts p, exercise e where p.Pid = '%s' and e.Type = p.Type group by p.Pid", paste(Pid6))
      loadData(query)
      
    })  
    
    output$responses7 <- DT::renderDataTable({
      input$submit7
      
      query <- sprintf("select * from finsta where exists(select sum(l1.Likes) < sum(l2.Likes)  from finsta l1, finsta l2 where group by l1.Sponsored)")
      loadData(query)
      
    })  
    
    output$responses8 <- DT::renderDataTable({
      input$submit8
      
      query <- sprintf("select Name, Location from food where Name = 'Choripan'")
      loadData(query)
      
    })  
    output$responses9 <- DT::renderDataTable({
      input$submit9
      
      query <- sprintf("select Name, Type from Trains where Name = 'Choripan'")
      loadData(query)
      
    })  
    output$responses10 <- DT::renderDataTable({
      input$submit10
      
      query <- sprintf("select Name, Pid from Shows where Name = 'Choripan'")
      loadData(query)
      
    })
    
    output$responses11 <- DT::renderDataTable({
      input$submit11
      
      query <- sprintf("select * from food where price < any( select price from food where Calories > 600) and Calories < 600")
      loadData(query)
      
    })
    
  

  }
)