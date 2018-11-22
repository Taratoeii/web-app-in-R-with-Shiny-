library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(reshape2)

forestfires <- read.csv('data/forestfires.csv')
freqofday <- read.csv('data/area_day.csv')
freqofmonth <- read.csv('data/area_month.csv')
chifreqofday <- read.csv('data/area_day1.csv')
datacorrelation <- read.csv('data/forestfiresCR.csv')
titleread <- read.csv('data/titlehy.csv')
r = read.csv('data/rain_more0.csv')


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data Set", icon = icon("tree",lib = "font-awesome"), tabName = "dataset",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Overview", tabName = "Overview", icon = icon("eye",lib = "font-awesome")),
    menuItem("Probability", tabName = "freq", icon = icon("fire", lib="glyphicon")),
    menuItem("Correlation of Forestfires", tabName = "Cor", icon = icon("handshake",lib = "font-awesome")),
    menuItem("Hypothesis Testing", tabName = "hypothesis", icon = icon("tint", lib="font-awesome")),
    menuItem("Chi-Square goodness of fit test", tabName = "Chi", icon = icon("search", lib="font-awesome"))
  )
)

body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "Overview",
            fluidRow(
              valueBoxOutput("value1")
              ,valueBoxOutput("valueemtry")
              ,valueBoxOutput("value2")
            ),
            fluidRow(
              
              box(
                title = "Temp forestfires of month"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("Tempbymonth", height = "300px")
              )
              
              ,box(
                title = "Temp forestfires of Day"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("Tempbyday",height = "300px")
            
              ),
              valueBoxOutput("value3")
              ,valueBoxOutput("valueemtry1")
              ,valueBoxOutput("value4")
            ),
            fluidRow(
              
              box(
                title = "Wind forestfires of month"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("Windbymonth", height = "300px")
              )
              
              ,box(
                title = "Wind forestfires of Day"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("Windbyday",height = "300px")
              ), 
              valueBoxOutput("value5")
              ,valueBoxOutput("valueemtry2")
              ,valueBoxOutput("value6")
            ),
            fluidRow(
              
              box(
                title = "ISI forestfires of month"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("ISImonth", height = "300px")
              )
              
              ,box(
                title = "ISI forestfires of Day"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("ISIday",height = "300px")
              ), 
              valueBoxOutput("value7")
              ,valueBoxOutput("valueemtry3")
              ,valueBoxOutput("value8")
            ),
            fluidRow(
              
              box(
                title = "RH forestfires of month"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("RHmonth", height = "300px")
              )
              
              ,box(
                title = "RH forestfires of Day"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("RHday",height = "300px")
              )
            )
    
    ),
    
    # Second tab content
    tabItem(tabName = "dataset",
            fluidPage(
              titlePanel("Forest Fires Dataset"),
              
              # Create a new Row in the UI for selectInputs
              fluidRow(
                column(4,
                       selectInput("mon",
                                   "Month:",
                                   c("All","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
                        
                ),
                column(4,
                       selectInput("day",
                                   "Day:",
                                   c("All","mon","tue","wed","thu","fri","sat","sun"))
                )
              ),
              # Create a new row for the table.
              DT::dataTableOutput("table")
            )
  
            
    ),
    #Third tab content
    tabItem(tabName = "freq",
            fluidPage(
              titlePanel("Discrete Probability Distribution"),
            fluidRow(
              
              box(
                title = "Freq fire of month"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("freqbymonth", height = "300px")
              ),
              
              box(
                title = "Freq fire of day"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("freqbyday", height = "300px")
              )),
            fluidRow(
              
              box(
                title = "Statistical Probability of fire each Month"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("firebymonth", height = "300px")
          
              
              ),
              valueBoxOutput("value9"),
              valueBoxOutput("value10")
              )

            )
              
            ),

    # Forth tab content
    tabItem(tabName = "Cor",
            fluidPage(
              titlePanel("Correlation of Forestfires"),

              fluidRow(
                pageWithSidebar(
                  headerPanel(''),
                  sidebarPanel(
                    selectInput('xcorre', 'X Variable', names(datacorrelation)),
                    selectInput('ycorre', 'Y Variable', names(datacorrelation))
                  ),
                  box(title = "Correlation of Forestfires"
                      ,status = "primary"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,plotOutput('plotcorrelation',height = "470px",width = "540px")
                  )
                )
              ),
              fluidRow(
                valueBoxOutput("valuecor")
              )
            )
    ),
    tabItem(tabName = "Chi",
            fluidPage(
              
              # Application title
              titlePanel("Chi-Square goodness of fit test"),
              # Sidebar with a slider input for number of bins 
              fluidRow(
                column(width = 6,
                       box(
                         width = NULL, background = "aqua",
                         "H0 : The number of time the foresfires is the same each day."
                       )
                )),
              fluidRow(
                box(
                  title = "Select Alpha :", status = "warning", solidHeader = TRUE,
                  
                  sliderInput("alpha", "a", 1, 50, 5,step = 0.1)),
                column(width = 4,
                       box(
                         title = "Chi-Square Test", width = NULL, solidHeader = TRUE,status = "primary",
                         textOutput("n_var"),
                         textOutput("df_var"),
                         textOutput("chisq_var"),
                         textOutput("a_var"),
                         textOutput("v_var")
                       ))
              ),
              fluidRow(
                plotOutput("chisqPlot")
              ),
              br()
              ,
              fluidRow(
                plotOutput("oePlot")
              )
            )
            ),
    tabItem(tabName = "hypothesis",
            h2("Hypothesis Testing"),
            h2(" "),
            h2(" "),
            
            fluidPage(
              box(
                title = titleread[1,]
                ,status = "primary"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,plotOutput("hypothesisout",height = "450px")
              )
              
            )
            
    )
            
    )
  )


# Put them together into a dashboardPage
dashboardPage(
  skin="red",
  dashboardHeader(title = "Forest Fires"),
  sidebar,
  body
)
