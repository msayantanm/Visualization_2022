library(shiny)
library(shinydashboard)
library(shiny)
library(tidyverse)
library(tidytext)
library(scales)
library(fuzzyjoin)
library(maps)
library(countrycode)
library(ggthemes)
library(bs4Dash)
library(fontawesome)
library(shinydashboardPlus)






df <- read.csv("/media/sayantan/misc/newfolder/visualization_2022/data/student-mat.csv")
df$TC <- df$Dalc + df$Walc


ui <- dashboardPage(
  dashboardHeader(
    title = "Student Alcohol Consumption",
    tags$li(class = "dropdown",
            tags$style(".main-header .navbar {margin-left: 350px}"),
            tags$style(".main-header {font-size:20px}")
    )
  ),
  dashboardSidebar(
    width = 350,
    tags$style(".main-header .navbar {margin-left: 350px}"),
    tags$style(".main-header .logo {width: 350px}"),
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("database"))
    ),
    br(),
    sidebarMenu(
      sidebarHeader("Causes and Relations"),
      menuItem("Alcohol Comsumption and Abscences for Male and Females", tabName = "ACAMF", icon = icon("chart-bar")),
      menuItem("Alcohol Consumption wrt Study Time and Parental Status", tabName = "ACSTPS", icon = icon("chart-bar")),
      menuItem("Final Grade wrt Family Relations and Relationship Status of the students", tabName = "FGFRRSS", icon = icon("chart-bar")),
      menuItem("Total Alcohol Consumption wrt Health Status and Father's Job", tabName = "TACHSFJ", icon = icon("chart-bar"))
    ),
    br()
  ),
  dashboardBody(
    tags$style("body {font-size: 20px}"),
    tabItems(
      tabItem(
        tabName = "intro",
        h1("Introduction", style = "font-size:42px"),
        p("In this project we study the relations and probable causes of Alcohol Consumption among Students. Alcohol is a widely and socially acceptable drug
        which we seem to forget has very much a negative impact in our lives. For developing kids and adolescents alcohol is one of the most dangerous and 
        harmful substance which harms them in every negative way."),
        p("We use a publicly available dataset to come up with some analysis to see how these factors affect.
              This is what we aim to address in this project."),
        br(),
        h2("Dataset:-"),
        tags$ul(
          tags$li(tags$b("Student Alcohol Consumption"), ": Daily and Weekly Alcohol Consumption among high school students and various factors like grades, Parental Status, 
                Health Condition, Study Time, etc"),
        ),
        br(),
        h2("Main variables that we will be looking at from the dataset:-"),
        tags$ul(
          tags$li(tags$b("Student Alcohol Consumption"), ": ", tags$code("Fedu","Fjob","studytime","internet","famrel","abscences","DALC","WALC")),
        ),
      ),
      tabItem(
        tabName = "ACAMF",
        h1("Alcohol Comsumption and Abscences for Male and Females"),
        plotOutput("ACAMF", height = "750px", width = "85%")
        
      ),
      tabItem(
        tabName = "ACSTPS",
        h1("Alcohol Consumption wrt study time and parental status"),
        plotOutput("ACSTPS", height = "750px", width = "85%")
      ),
      tabItem(
        tabName = "FGFRRSS",
        h1("Final Grade wrt Family Relations and Relationship Status of the students"),
        plotOutput("FGFRRSS", height = "750px", width = "85%")
      ),
      tabItem(
        tabName = "TACHSFJ",
        h1("Total Alcohol Consumption wrt Health Status and Father's Job"),
        plotOutput("TACHSFJ", height = "750px", width = "85%")
      )
    )
  )
)




server <- function(input, output){
  output$ACAMF <- renderPlot(ggplot(df) + geom_point(aes(Dalc, absences, color = internet)) + facet_grid(.~sex) + xlab("Alcohol Consumption") + ylab("Absences") + scale_color_discrete("Internet User")
  )
  
  
  output$ACSTPS <- renderPlot(ggplot(df[order(df$studytime),]) + geom_bar(aes(Dalc, fill = Pstatus)) + facet_wrap(studytime~., nrow = 2) + xlab("Daily Alchohol Consumption") + ylab("Number of Students") + scale_fill_discrete("Parents Status", labels = c("Apart","Together")) + ggtitle(("Daily Alchohol Consumption with respect to Number of Hours they are studying"))
  )
  
  output$FGFRRSS <- renderPlot(ggplot(df) + geom_point(aes(absences, G3, color = famrel)) + facet_wrap(goout~romantic) # + scale_color_discrete("Quality Time Spent with Family(units)")
                               
  )
  
  output$TACHSFJ <- renderPlot(ggplot(df, aes( TC)) + geom_bar(aes(fill = Fjob)) + facet_wrap(.~health)
  )
  
  
  
  
  
  
  
  
  
}
shinyApp(ui, server)