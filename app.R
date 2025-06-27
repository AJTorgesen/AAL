library(shiny)
library(ggplot2)
library(shinyjs)
library(bslib)
library(magrittr)
library(dplyr)
library(Rfast)
library(shinyWidgets)
library(lorem)
library(bsicons)
library(scales)
library(ggpubr)
library(fmsb)
library(plotly)
library(shinyscreenshot)
library(stringr)

#template
source("header.R")
source("footer.R")
source("home/home-tab.R")
source("assessment/assessment-tab.R")
source("downloads/downloads-tab.R")
source("resources/resources-tab.R")
source("about/learn-more.R")


js <- '
$("#home-getstarted").click(function(){
var leg=$(".ytvideo").attr("src");
$(".ytvideo").attr("src",leg);
});

$("#assessment-fourth_form-page_back").click(function(){
var leg=$(".ytvideo").attr("src");
$(".ytvideo").attr("src",leg);
});


$("#assessment-fourth_form-page_next").click(function(){
var leg=$(".ytvideo").attr("src");
$(".ytvideo").attr("src",leg);
});


'

theme_test <- bs_theme(bootswatch = "minty", secondary = "#1b8381", success = "darkgreen", danger = "red") %>%
  bs_add_rules(sass::sass_file("style.scss"))

ui <- fluidPage(
  useShinyjs(),
  tags$head(includeHTML(("www/google-analytics.html"))),
  tags$head(includeHTML(("www/google-analytics-2.html"))),
  style = "padding: 0;",
  #aalHeader,
  navbarPage(
    title = NULL, #replace this with NCIL Logo
    id = "navbar-aal",
    theme = theme_test,
    position = "static-top",
    header = NULL,
    footer = NULL,
    windowTitle = "AAL",
    inverse = TRUE,
      aalHomeUI("home"),
    #aalAssessmentUI("assessment"),
    # aalScoreUI("score"),
    aalResourceUI("Resources"),
    aalLearnMoreUI("Learn More")
    
             
             
    ),
  tags$script(HTML(js))
  )



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #bs_themer()
  #Go over namespace
  aaLHomeServer("home", parent_session = session)
  aaLAssessmentServer("assessment", parent_session = session)
  aaLLearnMoreServer("Learn More", parent_session = session)
  
    
}

# Run the appli?cation 
shinyApp(ui = ui, server = server)
