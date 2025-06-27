aalHomeUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = uiOutput(ns("home_tab_title")), value = ns("tab"),
           tabsetPanel(
             id = ns("home_forms"),
             type = "hidden",
             tabPanelBody(value = ns("main"),
             div(
               class = "container-fluid",
               style = "padding: 0;",
               id = "welcome-box",
               div(
                 class = "text-default bg-primary bkrd-height",
                 style = "background-size: cover; margin-bottom:0; background-image:url('Home-Background-Large.png'); padding: 2rem;",
                 div(
                   class = "home-text-container",
                   
                   h1(
                     class = "aal-bold",
                     id = "titletext",
                     "Welcome",
                     style = "margin: 0px; display: flex; justify-content: center; font-size: 4.5rem;"
                   ),
                   #Test Version (Remove for final product)
                   # h3("Test Version",
                   #    style = "margin: 0px; display: flex; justify-content: center;"),
                   br(),
                   div(
                     style = "font-size: large; text-align: center;",
                     
                     span(
                       'Welcome to the Adolescent Assessment of Literacy or AAL! This assessment has been
                        specially developed as a screener to assess literacy skills of students from the 3rd to the 12th grade in the fall. For a concise summary of the AAL please press the button below. When you are ready,
                       watch the video below then press "Get Started" to begin the assessment. Please ensure a stable wifi connection for the duration of the assessment. Thank you!'
                     ),
                     br(),
                     actionButton(ns("summary_button"),
                                  "Summary", class = "button-margins", width = "200px",
                                  onclick ="window.open('AAL_Summary.pdf', '_blank')")
                   ),
                   
                   hr(),
                   br(),
                   div(
                     class = "video-main",
                     style = "width: 100%; !important;",
                     div(class = "video-container2",
                         HTML('<iframe class="ytvideo" src="https://www.youtube.com/embed/YHMOAMqNuD8?si=kpdv906NaMkBAadI?rel=0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                     ),
                     
                   ),
                   br(),
                   actionButton(ns("getstarted"),
                                "Get Started",
                                style = "display: flex; margin:auto;")
                 )
               )
             )
             ),
             aalAssessmentUI("assessment")
             
           )
           
          
                 
   
  )
}

aaLHomeServer <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      home_title <- reactiveVal("Home")
      
      # observeEvent(input$stopvideo, {
      #   addClass(selector = ".ytvideo", class = "hide-video")
      # })
      
      observeEvent(input$getstarted,{
      
      updateNavbarPage(session = session, "home_forms", selected = "assessment-tab")
      addClass(selector = ".navbar-inverse", class = "hidenav")
      addClass(selector = ".tab-content", class = "top-margin")
      
      home_title("Score")
      })
      
      output$home_tab_title <- renderText({
        home_title()
      })
      
    }
  )
}
