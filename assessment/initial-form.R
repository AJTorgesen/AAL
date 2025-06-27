initialFormUI <- function(id) {
  ns <- NS(id)
  tabPanelBody(value = ns("tab"),
               
               
               div(
                 class = "container-fluid",
                 style = "padding: 0;",
                 div(
                   class = "assessment-header",
                   
                   span(
                     "Please choose your grade below, then press continue.",
                     style = "text-align: center; display: flex; justify-content: center; font-size: 2rem;"
                   )
                 ),
                 div(
                   class = "assessment-main",
                   
                   
                   div(
                     class = "flex-container",
                     
                     
                     radioGroupButtons(
                       ns("grade"),
                       label = NULL,
                       choices = c(
                         "3rd" = "3",
                         "4th" = "4",
                         "5th" = "5",
                         "6th" = "6",
                         "7th" = "7",
                         "8th" = "8",
                         "9th" = "9",
                         "10th" = "10",
                         "11th" = "11",
                         "12th" = "12"
                       ),
                       size = "lg"
                       # checkIcon = list(
                       #   yes = icon("ok", lib = "glyphicon")
                       # )
                     )
                   ),
                   br(),
                   # div(
                   #   class = "flex-container",
                   # 
                   #   radioGroupButtons(
                   #     ns("season"),
                   #     label = NULL,
                   #     choices = c(
                   #       "Fall" = "fall",
                   #       "Winter" = "winter",
                   #       "Spring" = "spring"
                   #     ),
                   #     size = "lg"
                   #   )
                   # ),
                   
                   
                   #Test Version (Remove this for final product)
                   # div(
                   #   class = "flex-container",
                   #   radioButtons(
                   #     ns("se_req"),
                   #     label = "Standard Error Requirement (For Testing)",
                   #     choices = c(
                   #       "No Requirement (Testing Max Question Stop Rule)" = 0.01,
                   #       "Normal Requirement (Testing Standard Error Stop Rule)" = 0.22
                   #     )
                   #   )
                   #   
                   # ),
                   hr(),
                   br(),
                   div(style = "display: flex; justify-content: center;",
                       actionButton(ns("formcontinue"),
                                    "Continue"))
                   
                   
                 )
               ))
  
  
  
}

initialFormServer <- function(id, assessment_session) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$formcontinue,{
        
        form <- switch (input$grade,
          "4" = "assessment-fourth_form-tab",
          "5" = "assessment-fifth_form-tab",
          "6" = "assessment-sixth_form-tab",
          "7" = "assessment-seventh_form-tab",
          "8" = "assessment-eighth_form-tab"
        )
        
        updateTabsetPanel(session = assessment_session, "assessment_forms", selected = "assessment-fourth_form-tab")
        addClass(selector = ":root", class = "changebackground")
        runjs("window.scrollTo(0, document.querySelector('.navbar').offsetHeight)")
      })
      return(input)
    }
  )
}