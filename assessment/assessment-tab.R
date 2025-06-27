source("assessment/initial-form.R")
source("assessment/fourth-form.R")
source("assessment/SKT.R")
source("assessment/score-form.R")


aalAssessmentUI <- function(id) {
  ns <- NS(id)
  tabPanel("Assessment",
           value = ns("tab"),
           tabsetPanel(
             id = ns("assessment_forms"),
             type = "hidden",
             initialFormUI(ns("initial_form")),
             scoreFormUI(ns("score_form")),
             fourthFormUI(ns("fourth_form")),
             
             SKTUI(ns("skt_form"))
           ))
}
aaLAssessmentServer <- function(id, parent_session) {
  moduleServer(id,
               function(input, output, session) {
                 
                 skt_finalTheta <- reactiveVal(NULL)
                 vkt_finalTheta <- reactiveVal(NULL)
                 wrt_finalTheta <- reactiveVal(NULL)
                 
                 initialForm_input <- initialFormServer("initial_form", assessment_session = session)
                 fourthForm_input <- fourthFormServer("fourth_form", assessment_session = session, initialForm_input, skt_finalTheta, vkt_finalTheta, wrt_finalTheta)
                 scoreForm_input <- scoreFormServer("score_form", assessment_session = session, highest_session = parent_session, initialForm_input, fourthForm_input, skt_finalTheta, vkt_finalTheta, wrt_finalTheta)
                 SKTServer("skt_form", assessment_session = session, initialForm_input)
               })
}
