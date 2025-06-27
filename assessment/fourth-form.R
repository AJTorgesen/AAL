

fourthFormUI <- function(id) {
  
 
  ns <- NS(id)
  tabPanelBody(
    value = ns("tab"),
    div(style = "position: relative; min-height: 90vh; background: lightgray;",
        div(style = "padding-bottom: 5rem;",
    tabsetPanel(
      id = ns("fourth_forms"),
      type = "hidden",
      # header = tagList(
      #     div(
      #       class = "assessment-info",
      #       uiOutput(ns("Grade_Title")),
      # 
      #     )
      # ),
      # #Page Video----
      # tabPanelBody(
      #   value = ns("IntroPage"),
      #   uiOutput(ns("Intro_Page"))
      # ),
      #Page Video----
      tabPanelBody(
        value = ns("VideoPage"),
        uiOutput(ns("Video_Page"))
      ),
      #Page 1----
      tabPanelBody(
        value = ns("page1"),
        uiOutput(ns("SKT_Question"))
      ),
      #Page 2----
      
      tabPanelBody(value = ns("page2"),
        uiOutput(ns("VKT_Question"))
      ),
      #Page 3----
      tabPanelBody(value = ns("page3"),
        uiOutput(ns("WRT_Question"))
      )
      
     
    ),
    div(
      style = "margin: auto; width: 20%; display: flex; justify-content: center;",
      actionButton(ns("page_next"),
                   "Next", class = "button-margins", width = "200px")
    )),
    div(
      class = "assessment-info",
      uiOutput(ns("Grade_Title")),
      
    )
    )
  )
  
}

fourthFormServer <- function(id, assessment_session, initialForm_input, skt_finalTheta, vkt_finalTheta, wrt_finalTheta) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      task <- reactiveVal("skt")
      
      isIntro <- reactiveVal(TRUE)
      
      isVideo <- reactiveVal(TRUE)
      
      
      
      
      output$Grade_Title <- renderUI({

        grade <- switch (initialForm_input$grade,
                        "3" = "Third",
                        "4" = "Fourth",
                        "5" = "Fifth",
                        "6" = "Sixth",
                        "7" = "Seventh",
                        "8" = "Eighth",
                        "9" = "Nineth",
                        "10" = "Tenth",
                        "11" = "Eleventh",
                        "12" = "Twelfth"
        )
        card(
        h4(paste("Grade: ", initialForm_input$grade)),
        h4(paste("Task: ", toupper(task()))),
        #Test Version (Remove for final product)
        # h4(paste("Question Number:", question_num())),
        # h4(paste("Standard Error:", round(standard_error(),3))),
        # h4(paste0("Current SE Req: >", initialForm_input$se_req))
        )
      })
      
      submitModal <- function(){
      
      modalDialog(title = "End of Current Task",
                  span(paste("You have completed the ", toupper(task()), " Task of the AAL. Please press 'Next' to go to the next Task"), style = "font-size: 1.5rem;"),
                  footer = tagList(
                    
                    actionButton(ns("submit"), "Next")
                  ),
                  easyClose = FALSE
                  )
      }
      
      submitModal2 <- function(){
        
        modalDialog(title = "END OF ASSESSMENT",
                    img(src = "stop.jpg", style = "width: 100%;"),
                    span(paste("Please get your caregiver or teacher to review and discuss your assessment result with you, then press proceed."), style = "font-size: 1.5rem;"),
                    footer = tagList(
                      
                      actionButton(ns("finalsubmit"), "Proceed")
                    ),
                    size = "l",
                    easyClose = FALSE
        )
      }
      
      skt_ItemBank <- read.csv("SKT.csv")
      vkt_ItemBank <- read.csv("VKT.csv")
      wrt_ItemBank <- read.csv("WRT.csv")
      
      
      ####Comment above and uncomment below to add in sample questions when publishing.######
      # skt_ItemBank <- read.csv("SKT_w_sample.csv")
      # vkt_ItemBank <- read.csv("VKT_w_sample.csv")
      # wrt_ItemBank <- read.csv("WRT_w_sample.csv")
      #item Bank columns
      #1: ID
      #2: Previous Text
      #3: Post Text
      #4: Choice 1
      #5: Choice 2
      #6: Choice 3
      #7: Correct Answer
      #8: Param A
      #9: Param B
      #10: Audio
      
      
     
      gradeVal <- reactiveVal()
     
      observe({
        gradeVal(ifelse(as.numeric(initialForm_input$grade) < 11, initialForm_input$grade, "10"))
      })
      
      skt <- reactiveValues(current = 1,
                            itemBank = skt_ItemBank,
                             previousList = vector(),
                             response = as.numeric(rep(NA, length(skt_ItemBank[,1]))),
                             theta = 0,
                             standard_error = NA,
                             itemParams = data.frame("A" = skt_ItemBank[,8], "B" = skt_ItemBank[,9], "C" = 0, "D" = 1)
      )
      vkt <- reactiveValues(current = 1,
                            itemBank = vkt_ItemBank,
                            previousList = vector(),
                            response = as.numeric(rep(NA, length(vkt_ItemBank[,1]))),
                            theta = 0,
                            standard_error = NA,
                            itemParams = data.frame("A" = vkt_ItemBank[,8], "B" = vkt_ItemBank[,9], "C" = 0, "D" = 1)
      )
      wrt <- reactiveValues(current = 1,
                            itemBank = wrt_ItemBank,
                            previousList = vector(),
                            response = as.numeric(rep(NA, length(wrt_ItemBank[,1]))),
                            theta = 0,
                            standard_error = NA,
                            itemParams = data.frame("A" = wrt_ItemBank[,8], "B" = wrt_ItemBank[,9], "C" = 0, "D" = 1)
      )
      
      final_score <- reactiveVal()
      
      #Test Version (Remove for final product) -------
      # standard_error <- reactiveVal(NA)
      # question_num <- reactiveVal(NA)
      # observe({
      #   if(task() == "skt")
      #   {
      #     
      #     standard_error(skt$standard_error)
      #     question_num(length(skt$previousList)+1)
      #     
      #   }
      #   else if(task() == "vkt")
      #   {
      #     standard_error(vkt$standard_error)
      #     question_num(length(vkt$previousList)+1)
      #   }
      #   else if(task() == "wrt")
      #   {
      #     standard_error(wrt$standard_error)
      #     question_num(length(wrt$previousList)+1)
      #   }
      # })
      #-----------------------------------------------
      observe({
        df <- filter(skt_ItemBank, grepl(paste0("g", gradeVal()), skt_ItemBank[,1]))
        df <- df[1:5,]
        df2 <- rbind(df, skt_ItemBank)
        df3 <- df2%>%distinct()
        row.names(df3) <- NULL
        skt$itemBank <- df3
        skt$reponse <- as.numeric(rep(NA, length(df3[,1])))
        skt$itemParams <- data.frame("A" = df3[,8], "B" = df3[,9], "C" = 0, "D" = 1)
      })
      
      observe({
        df <- filter(vkt_ItemBank, grepl(paste0("g", gradeVal()), vkt_ItemBank[,1]))
        df <- df[1:5,]
        df2 <- rbind(df, vkt_ItemBank)
        df3 <- df2%>%distinct()
        row.names(df3) <- NULL
        vkt$itemBank <- df3

        vkt$reponse <- as.numeric(rep(NA, length(df3[,1])))
        vkt$itemParams <- data.frame("A" = df3[,8], "B" = df3[,9], "C" = 0, "D" = 1)
        
      })
      
      observe({
        df <- filter(wrt_ItemBank, grepl(paste0("G", gradeVal()), wrt_ItemBank[,1]))
        df <- df[1:5,]
        df2 <- rbind(df, wrt_ItemBank)
        df3 <- df2%>%distinct()
        row.names(df3) <- NULL
        wrt$itemBank <- df3
        
        wrt$reponse <- as.numeric(rep(NA, length(df3[,1])))
        wrt$itemParams <- data.frame("A" = df3[,8], "B" = df3[,9], "C" = 0, "D" = 1)

      })
      
      # output$Intro_Page <- renderUI({
      #   ns <- session$ns
      #   
      #     div(
      #       class = "container-fluid",
      #       style = "padding: 0;",
      #       span(style = "display: flex; justify-content: center; font-size: x-large; padding-top: 2rem;",
      #            "Please watch the following video that explains what to expect from the Adolescent Assessment of Literacy. After you have completed the video please press next."
      #       ),
      #       br(),
      #       hr(),
      #       br(),
      #       div(
      #         class = "video-main",
      #         style = "width: 50%; !important;",
      #        
      #         img(src = "intro_video.png", style = "width: 100%;"),
      #         
      #         
      #       ),
      #       br(),
      #       div(
      #         style = "margin: auto; display: flex; justify-content: center;",
      #       actionButton(ns("page_back"),
      #                    "Back", class = "button-margins", width = "200px")
      #       )
      #       
      #     )
      #     
      # })
      
      output$Video_Page <- renderUI({
        ns <- session$ns
        if(task() == "skt")
        {
        div(
          class = "container-fluid",
          style = "padding: 0;",
          span(style = "display: flex; justify-content: center; font-size: x-large; padding-top: 2rem;",
               "Please watch the following video that explains how to take the Syntactic Knowledge Task (SKT) portion of the AAL. After you have completed the video please press next."
          ),
          br(),
          hr(),
          br(),
          div(
            class = "video-main",
            style = "width: 50%; !important;",
            div(class = "video-container2",
                HTML('<iframe class="ytvideo" src="https://www.youtube.com/embed/7COYBNigwa0?si=1pjqwUnV2Lroxa1P?rel=0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
            ),
            
          ),
                br(),
                div(
                  style = "margin: auto; display: flex; justify-content: center;",
                actionButton(ns("page_back"),
                             "Back", class = "button-margins", width = "200px")
                )
          
        )
        }
        else if(task() == "vkt")
        {
          div(
            class = "container-fluid",
            style = "padding: 0;",
            span(style = "display: flex; justify-content: center; font-size: x-large; padding-top: 2rem;",
                 "Please watch the following video that explains how to take the Vocabulary Knowledge Task (VKT) portion of the AAL. After you have completed the video please press next."
            ),
            br(),
            hr(),
            br(),
            div(
              class = "video-main",
              style = "width: 50%; !important;",
              div(class = "video-container2",
                  HTML('<iframe class="ytvideo" src="https://www.youtube.com/embed/Rl0gtsZWO7w?si=skfnq3vODEPUG-98?rel=0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
              )
              
            )
          )
        }
        else if(task() == "wrt")
        {
          div(
            class = "container-fluid",
            style = "padding: 0;",
            span(style = "display: flex; justify-content: center; font-size: x-large; padding-top: 2rem;",
                 "Please watch the following video that explains how to take the Word Recognition Task (WRT) portion of the AAL. After you have completed the video please press next."
            ),
            br(),
            hr(),
            br(),
            div(
              class = "video-main",
              style = "width: 50%; !important;",
              div(class = "video-container2",
                  HTML('<iframe class="ytvideo" src="https://www.youtube.com/embed/pdXDeHYDSK0?si=CHlkOZTv_J2PRGBd?rel=0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
              )
              
            )
            
          )
        }
      })
      
      output$SKT_Question <- renderUI({
        ns <- session$ns
        
        div(
          class = "container-fluid",
          style = "padding: 0; background: lightgray;",
          div(
            class = "assessment-main",
            
            actionLink(ns("playAudio"), "Click for Audio Assist", icon = icon("play", lib = "font-awesome"), style = "font-size: 3rem;"),
            span(style = "display: flex; justify-content: center; font-size: 2.5rem;",
              paste(skt$itemBank[skt$current,2], "_______", skt$itemBank[skt$current,3] )
            ),
            radioButtons(ns("skt_answer"),
                        NULL,
                        choiceNames = c(paste("a)",skt$itemBank[skt$current,4]), paste("b)",skt$itemBank[skt$current,5]), paste("c)",skt$itemBank[skt$current,6])),
                        choiceValues = c(skt$itemBank[skt$current,4], skt$itemBank[skt$current,5], skt$itemBank[skt$current,6])
                        
                        )
            
          )
          
        )
      })
      
      output$VKT_Question <- renderUI({
        ns <- session$ns
        
        div(
          class = "container-fluid",
          style = "padding: 0;",
          div(
            class = "assessment-main",
            
            span(style = "display: flex; justify-content: center; font-size: 2.5rem;",
              paste(vkt$itemBank[vkt$current,2], "_______", vkt$itemBank[vkt$current,3] )
            ),
            radioButtons(ns("vkt_answer"),
                         NULL,
                         choiceNames = c(paste("a)",vkt$itemBank[vkt$current,4]), paste("b)",vkt$itemBank[vkt$current,5]), paste("c)",vkt$itemBank[vkt$current,6])),
                         choiceValues = c(vkt$itemBank[vkt$current,4], vkt$itemBank[vkt$current,5], vkt$itemBank[vkt$current,6])
            )
          )
        )
      })
      
      output$WRT_Question <- renderUI({
        ns <- session$ns
        
        div(
          class = "container-fluid",
          style = "padding: 0;",
          div(
            class = "assessment-main",
            actionLink(ns("playAudio2"), "Click to Play Item", icon = icon("play", lib = "font-awesome"), style = "font-size: 3rem;"),
            
            radioButtons(ns("wrt_answer"),
                         NULL,
                         choiceNames = c(paste("a)",wrt$itemBank[wrt$current,4]), paste("b)",wrt$itemBank[wrt$current,5]), paste("c)",wrt$itemBank[wrt$current,6])),
                         choiceValues = c(wrt$itemBank[wrt$current,4], wrt$itemBank[wrt$current,5], wrt$itemBank[wrt$current,6])
                         )
          )
        )
      })
      
      audioCounter <- reactiveVal(0)
      
      observeEvent(input$playAudio, {
        
        if(audioCounter() < 3)
        {
          audioCounter(audioCounter() + 1)
          if (task() == "skt")
          {
            insertUI(
              selector = "#assessment-fourth_form-playAudio",
              where = "afterEnd",
              ui = tags$audio(
                src = paste0("skt/", skt$itemBank[skt$current, 10], ".mp3"),
                
                type = "audio/mp3",
                autoplay = TRUE,
                controls = NA,
                style = "display:none;"
              ),
              immediate = TRUE
            )
            
          }
        }
      })
      
      observeEvent(input$playAudio2, {
        enable("page_next")
        if(audioCounter() < 3)
        {
          audioCounter(audioCounter() + 1)
          if (task() == "wrt")
          {
            insertUI(
              selector = "#assessment-fourth_form-playAudio",
              where = "afterEnd",
              ui = tags$audio(
                src = paste0("wrt/", wrt$itemBank[wrt$current, 10], ".mp3"),
                type = "audio/mp3",
                autoplay = TRUE,
                controls = NA,
                style = "display:none;"
              ),
              immediate = TRUE
            )
            
          }
        }
      })
      
      evaluate <- function(items, answer, itemBank, itemParams) {
        items$response[items$current] <- ifelse(str_trim(answer) == str_trim(itemBank[items$current,7]), 1, 0) #determine if item is correct
        
        items$previousList <- append(items$previousList,items$current) #add current item to previous item list
        
        if(length(items$previousList) < 5) #if this is one of the first items
        {
          items$current <- items$current + 1 #go to the next item 
        }
        else
        {
          if(sum(items$response, na.rm = TRUE) == 0)
          {
            if(length(items$previousList) < 8)
            {
              x <- 1
              y <- length(items$previousList)
              while (x %in% items$previousList) {
                x <- Rfast::nth(itemBank[,9], y-4, index.return = TRUE)
                y <- y+1
              }
              items$current <- x
            }
            else
            {
              items$theta <- -5.0
              if(task() == "wrt")
              {
                wrt_finalTheta(as.numeric(items$theta))
                showModal(submitModal2())
              }
              else
              {
                showModal(submitModal())
              }
            }
          } 
          else if(sum(items$response, na.rm = TRUE) == length(items$previousList))
          {
            if(length(items$previousList) < 8)
            {
              x <- 1
              y <- length(items$previousList)
              while (x %in% items$previousList) {
                x <- Rfast::nth(itemBank[,9], y-4, descending = TRUE, index.return = TRUE)
                y <- y+1
              }
              items$current <- x
            }
            else
            {
              items$theta <- 5.0
              if(task() == "wrt")
              {
                wrt_finalTheta(as.numeric(items$theta))
                showModal(submitModal2())
              }
              else
              {
                showModal(submitModal())
              }
            }
            
          }
          else
          {
            items$theta <- thetaEst(itemParams, items$response, method = "ML") #estimate theta
            items$standard_error <- semTheta(items$theta, itemParams, x = items$response, method = "ML")
            #Change this line for stoppage  rules
            #Test Version (Change this to 0.22 for final product)
            #initialForm_input$se_req
            if(items$standard_error > 0.22 && length(items$previousList) < 30)
            {
              
              x <- nextItem(itemParams, theta = items$theta, out = as.vector(items$previousList)) #determine next item x
              
              items$current <- x$item #set item$current() to x
            }
            else
            {
              if(task() == "wrt")
              {
                wrt_finalTheta(as.numeric(wrt$theta))
                
                showModal(submitModal2())
              }
              else
              {
                showModal(submitModal())
              }
            }
          }
          
          
        }
      }
      
      observeEvent(input$page_back,{
        updateTabsetPanel(session = assessment_session, "assessment_forms", selected = "assessment-initial_form-tab")
      })
      
      observeEvent(input$page_next, {
        # if(isIntro())
        # {
        #   isIntro(FALSE)
        #   updateTabsetPanel(session = session, "fourth_forms", selected = "assessment-fourth_form-VideoPage")
        #   runjs("window.scrollTo(0, document.querySelector('.navbar').offsetHeight)")
        # }
        # else
        # {
        if(isVideo()){
          if(task() == "skt")
          {
            updateTabsetPanel(session = session, "fourth_forms", selected = "assessment-fourth_form-page1")
            runjs("window.scrollTo(0, document.querySelector('.navbar').offsetHeight)")
            disable("page_next")
            
            delay(1000, enable("page_next"))
          }
          else if(task() == "vkt")
          {
            updateTabsetPanel(session = session, "fourth_forms", selected = "assessment-fourth_form-page2")
            runjs("window.scrollTo(0, document.querySelector('.navbar').offsetHeight)")
            disable("page_next")
            
            delay(1000, enable("page_next"))
          }
          else if(task() == "wrt")
          {
            updateTabsetPanel(session = session, "fourth_forms", selected = "assessment-fourth_form-page3")
            runjs("window.scrollTo(0, document.querySelector('.navbar').offsetHeight)")
            disable("page_next")
            
          }
          isVideo(FALSE)
        }
        else {
          
          if (task() == "skt")
          {
            
            
            audioCounter(0)
            evaluate(
              items = skt,
              answer = input$skt_answer,
              itemBank = skt$itemBank,
              itemParams = skt$itemParams
            )
            disable("page_next")
            
            delay(1000, enable("page_next"))
            removeUI(
              selector = "div audio",
              multiple = TRUE
            )
            #print(paste("SKT Theta:", skt$theta))
            #print(paste("SKT Standard Error:", skt$standard_error))
            #print(paste("Current Question", skt$current))
          }
          else if (task() == "vkt")
          {
            
            
            evaluate(
              items = vkt,
              answer = input$vkt_answer,
              itemBank = vkt$itemBank,
              itemParams = vkt$itemParams
            )
            disable("page_next")
            
            delay(1000, enable("page_next"))
            #print(paste("VKT Theta:", vkt$theta))
            #print(paste("VKT Standard Error:", vkt$standard_error))
            #print(paste("Current Question", vkt$current))
            
          }
          else if (task() == "wrt")
          {
            disable("page_next")
            audioCounter(0)
            evaluate(
              items = wrt,
              answer = input$wrt_answer,
              itemBank = wrt$itemBank,
              itemParams = wrt$itemParams
            )
            removeUI(
              selector = "div audio",
              multiple = TRUE
            )
          }
        }
        #}
        
      })
      
      
      
      
    
      
     
      
      observeEvent(input$submit,{
        removeModal()
        isVideo(TRUE)
        if(task() == "skt")
        {
          task("vkt")
          skt_finalTheta(as.numeric(skt$theta))

          updateTabsetPanel(session = session, "fourth_forms", selected = "assessment-fourth_form-VideoPage")
          runjs("window.scrollTo(0, document.querySelector('.navbar').offsetHeight)")
        }
        else if(task() == "vkt")
        {
          task("wrt")
          vkt_finalTheta(as.numeric(vkt$theta))

          updateTabsetPanel(session = session, "fourth_forms", selected = "assessment-fourth_form-VideoPage")
          runjs("window.scrollTo(0, document.querySelector('.navbar').offsetHeight)")
        }


      })
      ####  Without VKT #####
      # observeEvent(input$submit,{
      #   removeModal()
      #   isVideo(TRUE)
      #   if(task() == "skt")
      #   {
      #     task("wrt")
      #     skt_finalTheta(as.numeric(skt$theta))
      #     
      #     updateTabsetPanel(session = session, "fourth_forms", selected = "assessment-fourth_form-VideoPage")
      #     runjs("window.scrollTo(0, document.querySelector('.navbar').offsetHeight)")
      #   }
      # })
      
      observeEvent(input$finalsubmit,{
        removeModal()
        
        updateTabsetPanel(session = assessment_session, "assessment_forms", selected = "assessment-score_form-tab")
        removeClass(selector = ".navbar-inverse", class = "hidenav")
        removeClass(selector = ":root", class = "changebackground")
        removeClass(selector = ".tab-content", class = "top-margin")
      })
      
      return(input)
    }
  )
}