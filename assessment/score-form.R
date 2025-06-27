scoreFormUI <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tabPanelBody(value = ns("tab"),
               div(
                 class = "container-fluid",
                 style = "padding: 0;",
                 div(
                   class = "container-fluid",
                   style = "padding: 1rem; margin: auto; width: 80%;",
                   
                   div(
                     style = "float: right;",
                     # conditionalPanel(
                     #   "false", # always hide the download button
                     #   downloadButton(ns("downloadData"))
                     # ),
                     actionButton(ns("print"),
                                  "Print",
                                  class = "button-margins", width = "200px")
                   ),
                   div(
                     class = "scorereport",
                     div(
                       style = "margin-bottom: 2rem;",
                       h1("Fall Score Report", style = "display: flex; justify-content: center;")
                     ),
                     div(
                       style = "margin-bottom: 2rem;  float: left;",
                       
                       div(
                         class = "colfive",
                         style = "padding: 1rem; box-shadow: 7px 7px 20px 0px #00000073;",
                         h3("Probability of Reading Risk", style = "text-align: center;"),
                         uiOutput(ns("pr_num"))
                         #plotlyOutput(ns("pls_plot"))
                       ),
                       
                       
                       
                       div(
                         style = "padding: 1rem; ",
                         class = "colsix",
                         span(
                           strong("Probability of Reading Risk"),
                           " is based on how this student did on all three tasks."
                         ),
                         card(
                           full_screen = FALSE,
                           card_header(class = "bg-primary",
                                       "Understanding Your Score"),
                           card_body(uiOutput(ns("plr_text")))
                         )
                       )
                     ),
                     
                     
                     div(
                       style = "margin-bottom: 2rem;  float: left;",
                       
                       div(
                         class = "colfive",
                         style = "padding: 1rem; box-shadow: 7px 7px 20px 0px #00000073;",
                         h3("Individual Task Ability Scores", style = "text-align: center;"),
                         plotOutput(ns("task_plots"))#, tableOutput(ns("task_table"))
                         
                       ),
                       
                       
                       div(
                         style = "padding: 1rem;",
                         class = "colsix",
                         span(
                           strong("Individual Task Ability Scores"),
                           " shows where this student's reading strengths are and which parts they may need help with.
                                   ",
                           strong("Syntactic Knowledge"),
                           " shows what the student knows about how words are combined into sentences, while ",
                           strong("Vocabulary Knowledge"),
                           " shows what they know about what different words mean.",
                           strong("Word Recognition"),
                           " shows how well they can hear a word pronounced and know how it’s spelled."
                         ),
                         card(
                           full_screen = FALSE,
                           card_header(class = "bg-primary",
                                       "Understanding Your Score"),
                           card_body(
                             "Scores that land in the green highlighted area of the graphic, from 115 and above means this student is already skilled at these parts of reading, while a score in the blue highlighted area indicates an average performace in that task. A score in the pink highlighted area means there’s room for improvement for that task."
                           )
                         )
                       )
                       
                     ),
                     #Green section is average score.
                     
                     div(
                       style = "margin-bottom: 2rem; float: left;",
                       
                       div(
                         class = "colfive",
                         style = "padding: 1rem; box-shadow: 7px 7px 20px 0px #00000073;",
                         h3("Individual Task Percentiles", style = "text-align: center;"),
                         plotOutput(ns("task_plot"))
                       ),
                       
                       
                       div(
                         style = "padding: 1rem;",
                         class = "colsix",
                         span(
                           strong("Individual Task Percentile"),
                           " shows how well this student is reading compared to grade-based peers in the normative sample.
                                   "
                         ),
                         card(
                           full_screen = FALSE,
                           card_header(class = "bg-primary",
                                       "Understanding Your Score"),
                           card_body(
                             "A percentile score represents the percentage of scores that are equal to or less than a given score. For example, if a student has a percentile rank of 90, their score was at or higher than 90% of
                                      the scores in the norming data set, meaning they performed equal to or better than 90% of their grade-based peers."
                           )
                         )
                       )
                     )
                   ),
                   
                   
                   div(
                     style = "float:left; width: 100%; margin-top: 3rem; margin-bottom: 3rem; display: flex; justify-content: center;",
                     actionButton(
                       ns("resources_btn"),
                       label = "Additional Resources",
                       class = "button-margins",
                       width = "200px"
                     )
                   ),
                   br(),
                   div(
                     style = "float:left; width: 100%; margin-top: 3rem; margin-bottom: 3rem; display: flex; justify-content: center;",
                     actionButton(
                       ns("restart_btn"),
                       label = "Take Assessment Again",
                       class = "button-margins",
                       width = "200px"
                     )
                   ),
                   br(),
                   div(
                     style = "float: left;",
                     span(
                       "*All cutpoints for determining reading risk are based on fall scores."
                     )
                   )
                   
                 )
               ))
             
             
             #----
             # uiOutput(ns("pls_ui")),
             # div(
             #   class = "container-fluid",
             #   style = "padding: 1rem; margin: auto; width: 80%;",
             #   card(
             #     height = 550,
             #     full_screen = TRUE,
             #     card_header(
             #       "Your Overall Probability of Literary Success"
             #     ),
             #     plotOutput(ns("pls_plot"))
             #   )
             # ),
             # div(
             #   class = "container-fluid",
             #   style = "padding: 1rem; margin: auto; width: 80%;",
             #   layout_column_wrap(
             #     width = 1/2,
             # card(
             #   height = 550,
             #   full_screen = TRUE,
             #   card_header(
             #     "Individual Scores based on Task"
             #   ),
             #   plotOutput(ns("task_plots_as")),
             #   card_body(
             #     fill = FALSE, gap = 0,
             #     card_title("A subtitle"),
             #     p(class = "text-muted", "And a caption")
             #   )
             # ),
             # card(
             #   height = 550,
             #   full_screen = TRUE,
             #   card_header(
             #     "Individual Percentile Rank based on Task"
             #   ),
             #   plotOutput(ns("task_plots_p")),
             #   card_body(
             #     fill = FALSE, gap = 0,
             #     card_title("A subtitle"),
             #     p(class = "text-muted", "And a caption")
             #   )
             # )
             # 
             #   )
             # ),
             # div(
             #   class = "video-main",
             #   style = "width: 50%;",
             #   uiOutput(ns("videoUI"))
             # 
             # 
             # ),
             # div(
             # style = "margin: auto; width: 20%; display: flex; justify-content: center; margin-bottom: 5rem;",
             # actionButton(ns("resources_btn"),
             #              label = "Additional Resources",
             #              class = "button-margins", width = "200px")
             # )
             
             #----
           
  
}

scoreFormServer <- function(id, assessment_session, highest_session, initialForm_input, fourthForm_input, skt_finalTheta, vkt_finalTheta, wrt_finalTheta) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      confirmModal <- function(){
        
        modalDialog(title = "Are You Sure?",
                    span('If you press "yes" all current data will be lost. You will not be able to return to this page. Is that ok?', style = "font-size: 1.5rem;"),
                    footer = tagList(
                      modalButton("Cancel"),
                      actionButton(ns("confirm_btn"), "Yes")
                    ),
                    easyClose = FALSE
        )
      }
      
      gradeVal <- reactiveVal()
      
      observe({
        gradeVal(ifelse(as.numeric(initialForm_input$grade) < 11, initialForm_input$grade, "10"))
      })
      
      plr_table <- read.csv("PLR Table.csv")
      
      skt_perc_table <- read.csv("SKT percentiles.csv")
      vkt_perc_table <- read.csv("VKT percentiles.csv")
      wrt_perc_table <- read.csv("WRT percentiles.csv")
      
      #Fall
      skt_perc_table_fall <- skt_perc_table[,1:3]
      vkt_perc_table_fall <- vkt_perc_table[,1:3]
      wrt_perc_table_fall <- wrt_perc_table[,1:3]
      
      #Winter
      # skt_perc_table_winter <- skt_perc_table[,c(1,2,4)]
      # vkt_perc_table_winter <- vkt_perc_table[,c(1,2,4)]
      # wrt_perc_table_winter <- wrt_perc_table[,c(1,2,4)]
      
      #Spring
      # skt_perc_table_spring <- skt_perc_table[,c(1,2,5)]
      # vkt_perc_table_spring <- vkt_perc_table[,c(1,2,5)]
      # wrt_perc_table_spring <- wrt_perc_table[,c(1,2,5)]
      
      
      zscore_helper_table <- read.csv("AAL Task Theta Stats.csv")
      
      
      calc_plr <- function(skt, vkt, wrt) {
        plr_bygrade <- filter(plr_table, grade == gradeVal())
        B1 <- plr_bygrade$estimate[plr_bygrade$term == "(Intercept)"]
        B2 <- plr_bygrade$estimate[plr_bygrade$term == "syntheta"]
        B3 <- plr_bygrade$estimate[plr_bygrade$term == "vktheta"]
        B4 <- plr_bygrade$estimate[plr_bygrade$term == "wrtheta"]
        score <- (exp(B1 + (B2*skt) + (B3*vkt) + (B4*wrt))/(1 + exp(B1 + (B2*skt) + (B3*vkt) + (B4*wrt))))
        score <- score*100
        return(score)
        
        
      }
      
      calc_percentile <- function(theta, task, grade_input) {
        
          p_table <- switch (task,
                             "skt" = skt_perc_table_fall,
                             "vkt" = vkt_perc_table_fall,
                             "wrt" = wrt_perc_table_fall
          )
        
        
        
        p_table_bygrade <- filter(p_table, grade == grade_input)
        i = 1
        while (theta > p_table_bygrade[i,2]) {
          i <- i+1
        }
        if(i > 1)
        {
          i <- i-1
        }
        return(p_table_bygrade[i, 3]/100)
      }
      
      calc_ability_scores <- function(theta, task)
      {
        task_stats <- zscore_helper_table %>% filter(Grade == gradeVal() & Task == task)
        
        task_zscore <- (theta - task_stats$Mean)/task_stats$SD
        
        ability_score <- (task_zscore*15) + 100
        
        return(ability_score)
        
      }
      
      
      plr <- reactiveVal()
      
      percentiles <- reactiveValues(skt = NULL,
                                    vkt = NULL,
                                    wrt = NULL)
      
      ability_scores <- reactiveValues(skt = NULL,
                                       vkt = NULL,
                                       wrt = NULL)
      
      observeEvent(fourthForm_input$finalsubmit, {
        
        req(skt_finalTheta)
        req(vkt_finalTheta)
        req(wrt_finalTheta)
        
        print(skt_finalTheta)
        print(vkt_finalTheta)
        print(wrt_finalTheta)
        
        plr(calc_plr(skt_finalTheta(), vkt_finalTheta(), wrt_finalTheta()))
        
        percentiles$skt <- calc_percentile(skt_finalTheta(), "skt", gradeVal())
        percentiles$vkt <- calc_percentile(vkt_finalTheta(), "vkt", gradeVal())
        percentiles$wrt <- calc_percentile(wrt_finalTheta(), "wrt", gradeVal())
        
        ability_scores$skt <- calc_ability_scores(skt_finalTheta(), "skt")
        ability_scores$vkt <- calc_ability_scores(vkt_finalTheta(), "vkt")
        ability_scores$wrt <- calc_ability_scores(wrt_finalTheta(), "wrt")
        
        
      })
      
      observeEvent(input$resources_btn, {
        updateNavbarPage(session = highest_session, "navbar-aal", selected = "Resources-tab")
      })
      
      observeEvent(input$restart_btn, {
        showModal(confirmModal())
      })
      
      observeEvent(input$confirm_btn, {
        removeModal()
        session$reload()
      })
      
      ##### To add in vkt simply include ability_scores$vkt and percentiles$vkt to the respective lists below. #####
      taskData <- reactive({
        df <- data.frame(Task = c("Syntactic Knowledge","Vocabulary Knowledge","Word Recognition"), Score = c(ability_scores$skt, ability_scores$vkt, ability_scores$wrt), Percentile = c(percentiles$skt, percentiles$vkt, percentiles$wrt))
        return(df)
      })
      
      output$pr_num <- renderUI({
        h1(paste0(round(plr(), 0), "%"), style = "font-size: 7rem; text-align: center;")
      })
      
      output$plr_text <- renderUI({
        cutpoint <- switch (initialForm_input$grade,
                         "3" = 0.4089604,
                         "4" = 0.3889223,
                         "5" = 0.4121221,
                         "6" = 0.3797906,
                         "7" = 0.3925424,
                         "8" = 0.4195261,
                         "9" = 0.3293159,
                         "10" = 0.3519461,
                         "11" = 0.3519461,
                         "12" = 0.3519461
        )
        if(plr() >= cutpoint*100)
        {
          span("Based on this probability of risk score this student ", strong("is not likely to achieve "), "at or above the 40th percentile on an end of school year, nationally-normed, measure of reading comprehension.*")
        }
        else
        {
          span("Based on this probability of risk score this student ", strong("is likely to achieve "), "at or above the 40th percentile on an end of school year, nationally-normed, measure of reading comprehension.*")
        }
        
        # span(strong("A low probability"), " (closer to 1%) means you are not at risk and are very likely to be successful in reading with your current
        #                amount of help.", strong("A high probability"), " (closer to 99%) means you may need extra help so you can understand and enjoy what you read.")
      })
      
      #cutpoint <- 50
      
      # output$pls_ui <- renderUI({
      #   if(pls() >= cutpoint)
      #   {
      #   div(
      #     style = "padding: 1rem; margin: auto; width: 80%;",
      #     layout_columns(
      #       col_widths = c(3,9),
      #       
      #     
      #     value_box(
      #       title = "Your Score:",
      #       value = round(pls(),0),
      #       showcase = bs_icon("graph-up-arrow"),
      #       span("Not at risk for reading difficulties"),
      #       showcase_layout = showcase_top_right(),
      #       theme_color = "success"
      #       
      #     ),
      #     #h3(paste(format(round(pls(),0))), style = " color: lightgreen !important; float: left; margin-right: 1rem;"),
      #     span(paste("Your score of", format(round(pls(),0)), "on the AAL is based on: item difficulty and number of correct answers. The scale below shows a 0-100 range, reflecting
      #                the probability of literacy success. Your score shows you are not at risk for reading difficulties. Please watch the video below for information on how to
      #                interpret your score."), style = "")
      #   )
      #    
      #   )
      #   }
      #   else
      #   {
      #     div(
      #       style = "padding: 1rem; margin: auto; width: 80%;",
      #       layout_columns(
      #         col_widths = c(3,9),
      #       value_box(
      #         title = "Your Score:",
      #         value = round(pls(),0),
      #         showcase = bs_icon("graph-down-arrow"),
      #         span("At risk for reading difficulties"),
      #         showcase_layout = showcase_top_right(),
      #         theme_color = "danger"
      #         
      #       ),
      # 
      #              #h3(paste(format(round(pls(),0))), style = " color: red !important; float: left; margin-right: 1rem;"),
      #              span(paste("Your score of", format(round(pls(),0)), "on the AAL is based on: item difficulty and number of correct answers. The scale below shows a 0-100 range, reflecting
      #                the probability of literacy success. Your score shows you are at risk for reading difficulties. Please watch the video below for information on how to
      #                interpret your score."), style = "")
      #       )
      #       
      #     )
      #   }
      # })
      
      # output$videoUI <- renderUI({
      #   if(pls() >= cutpoint)
      #   {
      #     img(src = "success_video.png", style = "width: 100%;")
      #   }
      #   else
      #   {
      #     img(src = "atrisk_video.png", style = "width: 100%;")
      #   }
      # })
      
      
      
      # draw_gauge_chart <- function(my_raw_value){
      #   h = 0
      #   k = 0
      #   r = .5
      #   r2 = .24
      #   r3 = .25
      #   
      #   
      #   theta = pi*0.01*my_raw_value
      #   x1 = h + (-1*r*cos(theta))
      #   y1 = k + r*sin(theta)
      #   
      #   
      #   x2 = h + (-1*r2*cos(theta))
      #   y2 = k + r2*sin(theta)
      #   
      #   
      #   plot_ly(
      #     # width = 700,
      #     # height = 700,
      #     
      #     type = "indicator",
      #     mode = "gauge+number",
      #     value = my_raw_value,
      #     number = list(
      #       # font = list(
      #       #   size = 75
      #       # ),
      #       suffix = "%"),    # <- Specify `prefix` or `suffix`
      #     gauge = list(
      #       axis = list(
      #         range = list(NULL,100),
      #         showticklabels = FALSE,
      #         ticks = ""),
      #       bar = list(
      #         color = "#78c2ad",
      #         thickness = 1),
      #       bgcolor = "white", 
      #       borderwidth = 2,
      #       bordercolor = "gray",
      #       threshold = list(
      #         line = list(color = "#f3969a", width = 10),
      #         thickness = 1,
      #         value = my_raw_value)
      #     )) %>%
      #     layout(
            # xaxis = list(
            #   range = c(-1,1),
            #   #fixedrange = TRUE,
            #   constrain = "range",
            #   # showgrid = FALSE,
            #   # showticklabels = FALSE,
            #   # zeroline = FALSE,
            #   scaleanchor= "y"
            # ),
            # yaxis = list(
            #   range = c(0,.67),
            #   #fixedrange = TRUE,
            #   constrain = "range",
            #   # showgrid = FALSE,
            #   # showticklabels = FALSE,
            #   # zeroline = FALSE,
            #   scaleanchor= "x"
            # ),
            # annotations = list(
            #   list(
            #     ax = x2,
            #     ay = y2,
            #     axref = "x",
            #     ayref = "y",
            #     x = x1,
            #     y = y1,
            #     xref = "x",
            #     yref = "y",
            #     showarrow= TRUE,
            #     arrowhead=2,
            #     arrowsize=0.7,
            #     arrowwidth=12,
            #     arrowcolor="#f3969a"
            #   )
            # ),
            
            # shapes = list(
            #   list(
            #     type = 'line',
            #     line = list(
            #       color = "#f3969a",
            #       width = 10
            #     ),
            #     x0 = x2,
            #     y0 = y2,
            #     x1 = x1,
            #     y1 = y1,
            #     xref = "x",
            #     yref = "y"
            #     
            #     
            #   )
            # ),
            
      #       margin = list(l = 20, r = 30),
      #       font = list(color = "#78c2ad", family = "Arial"))
      # }
      
      # output$pls_plot <- renderPlotly({
      #   print(paste("Pls:",round(pls(),0)))
      #   draw_gauge_chart(round(pls(),0))
        
        #----
        # df <- tibble::tribble(
        #   ~y, ~grp,
        #   50,  "At Risk",
        #   50,  "Not at Risk"
        #   
        # )
        # 
        # h_line <- round(pls(),0)
        # ggplot(data = df, aes(x = "")) +
        #   geom_col(aes(y = y, fill = grp), position = position_stack(reverse = TRUE)) +
        #   geom_hline(aes(yintercept = h_line), linewidth = 2) +
        #   geom_text(aes(1.7, h_line, label = paste("Your Score:", h_line), vjust = 1.4, hjust = 1.05), size = 10) +
        #   scale_fill_manual(values=c('red', 'darkgreen')) +
        #   ylim(0,100) +
        #   labs(x = "",y = "Probability of Literary Success") +
        #   coord_flip() +
        #   theme_classic() +
        #   theme(legend.position="bottom",legend.title = element_blank())
      #})
      
      # create_beautiful_radarchart <- function(data, color = "#78c2ad",
      #                                         vlabels = colnames(data), vlcex = 1.2,
      #                                         caxislabels = NULL, title = NULL, ...){
      #   radarchart(
      #     data, axistype = 0,
      #     pty = c(32,32,16),
      #     # Customize the polygon
      #     #pcol = color, pfcol = scales::alpha(color, 0.5),
      #     pcol = c("#78c2ad", "#f3969a", "black"), pfcol = c("#78c2ad", "#f3969a", NA),
      #     plwd = 2, plty = c(0,0,1),
      #     # Customize the grid
      #     cglcol = "grey", cglty = 1, cglwd = 0.8,
      #     # Customize the axis
      #     axislabcol = "darkgray",
      #     # Variable labels
      #     vlcex = vlcex, vlabels = vlabels,
      #     caxislabels = NULL, title = title, ...
      #   )
      # }
      
      
      # output$task_plots_as <- renderPlot({
      #   df <- data.frame(rbind(rep(130, 3), rep(0, 3), rep(130, 3), rep(40, 3),
      #                          c(ability_scores$skt, ability_scores$vkt, ability_scores$wrt)))
      #   colnames(df) <- c("SKT","VKT","WRT")
      #   op <- par(mar = c(0, 0, 1, 0))
      #   create_beautiful_radarchart(df, caxislabels = c(0, 25, 50, 75, 130))
      #   par(op)
      #   
      #   
      #   
      # })
      
      output$task_plots <- renderPlot({
        df <- taskData()
        df$NScore <- c(NA,NA,NA)
        
        for (i in 1:3) {
          if(df$Score[i] < 55)
          {
            df$NScore[i] <- 55
          }
          else
          {
            if(df$Score[i] > 145)
            {
              df$NScore[i] <- 145
            }
            else
            {
              df$NScore[i] <- df$Score[i]
            }
          }
          
        }
        
        
        ggplot(df, aes(Task, NScore)) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
          
          annotate("rect", xmin = -Inf, xmax = Inf, ymin = 45, ymax = 85, alpha = 0.2, fill = "#f3969a") +
          annotate("rect", xmin = -Inf, xmax = Inf, ymin = 85, ymax = 115, alpha = 0.2, fill = "lightblue") +
          annotate("rect", xmin = -Inf, xmax = Inf, ymin = 115, ymax = 155, alpha = 0.2, fill = "#78c2ad") +
          geom_bar(stat = "identity", width = 0.5, fill = "#78c2ad") +
          geom_text(aes(label = format(round(Score))), nudge_y = 5) +
          scale_y_continuous(breaks = c(55,100,145), labels = c("55 and Less", "100", "145 and Greater")) +
          coord_cartesian(ylim = c(55,150))
       
      },
      alt = reactive({paste0("A bar chart displaying the scores of each individual task the student received on the assessment. The bar showing Syntactic reading knowledge has a score of ", round(ability_scores$skt), ". The bar showing vocabulary knowledge has a score of ", round(ability_scores$vkt),
                             ". The bar showing word recognition ability has a score of ", round(ability_scores$wrt), ". There is a pink shaded region of the chart that indicates risk for difficulties for any score between 55 or lower and 85. There is also a light purple shaded region indicating success
                             in an individual task for any score between 115 and a score greater than 145.")})
      )
      
      # output$task_table_as <- renderTable({
      #   df <- data.frame(Task = c("Syntactic Knowledge","Vocabulary Knowledge","Word Reading"), `Ability Score` = c(ability_scores$skt, ability_scores$vkt, ability_scores$wrt))
      #   df
      # })
      # 
      # output$task_table <- renderTable({
      #   df <- data.frame(Task = c("Syntactic Knowledge","Vocabulary Knowledge","Word Reading"), `Ability Score` = c(ability_scores$skt, ability_scores$vkt, ability_scores$wrt))
      #   df
      # })
      
      #----
      # output$task_plots_as <- renderPlot({
      #   ggplot(taskData(), aes(Task, Score)) +
      #     theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
      #                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
      #     geom_bar(stat = "identity", width = 0.5, fill = "#1c75bc") +
      #     ylim(0,1000) +
      #     geom_text(aes(label = format(round(Score))), nudge_y = 25)
      # })
      
      percentile <- function(perc) {
        return(ordinal(perc*100))
      }
      
      # output$task_plots_p <- renderPlot({
      #   
      #   
      #   skt_perc <- percentiles$skt
      #   vkt_perc <- percentiles$vkt
      #   wrt_perc <- percentiles$wrt
      #   
      #   print(skt_perc)
      #   print(vkt_perc)
      #   print(wrt_perc)
      #   
      #   skt <- ggplot(data.frame(x = c(-3, 3)), aes(x)) +
      #     stat_function(fun = dnorm,
      #                   geom = "area",
      #                   fill = "#78c2ad",
      #                   xlim = c(-3, qnorm(skt_perc))) +
      #     stat_function(fun = dnorm,
      #                   geom = "line",
      #                   xlim = c(qnorm(skt_perc), 3)) +
      #     xlim(-3, 3) +
      #     theme_void()
      #   
      #   
      #   vkt <- ggplot(data.frame(x = c(-3, 3)), aes(x)) +
      #     stat_function(fun = dnorm,
      #                   geom = "area",
      #                   fill = "#78c2ad",
      #                   xlim = c(-3, qnorm(vkt_perc))) +
      #     stat_function(fun = dnorm,
      #                   geom = "line",
      #                   xlim = c(qnorm(vkt_perc), 3)) +
      #     xlim(-3, 3) +
      #     theme_void()
      #   
      #   wrt <- ggplot(data.frame(x = c(-3, 3)), aes(x)) +
      #     stat_function(fun = dnorm,
      #                   geom = "area",
      #                   fill = "#78c2ad",
      #                   xlim = c(-3, qnorm(wrt_perc))) +
      #     stat_function(fun = dnorm,
      #                   geom = "line",
      #                   xlim = c(qnorm(wrt_perc), 3)) +
      #     xlim(-3, 3) +
      #     theme_void()
      #   
      #   ggarrange(skt, vkt, wrt, labels = c(paste("SKT:", percentile(skt_perc), "Percentile"), paste("VKT:", percentile(vkt_perc), "Percentile"), paste("WRT:", percentile(wrt_perc), "Percentile")),
      #             ncol = 1, nrow = 3, label.x = 0.5, vjust = 3, hjust = -0.7, font.label = list(size = 18, color = "darkgray", face = "plain", family = NULL))
      #   
      # })
      
      observeEvent(input$print, {
        screenshot(selector = ".scorereport", filename = paste("AAL-ScoreReport-", Sys.Date(), sep = ""))
        #click("downloadData")
        #runjs("$('#downloadData')[0].click();") # DOWNLOAD BUTTON
        #print("for sure here... but are we...")
      })
      
      # output$downloadData <- downloadHandler(
      #   
      #   filename = function(){paste("AAL-ScoreReport-", Sys.Date(), ".pdf", sep="")},
      #   content = function(file){
      #     print("getting here?")
      #     file.copy(from = input$shinyscreenshot,
      #               to = file)
      #     pdf(file)
      #   }
      # )
      
      
      #----
      output$task_plot <- renderPlot({
        ggplot(taskData(), aes(Task, Percentile*100)) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
          geom_bar(stat = "identity", width = 0.5, fill = "#78c2ad") +
          scale_y_continuous(breaks = c(0,20,40,60,80,100), labels = c("1","20","40","60","80","99")) +
          coord_flip(ylim = c(0,110)) +
          geom_text(aes(label = Percentile*100), nudge_y = 3) +
          ylab("Percentile")
      },
      alt = reactive({paste0("A horizontal bar chart displaying the percentile rank of each individual task the student received on the assessment. The bar showing Syntactic reading knowledge has a percentile rank of ", percentiles$skt*100, ". The bar showing vocabulary knowledge has a percentile rank of ", percentiles$vkt*100,
                             ". The bar showing word recognition ability has a percentile rank of ", percentiles$wrt*100, ".")})
      )
      
      
      
      

    }
  )
}

