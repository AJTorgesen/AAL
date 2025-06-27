aalResourceUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Resources", value = ns("tab"), 
           div(
             class = "container-fluid",
             style = "padding: 0;",
             br(),
             h1("Resources", style = "display: flex; justify-content: center;")
             
             
           ),
           
           div(
             class = "container-fluid",
             style = "padding: 1rem; width: 80%; margin-left: 1rem;",
             h1("Parent Centers"),
             hr(),
             div(style = "margin-left: 2rem; margin-bottom: 6rem;",
             layout_column_wrap(
               width = 1/4,
               
             card(
               
               card_header(tags$a(href = "https://fcrr.org/", strong("Florida Center for Reading Research"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                   "FCRR is dedicated to providing parents, caregivers, and families with resources to support their children’s reading and success in school."
                 )
               )
             ),
             card(
               
               card_header(tags$a(href = "https://intensiveintervention.org/", strong("National Center on Intensive Intervention"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                   "The mission of NCII is to support intervention for students with severe and persistent learning and/or social, emotional, or behavioral needs."
                 )
               )
             ),
             card(
               
               card_header(tags$a(href = "https://www.parentcenterhub.org/find-your-center/", strong("Center for Parent Information & Resources"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                   "CPIR is a central hub for the network of Parent Centers serving families of children with disabilities."
                 )
               )
             )
             )
             )
           ),
           
           div(
             class = "container-fluid",
             style = "padding: 1rem; width: 80%; margin-left: 1rem;",
             h1("Grades 3-5"),
             hr(),
             div(style = "margin-left: 2rem; margin-bottom: 6rem;",
             h2("Teachers"),
             layout_column_wrap(
               width = 1/4,
               
             card(
               
               card_header(tags$a(href =  "https://ies.ed.gov/ncee/wwc/Docs/PracticeGuide/english_learners_pg_040114.pdf", strong("Teaching Academic Content and Literacy to English Learners in Elementary and Middle School"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "This guide describes how to teach academic content and literacy to English learners in elementary and middle school."
                 )
               )
             ),
             card(
               
               card_header(tags$a(href =  "https://ies.ed.gov/ncee/wwc/Docs/PracticeGuide/adlit_pg_082608.pdf", strong("Improving Adolescent Literacy: Effective Classroom and Intervention Practices"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "This guide describes how to provide effective literacy instruction for adolescent learners."
                 )
               )
             ),
             
             card(
               
               card_header(tags$a(href =  "https://ies.ed.gov/ncee/wwc/Docs/PracticeGuide/WWC-practice-guide-reading-intervention-full-text.pdf", strong("Providing Reading Interventions for Students in Grades 4–9"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "This practice guide provides four evidence-based recommendations that teachers can use to deliver reading interventions to meet the needs of their students."
                 )
               )
             ),
             
             card(
               
               card_header(tags$a(href =  "https://iris.peabody.vanderbilt.edu/module/pals26/challenge/#content", strong("PALS: A Reading Strategy for Grades 2–6"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "This module outlines the benefits of implementing PALS for Grades 2–6, a peer tutoring strategy in which students work in pairs to strengthen their reading skills. Also included are step-by-step instructions for each of the three PALS activities as well as printable PALS materials"
                 )
               )
             ),
             card(
               
               card_header(tags$a(href =  "https://ies.ed.gov/ncee/edlabs/regions/southeast/pdf/REL_2021086.pdf", strong("A Third-Grade Teacher's Guide to Supporting Family Involvement in Foundational Reading Skills"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "The information in this Third Grade Teacher's Guide is designed to assist teachers in supporting out-of-school literacy activities that are aligned to classroom instruction, informed by student need, grounded in evidence-based practices, and facilitated by ongoing parent-teacher communication. The Teacher's Guide provides a framework for literacy support activities presented during schools' family literacy nights and parent-teacher conferences."
                 )
               )
             )
             
           ),
           h2("Parents and Families"),
           layout_column_wrap(
             width = 1/4,
             
             
             card(
               
               card_header(tags$a(href =  "https://ies.ed.gov/ncee/edlabs/infographics/pdf/REL_SE_Literacy_Tips_for_Parents_of_Adolescents.pdf", strong("Literacy Tips for Parents of Adolescents"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "Parents of adolescents can initiate at home to help ensure successful literacy outcomes for their adolescent students."
                 )
               )
             ),
             
             card(
               
               card_header(tags$a(href =  "https://d2tic4wvo1iusb.cloudfront.net/documents/guidance-for-teachers/covid-19/Reading_with_TRUST_comic.pdf", strong("Helping Home Learning: Reading with TRUST Comic"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "Reading opportunities are everywhere. Follow the TRUST ideas to talk about anything your child enjoys reading."
                 )
               )
             ),
             card(
               
               card_header(tags$a(href =  "https://ride.ri.gov/sites/g/files/xkgbur806/files/IAC/StructuredLiteracy/GamesToSupportEarlyLiteracySkills-Sept2020.pdf", strong("Games to Support Early Literacy"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "The Rhode Island Department of Education (RIDE) has assembled this resource of games and activities that can be played at home to help boost early reading skills. There are games for pre-readers, early readers, and older students to develop skills to improve reading and comprehension."
                 )
               )
             ),
             card(
               
               card_header(tags$a(href =  "https://www.greatschools.org/gk/grades/3rd-grade/", strong("Great Schools Reading Videos for 3rd Grade"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "These videos by Great Schools give examples of third graders reading smoothly and successfully pronouncing tricky words to help you further indentify where your child should be for their grade level."
                 )
               )
             ),
             card(
               
               card_header(tags$a(href =  "https://www.greatschools.org/gk/grades/4th-grade/", strong("Great Schools Reading Videos for 4th Grade"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "These videos by Great Schools give examples of fourth graders reading smoothly and successfully pronouncing tricky words. There are also videos showing 4th graders comprehending what they are reading and learning from it. All of these videos will help you further indentify where your child should be for their grade level."
                 )
               )
             ),
             card(
               
               card_header(tags$a(href =  "https://www.greatschools.org/gk/grades/4th-grade/", strong("Great Schools Reading Videos for 5th Grade"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "These videos by Great Schools give examples of fifth graders reading smoothly and successfully pronouncing tricky words. There are also videos showing 4th graders comprehending what they are reading and learning from it. All of these videos will help you further indentify where your child should be for their grade level."
                 )
               )
             )
           )
           ),
           h1("Grades 6-8"),
           hr(),
           div(style = "margin-left: 2rem; margin-bottom: 6rem;",
           h2("Teachers"),
           layout_column_wrap(
             width = 1/4,
           
             card(
               
               card_header(tags$a(href =  "https://ies.ed.gov/ncee/wwc/Docs/PracticeGuide/english_learners_pg_040114.pdf", strong("Teaching Academic Content and Literacy to English Learners in Elementary and Middle School"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "This guide describes how to teach academic content and literacy to English learners in elementary and middle school."
                 )
               )
             ),
             
             card(
               
               card_header(tags$a(href =  "https://ies.ed.gov/ncee/wwc/Docs/PracticeGuide/adlit_pg_082608.pdf", strong("Improving Adolescent Literacy: Effective Classroom and Intervention Practices"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "This guide describes how to provide effective literacy instruction for adolescent learners."
                 )
               )
             ),
             card(
               
               card_header(tags$a(href =  "https://meadowscenter.org/wp-content/uploads/2022/04/ResourceMenu1.pdf", strong("Adolescent Literacy Resource Menu: A Guide for Instructional Leaders"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "This guide is organized by commonly experienced challenges (e.g., lack of modeling) to teachers implementing evidence-based adolescent literacy practices with fidelity."
                 )
               )
             ),
             
             card(
               
               card_header(tags$a(href =  "https://ies.ed.gov/ncee/wwc/Docs/PracticeGuide/WWC-practice-guide-reading-intervention-full-text.pdf", strong("What Works Clearinghouse Practice Guide: Providing Reading Interventions for Students in Grades 4-9"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "This practice guide provides four evidence-based recommendations that teachers can use to deliver reading interventions to meet the needs of their students."
                 )
               )
             ),
             
             card(
               
               card_header(tags$a(href =  "https://dwwlibrary.wested.org/resources/940", strong("Content Area Vocabulary: Activities Packet"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "View a resource packet illustrating activities and materials used by Pocomoke Middle School staff to teach vocabulary in the content areas, including examples from social studies, math, science, and art classrooms."
                 )
               )
             )
             
           ),
           
           h2("Parents and Families"),
           layout_column_wrap(
             width = 1/4,
             
             card(
               
               card_header(tags$a(href =  "https://ies.ed.gov/ncee/edlabs/infographics/pdf/REL_SE_Literacy_Tips_for_Parents_of_Adolescents.pdf", strong("Literacy Tips for Parents of Adolescents"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "Parents of adolescents can initiate at home to help ensure successful literacy outcomes for their adolescent students."
                 )
               )
             ),
             
             card(
               
               card_header(tags$a(href =  "https://d2tic4wvo1iusb.cloudfront.net/documents/guidance-for-teachers/covid-19/Reading_with_TRUST_comic.pdf", strong("Helping Home Learning: Reading with TRUST Comic"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "Reading opportunities are everywhere. Follow the TRUST ideas to talk about anything your child enjoys reading."
                 )
               )
             ),
             
             card(
               
               card_header(tags$a(href =  "https://ies.ed.gov/ncee/edlabs/infographics/pdf/REL_PA_How_Can_I_Help_My_Teen_with_Their_Reading_Skills_in_Different_Subjects.pdf", strong("How Can I Help My Teen With Their Reading Skills in Different Subjects?"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "Parents and caregivers can use this resource to support their teenager's reading skills."
                 )
               )
             ),
             
             card(
               
               card_header(tags$a(href =  "https://familyvoices.org/wp-content/uploads/2019/04/Advocating_HSMS.pdf", strong("Advocating for Yourself in Middle School and High School"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "This resource explains how to get what you need as a middle and high school student."
                 )
               )
             ),
             
             card(
               
               card_header(tags$a(href =  "https://meadowscenter.org/wp-content/uploads/2022/04/10Keys_Secondary_Web1.pdf", strong("10 Key Reading Practices for All Middle and High Schools"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "This handbook describes ten reading practices for middle and high schools with strong evidence of effectiveness from high-quality research, including selected grade level descriptions of what students should know and be able to do."
                 )
               )
             ),
             
             card(
               
               card_header(tags$a(href =  "https://www.understood.org/en/articles/5-things-your-middle-schooler-with-dyslexia-can-say-to-self-advocate", strong("5 Self-Advocacy Sentence Starters for Middle-Schoolers With Dyslexia"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "Learn how to build self-advocacy skills to help you get what they need. Rehearsing common situations can help you know where to start."
                 )
               )
             ),
             card(
               
               card_header(tags$a(href =  "https://www.greatschools.org/gk/levels/middle-school/", strong("Great Schools Reading Videos for Middle School"))),
               
               card_body(
                 fill = FALSE,
                 min_height = 200,
                 max_height = 300,
                 p(
                    
                   "What are the most important skills for your middle schooler to master? Great Schools worked with the experts to help you understand the key middle school skills, so you and your child can be ready for what’s next."
                 )
               )
             )
             
           )
           
           
           
           ),
           h1("Grades 9-12"),
           hr(),
           div(style = "margin-left: 2rem; margin-bottom: 6rem;",
               h2("Teachers"),
               layout_column_wrap(
                 width = 1/4,
                 
                 
                 
                 card(
                   
                   card_header(tags$a(href =  "https://ies.ed.gov/ncee/wwc/Docs/PracticeGuide/adlit_pg_082608.pdf", strong("Improving Adolescent Literacy: Effective Classroom and Intervention Practices"))),
                   
                   card_body(
                     fill = FALSE,
                     min_height = 200,
                     max_height = 300,
                     p(
                        
                       "This guide describes how to provide effective literacy instruction for adolescent learners."
                     )
                   )
                 ),
                 card(
                   
                   card_header(tags$a(href =  "https://meadowscenter.org/wp-content/uploads/2022/04/ResourceMenu1.pdf", strong("Adolescent Literacy Resource Menu: A Guide for Instructional Leaders"))),
                   
                   card_body(
                     fill = FALSE,
                     min_height = 200,
                     max_height = 300,
                     p(
                        
                       "This guide is organized by commonly experienced challenges (e.g., lack of modeling) to teachers implementing evidence-based adolescent literacy practices with fidelity."
                     )
                   )
                 ),
                 
                 
                 
                 card(
                   
                   card_header(tags$a(href =  "https://iris.peabody.vanderbilt.edu/module/palshs/", strong("PALS: A Reading Strategy for High School"))),
                   
                   card_body(
                     fill = FALSE,
                     min_height = 200,
                     max_height = 300,
                     p(
                        
                       "This module outlines the benefits of implementing PALS for high school, a peer tutoring strategy in which students work in pairs to strengthen their reading skills. Also included are step-by-step instructions for each of the three PALS activities as well as printable PALS materials."
                     )
                   )
                 ),
                 card(
                   
                   card_header(tags$a(href =  "https://dwwlibrary.wested.org/resources/912", strong("Analyzing Speeches: Homework Assignments and Note-Taking Templates"))),
                   
                   card_body(
                     fill = FALSE,
                     min_height = 200,
                     max_height = 300,
                     p(
                        
                       "High school teacher Jim Burke uses these note-taking templates with his high school English students to help them understand landmark speeches by Martin Luther King, Jr., Cesar Chavez, and Barack Obama."
                     )
                   )
                 ),
                 card(
                   
                   card_header(tags$a(href =  "https://dwwlibrary.wested.org/resources/918", strong("Building Academic Literacy Through Text Discussion"))),
                   
                   card_body(
                     fill = FALSE,
                     min_height = 200,
                     max_height = 300,
                     p(
                        
                       "Listen to high school English teacher Jim Burke describe the importance of providing opportunities for adolescents to discuss text. He explains how he establishes a supportive environment for discussions and describes note-taking organizers he uses to help students prepare for text discussions."
                     )
                   )
                 ),
                 card(
                   
                   card_header(tags$a(href =  "https://dwwlibrary.wested.org/resources/939", strong("ACCESS Class Vocabulary Chart"))),
                   
                   card_body(
                     fill = FALSE,
                     min_height = 200,
                     max_height = 300,
                     p(
                        
                       "A reading intervention teacher, uses this vocabulary chart to provide secondary students with a structure for learning word meaning. Watch the presentation in the 'See How' section of the 'Intensive Intervention' collection, High School Academic Literacy Intervention Class, to see her classroom in action."
                     )
                   )
                 )
                 
               ),
               
               h2("Parents and Families"),
               layout_column_wrap(
                 width = 1/4,
                 
                 card(
                   
                   card_header(tags$a(href =  "https://ies.ed.gov/ncee/edlabs/infographics/pdf/REL_SE_Literacy_Tips_for_Parents_of_Adolescents.pdf", strong("Literacy Tips for Parents of Adolescents"))),
                   
                   card_body(
                     fill = FALSE,
                     min_height = 200,
                     max_height = 300,
                     p(
                        
                       "Parents of adolescents can initiate at home to help ensure successful literacy outcomes for their adolescent students."
                     )
                   )
                 ),
                 
                 
                 
                 card(
                   
                   card_header(tags$a(href =  "https://ies.ed.gov/ncee/edlabs/infographics/pdf/REL_PA_How_Can_I_Help_My_Teen_with_Their_Reading_Skills_in_Different_Subjects.pdf", strong("How Can I Help My Teen With Their Reading Skills in Different Subjects?"))),
                   
                   card_body(
                     fill = FALSE,
                     min_height = 200,
                     max_height = 300,
                     p(
                        
                       "Parents and caregivers can use this resource to support their teenager's reading skills."
                     )
                   )
                 ),
                 
                 card(
                   
                   card_header(tags$a(href =  "https://familyvoices.org/wp-content/uploads/2019/04/Advocating_HSMS.pdf", strong("Advocating for Yourself in Middle School and High School"))),
                   
                   card_body(
                     fill = FALSE,
                     min_height = 200,
                     max_height = 300,
                     p(
                        
                       "This resource explains how to get what you need as a middle and high school student."
                     )
                   )
                 ),
                 
                 card(
                   
                   card_header(tags$a(href =  "https://meadowscenter.org/wp-content/uploads/2022/04/10Keys_Secondary_Web1.pdf", strong("10 Key Reading Practices for All Middle and High Schools"))),
                   
                   card_body(
                     fill = FALSE,
                     min_height = 200,
                     max_height = 300,
                     p(
                        
                       "This handbook describes ten reading practices for middle and high schools with strong evidence of effectiveness from high-quality research, including selected grade level descriptions of what students should know and be able to do."
                     )
                   )
                 ),
                 
                 card(
                   
                   card_header(tags$a(href =  "https://www.understood.org/en/articles/5-things-your-middle-schooler-with-dyslexia-can-say-to-self-advocate", strong("5 Self-Advocacy Sentence Starters for Middle-Schoolers With Dyslexia"))),
                   
                   card_body(
                     fill = FALSE,
                     min_height = 200,
                     max_height = 300,
                     p(
                        
                       "Learn how to build self-advocacy skills to help you get what they need. Rehearsing common situations can help you know where to start."
                     )
                   )
                 ),
                 card(
                   
                   card_header(tags$a(href =  "https://www.greatschools.org/gk/levels/high-school/", strong("Great Schools Videos on Milestones for Teens"))),
                   
                   card_body(
                     fill = FALSE,
                     min_height = 200,
                     max_height = 300,
                     p(
                        
                       "What skills does your teen need to learn before heading off into the world? These videos help highlight milestones of where your teen should be as the progress through high school."
                     )
                   )
                 )
                 
               )
               
               
               
           )
           
           
           )
  )
}

aaLResourceServer <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$getstarted,{
        updateNavbarPage(session = parent_session, "navbar-aal", selected = "assessment-tab")
      })
      
    }
  )
}
