aalLearnMoreUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Learn More", value = ns("tab"), 
           div(
             class = "container-fluid",
             style = "padding: 0;",
             br(),
             h1("Learn More", style = "display: flex; justify-content: center;")
           ),
           div(
             class = "container-fluid",
             style = "padding: 1rem;",
             div(
               style = "text-align: center; margin: auto; width: 70%; margin-bottom: 5rem;",
               h2("About Us"),
               br(),
               span("The", strong("National Center on Improving Literacy"), 
                    "(NCIL), operated by Boston University’s Wheelock College of Education and Human Development with funding from the United States Department of Education, is a partnership among literacy experts, university researchers, and technical assistance providers from the University of Oregon, Florida State University, and RMC Research Corporation. Our mission is to
                    increase access to, and use of, evidence-based approaches to screen, identify, and teach students with literacy-related disabilities, including dyslexia. We also strive to build individual and organizational capacity to assess students’ literacy-related skill, identify students with disabilities or those at risk of disabilities, and fully implement evidence-based literacy programs and professional development. For more information, please visit
                 ", tags$a("www.improvingliteracy.org", href =  "http://www.improvingliteracy.org", target = "_blank"))
             ),
             layout_column_wrap(
               width = 1/2,
               style = "margin: auto; width: 70%;",
               div(
                 style = "text-align: center; float: left;",
                 h2("Technical Specifications"),
                 span('Press the "Click Here" button below to view further technical information regarding the AAL.'),
                 br(),
                 actionButton(ns("technical_specs"),
                              "Click Here", class = "button-margins", width = "200px",
                              onclick ="window.open('AAL 3-12 Tech Manual.pdf', '_blank')")
               ),
               
               div(
                 style = "text-align: center; float: left;",
                 h2("Feedback Form"),
                 span("Please follow the link below to fill out a feedback form on your experience with the AAL. Any and all feedback is greatly appreciated to help improve the overall experience for students, teachers, and parents when navigating the AAL."),
                 br(),
                 actionButton(ns("feedback_form"),
                              "Click Here", class = "button-margins", width = "200px",
                              onclick ="window.open('https://forms.gle/tWiRUBjb72nhFV5CA', '_blank')")
               )),
             br(),br(),
             layout_column_wrap(
               width = 1/2,
               style = "margin: auto; width: 70%;",
               div(
                 style = "text-align: center; float: left;",
                 h2("Acknowledgements"),
                 span("Use the button below to view a list of contributors that helped put this project together."),
                 br(),
                 actionButton(ns("contributors"),
                              "Click Here", class = "button-margins", width = "200px",
                              onclick = "window.open('https://docs.google.com/document/d/1t1hBj6mKLgzSSX58Us30GA42UcxKnmCF3pcOqJyT7Yk/edit?usp=sharing', '_blank')"
                 )
               ),
               div(
                 style = "text-align: center; float: left;",
                 h2("Technical Support"),
                 span("If you run into any technical problems or have any questions for the AAL team follow the link below for a contact form."),
                 br(),
                 actionButton(ns("techsupport"),
                              "Click Here", class = "button-margins", width = "200px",
                              onclick ="window.open('https://contact.improvingliteracy.org/contact-form/', '_blank')"
                 )
               )
             )
           )
           
  )
}

aaLLearnMoreServer <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # observeEvent(input$technical_specs, {
      #   print("working")
      #   file.show("AAL 3-12 Tech Manual v1_2.pdf")
      # })
      
      
      
    }
  )
}
