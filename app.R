
#Set some General Info

set_values <- read.csv("values.csv")

admin_email <- set_values[1,1][[1]]
admin_pass <- set_values[2,1][[1]]
MainTitle <- set_values[3,1][[1]]
sheet_id <- set_values[4,1][[1]]
authorized_email <- set_values[5,1][[1]]

#Load in the libraries

library(shiny)
library(shinyWidgets)
library(DT)
library(googlesheets4)
library(ggplot2)

#Authorize the app to use my school google account
sheets_auth(
    cache = ".secrets",
    email = authorized_email
)

#Get basic data and student requests from google
test_dat <- read_sheet(sheet_id)

student_requests <- read_sheet(sheet_id, sheet = "mentor_requests")

mentor_emails <- read_sheet(sheet_id, sheet = "Mentor Emails")

all_students <- test_dat


##### UI #######
ui <- bootstrapPage(
  
  #load in the font from google 
  tags$link(href='https://fonts.googleapis.com/css?family=Montserrat', rel='stylesheet'),
  
  #Lots of CSS
  tags$head(tags$style(HTML('
    
                                body {
                               font-family:"Montserrat";
                                }
                              .navbar-bg {
                               background-color: #003776 !important;
                              }
                              .nav > li > a:hover, .nav > li > a:focus {
                              background-color: #FFFFFF;
                                }
                              .bg-grey {
                                background-color: #d6d6d6;
                              }
                              .bg-dark-grey {
                                background-color: #696868;
                                color: white;
                              }
                              hr{
                                height: 3px;
                                background-color: #014093;
                                border: none;
                                width:10%;
                              }
                              
                              .sidenav {
                                height: 100%; /* Full-height: remove this if you want "auto" height */
                                width: 230px; /* Set the width of the sidebar */
                                position: fixed; /* Fixed Sidebar (stay in place on scroll) */
                                z-index: 1; /* Stay on top */
                                top: 0; /* Stay at the top */
                                left: 0;
                                background-color: #111; /* Black */
                                overflow-x: hidden; /* Disable horizontal scroll */
                                padding-top: 20px;
                              }

                              /* The navigation menu links */
                              .sidenav a {
                                padding: 6px 8px 6px 16px;
                                text-decoration: none;
                                font-size: 20px;
                                color: #818181;
                                display: block;
                              }

                              /* When you mouse over the navigation links, change their color */
                              .sidenav a:hover {
                                color: #f1f1f1;
                              }


                              /* On smaller screens, where height is less than 450px, change the style of the sidebar (less padding and a smaller font size) */
                              @media screen and (max-height: 450px) {
                                .sidenav {padding-top: 15px;}
                                .sidenav a {font-size: 18px;}
                              }
                              
                              '))),
  
  #The entire UI is generated in the Server and output here
  uiOutput("Page")
  
)

#### SERVER #####
server <- function(input, output, session) {
  
  
  ################################################################################################
  ##### LOGIN PAGE STUFF   #######################################################################
  ################################################################################################
  
  
  
  #Create MOdal as soon as app runs, this is the login page
  showModal(modalDialog(
    title = "User Login",
    easyClose = FALSE,
    footer = NULL,
    size = "s",
    textInput("user_email", "Email: ", placeholder = "Enter your school Email"),
    passwordInput("user_password", "Inumber", placeholder = "Enter your Inumber"),
    actionButton(inputId='Submit',
                 label="Submit",
                 style = "color: #fff; background-color: #00aaf2; border-color: #00aaf2; font-size: 12px; padding: 5px 5px;", width = 60),
#    actionButton(inputId='request_pass',
#                 label="Request Password",
#                 style = "color: #fff; background-color: #ff8519; border-color: #ff8519; font-size: 12px; padding: 5px 5px;", width = 120),
    br(),
    textOutput("Login_fail"),
    br()
#    uiOutput("password_req_ui")
    
    
  ))
  
  #Important Values
  
  login_page_open <- reactiveVal(TRUE)
  
  logged_in_person <- reactiveVal(0)
  
  login_attempts <- reactiveVal(0)
  
#  requesting_password <- reactiveVal(FALSE)
  
  login_admin <- reactiveVal(FALSE)
  
  main_dat <- reactiveVal(test_dat)
  
  #Remove modal when user presses submit button
  observeEvent(input$Submit, {
    
    #Save input email and password as "email" and "password"
    email <- input$user_email
    password <- input$user_password
    
    
    #if the input email and password are in the list continue else fail
    if (email ==  admin_email & password == admin_pass) {
      
      #Remove the Modal
      removeModal()
      #show login page is removed
      login_page_open(FALSE)
      #admin is logged in
      login_admin(TRUE)
      
      
    }else{
      
      if (
        email %in% test_dat$Email &
        password %in% test_dat$INumber1
        
      ) {
        #Ensure that email and password match up
        if (
          which(test_dat$Email == email) == 
          which(test_dat$INumber1 == password)
        ) {
          #Remove the Modal
          removeModal()
          #show login page is removed
          login_page_open(FALSE)
          #return index for person logged in
          logged_in_person(which(test_dat$Email == email))
          
        }else{
          #increase the number of login attempts for incorrect login
          login_plus <- login_attempts() + 1
          login_attempts(login_plus)
        }
      }else{
        #increase the number of login attempts for incorrect login
        login_plus <- login_attempts() + 1
        login_attempts(login_plus)
      }
    }
  })
  
  #Show if login page is open
  output$login_page_indicator <- renderText({
    
    login_page_open()
    
  })
  
  #show info of person logged in
  output$person_info <- renderTable({
    
    dat_out <- test_dat[logged_in_person(),]
    
    dat_out
    
  })
  
  #Display Login Fail
  output$Login_fail <- renderText({
    
    if (login_attempts() > 0) {
      "Failed to Login: Incorrect Email or Password"
    }else{
      ""
    }
    
  })
  
  #Change requestion password to true when request password button is clicked
  observeEvent(input$request_pass,{
    
    requesting_password(TRUE)
   
  })
  
  #This is the UI for requesting a password
  output$password_req_ui <- renderUI({
    
    if (requesting_password()) {
      tagList(
      br(),
      pickerInput(
        inputId = "rqst_password_name",
        label = "Select your name", 
        choices = test_dat$Display_Name,
        options = list(
          `live-search` = TRUE)
      ),
      p("An email will be sent to your school email with your password"),
      actionButton(inputId='Submit_password_request',
                   label="Send Request",
                   style = "color: #fff; background-color: #00aaf2; border-color: #00aaf2; font-size: 12px; padding: 5px 5px;", width = 120)
      )
    }
    
  })
  
#  observeEvent(input$Submit_password_request,{
    
#    name  <- input$rqst_password_name
    
#    pass_row <- test_dat[test_dat$Display_Name == name,]
    
#    password <- pass_row$Password[[1]]
    
    
#    message <- paste0("Your (", name, ") password is ", password, "for the online mentor application.")
    
    
#    send.mail(from = "tymaxjones317@gmail.com",
#              to = "jon16033@byui.edu",
#              subject = "Mentor app Password",
#              body = message,
#              smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "tymaxjones317@gmail.com", passwd = "remus317", ssl = TRUE),
#              authenticate = TRUE,
#              send = TRUE)
    
#    sendSweetAlert(
#      session = session,
#      title = "Password Sent",
#      text = "Check your school email",
#      type = "success"
#    )
    
#  })
  
  #################################################################################################################
  ######                   THE OUTPUT PAGE            ############################################################
  ######                                              #############################################################
  #################################################################################################################
  
  #This is the page that is output to the UI
  output$Page <- renderUI({
    
    #Check if the admin is logged in
    if(login_admin()){
      
      tagList(
        
        #This is the top bar that will have the log out button
        tags$ul(class = "nav nav-pills navbar-right",
                br(),
                tags$li(
                  a(
                    actionButton(inputId = "student_log_out",
                                 label = "Log Out")
                  )
                ),
                
                tags$li(role = "presentation",
                        tags$a(href = "#",
                               ""))
                
        ),
        
        br(),
        br(),
        
        #This is the main admin page, it contains the selection for types of views and a fluid output for the different types of UI
        div(class= "fluid-page",
            div(class = "row text-center",
                div(class = "col-xl-12",style = "margin-left: 180px; margin-right: 180px;",
                    br(),
                    strong(h1('Mentor Dashboard')),
                    hr(),
                    radioGroupButtons(
                      inputId = "display_type",
                      label = "",
                      #This is all the types of pages
                      choices = c("All Students", "Summaries", "Export CSV", "Student Requests"),
                      status = "info",
                      justified = TRUE,
                      checkIcon = list(
                        yes = icon("ok", 
                                   lib = "glyphicon"))
                    ),
                    br(),
                    #This is the UI Output that will change based on the page type selected
                    uiOutput("admin_page")
                    
                )
                
                
            )
        )
      )
      
      
    }else{
      #This is ELSE the admin login was not given, so it will give you the student page
      #We also check to make sure that the login_page has been closed
      if (!login_page_open()) {
        
        tagList(
          
          #This is the top bar of the student page to show the log out button
          tags$ul(class = "nav nav-pills navbar-right",
                  br(),
                  tags$li(
                    a(
                      actionButton(inputId = "student_log_out",
                                   label = "Log Out")
                    )
                  ),
                  #This is just for making space, this is probably not the best way to do this but im a lazy idiot
                  tags$li(role = "presentation",
                          tags$a(href = "#",
                                 ""))
                  
          ),
          br(),
          br(),
          div(class = "row text-center",
              div(class = "col-xl-12",
                  br(),
                  #Header
                  strong(h1(MainTitle)),
                  hr(),
                  br()
              )
          ),
          div(class = "row text-center", style = "margin-left: 180px; margin-right: 180px;",
              
              #Here's the left column
              div(class = "col-md-6",
                  tags$div(class = "panel panel-info",
                           tags$div(class = "panel-heading",
                                    h3("Personal Info")),
                           tags$div(class = "panel-body",
                                    #All these text outputs are personalized to the person that logged in
                                    h4(textOutput("name")),
                                    h4(textOutput("Class")),
                                    h4(textOutput("Credits")),
                                    h4(textOutput("Track"))
                                    
                           ))
              ),
              #This is the right column
              div(class = "col-md-6",
                  tags$div(class = "panel panel-primary",
                           tags$div(class = "panel-heading",
                                    #We are just displaying the mentor information
                                    h3("Mentor Info")),
                           tags$div(class = "panel-body",
                                    #Displays the Mentor Info, it's personalized to the person that is logged in
                                    h4(textOutput("mentor_name")),
                                    h4(textOutput("mentor_email")),
                                    
                                    hr(),
                                    #This button should open up your emailing application and send an email to your mentor
                                    uiOutput("msg_mentor_button"),
                                    br(),
                                    br(),
                                    #When you press this button it will open up a modal that will allow you to request a new mentor
                                    actionButton("rqst_mentor", "Request Specific Mentor", icon("user-friends"), 
                                                 style="color: #fff; background-color: #00a65a; border-color: #008d4c", width = 220)
                                    
                                    
                                    
                                    
                           ))
              )
          )
          
          
          
          
        )
        
      }
      
    }
  })
  
  
  
  #########################################################################################################
  #######          STUDENT PAGE STUFF         #############################################################
  ####### This is a bunch of the logic for the student page ###############################################
  #########################################################################################################
  
  
  #this is the general data  for the student that is logged in
  student_dat <- reactive({
    
    all_students[logged_in_person(),]
    
  })
  #Display student name
  output$name <- renderText({
    
    dat <- student_dat()
    
    paste0("Name: ", dat[,"Display_Name"])
    
  })
  #Display Student Class
  output$Class <- renderText({
    
    dat <- student_dat()
    
    paste0("Class: ", dat[,"Classification" ])
  })
  
  #Display student credits
  output$Credits <- renderText({
    
    dat <- student_dat()
    
    paste0("Completed Credits: ", dat[,"Cumulative_Earned_Credits"])
    
  })
  
  #Display student track
  output$Track <- renderText({
    
    dat <- student_dat()
    
    paste0("Track: ", dat[,"Track"])
    
  })
  
  #Display the Mentor name
  output$mentor_name <- renderText({
    
    dat <- student_dat()
    
    paste0("Name: ", dat[,"Mentor"])
    
    
  })
  
  
  #Display the mentor email
  output$mentor_email <- renderText({
    
    dat <- student_dat()
    
    
    paste0("Email: ", mentor_emails[ mentor_emails$Mentor == test_dat[logged_in_person(),"Mentor"][[1]], "Email"][[1]])
    
  })
  
  
  #Create the UI button that emails their mentor
  output$msg_mentor_button <- renderUI({
    
    a(actionButton(inputId = "email_Mentor", label = "Message Mentor", 
                   style="color: #fff; background-color: #00a65a; border-color: #008d4c",
                   icon = icon("envelope", lib = "font-awesome")),
      href=paste0("mailto:",mentor_emails[ mentor_emails$Mentor == test_dat[logged_in_person(),"Mentor"][[1]], "Email"][[1]])
    )
    
  })
  
  
  #Create modal for requesting new mentor
  observeEvent( input$rqst_mentor, {
    
    showModal(modalDialog(
      title = "Request new Mentor",
      pickerInput("rqst_mentor_mentor", "Request Mentor", choices = unique(all_students[,"Mentor"])),
      actionButton(inputId = "rqst_mentor_send",label = "Send", icon("paper-plane"), 
                   style="color: #fff; background-color: #00a65a; border-color: #008d4c", width = 160)
      
    )
    )
    
  })
  
  #Send the Mentor Request
  observeEvent(input$rqst_mentor_send, {
    
    send_student <- all_students[logged_in_person(),"Display_Name"]
    send_inumber <- all_students[logged_in_person(),"INumber1"]
    send_mentor <- input$rqst_mentor_mentor
    
    full_send <- paste0(Sys.Date() + " " + send_student, "(", send_inumber, ") has requested a new Mentor: ", send_mentor)
    
    
    sheet_append(ss = '1a1p-CGHsdt2cbeX0Vhs33Q38lSDpGv-m8GzQFvl5Ggo', 
                 data =  data.frame(full_send),
                 sheet = 2)
    
    sendSweetAlert(
      session = session,
      title = "Request Sent",
      text = "",
      type = "success"
    )
    
    
  })
  
  
  #Student has logged in
  observeEvent(input$student_log_out,{
    
    login_page_open(TRUE)
    
    login_admin(FALSE)
    
    
    
    showModal(modalDialog(
      title = "User Login",
      easyClose = FALSE,
      footer = NULL,
      size = "s",
      textInput("user_email", "Email: ", placeholder = "Enter your Email"),
      passwordInput("user_password", "Password", placeholder = "Enter your Password"),
      actionButton(inputId='Submit',
                   label="Submit",
                   style = "color: #fff; background-color: #00aaf2; border-color: #00aaf2; font-size: 12px; padding: 5px 5px;", width = 60),
  #    actionButton(inputId='request_pass',
   #                label="Request Password",
  #                 style = "color: #fff; background-color: #ff8519; border-color: #ff8519; font-size: 12px; padding: 5px 5px;", width = 120),
      br(),
      textOutput("Login_fail")
      
      
      
    ))
    
  })
  
  
  
  ##############################################################################################################
  ##############                   ADMING PAGE                                    ##############################
  ##############                     STUFF                                        ##############################
  ##############################################################################################################
  
  #This is the first page where we see all the students
  output$studentView <- renderDT({
    
    all_students1 <- main_dat()
    
    datatable(all_students1[,c("Display_Name", "Gender", "Major", "Track", "Classification", "INumber1", "Mentor")],
              filter = list(position = 'top', clear = FALSE),
              options = list(dom = 'tp'))
    
  })
  
  #This is the output for the second page
  output$mentorViewdt <- renderDT({
    
    all_students1 <- main_dat()
    
    datatable(all_students1[all_students1$Mentor == input$mentor_view_mentor,c("Display_Name", "Gender", "Major", "Track", "Classification", "INumber1")],selection = 'single')
    
  })
  
  
  #When they submit a change to the mentor write it to the google sheet and whatnot
  observeEvent(input$edit_mentor_student_submit,{
    
    
    dataframe <- main_dat()
    
    student_ids <- input$studentView_rows_selected
    mentor <- input$assign_mentor_student
    
    rows <- student_ids + 1
    range1 <- paste0("K",rows)
    
    
    for (i in 1:length(range1)) {
      range_write(
        sheet_id,
        data.frame(mentor),
        sheet = "Current_Data",
        range = range1[i],
        col_names = FALSE,
        reformat = TRUE
      )  
    }
    
    
    
    dataframe[student_ids, "Mentor"] <- mentor
    
    
    main_dat(dataframe)
    
    
    
    sendSweetAlert(
      session = session,
      title = "Mentor Changed",
      text = "",
      type = "success"
    )
    
    
    
    
  })
  
  #open up the modal when it says to switch mentor
  observeEvent(input$edit_student_mentor, {
    showModal(modalDialog(
      title = "Change Student Mentor",
      pickerInput("assign_mentor_student", "Assign Mentor", choices = unique(all_students[,"Mentor"])),
      actionButton(inputId = "edit_mentor_student_submit",label = "Submit", icon("paper-plane"), 
                   style="color: #fff; background-color: #00a65a; border-color: #008d4c", width = 160)
      
    )
    )
  })
  
  
  #Show a different page of the admin dashboard based on which tab is selected
  output$admin_page <- renderUI({
    
    selection <- input$display_type
    
    #This is the first page for all students
    if (selection == "All Students") {
      tagList(  
        DTOutput("studentView"),
        br(),            
        actionButton(inputId = "edit_student_mentor",label = "Change Student Mentor", icon("user-friends"), 
                     style="color: #fff; background-color: #00a65a; border-color: #008d4c", width = 220),
        br(),
        br(),
        br()
      )
      
      #Second Page
    }else if (selection == "Mentor Assignments") {
      tagList(
        
        div(class = "fluid-page",
            div(class = "col-md-6",
                
                pickerInput("mentor_view_mentor", "Mentor", choices = unique(all_students[,"Mentor"])),
                br(),
                DTOutput("mentorViewdt"),
                actionButton(inputId = "edit_student_mentor",label = "Change Student Mentor", icon("user-friends"), 
                             style="color: #fff; background-color: #00a65a; border-color: #008d4c", width = 220),
                br(),
                br(),
                br() 
                
                
            )
        )
        
        
        
      )
      
      #Third Page
    }else if (selection == "Summaries"){
      tagList(
        
        br(),
        br(),
        h4("Total Mentees per Mentor"),
        hr(),
        plotOutput("menteePerMentor"),
        br(),
        br(),
        br(),
        br(),
        h4("Mentees per Mentor by Class"),
        hr(),
        plotOutput("classPerMentor"),
        br(),
        br(),
        br(),
        br(),
        h4("Mentees per Mentor by Track"),
        hr(),
        plotOutput("trackPerMentor"),
        br(),
        br(),
        br()
        
      )
      
      
      #Fourth Page
    }else if(selection == "Student Requests"){
      
      tagList(
        
        
        div(class= "fluid-page",
            div(class = "row text-center",
                div(class = "col-xl-12",
                    column(width = 12, align = "center", tableOutput("student_mentor_requests"))
                    
                )))
        
        
        
        
      )
      
      
      #Last Page
    }else{
      tagList(
        
        div(class= "fluid-page",
            div(class = "row text-center",style = "margin-left: 200px; margin-right: 200px;",
                div(class = "col-md-6",
                    h1("Download Full Data Set"),
                    hr(),
                    downloadButton("downloadData", "Download"),
                    br(),
                    p("Download the full dataset that is currently in use"),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    
                    h1("Download Changes"),
                    hr(),
                    downloadButton("downloadData2", "Download"),
                    br(),
                    p("Download the rows of the current dataset that have changed since the last upload"),
                    br()              
                    
                ),
                
                div(class = "col-md-6",
                    h1("Input New Semester"),
                    hr(),
                    actionButton(inputId = "submit_new_semester_modal", label = "Submit New Semester"),
                    br(),
                    p("Open menu to upload a file for a new semester"),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    h1("View All Datasets"),
                    hr(),
                    actionButton(inputId = "view_tabs_admin", label = "Display" ),
                    br(),
                    p("View all of the currently saved datasets"),
                    column(align = "center", tableOutput("sheet_tabs"), width = 12)
                    
                    
                )
                
                
                
                
            )
        )
        
        
        
        
        
      )
      
      
    }
    
    
  })
  
  #This is the modal that will open when they press a new semester in the export CSV tab
  
  observeEvent(input$submit_new_semester_modal,{
    
    showModal(modalDialog(
      title = "Upload new Semester",
      radioGroupButtons(
        inputId = "upload_semester_season",
        label = "Semester",
        choices = c("Fall","Winter", "Spring", "Summer"),
        status = "primary",
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon"))
      ),
      numericInput("upload_semester_year", "Year", min = 2021, max = 2100, value = NULL),
      fileInput("new_semester_upload", label = "Import CSV File", accept = ".csv", width = "100%"),
      actionButton(inputId = "submit_new_semester",label = "Submit", icon("paper-plane"), 
                   style="color: #fff; background-color: #00a65a; border-color: #008d4c", width = 160)
      
    )
    )
    
  })
  
  file_upload_Data <- reactive({
    
    inFile <- input$new_semester_upload
    
    if (is.null(inFile)){
      return(NULL)
    }
    
    read.csv(inFile$datapath)
    
    
  })
  
  
  observeEvent(input$submit_new_semester, {
    
    sem <- input$upload_semester_season
    yr <- input$upload_semester_year
    
    sheetname <- paste0(sem,"-",yr)
    
    sheet_add(ss = sheet_id, sheet = sheetname)
    
    sheet_write(file_upload_Data(), ss = sheet_id, sheet = sheetname)
    
    sheet_write(file_upload_Data(), ss = sheet_id, sheet = "Current_Semester_Original")
    
    sheet_write(file_upload_Data(), ss = sheet_id, sheet = "Current_Data")
    
    
    sendSweetAlert(
      session = session,
      title = "Semester Uploaded",
      text = "",
      type = "success"
    )
    
  })
  
  
  #This is the graph showing how many mentees each mentor has
  output$menteePerMentor <- renderPlot({
    
    all_students1 <- main_dat()
    
    plot_df <- as.data.frame(table(all_students1$Mentor))
    ggplot(plot_df) +
      geom_col(aes(x = reorder(Var1, Freq), y = Freq), fill = "#003776") +
      geom_text(aes(x = reorder(Var1, Freq), y = Freq + 1, label = Freq)) +
      theme_minimal() +
      coord_flip() +
      xlab("") +
      ylab("# of Mentees")
    
  })
  
  #This is the mentees in each class per mentor
  output$classPerMentor <- renderPlot({
    
    all_students1 <- main_dat()
    
    plot_class_df <- as.data.frame(table(all_students1[,c("Mentor", "Classification")]))
    plot_class_df$Classification <- factor(plot_class_df$Classification, levels = c("Freshman", "Sophomore", "Junior", "Senior"))
    ggplot(plot_class_df) +
      geom_col(aes(x = reorder(Mentor, Freq), y = Freq), fill = "#003776") +
      geom_text(aes(x = reorder(Mentor, Freq), y = Freq + 1, label = Freq)) +
      facet_grid(cols = vars(Classification)) +
      coord_flip() +
      xlab("") +
      ylab("# of Mentees") +
      theme_minimal()
    
  })
  
  #This is the mentees in each track per mentor
  output$trackPerMentor <- renderPlot({
    
    all_students1 <- main_dat()
    
    plot_track_df <- as.data.frame(table(all_students1[,c("Mentor", "Track")]))
    plot_track_df$Track <- factor(plot_track_df$Track, levels = c("Winter/Spring", "Spring/Fall", "Fall/Winter"))
    ggplot(plot_track_df) +
      geom_col(aes(x = reorder(Mentor, Freq), y = Freq), fill = "#003776") +
      geom_text(aes(x = reorder(Mentor, Freq), y = Freq + 1, label = Freq)) +
      facet_grid(cols = vars(Track)) +
      coord_flip() +
      xlab("") +
      ylab("# of Mentees") +
      theme_minimal()
  })
  
  #This just outputs the student requests
  output$student_mentor_requests <- renderTable({
    
    student_requests
    
  })
  
  #This downloads the full dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("student_Mentors_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(test_dat[,1:11], file, row.names = FALSE)
    }
  )
  
  
  #This downloads the changes that have been made
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("student_Mentor_changes_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      
      original_dat <- read_sheet(sheet_id, sheet = "Current_Semester_Original")
      out_dat <- test_dat[original_dat$Mentor != test_dat$Mentor,1:11]
      write.csv(out_dat, file, row.names = FALSE)
    }
  )
  
  
  #showing the tabs 
  tabs <- eventReactive(input$view_tabs_admin, {
    sheet_tabs <- googlesheets4::sheet_properties(sheet_id)[1:2]
    
  })
  
  #Output the tabs
  output$sheet_tabs <- renderTable({
    
    tabs()
    
  })
  
  
  
}









#Run it
shinyApp(ui = ui, server = server)
