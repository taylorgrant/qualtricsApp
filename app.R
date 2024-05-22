
##%######################################################%##
#                                                          #
####            Qualtrics Survey Summarizer             ####
#                                                          #
##%######################################################%##

# load packages -----------------------------------------------------------
pacman::p_load(tidyverse, janitor, here, glue, GSPQualtricsR, shiny,
               shinydashboard, shinyWidgets, gt)

# read in data for now #
# sids <- fetch_sids()
sids <- readRDS("/srv/shiny-server/qualtrics/data/qualtrics_sids.rds")

#   ____________________________________________________________________________
#   SHINY APP                                                               ####


##  ............................................................................
##  UI                                                                      ####

# header ------------------------------------------------------------------
header <- dashboardHeader(title = "Qualtrics Summarizer",
                          tags$li(a(href = 'https://goodbysilverstein.com/',
                                    img(src = 'https://images.squarespace-cdn.com/content/v1/5ea9e40f1c49ae0355b4d859/1615824766494-NPIZRASFQS47W5GHS0UY/RepData_Goodby_Silverstein.png',
                                        title = "GS&P", height = "30px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown"))

# sidebar -----------------------------------------------------------------
sidebar <- dashboardSidebar(
  width = 325,
  sidebarMenu(
    id = "tabs",
    convertMenuItem(
      menuItem("Begin here:",
               tabName = "qualsurvey",
               icon = icon("square-poll-vertical"),
               selected=T,
               menuItem("Available Surveys", uiOutput("surveySelect")),
               uiOutput("importSurvey",
                        align = "center"),
               br(),
               menuItem("Target Variables:",
                        uiOutput("targetBlock"),
                        uiOutput("targetVariable")),
               menuItem("Crosstab Variables:",
                        uiOutput("groupBlock"),
                        uiOutput("groupVariable")),
               menuItem("Filter Variables:",
                        uiOutput("filterBlock"),
                        uiOutput("filterVariable"),
                        uiOutput("filterChoices")),
               menuItem("Top Box",
                        uiOutput("likertTop")),
               menuItem("Bottom Box",
                        uiOutput("likertBottom")),
               menuItem("Confidence Level:",
                        radioButtons("conf_level",
                                     label = "Select confidence:",
                                     inline = TRUE,
                                     choices = c("80%" = ".80",
                                                 "90%" = ".90",
                                                 "95%" = ".95"),
                                     selected = c("90%" = ".90"))),
               menuItem("Summarize Block:",
                        uiOutput('runBlock',
                                 align = "center"),
                        uiOutput('download',
                                 align = "center"))
      ), "qualsurvey"
    ),
    convertMenuItem(
      menuItem("FAQ", tabName = "faq",
               icon = icon("square-poll-vertical"),
               selected = F), "faq"
    )
  )
)


# body --------------------------------------------------------------------
body <- dashboardBody(
  ## CSS styling for the validation error message on Monthly Sales ##
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: blue;
        font-size: 130%;
      }
      .shiny-output-error-myClass {
        color: red;
        font-size: 100%;
      }
    "))
  ),
  # tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  tabItems(
    # conditionally render the output using inputs (server side)
    tabItem("qualsurvey", uiOutput("tab1"),
            shinysky::busyIndicator(text = 'Please wait...', wait = 1500)),
    tabItem("faq", uiOutput("tab2"),
            shinysky::busyIndicator(text = 'Please wait...', wait = 1500))
  )
)

# build the UI ------------------------------------------------------------
ui = dashboardPage(header, sidebar, body, skin = "black")

##  ............................................................................
##  SERVER                                                                  ####

server <- function(input, output, session) {
  #session$onSessionEnded(function() {
  #  stopApp()
  #})
  # load environmental variables manually (shiny server isn't finding the environ file)
  Sys.setenv(QUALTRICS_API_KEY='FPUlMwd9087imCFqKOfmlkvvAR0bda8XJQyP95jl')
  Sys.setenv(QUALTRICS_KEY='FPUlMwd9087imCFqKOfmlkvvAR0bda8XJQyP95jl') 
  Sys.setenv(QUALTRICS_BASE_URL='sjc1.qualtrics.com')
  Sys.setenv(QUALTRICS_SUBDOMAIN='sjc1')
  
  
  # SURVEY SELECTION --------------------------------------------------------
  output$surveySelect <- renderUI({
    selectInput("surveySelect", "Select a survey:",
                c("", sids$name)
    )
  })
  
  # import button once survey is selected
  observeEvent(input$surveySelect, {
    output$importSurvey <- renderUI({
      if (input$surveySelect == '') return()
      actionButton("importSurvey",
                   label = "Import Survey",
                   onclick = "var $btn=$(this); setTimeout(function(){$btn.remove();},0);",
                   icon = icon("upload"),
                   width = "200px",
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    })
  })
  
  # read in data on survey selection
  # imported <- shiny::reactiveValues(data = data.frame(), name = "data")
  rv <- shiny::reactiveValues(data = data.frame(), name = "data")
  
  shiny::observeEvent(input$importSurvey, {
    # rv$data <- readRDS("~/Desktop/tmpdata/tmpdata.rds")
    rv$data <- fetch_survey_data(sids[sids$name == input$surveySelect,]$id)
    # dropping any TEXT open end question
    rv$data$toc <- rv$data$toc[rv$data$toc$question_type != "TE",]
  })
  
  # metadata to prove survey read in
  output$tab1 <- renderUI({
    validate(
      need(input$importSurvey != "", "To begin, please select a survey from the dropdown menu at left")
    )
    req(input$importSurvey, rv$data)
    tabItem("qualsurvey", h4("Imported Survey"),
            fluidRow(
              box(width = 4,
                  renderTable(data.frame(Metadata = c("Name", "Question Blocks", "Total Questions", "Respondents", "Avg Completion Time"),
                                         Response = c(sids[sids$name == input$surveySelect,]$name,
                                                      length(unique(rv$data$toc$block)),
                                                      length(unique(rv$data$toc$question_id)),
                                                      nrow(rv$data$svy$variables),
                                                      survey_duration(mean(rv$data$svy$variables$Duration__in_seconds_)))))
              )))
  })
  
  
  # VARIABLE SELECTION ------------------------------------------------------
  # on load, pull in the survey blocks
  output$targetBlock <- renderUI({
    req(input$importSurvey, rv$data)
    selectInput("target_block", "Target Block",
                choices = c("ALL BLOCKS", unique(rv$data$toc$block)))
  })
  # pull in unique qids and q names
  output$targetVariable <- renderUI({
    req(input$importSurvey, rv$data)
    toc_list <- c("", rv$data$toc[!duplicated(rv$data$toc$question_id), ]$question_id)
    names(toc_list) <- c("", paste0("Q", rv$data$toc[!duplicated(rv$data$toc$question_id), ]$question_order, ": ",
                                    rv$data$toc[!duplicated(rv$data$toc$question_id), ]$question_text))
    # names are seen by user, id is on the backend
    selectInput("target_variable", "Target Question:",
                choices = toc_list)
  })
  
  # group selection updates target variables available
  observeEvent(input$target_block, {
    req(input$importSurvey, rv$data, input$target_block)
    # call helper to filter questions as necessary
    updated_toc <- toc_filter(rv$data$toc, input$target_block)
    # update
    updateSelectInput(
      session = session,
      inputId = "target_variable",
      choices = c("", updated_toc)
    )
  })
  
  
  # FILTER VARIABLE SELECTION -----------------------------------------------
  # option to filter variables
  output$filterBlock <- renderUI({
    req(input$importSurvey, rv$data)
    selectInput("filter_block", "Filter Block",
                choices = c("ALL BLOCKS", unique(rv$data$toc$block)))
  })
  # for filter -- pull in qnames and export_name
  output$filterVariable <- renderUI({
    req(input$importSurvey, rv$data)
    filter_toc <- pull_filter_toc(rv$data$toc, blockfilter = "")
    # names are seen by user, id is on the backend
    selectInput("filter_variable", "Filter Question:",
                choices = filter_toc)
  })
  # for filter -- filter selection updates target variables available
  observeEvent(input$filter_block, {
    req(input$importSurvey, rv$data, input$filter_block)
    # call helper to filter questions as necessary
    updated_toc <- pull_filter_toc(rv$data$toc, input$filter_block)
    # update
    updateSelectInput(
      session = session,
      inputId = "filter_variable",
      choices = c("", updated_toc)
    )
  })
  # for filter -- provide variable options to keep
  observeEvent(input$filter_variable, {
    output$filterChoices <- renderUI({
      req(input$importSurvey, rv$data)
      checkboxGroupInput("filter_choices", "Check groups to keep:",
                         choices = filter_choices(rv$data$svy$variables, input$filter_variable)
      )
    })
  })
  
  
  # TOP / BOTTOM BOX --------------------------------------------------------
  # top/bottom box -- check to see if target variable is Likert
  observeEvent(input$target_variable, {
    output$likertTop <- renderUI({
      validate(
        need(unique(rv$data$toc[rv$data$toc$question_id == input$target_variable,]$selector_type) == "Likert",
             'Top Box requires a Likert question'),
        errorClass = 'myClass')
      
      t1 <- attr(unique(rv$data$svy$variables[[rv$data$toc[rv$data$toc$question_id == input$target_variable,]$export_name[1]]]), "labels")
      topLike <- setNames(1:length(t1), names(t1))
      
      checkboxGroupInput("top_box", "Top Box:",
                         choices = topLike)
    })
    output$likertBottom <- renderUI({
      validate(
        need(unique(rv$data$toc[rv$data$toc$question_id == input$target_variable,]$selector_type) == "Likert",
             'Bottom Box requires a Likert question'),
        errorClass = 'myClass')
      b1 <- attr(unique(rv$data$svy$variables[[rv$data$toc[rv$data$toc$question_id == input$target_variable,]$export_name[1]]]), "labels")
      bottomLike <- setNames(1:length(b1), names(b1))
      
      checkboxGroupInput("bottom_box", "Bottom Box:",
                         choices = bottomLike)
    })
  })
  
  
  # CROSSTAB VARIABLE SELECTION ---------------------------------------------
  # crosstab grouping variables
  # on load, pull in the survey blocks
  output$groupBlock <- renderUI({
    req(input$importSurvey, rv$data)
    selectInput("group_block", "Crosstab Block",
                choices = c("ALL BLOCKS", unique(rv$data$toc$block)))
  })
  # crosstab grouping variables
  # pull in unique qids and q names
  output$groupVariable <- renderUI({
    req(input$importSurvey, rv$data)
    toc_list <- c("", rv$data$toc[!duplicated(rv$data$toc$question_id), ]$question_id)
    names(toc_list) <- c("", paste0("Q", rv$data$toc[!duplicated(rv$data$toc$question_id), ]$question_order, ": ",
                                    rv$data$toc[!duplicated(rv$data$toc$question_id), ]$question_text))
    # names are seen by user, id is on the backend
    selectInput("group_variable", "Crosstab Question:",
                choices = toc_list)
  })
  # group selection updates target variables available
  observeEvent(input$group_block, {
    req(input$importSurvey, rv$data, input$group_block)
    # call helper to filter questions as necessary
    updated_toc <- toc_filter(rv$data$toc, input$group_block)
    # update
    updateSelectInput(
      session = session,
      inputId = "group_variable",
      choices = c("", updated_toc)
    )
  })
  
  
  # SUMMARIZE ENTIRE BLOCK --------------------------------------------------
  
  output$runBlock <- renderUI({
    req(input$importSurvey, rv$data, input$target_block)
    actionButton("run_block", "Run Block", width = "160px",
                 icon = icon("person-running"))
  })
  output$download <- renderUI({
    req(input$run_block, block_summary())
    shiny::downloadButton("downloadData", "Download your data",
                          width = "150px")
  })
  
  # TAB 1 - Simple Summaries ------------------------------------------------
  
  inputs <- reactive({
    block <- input$target_block
    target <- input$target_variable
    group <- input$group_variable
    top <- input$top_box
    bottom <- input$bottom_box
    filterQ <- input$filter_variable
    filter_choices <- input$filter_choices
    conf_level = input$conf_level
    list(block = block,
         target = target, group = group, top = top,
         bottom = bottom, filterQ = filterQ ,
         filter_choices = filter_choices, conf_level = conf_level)
  })
  
  simple_summary <- reactive({
    
    # if only target variable (no group or filter)
    if ((is.null(inputs()$group) || inputs()$group == "") && is.null(inputs()$filter_choices)) {
      
      params <- list(qid = input$target_variable, gid = NULL,
                     fiq = list(filterQ = NULL, filter_choices = NULL), ci = inputs()$conf_level,
                     top_box = inputs()$top, bottom_box = inputs()$bottom)
      summarize_question(parameters = params, data = rv$data)
      
      # if target variable and group variable (no filter)
    } else if ((!is.null(inputs()$group) && inputs()$group != "") && is.null(inputs()$filter_choices)) {
      
      params <- list(qid = inputs()$target, gid = inputs()$group,
                     fiq = list(filterQ = NULL, filter_choices = NULL), ci = inputs()$conf_level,
                     top_box = inputs()$top, bottom_box = inputs()$bottom)
      summarize_question(parameters = params, data = rv$data)
      
      # if target variable and filter (no group)
    } else if ((is.null(inputs()$group) || inputs()$group == "") && !is.null(inputs()$filter_choices)) {
      
      params <- list(qid = inputs()$target, gid = NULL,
                     fiq = list(filterQ = inputs()$filterQ, filter_choices = inputs()$filter_choices), ci = inputs()$conf_level,
                     top_box = inputs()$top, bottom_box = inputs()$bottom)
      summarize_question(parameters = params, data = rv$data)
      
      # if target variable, group variable, and filter
    } else if ((!is.null(inputs()$group) && inputs()$group != "") && !is.null(inputs()$filter_choices)) {
      
      params <- list(qid = inputs()$target, gid = inputs()$group,
                     fiq = list(filterQ = inputs()$filterQ, filter_choices = inputs()$filter_choices), ci = inputs()$conf_level,
                     top_box = inputs()$top, bottom_box = inputs()$bottom)
      summarize_question(parameters = params, data = rv$data)
    }
    
  })
  
  
  # SIDE RAIL - SUMMARIZE ALL  ----------------------------------------------
  
  block_summary <- eventReactive(input$run_block, {
    
    if ((is.null(inputs()$group) || inputs()$group == "") && is.null(inputs()$filter_choices)) {
      
      params <- list(qid = input$target_variable, gid = NULL,
                     fiq = list(filterQ = NULL, filter_choices = NULL), ci = inputs()$conf_level)
      summarize_block(block_choice = inputs()$block, parameters = params, data = rv$data)
      
      # if target variable and group variable (no filter)
    } else if ((!is.null(inputs()$group) && inputs()$group != "") && is.null(inputs()$filter_choices)) {
      
      params <- list(qid = inputs()$target, gid = inputs()$group,
                     fiq = list(filterQ = NULL, filter_choices = NULL), ci = inputs()$conf_level)
      summarize_block(block_choice = inputs()$block, parameters = params, data = rv$data)
      
      # if target variable and filter (no group)
    } else if ((is.null(inputs()$group) || inputs()$group == "") && !is.null(inputs()$filter_choices)) {
      
      params <- list(qid = inputs()$target, gid = NULL,
                     fiq = list(filterQ = inputs()$filterQ, filter_choices = inputs()$filter_choices), ci = inputs()$conf_level)
      summarize_block(block_choice = inputs()$block, parameters = params, data = rv$data)
      
      # if target variable, group variable, and filter
    } else if ((!is.null(inputs()$group) && inputs()$group != "") && !is.null(inputs()$filter_choices)) {
      
      params <- list(qid = inputs()$target, gid = inputs()$group,
                     fiq = list(filterQ = inputs()$filterQ, filter_choices = inputs()$filter_choices), ci = inputs()$conf_level)
      summarize_block(block_choice = inputs()$block, parameters = params, data = rv$data)
    }
  })
  
  
  # DOWNLOAD HANDLER --------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Block-", inputs()$block, ".xlsx", sep="")
    },
    content = function(file) {
      xlsx_build(block_summary(), file)
    }
  )
  
  # OUTPUTS -----------------------------------------------------------------
  
  
  # output$value <- renderPrint({ length(block_summary()) })
  output$within <- DT::renderDT({
    build_table(simple_summary()$out_win, ci = as.numeric(input$conf_level))
  })
  
  output$between <- DT::renderDT({
    build_table(simple_summary()$out_bwn, ci = as.numeric(input$conf_level))
  })
  
  observe({
    if (!is.null(inputs()$group) && inputs()$group != "") {
      output$about_within <- renderText({
        HTML("<h4>Within Groups</h4>This table provides results from statistical significance tests <strong><em>within</em></strong> groups. We compare responses for each answer (column) across a single group (row).")
      })
    }
  })
  
  observe({
    if (!is.null(inputs()$group) && inputs()$group != "") {
      output$about_between <- renderText({
        HTML("<h4>Between Groups</h4>This table provides results from statistical significance tests <strong><em>between</em></strong> groups. We compare responses from a single answer (row) across each group (column).")
      })
    }
  })
  
  
  observeEvent(input$target_variable, {
    output$tab1 <- renderUI({
      tab1_ui <- tabItem("qualsurvey", h4("Survey Results"), value="test1",
                         fluidRow(
                           box(
                             width = 10,
                             validate(
                               need(input$target_variable != "", "Please select a question from the dropdown...")
                             ),
                             validate(
                               need(rv$data$toc[rv$data$toc$question_id == input$target_variable,]$question_type != "TE", "This app doesn't work with open ends right now...")
                             ),
                             # verbatimTextOutput("value")
                             DT::DTOutput("within")
                           ),
                           box(
                             width = 2,
                             htmlOutput("about_within")
                           )),
                         fluidRow(
                           box(
                             width = 10,
                             if (!is.null(inputs()$group) && inputs()$group != "") {
                               DT::DTOutput("between")
                             }
                           ),
                           box(
                             width = 2,
                             htmlOutput("about_between")
                           ))
      )
    })
  })
  
  # render dash; made page a little wider for some tables
  output$tab2 <- renderUI({
    tab2_ui <- tabItem("faq", h4("Frequently Asked Questions"), value="test2",
                       fluidRow(
                         box(
                           width = 10,
                           uiOutput("about") 
                         )))
  })
  
  # SHINY SERVER - ABOUT -------------------------------------------------------
  
  output$about <- renderUI({
    HTML("<h4>&#9968; What is this?</h4>This is an app that summarizes our Qualtrics surveys and outputs the results
         in formatted tables. The app offers crosstab support, response filtering, and top/bottom boxing. 
         Confidence intervals for statistical testing can also be toggled between 80%, 90%, and 95%. 
         <br><br>
         The app also allows for block analysis - it will summarize all questions included in the survey
         block. 
         <h4>&#9968; How do I get started?</h4> Every day our server pulls the IDs of all available surveys in Qualtrics. Go to <code>Begin here:</code>
         <code>&rarr;</code> <code>Available surveys</code> and select your survey of interest from the dropdown menu. Once you've selected your survey
         press the <code>Import Survey</code> button. The import will take up to a minute depending on the size of the survey. 
         <h4>&#9968; What kind of analysis options are available?</h4>
         <b>Target variable:</b><br>
         This is your variable of interest. You're presented with two dropdown menus. The first, <code>Target Block</code> will narrow down 
         the questions by block. The second dropdown, <code>Target Question</code> gives you the questions to choose from within the selected block. 
         If you don't want to narrow down the selection, or don't know which block your question of interest is in, 
         then keep the <code>Target Block</code> filter set to 'ALL BLOCKS'.
         <br>
         <i>NOTE: Open end, text based questions are not currently supported.</i>
         <br>
         <b>Crosstab Variables:</b><br>
         The variable to use for cross-tabbing. The format is similar to the Target Variable - you can filter the block
         to help to narrow down the range of questions, and if you don't want to narrow down, leave the block as 'ALL BLOCKS'.
         Most demographic variables are found in either the 'Front End' or 'Back End Demos' blocks. 
         <br>
         <b>Filter Variables:</b><br>
         If you want to filter responses by a certain group, e.g., females only, this is how you do it. Select the <code>Target Block</code> to narrow down questions
         and then the <code>Target Question</code>. A drop down of radio buttons will appear in the left hand rail. Select those groups/answers that you want to include. 
         Any variables left unchecked will not be included in the results.
         <br>
         <b>Top Box/Bottom Box:</b><br>
         If the Target Question is programmed as a Likert response, opening up these two dropdown menus will allow the user to select which variables fall into the 
         Top and Bottom Boxes. You can choose as many variables as you want to fall into each bucket, but be aware that there are no safeguards - if you 
         put the same variable into both boxes, it will be counted twice. When the tables reformat with your selections, the names will change to `Top/Bottom N Box`
         depending on how many variables were selected into each bucket. 
         <br><i><font color='#FF0000'>IMPORTANT:</font> this app processes the data in real time, so as you make selections, it will re-summarize the data. This has consequences,
         one of which is that if you no longer want to box summarize, you should uncheck the selections before moving to a new Target Variable. Failure to do 
         so may result in other variables being processed with the `Top/Bottom N Box` names. If this happens, just refresh the page and reload the survey (sorry).</i>
         <br>
         <b>Confidence Intervals:</b><br>
         The app comes with three confidence levels - 80%, 90%, and 95%. The default is 90%, but checking any of the other levels will immediately re-process the data 
         as well as all statistical significance tests. 
         <br>
         <h4>&#9968; Why do the crosstabs have two tables?</h4>
         There are two ways that we can think about testing significance of crosstabs - (1) within groups, and (2) between groups. Both tables are presented within the app.
         <br>
         <b>Within groups:</b><br>This captures whether responses to the target variable significantly vary within each of the crosstabbed groups. For example, 
         if you were to crosstab a variable with Generations, the within testing would determine if responses significantly varied within each generational cohort, 
         independent of the other cohorts, e.g., is there significant differences in response rates amongst Millennials.  
         <br>
         <b>Between groups:</b><br>This captures whether responses significantly differ between the crosstabbed groups. If we were to crosstab by Generations, this evaluates
         whether a response type varies between each generational cohort, e.g., Gen Z vs. Millennials. 
         <h4>&#9968; How do I export my analysis?</h4>
         After running any analysis - single question or crosstabbed, you can simply press the <code>Excel</code> button. It will immediately download the data as 
         an xlsx file with the question number - e.g., 'Q16.xlsx'.
         <h4>&#9968; What about Block Summaries?</h4>
         Block summaries work by holding all analysis options constant and then running them across all questions within the specified target block. 
         <br>
         <b>To run a block summary:</b><br>
         <ol type='1'>
         <li>From the <code>Target Variable</code> dropdown menu, select the <code>Target Block</code> of interest.<br></li>
         <li>Choose a crosstab variable if you want to crosstab all questions within the block. If no crosstab, just leave as is.</li>
         <li>Select your filter options if you want to restrict all questions within the block to a specific response, e.g., Gender = Female. If no filter, leave as is.</li>
         <li>Push the <code>Run Block</code> button.</li>
         <li>When the block has run, push the <code>Download the data</code> button to download a .xlsx file. The file will save as 
         'Block-[Block Name].xlsx' to your Downloads folder.
         </ol>
         Each question within the block is in a separate tab within the workbook. If a crosstab variable was used, then both the Within and Between analyses
         will be included, each in it's own tab and identified as either '_win' or '_bwn'.<br>
         <i>NOTE: Top and Bottom Box summaries are not available in block summaries.</i>
         <h4>&#9968; I have a question that is clearly a Likert scale, but I can't Top/Bottom Box summarise it. Why not?</h4>
         This will happen if the variable wasn't programmed as a Likert scaled question in Qualtrics. For example, if a Likert scaled question 
         was originally programmed as a multiple choice question, there is no way for the app to a priori assume that it could/should be summarized by boxing.
         <h4>&#9968; Can I create my own variables for use in the analysis?</h4>
         Yes. You can create your own variables in Qualtrics, and they will be found by the app when reading in the data. The only thing you have to do is 
         name your variable so that it starts with a lower-case 'x'. For example, assume you were creating a variable for women that drive a set of 
         luxury EVs, you would create your variable and name it something like: 'xFemaleEV'. That variable will be found by the app and placed into a 
         separate 'User Generated' Target Block. 
         ")
  })
}



# Run the application
shinyApp(ui = ui, server = server)
