##%######################################################%##
#                                                          #
####            Qualtrics Survey Summarizer             ####
#                                                          #
##%######################################################%##

# load packages -----------------------------------------------------------
pacman::p_load(tidyverse, janitor, here, glue, GSPQualtricsR, shiny,
               shinydashboard, shinyWidgets, gt)

# read in data for now #
# out <- readRDS("~/Desktop/tmpdata/tmpdata.rds")
# sids <- fetch_sids()

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
    tabItem("crosstab", uiOutput("tab2"),
            shinysky::busyIndicator(text = 'Please wait...', wait = 1500))
  )
)

# build the UI ------------------------------------------------------------
ui = dashboardPage(header, sidebar, body, skin = "black")

##  ............................................................................
##  SERVER                                                                  ####

server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })

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


  output$value <- renderPrint({ length(block_summary()) })
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
}


# can potentially move placement of DT buttons https://community.rstudio.com/t/adjust-place-of-buttons-of-a-dt-table-in-shiny/135600
# this would allow the caption to go on top



# Run the application
shinyApp(ui = ui, server = server)
