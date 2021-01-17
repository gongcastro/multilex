# user interface of the app

shinyUI(
  dashboardPage(
    skin = "black",
    dashboardHeader(
      title = "MultiLex"
    ),
    dashboardSidebar(
      width = 150,
      sidebar_menu <- sidebarMenu(
        id = "tabs",
        menuItem(
          text = "Dashboard",
          tabName = "tab_dashboard",
          icon = icon("tachometer-alt")
        ),
        menuItem(
          text = "Logs",
          tabName = "tab_logs",
          icon = icon("calendar-alt")
        ),
        menuItem(
          text = "Vocabulary",
          tabName = "tab_vocabulary",
          icon = icon("language")
        ),
        menuItem(
          text = "Norms",
          tabName = "tab_norms",
          icon = icon("percent")
        ),
        menuItem(
          text = "Pool",
          tabName = "tab_pool",
          icon = icon("list")
        ),
        menuItem(
          text = "Participants",
          icon = icon("user-friends"),
          href = "https://docs.google.com/spreadsheets/d/164DMKLRO0Xju0gdfkCS3evAq9ihTgEgFiuJopmqt7mo/edit#gid=562619943"
        ),
        menuItem(
          text = "New participant",
          tabName = "tab_new_participant",
          icon = icon("baby")
        ),
        menuItem(
          text = "GitHub",
          icon = icon("github"),
          href = "https://github.com/gongcastro/multilex"
        )
      )
    ),
    dashboardBody(
      tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
      tabItems(
        tabItem(
          tabName = "tab_dashboard",
          column(
            width = 5,
            fluidRow(
              box(
                width = 12,
                collapsible = TRUE,
                div(
                  img(src = "logo.png", height = 100, width = 90, style="display: inline-block; margin-left: 10%; margin-right: 1%;"),
                  img(src = "babylab.png", height = 100, width = 150, style="display: inline-block; margin-left: 1%; margin-right: 10%;")
                ),
                br(), br(),
                p("MultiLex is an R package that allows establishing reproducible workflows for assessing lexical development online using formR. This package extends the functionalities of formr (see formr repository) to ease the standardisation of online vocabulary checklists used by developmental psychologists. This Shiny app offers an interface to retrieve data from the formr questionnaires. You can find more information about the `multilex` R package in the section GitHub."),
                br(),
                actionButton(
                  "update", "Update data",
                  width = "100%",
                  icon = icon("arrow-circle-down"),
                  style = "color: #fff; background-color: #34a4eb; border-color: #34a4eb"
                )
              )
            ),
            fluidRow(
              box(
                width = 12,
                selectInput(
                  inputId = "dashboard_version",
                  label = "Version",
                  choices = c(
                    "BL-Long-1", "BL-Long-2",
                    "BL-Short-A", "BL-Short-B", "BL-Short-C", "BL-Short-D",
                    "BL-Lockdown-A", "BL-Lockdown-B", "BL-Lockdown-C", "BL-Lockdown-D"
                  ),
                  selectize = TRUE,
                  multiple = TRUE,
                  selected = c(
                    "BL-Long-1", "BL-Long-2",
                    "BL-Short-A", "BL-Short-B", "BL-Short-C", "BL-Short-D",
                    "BL-Lockdown-A", "BL-Lockdown-B", "BL-Lockdown-C", "BL-Lockdown-D"
                  )
                ),
                sliderInput(
                  inputId = "dashboard_age",
                  label = "Age",
                  min = 1,
                  max = round(max(responses$age, na.rm = TRUE)),
                  value = c(1, round(max(responses$age, na.rm = TRUE))),
                  step = 1
                )
              )
            )
          ),
          column(
            width = 7,
            tabBox(
              id = "dashboard_tabbox",
              title = "Dashboard",
              side = "right",
              width = 12,
              tabPanel(
                title = "Responses by date and version",
                plotOutput(outputId = "responses_dates")
              ),
              tabPanel(
                title = "Total responses by date",
                plotOutput(outputId = "responses_dates_summary")
              ),
              tabPanel(
                title = "Responses by age",
                plotOutput(outputId = "responses_ages")
              ),
              tabPanel(
                title = "Responses by language profile",
                plotOutput(outputId = "responses_lps")
              )
            )
          )
        ),
        tabItem(
          tabName = "tab_logs",
          fluidRow(
            box(
              title = "Logs",
              width = 5,
              p("This section shows information about participants' responses to the questionnaire. Each row is one response. One participant may have filled it more than once. The 'time' column indicates how many times a given participant has filled the questionnaire after their last response."),
              downloadButton(
                outputId = "logs_download",
                label = "Download"
              )
            ),
            box(
              title = "New responses",
              footer = "These participants have not been marked as 'Successful' in the Participants database.",
              status = "warning",
              solidHeader = FALSE,
              collapsible = TRUE,
              DT::dataTableOutput(outputId = "logs_successful"),
              width = 7
            )
          ),
          fluidRow(
            DT::dataTableOutput(outputId = "logs_all")
          )
        ),
        tabItem(
          tabName = "tab_vocabulary",
          fluidRow(
            box(
              title = "Vocabulary sizes",
              p("This section shows the computed comprehensive and productive vocabulary sizes of each response to the questionnaire, expressed in both relative (percentage) and absolute (number of words) terms."),
              downloadButton(
                outputId = "vocabulary_download",
                label = "Download"
              )
            ),
            DT::dataTableOutput(outputId = "vocabulary")
          ),
          tabItem(
            tabName = "tab_norms",
            fluidRow(
              box(
                title = "Item norms",
                p("This section shows the estimated proportion of participants that understand/produce each item. Results are computed for both translation equivalents (in Catalan and Spanish)."),
                downloadButton(
                  outputId = "norms_download",
                  label = "Download"
                )
              ),
              column(
                width = 2,
                selectInput(
                  inputId = "norms_item",
                  label = "Item",
                  choices = unique(norms$item),
                  selected = "cat_casa",
                  multiple = TRUE,
                  selectize = TRUE
                ),
                checkboxGroupInput(
                  inputId = "norms_lp",
                  label = "Language profile",
                  choices = c("Bilingual", "Monolingual", "Other"),
                  selected = c("Bilingual", "Monolingual")
                ),
                checkboxGroupInput(
                  inputId = "norms_item_dominance",
                  label = "Item dominance",
                  choices = c("L1", "L2"),
                  selected = c("L1")
                )
              ),
              column(
                width = 10,
                plotOutput(outputId = "norms_plot")
              )
            ),
            fluidRow(
              DT::dataTableOutput(outputId = "norms_table")
            )
          ),
          tabItem(
            tabName = "tab_pool",
            fluidRow(
              box(
                title = "Pool",
                p("This section shows the list of items included in all versions of the questionnaire, along with linguistic and lexical information. You can also acess this information in R running `multilex::pool`."),
                downloadButton(
                  outputId = "pool_download",
                  label = "Download"
                )
              )
            ),
            DT::dataTableOutput(outputId = "pool")
          ),
          tab_responses <- tabItem(
            tabName = "tab_responses",
            h2("In progress...")
          ),
          tabItem(
            tabName = "tab_new_participant",
            fluidRow(
              column(
                width = 5,
                status = "warning",
                box(
                  status = "warning",
                  textInput(
                    inputId = "id",
                    label = "ID",
                    value = NA_integer_,
                    placeholder = "bilexicon_0000"
                  ),
                  textInput(
                    inputId = "id_exp",
                    label = "ID (experiment)",
                    value = NA_integer_,
                    placeholder = "cognatepriming00"
                  ),
                  textInput(
                    inputId = "id_db",
                    label = "ID (database)",
                    value = NA_integer_,
                    placeholder = "00000"
                  ),
                  textInput(
                    inputId = "code",
                    label = "Code",
                    value = paste0("BL", max(as.numeric(gsub("BL", "", participants$code)))+1),
                    placeholder = paste0("BL", max(as.numeric(gsub("BL", "", participants$code)))+1)
                  ),
                  numericInput(
                    inputId = "time",
                    label = "Time",
                    min = 1,
                    value = 1,
                    step = 1
                  ),
                  dateInput(
                    inputId = "date_birth",
                    label = "Date of birth",
                    max = today(),
                    value = today(),
                    weekstart = 1,
                    autoclose = TRUE
                  )
                ),
                box(
                  selectInput(
                    inputId = "study",
                    label = "Study",
                    choices = studies,
                    selectize = TRUE,
                    multiple = FALSE,
                    selected = "BiLexicon"
                  ),
                  dateInput(
                    inputId = "date_test",
                    label = "Date of testing",
                    value = today(),
                    weekstart = 1,
                    autoclose = TRUE
                  ),
                  selectInput(
                    inputId = "cdi",
                    label = "CDI",
                    choices = cdi,
                    selectize = TRUE,
                    multiple = FALSE
                  ),
                  selectInput(
                    inputId = "version",
                    label = "Version",
                    choices = version,
                    selectize = TRUE,
                    multiple = FALSE
                  )
                ),
                box(
                  selectInput(
                    inputId = "call",
                    label = "Call status?",
                    choices = c("Successful", "Pending", "Try again", "Stop"),
                    selectize = TRUE,
                    multiple = FALSE,
                    selected = "BiLexicon"
                  ),
                  checkboxInput(
                    inputId = "email_ready",
                    label = "Email ready?",
                    value = FALSE
                  ),
                  checkboxInput(
                    inputId = "email_sent",
                    label = "Email sent?",
                    value = FALSE
                  ),
                  checkboxInput(
                    inputId = "reminded",
                    label = "Reminded?",
                    value = FALSE
                  ),
                  checkboxInput(
                    inputId = "completed",
                    label = "Completed?",
                    value = FALSE
                  ),
                  dateInput(
                    inputId = "date_sent",
                    label = "Date email was sent",
                    value = lubridate::today(),
                    weekstart = 1,
                    autoclose = TRUE
                  ),
                  textInput(
                    inputId = "comments",
                    label = "Comments"
                  )
                ),
                useShinyalert()
              ),
              column(
                width = 7,
                DT::dataTableOutput(outputId = "participants")
              )
            ),
            br(),
            fluidRow(
              column(
                width = 2,
                actionButton(
                  "send", "Save",
                  width = "100%",
                  icon = icon("save"),
                  style = "color: #fff; background-color: #3cc977; border-color: #3cc977"
                ),
                br(),
                actionButton(
                  "email", "Email",
                  width = "100%",
                  icon = icon("envelope"),
                  style = "color: #fff; background-color: #34a4eb; border-color: #34a4eb"
                ),
                br(),
                actionButton(
                  "close", "Close",
                  width = "100%",
                  icon = icon("times-circle"),
                  style = "color: #fff; background-color: #c8102f; border-color: #c8102f"
                )
              ),
              column(
                width = 10,
                box(
                  title = "Subject and URL",
                  solidHeader = TRUE,
                  textOutput("email_subject"),
                  br(),
                  textOutput("email_link")
                ),
                box(
                  title = "Body",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  textOutput("email_body")
                )
              )
            )
          )
        )
      )
    )
  )
)
