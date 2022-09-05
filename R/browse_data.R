#'
#' @return
#' @author Michal Burda
#' @export
#' @importFrom shiny div
#' @importFrom shiny includeHTML
#' @importFrom shiny uiOutput
#' @importFrom shiny renderUI
#' @importFrom shiny plotOutput
#' @importFrom shiny renderPlot
#' @importFrom shiny req
#' @importFrom shiny shinyApp
#' @importFrom shiny fluidPage
#' @importFrom shiny fluidRow
#' @importFrom shiny column
#' @importFrom shiny titlePanel
#' @importFrom shiny sidebarLayout
#' @importFrom shiny sidebarPanel
#' @importFrom shiny mainPanel
#' @importFrom shiny tabsetPanel
#' @importFrom shiny tabPanel
#' @importFrom shiny reactive
#' @importFrom shiny checkboxInput
#' @importFrom shiny sliderInput
#' @importFrom shiny selectInput
#' @importFrom DT datatable
#' @importFrom DT renderDataTable
#' @importFrom DT dataTableOutput
#' @importFrom tools Rd2HTML
#' @importFrom tools Rd_db
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 aes
browse_data <- function(data_list = list_data(TRUE)) {
    window_name <- 'Package Data Browser'
    anyChoice <- '-- any --'
    packageChoices <- c(anyChoice, levels(data_list$package))
    classChoices <- c(anyChoice, levels(data_list$class))
    modeChoices <- c(anyChoice, levels(data_list$mode))
    typeChoices <- c(anyChoice, levels(data_list$type))

    createSlider <- function(columnName, label) {
        sliderInput(columnName,
                    label,
                    step = 1,
                    min = min(data_list[[columnName]], na.rm = TRUE),
                    max = max(data_list[[columnName]], na.rm = TRUE),
                    value = c(min(data_list[[columnName]], na.rm = TRUE),
                              max(data_list[[columnName]], na.rm = TRUE)))
    }

    filterEquality <- function(d, column, value, naAllowed) {
        naCondition <- FALSE
        if (naAllowed) {
            naCondition <- is.na(d[[column]])
        }
        choiceCondition <- TRUE
        if (value != anyChoice) {
            choiceCondition <- !is.na(d[[column]]) & (d[[column]] == value)
        }

        d[naCondition | choiceCondition, ]
    }

    filterRange <- function(d, column, values, naAllowed) {
        naCondition <- FALSE
        if (naAllowed) {
            naCondition <- is.na(d[[column]])
        }

        d[naCondition | (!is.na(d[[column]]) & d[[column]] >= values[1] & d[[column]] <= values[2]), ]
    }

    ui <- fluidPage(
        titlePanel(window_name),
        sidebarLayout(
            sidebarPanel(
                selectInput('package', 'package', packageChoices),
                selectInput('class', 'class', classChoices),
                selectInput('mode', 'mode', modeChoices),
                selectInput('type', 'type', typeChoices),
                createSlider('records', 'records'),
                createSlider('variables', 'variables'),
                createSlider('numericVariables', 'numeric variables'),
                createSlider('factorVariables', 'factor variables'),
                createSlider('characterVariables', 'character variables'),
                createSlider('logicalVariables', 'logical variables'),
                checkboxInput('loadable', 'loadable', value = TRUE),
                checkboxInput('naAllowed', 'allow NA values', value = TRUE)
            ),
            mainPanel(
                dataTableOutput('dataset_table'),
                tabsetPanel(type = 'tabs',
                    tabPanel('summary',
                        fluidRow(
                            column(4, plotOutput('records_plot', height = '200px')),
                            column(4, plotOutput('variables_plot', height = '200px'))
                        ),
                        fluidRow(
                            column(4, plotOutput('class_plot', height = '200px')),
                            column(4, plotOutput('mode_plot', height = '200px')),
                            column(4, plotOutput('type_plot', height = '200px'))
                        )
                    ),
                    tabPanel('dataset', dataTableOutput('data_table')),
                    tabPanel('description', uiOutput('help'))
                )
            )
        )
    )

    server <- function(input, output, session) {
        tmp <- tempfile()

        datasets <- reactive({
            result <- data_list
            result <- filterEquality(result, 'loadable', input$loadable, input$naAllowed)
            result <- filterEquality(result, 'package', input$package, input$naAllowed)
            result <- filterEquality(result, 'class', input$class, input$naAllowed)
            result <- filterEquality(result, 'mode', input$mode, input$naAllowed)
            result <- filterEquality(result, 'type', input$type, input$naAllowed)
            result <- filterRange(result, 'records', input$records, input$naAllowed)
            result <- filterRange(result, 'variables', input$variables, input$naAllowed)
            result <- filterRange(result, 'numericVariables', input$numericVariables, input$naAllowed)
            result <- filterRange(result, 'factorVariables', input$factorVariables, input$naAllowed)
            result <- filterRange(result, 'characterVariables', input$characterVariables, input$naAllowed)
            result <- filterRange(result, 'logicalVariables', input$logicalVariables, input$naAllowed)

            result
        })

        output$dataset_table <- DT::renderDataTable({
            datatable(datasets(),
                      rownames = FALSE,
                      filter = 'none',
                      selection = 'single',
                      class = 'compact nowrap hover order-column stripe',
                      options = list(paging = TRUE))
        })

        output$help <- renderUI({
            selected <- req(input$dataset_table_rows_selected)
            if (!is.null(selected)) {
                name <- datasets()[selected, 'name']
                package <- as.character(datasets()[selected, 'package'])
                rdfile <- paste0(name, '.Rd')
                Rd2HTML(Rd_db(package)[[rdfile]], tmp, no_links = TRUE, package=package)
                includeHTML(tmp)
            }
        })

        output$data_table <- DT::renderDataTable({
            selected <- req(input$dataset_table_rows_selected)
            d <- data.frame()
            if (!is.null(selected)) {
                name <- datasets()[selected, 'name']
                package <- as.character(datasets()[selected, 'package'])
                d <- load_data(name, package)
            }
            datatable(d,
                      rownames = TRUE,
                      filter = 'none',
                      selection = 'none',
                      class = 'compact nowrap hover order-column stripe',
                      options = list(paging = TRUE))
        })

        output$records_plot <- renderPlot({
            ggplot(datasets()) + geom_histogram(aes(x = records), bins = 30)
        })

        output$variables_plot <- renderPlot({
            ggplot(datasets()) + geom_histogram(aes(x = variables), bins = 30)
        })

        output$class_plot <- renderPlot({
            ggplot(datasets()) + geom_bar(aes(x = class), na.rm = TRUE) + coord_flip()
        })

        output$mode_plot <- renderPlot({
            ggplot(datasets()) + geom_bar(aes(x = mode), na.rm = TRUE) + coord_flip()
        })

        output$type_plot <- renderPlot({
            ggplot(datasets()) + geom_bar(aes(x = type), na.rm = TRUE) + coord_flip()
        })
    }

    shinyApp(ui, server)
}
