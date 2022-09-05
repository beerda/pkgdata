#'
#' @return
#' @author Michal Burda
#' @export
#' @importFrom shiny fillCol
#' @importFrom shiny fillRow
#' @importFrom shiny div
#' @importFrom shiny includeHTML
#' @importFrom shiny uiOutput
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny dialogViewer
#' @importFrom shiny runGadget
#' @importFrom shiny shinyApp
#' @importFrom miniUI miniPage
#' @importFrom miniUI miniContentPanel
#' @importFrom miniUI gadgetTitleBar
#' @importFrom DT datatable
#' @importFrom DT renderDataTable
#' @importFrom DT dataTableOutput
#' @importFrom tools Rd2HTML
#' @importFrom tools Rd_db
browse_data <- function(datasets = list_data(TRUE)) {
    window_name <- 'Package Data Browser'

    ui <- miniPage(
        gadgetTitleBar(window_name),
        fillCol(
            miniContentPanel(
                div(dataTableOutput('dataset_table'), style = 'font-size: 100%')
            ),
            miniContentPanel(
                div(DT::dataTableOutput('data_table'), style = 'font-size: 100%')
            ),
            #miniContentPanel(
                #uiOutput('help', inline = TRUE)
            #)
        )
    )

    server <- function(input, output, session) {
        tmp <- tempfile()

        output$dataset_table <- DT::renderDataTable({
            datatable(datasets,
                      rownames = FALSE,
                      filter = 'none',
                      selection = 'single',
                      class = 'compact nowrap hover order-column stripe',
                      options = list(dom = 't', # remove search input
                                     paging = FALSE))
        })

        output$help <- renderUI({
            selected <- req(input$dataset_table_rows_selected)
            if (!is.null(selected)) {
                name <- datasets[selected, 'name']
                package <- datasets[selected, 'package']
                rdfile <- paste0(name, '.Rd')
                Rd2HTML(Rd_db(package)[[rdfile]], tmp, no_links = TRUE, package=package)
                includeHTML(tmp)
            }
        })

        output$data_table <- DT::renderDataTable({
            selected <- req(input$dataset_table_rows_selected)
            d <- data.frame()
            if (!is.null(selected)) {
                name <- datasets[selected, 'name']
                package <- datasets[selected, 'package']
                d <- load_data(name, package)
            }
            datatable(d,
                      rownames = TRUE,
                      filter = 'none',
                      selection = 'none',
                      class = 'compact nowrap hover order-column stripe',
                      options = list(paging = TRUE))
        })
    }

    #viewer <- dialogViewer(window_name, width = 1600, height = 1200)
    #runGadget(ui, server, viewer = viewer)
    shinyApp(ui, server)
}
