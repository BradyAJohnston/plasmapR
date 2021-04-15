#' #' Gadget Function
#'
#' plasmid_gadget <- function(data, xvar, yvar) {
#'   ui <- miniUI::miniPage(
#'     miniUI::gadgetTitleBar("Drag to select points"),
#'     miniUI::miniContentPanel(
#'       # The brush="brush" argument means we can listen for
#'       # brush events on the plot using input$brush.
#'       shiny::plotOutput("plot", height = "100%", brush = "brush")
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     # Render the plot
#'     output$plot <- shiny::renderPlot({
#'       # Plot the data with x/y vars indicated by the caller.
#'       ggplot2::ggplot(data, ggplot2::aes_string(xvar, yvar)) +
#'         ggplot2::geom_point()
#'     })
#'
#'     # Handle the Done button being pressed.
#'     shiny::observeEvent(input$done, {
#'       # Return the brushed points. See ?shiny::brushedPoints.
#'       shiny::stopApp(shiny::brushedPoints(data, input$brush))
#'     })
#'   }
#'
#'   shiny::runGadget(ui, server)
#' }
#'
#' plasmid_gadget(mtcars, "hp", "mpg")
#'
