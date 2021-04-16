#' Gadget Function
#'
#' @export
plasmid_gadget <- function(plasmid) {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Drag to select points"),
    miniUI::miniButtonBlock(
      shiny::sliderInput(
        inputId = "rotation",
        label = "Rotate Plot",
        min = -180,
        max = 180,
        value = 0,
        step = 1
      ),
      shiny::sliderInput(
        inputId = "nudge",
        label = "Nudge Labels",
        min = 0,
        max = 3,
        value = 0.4,
        step = 0.1
      )
    ),
    miniUI::miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      shiny::plotOutput("plot", height = "100%")
    )
  )

  server <- function(input, output, session) {

    # Render the plot
    output$plot <- shiny::renderPlot({
      # Plot the data with x/y vars indicated by the caller.
      plasmapR::render_plasmap(plasmid,
        rotation = input$rotation,
        labelNudge = input$nudge
      )
    })

    # Handle the Done button being pressed.
    shiny::observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      shiny::stopApp(
        plasmapR::render_plasmap(plasmid, rotation = input$rotation)
      )
    })
  }

  shiny::runGadget(ui, server, viewer = dialogViewer("plasmap",
    width = 1000,
    height = 1000
  ))
}
