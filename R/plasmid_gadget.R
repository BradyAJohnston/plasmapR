#' Gadget Function
#'
#' @export
plasmid_gadget <- function(plasmid) {
  ui <- miniUI::miniPage(
    # Define the gadget title
    miniUI::gadgetTitleBar("Drag to select points"),

    # define the user-interface
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

    # output the content
    miniUI::miniContentPanel(
      shiny::plotOutput("plot", height = "100%")
    )
  )

  server <- function(input, output, session) {
    options(shiny.useragg = TRUE)

    # Render the plot
    output$plot <- shiny::renderPlot({
      plasmapR::render_plasmap(plasmid,
        rotation = input$rotation,
        labelNudge = input$nudge
      )
    })

    # Handle the Done button being pressed.
    shiny::observeEvent(input$done, {

      # return the same plot with the same options
      shiny::stopApp(
        plasmapR::render_plasmap(plasmid, rotation = input$rotation)
      )
    })
  }

  # run gadget as popup, as specified size.
  shiny::runGadget(ui, server, viewer = dialogViewer("plasmap",
    width = 1000,
    height = 1000
  ))
}
