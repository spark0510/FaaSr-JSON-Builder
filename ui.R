library("shiny")
library("DiagrammeR")

# this is for ui
ui <- fluidPage(
  fluidRow(
    # put a photo on the left top corner
    column(6, titlePanel(title=span(img(height=40, width=40, src="faasr_logo.jpg"), "FaaSr: JSON BUILDER"))),
    # put an exit button on the right corner.
    column(6, align = "right",
           div(style = "height: 20px;"),
           tags$button(
             id = 'close',
             type = "button",
             class = "btn action-button",
             onclick = "function(){window.close();};",  # close browser
             "Close window"
           )
    )
  ),
  fluidRow(
    # put a select input for selecting types.
    # it will call ui1
    column(4, wellPanel(
      selectInput("select1", "Select type:", c("FaaS Server", "Data Server", "Functions", "General Configuration")),
      br(),
      uiOutput("ui1")
    )
    ),
    column(8, wellPanel(
      fluidRow(
        # this is for upload button.
        column(4,
               h6(),
               fileInput("file1", "Choose JSON File",
                         width="100%",
                         multiple = FALSE,
                         accept = c("text/json",
                                    ".json")),
        ),
        # this is for download/removing button.
        column(2, 
               h5("", style = "height: 15px; visibility: hidden;"),
               actionButton("file_del","Remove")),
        column(2, br()),
        column(4, 
               h5(HTML("<b>Download JSON File</b>")),
               downloadButton("downjson", "Download JSON")
        )
      )
    ), wellPanel(
      # show the diagrams.
      fluidRow(
        column(12, grVizOutput("fsm_func", height="50%", width="100%"),
               br()),
      ),
      fluidRow(
        column(6, grVizOutput("fsm_data", height="10%", width="70%")),
        column(6, grVizOutput("fsm_faas", height="10%", width="70%"))
      )
    )
    )
  )
)