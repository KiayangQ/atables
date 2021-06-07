
#' @title output tables
#'
#' @description A simple function that helps to output tables in .docx
#'
#'
#'
#' @importFrom cli cli_alert_success
#'
#' @importFrom purrr map map2
#'
#' @importFrom flextable set_flextable_defaults flextable body_add_flextable set_caption
#'
#' @import officer
#'
#' @import magrittr
#'
#' @import shiny
#'
#' @import miniUI
#'



output_tables <- function() {
  if (length(ls(envir = .GlobalEnv))==0){
    stop("No object was detected on global environment")
  }else{
    temp <-  ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv),function(x)any(class(get(x))%in%c("data.frame")))]
  }
  ui <- miniPage(
    gadgetTitleBar("Output tables to a .docx file"),
    miniContentPanel(
      # Define layout, inputs, outputs
      selectInput(
        inputId = "tableName", label = "Select Table:",
        selected = NULL, multiple = TRUE,
        choices = c(temp)
      ),
      selectInput(
        inputId = "Nums", label = "Number of decimals:",
        selected = 2,
        choices = c(1,2,3,4,5)
      ),
      selectInput(
        inputId = "Themes", label = "Select a theme:",
        selected = "theme_booktabs",
        choices = c("theme_booktabs","theme_alafoli","theme_vader","theme_box","theme_vanilla","theme_zebra","theme_tron_legacy","theme_tron")
      ),
      textInput("filename",label="File name:",value="output.docx"),
      radioButtons("caps","Input captions:",choices=c("Yes"="yes","No"="no"),selected="no"),
      conditionalPanel(
        condition = "input.caps == 'yes'",
        textInput("captions",label="Captions(semicolon as separator)",value="")
      )
    )
  )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.

    # When the Done button is clicked, return a value
    observeEvent(input$done,{
      tables <- map(input$tableName,function(x)get(x))
      if (input$caps=='yes'){
          output_docx1(tables,filename = input$filename,captions = input$captions,digits = as.numeric(input$Nums),theme=input$Themes)
      }else{
        output_docx1(tables,filename = input$filename,digits = as.numeric(input$Nums),theme=input$Themes)
        }
      stopApp()
    })
  }

  runGadget(ui, server,viewer = dialogViewer("Output Tables"))
}
