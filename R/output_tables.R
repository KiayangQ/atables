library(shiny)
library(miniUI)
library(purrr)
library(officer)
library(magrittr)
library(flextable)
library(cli)

temp <-  ls()[sapply(ls(),function(x)any(class(get(x))%in%c("data.frame")))]

output_tables <- function() {

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
      textInput("filename",label="File name",value="output.docx"),
      radioButtons("caps","Input captions?",choices=c("Yes"="yes","No"="no"),selected="no"),
      conditionalPanel(
        condition = "input.caps == 'yes'",
        textInput("captions",label="Captions",value="")
      )
    )
  )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.

    # When the Done button is clicked, return a value
    observeEvent(input$done,{
      tables <- map(input$tableName,function(x)get(x))
      if (input$caps=='yes'){
          output_docx1(tables,filename = input$filename,captions = input$captions,digits = input$Nums)
      }else{
        output_docx1(tables,filename = input$filename,digits = input$Nums)
        }
      stopApp()
    })
  }

  runGadget(ui, server,viewer = dialogViewer("Output Tables"))
}
