library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(magrittr)
library(readxl)
library(shinythemes)
# Define UI for application that draws a histogram


ui <- pageWithSidebar(headerPanel("Indices Macroeconomicos de paises miembros del T-MEC"),
    sidebarPanel(
      selectInput("Seleccionar", "Seleccionar Indice", 
                  choices = list("Producto Interno Bruto","Volumen de Importaciones" ,
                                 "Volumen de Exportaciones",
                                 "Desempleo")
      )),
    
    mainPanel(
      dygraphOutput(outputId = "PIB",width = "100%", height = "400px"),
      dygraphOutput(outputId = "Vol.Imp",width = "100%", height = "400px"),
      dygraphOutput(outputId = "VolExp",width = "100%", height = "400px"),
      dygraphOutput(outputId = "Desempleo",width = "100%", height = "400px")
    )
  )


# Define server logic required 
server <- function(input, output, session) {
  output$PIB <- renderDygraph(if (input$Seleccionar =="Producto Interno Bruto")
    {dygraph(PIB, main = "Producto Interno Bruto de paises miembros del T-MEC",
                                       ylab = "Dolares (en Billones)")%>% dyRangeSelector()%>%
      dyAxis("x", label = "Año (1994 - 2020)") %>%
      dyLegend(width = 400)%>%
      dyOptions(fillGraph = TRUE, includeZero = TRUE, axisLineWidth = 1,
                axisLabelFontSize = 10,
                drawPoints = TRUE, pointSize = 2, pointShape = "diamond", drawGrid = FALSE) %>%
      dySeries("PIBCan", label = "PIB Canada", color = "red") %>%
      dySeries("PIBMex", label = "PIB Mexico", color = "darkgreen") %>%
      dySeries("PIBUsa", label = "PIB EEUU", color = "blue") %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.5,
                  hideOnMouseOut = TRUE,
                  highlightSeriesOpts = list(strokeWidth = 1))%>%
      dyCrosshair(direction = "vertical")%>%
      dyRangeSelector(height = 15)
  })
  output$Vol.Imp <- renderDygraph(if (input$Seleccionar =="Volumen de Importaciones")
    {dygraph(Vol.Imp, main = "Volumen de importacion de bienes y servicios",
                                           ylab = "cambio porcentual %")%>% dyRangeSelector()%>%
      dyAxis("x", label = "Año (1994 - 2020)") %>%
      dyLegend(width = 400)%>%
      dyOptions(fillGraph = TRUE, includeZero = TRUE, axisLineWidth = 1,
                axisLabelFontSize = 10,
                drawPoints = TRUE, pointSize = 2, pointShape = "diamond", drawGrid = FALSE) %>%
      dySeries("VolImCan", label = "Canada", color = "red") %>%
      dySeries("VolImMex", label = "Mexico", color = "darkgreen") %>%
      dySeries("VolImUsa", label = "EEUU", color = "blue") %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.5,
                  hideOnMouseOut = TRUE,
                  highlightSeriesOpts = list(strokeWidth = 1))%>%
      dyCrosshair(direction = "vertical")%>%
      dyRangeSelector(height = 15)})
  
  output$VolExp <- renderDygraph(if (input$Seleccionar =="Volumen de Exportaciones")
    {dygraph(VolExp, main = "Volumen de exportación de bienes y servicios",
                                          ylab = "cambio porcentual %")%>% dyRangeSelector()%>%
      dyAxis("x", label = "Año (1994 - 2020)") %>%
      dyLegend(width = 400)%>%
      dyOptions(fillGraph = TRUE, includeZero = TRUE, axisLineWidth = 1,
                axisLabelFontSize = 10,
                drawPoints = TRUE, pointSize = 2, pointShape = "diamond", drawGrid = FALSE) %>%
      dySeries("VolExCan", label = "Canada", color = "red") %>%
      dySeries("VolExMex", label = "Mexico", color = "darkgreen") %>%
      dySeries("VolExUsa", label = "EEUU", color = "blue") %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.5,
                  hideOnMouseOut = TRUE,
                  highlightSeriesOpts = list(strokeWidth = 1))%>%
      dyCrosshair(direction = "vertical")%>%
      dyRangeSelector(height = 15)})
  
  output$Desempleo <- renderDygraph(if (input$Seleccionar =="Desempleo")
    {dygraph(Desempleo, main = "Tasa de desempleo de paises miembros del T-MEC",
                                             ylab = "Tasa%")%>% dyRangeSelector()%>%
      dyAxis("x", label = "Año (1994 - 2020)") %>%
      dyLegend(width = 400)%>%
      dyOptions(fillGraph = TRUE, includeZero = TRUE, axisLineWidth = 1,
                axisLabelFontSize = 10,
                drawPoints = TRUE, pointSize = 2, pointShape = "diamond", drawGrid = FALSE) %>%
      dySeries("TasDesCan", label = "Canada", color = "red") %>%
      dySeries("TasDesMex", label = "Mexico", color = "darkgreen") %>%
      dySeries("TasDesUsa", label = "EEUU", color = "blue") %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.5,
                  hideOnMouseOut = TRUE,
                  highlightSeriesOpts = list(strokeWidth = 1))%>%
      dyCrosshair(direction = "vertical")%>%
      dyRangeSelector(height = 15)})
}
# Run the application 
shinyApp(ui = ui, server = server)
