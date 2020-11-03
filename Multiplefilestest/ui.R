#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Test"),
    sidebarLayout(
        sidebarPanel(

            selectInput("countryInput", "Landscape",
                        choices = c("National Park 1", "national Park 2", "National Park 3", "National Park4")),
            selectInput("IndexInput", "Index",
                        choices = c("NDVI", "EVI" )),
            dateRangeInput()
        ),
        mainPanel(plotOutput("Raster plot"),
        )
    )
)
