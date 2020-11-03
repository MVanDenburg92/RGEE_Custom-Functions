library(shiny)


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

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
