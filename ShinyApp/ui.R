
library(shiny)

print("Start Shiny UI")
shinyUI(
    fluidPage(
        # Application title
        titlePanel("NLP-based Word Prediction Project"),
        sidebarLayout(
            sidebarPanel(
                textInput("text", "Input Text:", value = "")
            ),
            mainPanel(
                h3("Prediction:", textOutput("Prediction"))
            )
        ) 
    )
)
print("Shiny UI Started")