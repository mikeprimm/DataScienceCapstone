
library(shiny)

print("Start Shiny UI")
shinyUI(
    fluidPage(
        # Application title
        titlePanel("NLP-based Word Prediction Project"),
        sidebarLayout(
            sidebarPanel(
                textInput("text", "Input Text:", value = ""),
                h3("Prediction (top):", textOutput("Prediction")),
                conditionalPanel(condition = "(output.Prediction2 != '')",
                                 h3("Prediction (second):", textOutput("Prediction2"))),
                conditionalPanel(condition = "(output.Prediction3 != '')",
                                 h3("Prediction (third):", textOutput("Prediction3")))
            ),
            mainPanel(
                plotOutput("WordCloud"),
                h5("Execution time: ", textOutput("ExecTime")),
                h5("Memory Used: ", textOutput("MemUsage"))
            )
        ) 
    )
)
print("Shiny UI Started")