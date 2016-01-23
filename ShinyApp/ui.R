
library(shiny)

print("Start Shiny UI")
shinyUI(
    fluidPage(
        # Application title
        titlePanel("NLP Word Prediction Project"),
        p("By Mike Primm"),
        p("This project demonstrates a next word prediction algorithm, based on N-Gram frequency analysis and Markov chains, derived from blog, news, and Twitter data provided by SwiftKey."),
        p("The implementation has attempted to maximize the leverage of the supplied training data (deriving the model from analysis of almost 7 million provided sentences), while minimizing the computational and memory cost of the production use of the algorithm."),
        p("Enter any desired text, including punctuation or multiple sentences, and algorithm will attempt to guess most likely next word after last one entered."),
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