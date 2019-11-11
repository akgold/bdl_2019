library(shiny)
library(DBI)
library(dplyr)
library(ggplot2)

# Connect to database at start of session
con <- dbConnect(odbc::odbc(),
                 Driver="postgresql",
                 Server = "localhost",
                 Port = "5432",
                 Database = "postgres",
                 UID = "postgres",
                 PWD = Sys.getenv("db_pass"),
                 BoolsAsChar = "",
                 timeout = 10)
dbExecute(con, "SET search_path=london;")
df <- tbl(con, "plan")

choices <- list(
    `Total Time` = "total_time",
    `Planning to Start Time` = "start_time",
    `Start to Completion Time` = "work_time"
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Planning Permissions in London"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("var",
                        "Which Variable?",
                        choices = choices,
                        selected = "Total"),
            selectInput("pa",
                        "Planning Authority",
                        choices = "",
                        multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("timePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # Get choices for pa from data
    all_pa <- df %>%
        pull(planning_authority) %>%
        unique()

    # Update input dropdown
    updateSelectInput(session, "pa",
                      choices = c("All", all_pa))

    # Process to allow ALL
    pa <- reactive(ifelse(input$pa == "All", all_pa, input$pa))

    # Filter data as appropriate
    dat <- reactive({
        req(pa())

        # Need to evaluate reactive ahead
        pa <- pa()

        df %>%
            filter(planning_authority %in% pa)
    })

    output$timePlot <- renderPlot({
        req(dat())

        var <- as.symbol(input$var)

        mean <- dat() %>%
            summarize(m = mean(!!var)) %>%
            pull(m)

        ggplot(dat()) +
            geom_density(aes(x = !!var)) +
            xlab(paste(names(choices[choices == input$var]), "(Days)")) +
            ylab("Density") +
            scale_y_continuous(labels = scales::percent) +
            geom_vline(xintercept = mean) +
            geom_label(aes(x = mean, y = 0.0003,
                           label = glue::glue("Mean: {round(mean)}"))) +
            ggthemes::theme_clean()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
