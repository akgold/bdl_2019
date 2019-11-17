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

time_choices <- list(
    `Total Time` = "total_time",
    `Planning to Start Time` = "start_time",
    `Start to Completion Time` = "work_time"
)

other_choices <- list(
    `Total Units Proposed` = "proposed_total_residential_units",
    `Total Bedrooms Proposed` = "proposed_total_bedrooms",
    `Total Floorspace Proposed` = "proposed_total_floorspace"
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Planning Permissions in London"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("var1",
                        "Which Time Variable?",
                        choices = time_choices,
                        selected = "Total"),
            selectInput("var2",
                        "Which other variable?",
                        choices = other_choices,
                        selected = "Total Floorspace Proposed"),
            selectInput("pa",
                        "Planning Authority",
                        choices = "",
                        multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # Get choices for pa from data
    all_pa <- df %>%
        count(planning_authority) %>%
        pull(planning_authority)

    # Update input dropdown
    updateSelectInput(session, "pa",
                      choices = c("All", all_pa),
                      selected = "All")

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

    output$plot <- renderPlot({
        req(dat())

        # rlang magic
        time_var <- as.symbol(input$var1)
        other_var <- as.symbol(input$var2)

        # PUSH SUMMARIZATION TO DATA
        means <- dat() %>%
            summarize(mean1 = mean(!!time_var),
                      mean2 = mean(!!other_var)) %>%
            collect()
        mean1 <- means$mean1
        mean2 <- means$mean2

        x_name <- names(time_choices[time_choices == input$var1])
        y_name <- names(other_choices[other_choices == input$var2])

        # PUSH PLOTTING TO DATA
        dbplot::dbplot_raster(dat(),
                              x = !!time_var,
                              y = !!other_var) +
            xlab(x_name) +
            ylab(y_name) +
            ggthemes::theme_clean() +
            scale_fill_gradient(low = "#ffffff", high = "#5782b2") +
            geom_vline(xintercept = mean1) +
            geom_hline(yintercept = mean2) +
            scale_y_log10(labels = scales::comma) +
            scale_x_log10(labels = scales::comma) +
            geom_label(aes(x = mean1, y = mean2/2, label = glue::glue("Mean: {mean1}"))) +
            geom_label(aes(x = mean1/2, y = mean2, label = glue::glue("Mean: {mean2}")))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
