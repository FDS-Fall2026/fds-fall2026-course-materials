# Purpose: Standalone Shiny dashboard for the "Visualizing Financial Data"
#          lesson. This is the live, runnable counterpart to the reactive
#          code shown (with eval: false) in
#          Financial-Data-Visualization.qmd.
#
# Data: shared with the lesson document via Data/DataVizDemoData.rds,
#       produced by GenerateDataVizDemoData.R. If that file does not yet
#       exist, this script generates it on first run.
#
# Run with shiny::runApp("Topics/VizForFDS/ShinyDemo.R"), or open this file in
# Positron/RStudio and use the "Run App" button if available.

# Packages
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(scales)
library(here)

#Load Data
data_path <- here("Data", "DataVizDemoData.rds")

if (!file.exists(data_path)) {
  source(here("Topics", "VizForFDS", "GenerateDataVizDemoData.R"))
}

## Engineer
viz_demo_data <- readRDS(data_path)
asset_data <- viz_demo_data$asset_data
asset_specifications <- viz_demo_data$asset_specifications


# Create the "fluid" layout
ui <- fluidPage(
  fluidRow(
    column(
      width = 5,
      selectizeInput(
        inputId = "selected_assets",
        label = "Select exactly two assets:",
        choices = setNames(
          asset_specifications$ticker,
          paste0(
            asset_specifications$ticker,
            " — ",
            asset_specifications$asset_name
          )
        ),
        selected = c("TECH", "BANK"),
        multiple = TRUE,
        options = list(
          maxItems = 2,
          plugins = list("remove_button")
        )
      )
    ),
    column(
      width = 5,
      dateRangeInput(
        inputId = "selected_dates",
        label = "Select a date range:",
        start = min(asset_data$date),
        end = max(asset_data$date),
        min = min(asset_data$date),
        max = max(asset_data$date),
        format = "yyyy-mm-dd"
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      plotlyOutput(
        outputId = "shiny_price_plot",
        height = "430px"
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      plotlyOutput(
        outputId = "shiny_return_histograms",
        height = "390px"
      )
    )
  )
)

# Server function defines the logic that processes user inputs and generates outputs
server <- function(input, output, session) {
  selected_asset_data <- reactive({
    validate(
      need(
        length(input$selected_assets) == 2,
        "Select exactly two assets."
      ),
      need(
        length(input$selected_dates) == 2,
        "Select a valid start and end date."
      )
    )

    filtered_data <- asset_data %>%
      filter(
        ticker %in% input$selected_assets,
        date >= input$selected_dates[1],
        date <= input$selected_dates[2]
      )

    validate(
      need(
        nrow(filtered_data) > 0,
        "The selected date range contains no observations."
      ),
      need(
        n_distinct(filtered_data$ticker) == 2,
        "The selected period must contain observations for both assets."
      )
    )

    filtered_data %>%
      group_by(ticker) %>%
      arrange(date, .by_group = TRUE) %>%
      mutate(
        selected_period_index = 100 * price / first(price),
        selected_period_return = selected_period_index / 100 - 1
      ) %>%
      ungroup()
  })
  # generate the price plot via plotly
  output$shiny_price_plot <- renderPlotly({
    plot_data <- selected_asset_data() %>%
      mutate(
        tooltip = paste0(
          "<b>",
          asset_name,
          "</b>",
          "<br>Date: ",
          format(date, "%B %d, %Y"),
          "<br>Selected-period index: ",
          number(selected_period_index, accuracy = 0.1),
          "<br>Selected-period return: ",
          percent(selected_period_return, accuracy = 0.1)
        )
      )

    plot_ly(
      data = plot_data,
      x = ~date,
      y = ~selected_period_index,
      color = ~ticker,
      type = "scatter",
      mode = "lines",
      text = ~tooltip,
      hoverinfo = "text"
    ) %>%
      plotly::layout(
        title = list(
          text = paste0(
            "Selected Assets: Indexed Prices",
            "<br><sup>Each asset equals 100 on its first selected date</sup>"
          ),
          x = 0.02
        ),
        xaxis = list(title = ""),
        yaxis = list(title = "Indexed Price"),
        legend = list(
          title = list(text = "Ticker"),
          orientation = "h",
          x = 0,
          y = -0.18
        ),
        hovermode = "closest"
      )
  })
  # generate the histogram via plotly
  output$shiny_return_histograms <- renderPlotly({
    histogram_data <- selected_asset_data()

    return_histogram_plot <- histogram_data %>%
      ggplot(aes(x = daily_return, fill = ticker)) +
      geom_histogram(
        binwidth = 0.005,
        boundary = 0,
        color = "white",
        linewidth = 0.25,
        alpha = 0.85
      ) +
      geom_vline(
        xintercept = 0,
        color = "gray35",
        linewidth = 0.5,
        linetype = "dashed"
      ) +
      facet_wrap(vars(ticker), ncol = 2) +
      scale_x_continuous(
        labels = label_percent(accuracy = 1)
      ) +
      labs(
        title = "Selected Assets: Daily Return Distributions",
        subtitle = "Common bins and axes support cross-asset comparison",
        x = "Daily Return",
        y = "Number of Observations"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
      )

    ggplotly(
      return_histogram_plot,
      tooltip = c("x", "count")
    )
  })
}

# Comine the UI and servier into an app
shinyApp(ui, server)
