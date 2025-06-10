library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(DT)
library(lubridate)
library(ggthemes)

# === ENVIRONMENT CONFIG ===
supabase_url <- Sys.getenv("SUPABASE_URL")
api_key <- Sys.getenv("SUPABASE_API_KEY")

# === DATA FETCH FUNCTION ===
load_data <- function(table_name) {
  res <- GET(
    url = paste0(supabase_url, "/rest/v1/", table_name),
    add_headers(
      apikey = api_key,
      Authorization = paste("Bearer", api_key)
    ),
    query = list(select = "*")
  )
  
  if (status_code(res) != 200) {
    stop(paste("Failed to fetch data from", table_name, ":", content(res, "text")))
  }
  
  raw_json <- content(res, as = "text", encoding = "UTF-8")
  parsed_data <- fromJSON(raw_json, simplifyDataFrame = TRUE)
  df <- as_tibble(parsed_data)
  return(df)
}

# === UI ===
ui <- navbarPage(
  title = div(style = "color: steelblue; background-color: white; font-size: 24px; font-weight: bold;", "UrbanFit Dashboard"),
  tabPanel("Landing Page",
           fluidPage(
             h3("Welcome to UrbanFit Dashboard")
           )
  ),
  tabPanel("Fundamentals",
           sidebarLayout(
             sidebarPanel(
               h4("Select Date Range"),
               dateInput("from_date", "From:", value = Sys.Date() - 30),
               dateInput("to_date", "To:", value = Sys.Date()),
               br()
             ),
             mainPanel(
               p(style = "font-weight: bold; font-size: 16px", 
                 "Click on the plot of interest to display the data below."),
               fluidRow(
                 column(6,
                        div(style = "border: 1px solid #ccc; padding: 5px;",
                            plotOutput("barplot_transactions", height = "300px", click = "click_transactions")
                        )
                 ),
                 column(6,
                        div(style = "border: 1px solid #ccc; padding: 5px;",
                            plotOutput("checkins_plot", height = "300px", click = "click_checkins")
                        )
                 )
               ),
               fluidRow(
                 column(6,
                        div(style = "border: 1px solid #ccc; padding: 5px;",
                            plotOutput("classes_plot", height = "300px", click = "click_classes")
                        )
                 ),
                 column(6,
                        div(style = "border: 1px solid #ccc; padding: 5px;",
                            plotOutput("sales_plot", height = "300px", click = "click_sales")
                        )
                 )
               ),
               br(),
               h4(textOutput("table_title")),
               DTOutput("selected_table")
             )
           )
  ),
  tabPanel("Membership",
           sidebarLayout(
             sidebarPanel(
               h4("Select Date Range"),
               dateInput("from_date_membership", "From:", value = Sys.Date() - 30),
               dateInput("to_date_membership", "To:", value = Sys.Date())
             ),
             mainPanel(
               div(style = "border: 1px solid #ccc; padding: 5px;",
                   plotOutput("top_members_plot", height = "400px")
               ),
               br(),
               div(style = "border: 1px solid #ccc; padding: 5px;",
                   plotOutput("declining_attendance_plot", height = "400px")
               ),
               br(),
               p("This plot uses linear regression (via the lm() function) to estimate attendance trends over time for each member. Each line represents the actual weekly attendance, while the dashed regression line approximates the overall trend. A negative slope suggests declining attendance.")
             )
           )
  )
)

# === SERVER ===
server <- function(input, output, session) {
  # Load datasets
  transactions <- reactive({ load_data("transactions") })
  checkins <- reactive({ load_data("checkins") })
  classes <- reactive({ load_data("class_signups") })
  sales <- reactive({ load_data("shopify_sales") })
  
  clicked_table <- reactiveVal("Transactions")
  
  observeEvent(input$click_transactions, { clicked_table("Transactions") })
  observeEvent(input$click_checkins, { clicked_table("Check-ins") })
  observeEvent(input$click_classes, { clicked_table("Classes") })
  observeEvent(input$click_sales, { clicked_table("Sales") })
  
  output$barplot_transactions <- renderPlot({
    transactions() |>
      filter(date >= input$from_date, date <= input$to_date) |>
      mutate(amount = as.numeric(amount)) |>
      group_by(category) |>
      summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") |>
      ggplot(aes(x = category, y = total)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      labs(title = "Spending by Category", y = "Total Amount", x = NULL) +
      theme_minimal(base_size = 16) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$checkins_plot <- renderPlot({
    checkins() |>
      mutate(date = as.Date(timestamp)) |>
      filter(date >= input$from_date, date <= input$to_date) |>
      count(date) |>
      ggplot(aes(x = date, y = n)) +
      geom_line(color = "forestgreen", linewidth = 1.2) +
      labs(title = "Check-ins per Day", y = "Total Check-ins", x = NULL) +
      theme_minimal(base_size = 16)
  })
  
  output$classes_plot <- renderPlot({
    df <- classes() |>
      filter(signup_date >= input$from_date, signup_date <= input$to_date) |>
      count(class_type) |>
      mutate(percentage = round(100 * n / sum(n), 1),
             label = paste0(class_type, ": ", percentage, "%"))
    
    ggplot(df, aes(x = "", y = n, fill = class_type)) +
      geom_col(width = 1) +
      scale_fill_tableau() +
      coord_polar(theta = "y") +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
      labs(title = "Class Signups by Type", y = NULL, x = NULL, fill = "Class Type") +
      theme_void(base_size = 16) +
      theme(legend.position = "right")
  })
  
  output$sales_plot <- renderPlot({
    sales() |>
      filter(purchase_date >= input$from_date, purchase_date <= input$to_date) |>
      mutate(amount = as.numeric(amount)) |>
      group_by(product) |>
      summarise(revenue = sum(amount, na.rm = TRUE), .groups = "drop") |>
      ggplot(aes(x = product, y = revenue)) +
      geom_col(fill = "goldenrod", alpha = 0.8) +
      labs(title = "Sales by Product", y = "Total Revenue", x = NULL) +
      theme_minimal(base_size = 16) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$top_members_plot <- renderPlot({
    df <- classes()
    
    if (!all(c("signup_date", "member_id") %in% names(df))) {
      plot.new()
      title("Missing fields in class_signups data.")
      return()
    }
    
    df <- df |>
      filter(signup_date >= input$from_date_membership, signup_date <= input$to_date_membership) |>
      count(member_id, name = "classes_attended") |>
      arrange(desc(classes_attended)) |>
      slice_head(n = 10)
    
    if (nrow(df) == 0) {
      plot.new()
      title("No class attendance data for selected date range.")
      return()
    }
    
    ggplot(df, aes(x = reorder(member_id, classes_attended), y = classes_attended)) +
      geom_col(fill = "darkorchid") +
      coord_flip() +
      labs(
        title = "Top 10 Most Active Members by Class Signups", 
        x = "Member ID", 
        y = "Number of Classes") +
      theme_minimal(base_size = 16)
  })
  
  output$declining_attendance_plot <- renderPlot({
    df <- classes()
    
    if (!all(c("signup_date", "member_id") %in% names(df))) {
      plot.new()
      title("Missing fields in class_signups data.")
      return()
    }
    
    df <- df |>
      mutate(signup_date = as.Date(signup_date),
             week = floor_date(signup_date, "week")) |>
      count(member_id, week, name = "classes")
    
    slope_df <- df |>
      group_by(member_id) |>
      filter(n() >= 4) |>
      group_modify(~{
        tryCatch({
          model <- lm(classes ~ as.numeric(week), data = .x)
          tibble(slope = coef(model)[2])
        }, error = function(e) tibble(slope = NA_real_))
      }) |>
      filter(!is.na(slope), slope < 0) |>
      arrange(slope)
    
    top_decliners <- slope_df |> slice_head(n = 5) |> pull(member_id)
    
    plot_data <- df |> filter(member_id %in% top_decliners)
    
    ggplot(plot_data, aes(x = week, y = classes, color = member_id, group = member_id)) +
      geom_line(size = 1.5) +
      scale_color_tableau() +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1) +
      labs(
        title = "Top 5 Members with Declining Attendance", 
        x = "Week", 
        y = "Classes Attended",
        color = "Member ID") +
      theme_minimal(base_size = 16)
  })
  
  output$table_title <- renderText({
    paste("Data Table -", clicked_table())
  })
  
  output$selected_table <- renderDT({
    switch(clicked_table(),
           "Transactions" = transactions() |> filter(date >= input$from_date, date <= input$to_date),
           "Check-ins" = checkins() |> mutate(date = as.Date(timestamp)) |> filter(date >= input$from_date, date <= input$to_date),
           "Classes" = classes() |> filter(signup_date >= input$from_date, signup_date <= input$to_date),
           "Sales" = sales() |> filter(purchase_date >= input$from_date, purchase_date <= input$to_date)
    )
  })
}

# === RUN APP ===
shinyApp(ui, server)
