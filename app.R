library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(DT)
library(lubridate)
library(ggthemes)

# === ENVIRONMENT CONFIG ===
supabase_url <- Sys.getenv("SUPABASE_URL")
anon_key <- Sys.getenv("SUPABASE_API_KEY")
service_key <- Sys.getenv("SUPABASE_SERVICE_KEY")

# Choose service key if it's defined, else fall back to anon
supabase_key <- if (nzchar(service_key)) service_key else anon_key


# === DATA FETCH FUNCTION ===
load_data <- function(table_name) {
  res <- GET(
    url = paste0(supabase_url, "/rest/v1/", table_name),
    add_headers(
      Authorization = paste("Bearer", supabase_key),
      apikey = supabase_key
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
             tags$head(
               tags$style(HTML("
        .landing-title {
          font-size: 36px;
          font-weight: bold;
          color: steelblue;
        }
        .landing-subtitle {
          font-size: 20px;
          margin-top: 10px;
          color: #444;
        }
        .landing-section {
          margin-top: 30px;
        }
        .feature-box {
          background-color: #f8f9fa;
          border-radius: 10px;
          padding: 20px;
          margin-top: 15px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05);
        }
      "))
             ),
             
             # Enhanced Design Elements
             div(class = "landing-title", "Welcome to UrbanFit Dashboard"),
             div(class = "landing-subtitle",
                 "Gain real-time insights into your gym's operations, financial health, and member engagement."),
             div(class = "feature-box",
                 "This is a real dashboard for a mock company, powered by randomly generated data to produce delightfully non-sensical insights. ",
                 "The backend Supabase database is fully functional (though it stores only fake data), and the full source code is available on GitHub: ",
                 tags$a(href = "https://github.com/MLDavies/urbanfit-dashboard", 
                        "MLDavies/urbanfit-dashboard", 
                        target = "_blank")
             ),
             
             div(class = "landing-section",
                 h3("📊 What You Can Do"),
                 div(class = "feature-box",
                     tags$ul(
                       tags$li("View and compare financial transactions, class signups, and product sales"),
                       tags$li("Explore trends in member activity and retention"),
                       tags$li("Identify top-performing classes and instructors"),
                       tags$li("Track attendance patterns and spot declining engagement early")
                     )
                 )
             ),
             
             div(class = "landing-section",
                 h3("🛠 How It Works"),
                 div(class = "feature-box",
                     "This dashboard integrates data from your gym's point-of-sale, class signups, check-ins, and online store.
             It pulls from a centralized Supabase database and renders clean visual summaries using R and Shiny."
                 )
             ),
             
             div(class = "landing-section",
                 h3("🚀 Get Started"),
                 div(class = "feature-box",
                     "Use the tabs at the top to explore different views of your business performance.
             Try clicking on the visualizations to filter the tables below!"
                 )
             )
           )
  ),
  tabPanel("Fundamentals",
           sidebarLayout(
             sidebarPanel(
               h4("Select Date Range"),
               dateInput("from_date", "From:", value = "2025-04-11"),
               dateInput("to_date", "To:", value = "2025-06-07"),
               br(),
               p(strong("📈 About the Plots")),
               p("Each plot provides key insight into different aspects of gym operations:"),
               tags$ul(
                 tags$li(strong("Transactions:"), " Shows total spending by category to help track business costs."),
                 tags$li(strong("Check-ins:"), " Displays member check-ins over time to monitor daily activity."),
                 tags$li(strong("Class Signups:"), " Reveals which class types are most popular among members."),
                 tags$li(strong("Sales:"), " Highlights revenue by product to understand shop performance.")
               ),
               p("Click on any plot to view the detailed data table below."),
               
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
               dateInput("to_date_membership", "To:", value = Sys.Date()),
               br(),
               p(strong("👥 About the Membership Analysis")),
               p("This section helps identify key trends in member engagement:"),
               tags$ul(
                 tags$li(strong("Top Members Plot:"), 
                         " Highlights the 10 most active members based on class attendance during the selected time frame."),
                 tags$li(strong("Declining Attendance Plot:"), 
                         " Uses linear regression models to detect members whose class attendance is consistently decreasing over time. Members with the steepest negative trends are highlighted.")
               ),
               p("Understanding both highly active and declining members is critical for retention and engagement strategies."),
               p("Use this page to monitor attendance behavior and take proactive steps to support member loyalty."),
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


