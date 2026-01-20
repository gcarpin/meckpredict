
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(randomForest)
library(DT)
library(plotly)
library(scales)
library(caret)  


data_folder <- "data" 

load_data <- function() {
  # Robust check for directory existence
  if (!dir.exists(data_folder)) {
    # Fallback: If "data" folder is missing, check current directory
    if(length(list.files(pattern = "\\.xlsx$")) > 0) {
      data_folder <<- "." 
    } else {
      stop("ERROR: Could not find 'data' folder or .xlsx files in the project root.")
    }
  }
  
  files <- list.files(path = data_folder, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
  
  if(length(files) == 0) {
    stop("ERROR: No .xlsx files found in the directory.")
  }
  
  print(paste("Loading", length(files), "files from server..."))
  
  df <- files %>% 
    map_dfr(~read_excel(.x) %>% 
              mutate(across(everything(), as.character))) %>% 
    type_convert() %>% 
    rename_with(~ gsub("[\\. ]", "_", .x)) %>% 
    rename(
      YearBuilt = Year_Built,
      FullBath = Full_Bath,
      HalfBath = Half_Bath,
      Price = any_of(c("Price", "Sale_Price_Scraped")) 
    ) %>% 
    mutate(
      Price = as.numeric(Price),
      Sqft = as.numeric(Sqft),
      Year = as.numeric(Year),
      FullBath = as.numeric(FullBath),
      HalfBath = as.numeric(HalfBath),
      Bdrms = as.numeric(Bdrms),
      YearBuilt = as.numeric(YearBuilt)
    ) %>% 
    mutate(TotalBaths = FullBath + (0.5 * HalfBath)) %>% 
    filter(!is.na(Price), Price > 10000, Sqft > 500)
  
  return(df)
}

# Load data immediately
housing_data <- load_data()

# Train Model (Note: In production, it is better to save/load an .rds file, 
# but this works if the dataset is small enough to train on startup)
train_data <- housing_data %>% 
  select(Price, Sqft, Bdrms, TotalBaths, YearBuilt) %>% 
  na.omit()

rf_model <- randomForest(Price ~ Sqft + Bdrms + TotalBaths + YearBuilt, 
                         data = train_data, ntree = 50)
print("Model Trained Successfully.")


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Mecklenburg Market"),
  
  dashboardSidebar(
    sidebarMenu(
   
      menuItem("Market Trends", tabName = "dashboard", icon = icon("chart-line")),
      

      menuItem("Consumer Behavior", tabName = "behavior", icon = icon("users")),
      

      menuItem("Valuation Tool", tabName = "predictor", icon = icon("calculator")),
      

      menuItem("Model Insights", tabName = "insights", icon = icon("microscope")),
      
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    ),
    hr(),
    sliderInput("year_filter", "Filter Years:",
                min = 2018, max = 2025, value = c(2018, 2025), sep = "")
  ),
  
  dashboardBody(
    tabItems(
      # --- TAB 1: MARKET TRENDS (RQ1 & RQ2) ---
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("avg_price_box"),
                valueBoxOutput("avg_ppsf_box"),
                valueBoxOutput("total_sales_box")
              ),
              fluidRow(
                # Macro Plot: Price vs Rates
                box(title = "Economic Impact", status = "danger", solidHeader = TRUE,
                    plotlyOutput("macro_plot"), 
                    width = 12,
                    
                    # Summary Explanation
                    br(),
                    div(style = "padding: 10px; background-color: #f9f9f9; border-left: 5px solid #dd4b39;",
                        h4("COVID Effect"),
                        p("In 2020, a distinct economic anomaly occurred where housing prices appreciated despite the global recession caused by the COVID-19 pandemic. 
                          To mitigate economic shock, the Federal Reserve aggressively lowered the Federal Funds Rate to near-zero levels. 
                          This monetary intervention directly suppressed borrowing costs, driving 30-year mortgage rates to historic lows.")
                    )
                )
              ),
              fluidRow(
                # PPSF Plot: Answers "How have similar attributes changed?"
                box(title = "Price per Square Foot", status = "primary", solidHeader = TRUE,
                    plotlyOutput("ppsf_plot"), width = 12
                    )
              )
      ),
      
      # --- TAB 2: CONSUMER BEHAVIOR (RQ3) ---
      tabItem(tabName = "behavior",
              fluidRow(
                box(title = "Consumer Preferences on House Size", status = "warning", solidHeader = TRUE, width = 12,
                    plotlyOutput("size_boxplot")
                  
                )
              ),
              fluidRow(
                box(title = "Transaction Volume by Year", status = "info",
                    plotlyOutput("volume_plot"), width = 12)
              )
      ),
      
      # --- TAB 3: VALUATION TOOL (RQ4) ---
      tabItem(tabName = "predictor",
              fluidRow(
                box(title = "Random Forest Price Predictor", status = "success", solidHeader = TRUE, width = 4,
                    
                    numericInput("input_sqft", "Square Footage:", 2000, min=500, max=10000),
                    numericInput("input_bed", "Bedrooms:", 3, min=1, max=10),
                    numericInput("input_bath", "Total Baths:", 2.5, min=1, max=10),
                    numericInput("input_year", "Year Built:", 2010, min=1900, max=2025),
                    actionButton("predict_btn", "Calculate Estimate", class = "btn-lg btn-success")
                ),
                box(title = "Estimated Market Value", status = "success", solidHeader = TRUE, width = 8,
                    h1(textOutput("prediction_result"), style = "color: green; font-weight: bold;"),
                    hr(),
                    h4("Consumer Application"),
                    p("Consumers can use this estimate as a baseline when negotiating offers or listing their home. 
                ")
                )
              )
      ),
      
      # --- TAB 4: MODEL INSIGHTS ---
      tabItem(tabName = "insights",
              fluidRow(
                # Partial Dependence Plot
                box(title = "Partial Dependendence Plots on Independent Variables", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4, selectInput("pdp_var", "Select Attribute:", 
                                            choices = c("Square Footage" = "Sqft", 
                                                        "Year Built" = "YearBuilt", 
                                                        "Total Baths" = "TotalBaths",
                                                        "Bedrooms" = "Bdrms"))),
                      column(8, p("Marginal Effect of the selected variable. 
                                  "))
                    ),
                    plotlyOutput("pdp_plot")
                )
              ),
              fluidRow(
                # Confusion Matrix
                box(title = "Confusion Matrix", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("confusion_matrix_plot"),
                    p("Darker blue squares represent correct predictions. 
                      Values off the diagonal indicate classification errors.")
                ),
                box(title = "Model Accuracy", status = "info", width = 6,
                    uiOutput("confusion_summary")
                )
              )
      ),
      
      # --- TAB 5: DATA ---
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, dataTableOutput("raw_table"))
              )
      )
    )
  )
)

# --- 3. SERVER LOGIC ---
server <- function(input, output) {
  
  filtered_data <- reactive({
    housing_data %>%
      filter(Year >= input$year_filter[1], Year <= input$year_filter[2])
  })
  
  output$avg_price_box <- renderValueBox({
    avg_val <- mean(filtered_data()$Price, na.rm = TRUE)
    valueBox(paste0("$", format(round(avg_val), big.mark = ",")), "Avg Sale Price", icon = icon("dollar-sign"), color = "green")
  })
  
  output$avg_ppsf_box <- renderValueBox({
    ppsf <- mean(filtered_data()$Price / filtered_data()$Sqft, na.rm = TRUE)
    valueBox(paste0("$", round(ppsf, 2)), "Avg Price/SqFt", icon = icon("ruler"), color = "orange")
  })
  
  output$total_sales_box <- renderValueBox({
    valueBox(nrow(filtered_data()), "Total Transactions", icon = icon("home"), color = "blue")
  })
  
  # --- RQ1: Macro Plot (Price vs Rates) ---
  output$macro_plot <- renderPlotly({
    # Aggregate Price
    price_data <- filtered_data() %>%
      group_by(Year) %>%
      summarise(Avg_Price = mean(Price, na.rm = TRUE))
    
    # Macro Data (Hardcoded for 2018-2025)
    macro_data <- data.frame(
      Year = 2018:2025,
      Mortgage_Rate = c(4.54, 3.94, 3.10, 2.96, 5.34, 6.81, 6.90, 6.50),
      Fed_Funds_Rate = c(1.83, 2.16, 0.36, 0.08, 1.68, 5.02, 5.33, 5.30)
    )
    
    plot_df <- left_join(price_data, macro_data, by = "Year")
    
    plot_ly(plot_df) %>%
      add_bars(x = ~Year, y = ~Avg_Price, name = "Avg Home Price",
               marker = list(color = '#b0bec5'), opacity = 0.6) %>%
      add_lines(x = ~Year, y = ~Mortgage_Rate, name = "30Y Mortgage Rate (%)",
                yaxis = "y2", line = list(color = '#d32f2f', width = 3)) %>%
      add_lines(x = ~Year, y = ~Fed_Funds_Rate, name = "Fed Funds Rate (%)",
                yaxis = "y2", line = list(color = '#1976d2', width = 3, dash = 'dot')) %>%
      layout(
        yaxis = list(title = "Average Price ($)"),
        yaxis2 = list(overlaying = "y", side = "right", title = "Interest Rates (%)", range = c(0, 8)),
        xaxis = list(title = "Year"),
        legend = list(orientation = "h", x = 0.1, y = 1.1),
        margin = list(r = 50)
      )
  })
  
  # --- RQ2: Price Per SqFt Trend ---
  output$ppsf_plot <- renderPlotly({
    trend <- filtered_data() %>%
      mutate(PPSF = Price / Sqft) %>%
      group_by(Year) %>%
      summarise(Median_PPSF = median(PPSF, na.rm=TRUE))
    
    p <- ggplot(trend, aes(x = Year, y = Median_PPSF)) +
      geom_line(color = "#e67e22", size = 1.2) +
      geom_point(color = "#d35400", size = 3) +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal() +
      labs(y = "Median Price Per Sq. Ft.", x = "Year")
    
    ggplotly(p)
  })
  
  # --- RQ3: Size Distribution (Boxplot) ---
  output$size_boxplot <- renderPlotly({
    # Limit outliers for cleaner visualization
    p <- ggplot(filtered_data(), aes(x = factor(Year), y = Sqft, fill = factor(Year))) +
      geom_boxplot(alpha = 0.6, outlier.shape = NA) +
      coord_cartesian(ylim = c(1000, 4500)) + # Zoom in on the "common" home sizes
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "Year Sold", y = "Square Footage")
    
    ggplotly(p)
  })
  
  output$volume_plot <- renderPlotly({
    vol <- filtered_data() %>% count(Year)
    p <- ggplot(vol, aes(x = Year, y = n)) +
      geom_bar(stat="identity", fill="#3498db") +
      theme_minimal() +
      labs(y = "Number of Sales")
    ggplotly(p)
  })
  
  # --- RQ4: Prediction Logic ---
  observeEvent(input$predict_btn, {
    new_home <- data.frame(
      Sqft = input$input_sqft,
      Bdrms = input$input_bed,
      TotalBaths = input$input_bath,
      YearBuilt = input$input_year
    )
    
    pred_price <- predict(rf_model, new_home)
    
    output$prediction_result <- renderText({
      paste0("$", format(round(pred_price, -2), big.mark = ","))
    })
  })
  
  # --- MODEL INSIGHTS LOGIC ---
  
  output$pdp_plot <- renderPlotly({
    req(input$pdp_var)
    
    sample_data <- train_data %>% 
      sample_n(min(nrow(train_data), 500)) %>%
      na.omit()
    
    var_name <- input$pdp_var
    is_discrete <- var_name %in% c("Bdrms", "TotalBaths")
    
    if(is_discrete) {
      var_vals <- sort(unique(sample_data[[var_name]]))
      var_range <- var_vals[var_vals < 10 & var_vals > 0] 
    } else {
      q <- quantile(sample_data[[var_name]], probs = c(0.05, 0.95))
      var_range <- seq(q[1], q[2], length.out = 20)
    }
    
    # Calculate PDP
    pdp_results <- numeric(length(var_range))
    
    for(i in seq_along(var_range)) {
      temp_df <- sample_data
      temp_df[[var_name]] <- var_range[i]
      
      preds <- predict(rf_model, temp_df)
      pdp_results[i] <- mean(preds, na.rm = TRUE)
    }
    
    plot_data <- data.frame(Value = var_range, Avg_Price = pdp_results)
    

    if(is_discrete) {
      # Lollipop / Scatter for Discrete
      p <- ggplot(plot_data, aes(x = Value, y = Avg_Price)) +
        # The stick
        geom_segment(aes(x = Value, xend = Value, y = min(Avg_Price)*0.95, yend = Avg_Price), 
                     color = "#7f8c8d", size = 0.5) +
        # The pop
        geom_point(color = "#e74c3c", size = 4) +
        scale_y_continuous(labels = scales::dollar) +
        scale_x_continuous(breaks = var_range) + # Force integer ticks
        theme_minimal() +
        labs(x = var_name, y = "Avg Predicted Price", 
             title = paste("Effect of", var_name, "on Value"))
    } else {
      # Line for Continuous
      p <- ggplot(plot_data, aes(x = Value, y = Avg_Price)) +
        geom_line(color = "#2980b9", size = 1.2) +
        scale_y_continuous(labels = scales::dollar) +
        theme_minimal() +
        labs(x = var_name, y = "Avg Predicted Price", 
             title = paste("Trend: Higher", var_name, "vs Value"))
    }
    
    ggplotly(p)
  })
  

  confusion_results <- reactive({

    preds <- predict(rf_model, train_data)
    actuals <- train_data$Price
    
    # Define Tiers
    breaks <- c(0, 350000, 600000, 900000, Inf)
    labels <- c("Economy", "Mid-Range", "High-End", "Luxury")
    
    pred_bins <- cut(preds, breaks = breaks, labels = labels)
    actual_bins <- cut(actuals, breaks = breaks, labels = labels)
    
    # Handle NAs if cut() produces them
    valid_indices <- !is.na(pred_bins) & !is.na(actual_bins)
    
    cm <- confusionMatrix(pred_bins[valid_indices], actual_bins[valid_indices])
    return(cm)
  })
  
  output$confusion_matrix_plot <- renderPlot({
    cm <- confusion_results()
    cm_table <- as.data.frame(cm$table)
    
    ggplot(cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white", size = 5) +
      scale_fill_gradient(low = "#d6eaf8", high = "#2874a6") +
      theme_minimal() +
      labs(x = "Actual Market Tier", y = "Predicted Market Tier") +
      theme(axis.text = element_text(size = 12))
  })
  
  output$confusion_summary <- renderUI({
    cm <- confusion_results()
    acc <- percent(cm$overall['Accuracy'], accuracy = 0.1)
    kappa <- round(cm$overall['Kappa'], 2)
    
    HTML(paste0(
      "<div style='background-color: #f4f6f7; padding: 15px; border-radius: 5px; border-left: 5px solid #2874a6;'>",
      "<h4 style='margin-top:0;'>Accuracy: ", acc, "</h4>",
      "<p>This model correctly identifies the price tier for <strong>", acc, "</strong> of homes.</p>",
     
      "</div>"
    ))
  })
  
  # --- Data Table ---
  output$raw_table <- renderDataTable({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)