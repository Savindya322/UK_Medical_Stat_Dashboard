# Load required libraries
library(shiny)
library(tidyverse)
library(plotly)
library(bslib)

# ==========================================
# 1. GLOBAL DATA PROCESSING
# ==========================================
# Read the CSV file
raw_data <- read_csv("England2.csv", show_col_types = FALSE)

# The vital indicators we want to focus on for the line charts
vital_indicators <- c(
  "Life expectancy at birth, male (years)",
  "Life expectancy at birth, female (years)",
  "Mortality rate, infant (per 1,000 live births)",
  "Fertility rate, total (births per woman)",
  "Birth rate, crude (per 1,000 people)",
  "Incidence of tuberculosis (per 100,000 people)",
  "Prevalence of overweight, weight for height (modeled estimate, % of children under 5)", 
  "Hospital beds (per 1,000 people)",
  "Current health expenditure (% of GDP)"
)

# Clean and reshape the line chart data from wide to long format
clean_data <- raw_data %>%
  filter(`Indicator Name` %in% vital_indicators) %>%
  pivot_longer(
    cols = `1960`:`2024`, 
    names_to = "Year", 
    values_to = "Value"
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Value))

# Get min and max years for the UI sliders
min_year <- min(clean_data$Year)
max_year <- max(clean_data$Year)

# ==========================================
# 2. USER INTERFACE (UI)
# ==========================================
ui <- page_navbar(
  title = "United Kingdom Health Trends (1960-2024)",
  theme = bs_theme(bootswatch = "flatly", primary = "#2C3E50"),
  
  # Global Sidebar for filters
  sidebar = sidebar(
    title = "Controls",
    
    # 1. Slider for the Line Charts (Range)
    sliderInput("year_range", 
                "Time Period :",
                min = min_year, 
                max = max_year, 
                value = c(min_year, max_year),
                step = 1,
                sep = ""),
    hr(),
    
    # 2. Slider specifically for the Population Pyramid (Single Year)
    sliderInput("pyramid_year", 
                "Select Year for Population Pyramid:",
                min = min_year, 
                max = max_year, 
                value = max_year,
                step = 1,
                sep = "",
                animate = animationOptions(interval = 800, loop = FALSE)),
    hr(),
    p("Data Source: World Bank / UK National Statistics")
  ),
  
  # TAB 1: Overview (Mortality & Life Expectancy)
  nav_panel("Mortality & Life Expectancy", 
            # UPDATED: Added Male and Female Life Expectancy KPIs here
            layout_columns(
              value_box(title = "Latest Life Expectancy (Male)", value = textOutput("kpi_life_male"), theme_color = "primary"),
              value_box(title = "Latest Life Expectancy (Female)", value = textOutput("kpi_life_female"), theme_color = "info"),
              value_box(title = "Latest Infant Mortality", value = textOutput("kpi_infant"), theme_color = "danger")
            ),
            layout_columns(
              card(
                card_header("Life Expectancy at Birth (By Gender)"),
                plotlyOutput("life_expectancy_plot", height = "350px")
              ),
              card(
                card_header("Infant Mortality Trend"),
                plotlyOutput("infant_mortality_plot", height = "350px")
              )
            )
  ),
  
  # TAB 2: Demographics & Fertility (With Pyramid)
  nav_panel("Demographics",
            layout_columns(
              col_widths = c(5, 7), # Split the screen: 5 columns left, 7 columns right
              
              # Left Side: Stacked Natality Charts
              layout_columns(
                col_widths = 12, # Force vertical stacking inside the left column
                card(
                  card_header("Total Fertility Rate"),
                  plotlyOutput("fertility_plot", height = "250px")
                ),
                card(
                  card_header("Crude Birth Rate"),
                  plotlyOutput("cbr_plot", height = "250px")
                )
              ),
              
              # Right Side: The Population Pyramid
              card(
                card_header("Population Pyramid"),
                plotlyOutput("pyramid_plot", height = "560px")
              )
            )
  ),
  
  # TAB 3: Epidemiology & Disease
  nav_panel("Epidemiology (Disease)",
            layout_columns(
              card(
                card_header("Infectious Disease Incidence"),
                p("Incidence of Tuberculosis per 100,000 people (Communicable)."),
                plotlyOutput("tb_plot", height = "350px")
              ),
              card(
                card_header("Overweight Prevalence (Risk Factor)"),
                p("Prevalence of overweight children under 5."),
                plotlyOutput("overweight_plot", height = "350px")
              )
            )
  ),
  
  # TAB 4: Healthcare System
  nav_panel("Healthcare System",
            layout_columns(
              card(
                card_header("Hospital Capacity"),
                p("Beds available per 1,000 people."),
                plotlyOutput("beds_plot", height = "350px")
              ),
              card(
                card_header("Health Expenditure"),
                p("Total health expenditure as a percentage of the national GDP."),
                plotlyOutput("expenditure_plot", height = "350px")
              )
            )
  )
)

# ==========================================
# 3. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  # Reactive subset for Line Charts (Range)
  filtered_data <- reactive({
    clean_data %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2])
  })
  
  # --- KPI: Infant Mortality ---
  output$kpi_infant <- renderText({
    val <- filtered_data() %>% 
      filter(`Indicator Name` == "Mortality rate, infant (per 1,000 live births)") %>%
      filter(Year == max(Year)) %>% pull(Value)
    if(length(val) > 0) paste0(round(val, 2), " per 1000") else "N/A"
  })
  
  # --- KPI: Life Expectancy (Male) [NEW] ---
  output$kpi_life_male <- renderText({
    val <- filtered_data() %>% 
      filter(`Indicator Name` == "Life expectancy at birth, male (years)") %>%
      filter(Year == max(Year)) %>% pull(Value)
    if(length(val) > 0) paste0(round(val, 1), " Years") else "N/A"
  })
  
  # --- KPI: Life Expectancy (Female) [NEW] ---
  output$kpi_life_female <- renderText({
    val <- filtered_data() %>% 
      filter(`Indicator Name` == "Life expectancy at birth, female (years)") %>%
      filter(Year == max(Year)) %>% pull(Value)
    if(length(val) > 0) paste0(round(val, 1), " Years") else "N/A"
  })
  
  # --- Plot: Infant Mortality ---
  output$infant_mortality_plot <- renderPlotly({
    df <- filtered_data() %>% 
      filter(`Indicator Name` == "Mortality rate, infant (per 1,000 live births)")
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#E74C3C", linewidth = 1) +
      geom_point(color = "#E74C3C", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "Deaths per 1,000 live births")
    
    ggplotly(p)
  })
  
  # --- Plot: Life Expectancy by Gender ---
  output$life_expectancy_plot <- renderPlotly({
    df <- filtered_data() %>%
      filter(grepl("Life expectancy at birth", `Indicator Name`)) %>%
      mutate(Gender = case_when(
        grepl("female", `Indicator Name`) ~ "Female",
        grepl("male", `Indicator Name`) ~ "Male"
      ))
    
    p <- ggplot(df, aes(x = Year, y = Value, color = Gender)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = c(
        "Female" = "#8E44AD", 
        "Male" = "#2980B9"    
      )) +
      theme_minimal() +
      labs(x = "Year", y = "Age (Years)") +
      theme(legend.position = "bottom", legend.title = element_blank())
    
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # --- Plot: Fertility ---
  output$fertility_plot <- renderPlotly({
    df <- filtered_data() %>% 
      filter(`Indicator Name` == "Fertility rate, total (births per woman)")
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#2C3E50", linewidth = 1) +
      geom_point(color = "#2C3E50", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "Births per woman") 
    
    ggplotly(p)
  })
  
  # --- Plot: Crude Birth Rate ---
  output$cbr_plot <- renderPlotly({
    df <- filtered_data() %>% 
      filter(`Indicator Name` == "Birth rate, crude (per 1,000 people)")
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#16A085", linewidth = 1) +
      geom_point(color = "#16A085", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "Births per 1,000 people")
    
    ggplotly(p)
  })
  
  # --- Plot: Population Pyramid (Single Year) ---
  output$pyramid_plot <- renderPlotly({
    # Select specifically rows 1482 to 1523 (which corresponds to Excel rows 1483 to 1524)
    pyramid_data <- raw_data %>%
      slice(254:297) %>%
      pivot_longer(cols = `1960`:`2024`, names_to = "Year", values_to = "Percentage") %>%
      mutate(Year = as.numeric(Year)) %>%
      filter(Year == input$pyramid_year) %>%  # <--- Uses the new single-year slider
      filter(!is.na(Percentage))
    
    # Process Age and Gender
    pyramid_data <- pyramid_data %>%
      mutate(
        Gender = ifelse(grepl("female", `Indicator Name`, ignore.case = TRUE), "Female", "Male"),
        # Extract the age numbers (e.g., "0-4" or "100+")
        Age_Group = str_extract(`Indicator Name`, "^[0-9]+(-[0-9]+|\\+)")
      ) %>%
      filter(!is.na(Age_Group))
    
    # Create an ordered factor so the pyramid sorts correctly (0-4 at the bottom, 100+ at the top)
    age_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                    "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", 
                    "95-99", "100+")
    
    pyramid_data <- pyramid_data %>%
      mutate(Age_Group = factor(Age_Group, levels = age_levels)) %>%
      # Make Male percentages negative so they draw on the left side of the axis
      mutate(Percentage = ifelse(Gender == "Male", -Percentage, Percentage))
    
    p <- ggplot(pyramid_data, aes(x = Age_Group, y = Percentage, fill = Gender)) +
      geom_col() +
      coord_flip() + 
      scale_fill_manual(values = c("Male" = "#2980B9", "Female" = "#C0392B")) +
      scale_y_continuous(labels = abs) + # Keep axis labels positive
      theme_minimal() +
      labs(x = "Age Group", y = "Percentage of Population (%)") +
      theme(
        legend.position = "bottom",
        axis.text.y = element_text(size = 9)
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>% 
      layout(hovermode = "y unified")
  })
  
  # --- Plot: Tuberculosis Incidence ---
  output$tb_plot <- renderPlotly({
    df <- filtered_data() %>% 
      filter(`Indicator Name` == "Incidence of tuberculosis (per 100,000 people)")
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#E74C3C", linewidth = 1) + 
      geom_point(color = "#E74C3C", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "Cases per 100,000")
    
    ggplotly(p)
  })
  
  # --- Plot: Overweight Prevalence ---
  output$overweight_plot <- renderPlotly({
    df <- filtered_data() %>% 
      filter(`Indicator Name` == "Prevalence of overweight, weight for height (modeled estimate, % of children under 5)")
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#F39C12", linewidth = 1) + 
      geom_point(color = "#F39C12", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "Prevalence (%)")
    
    ggplotly(p)
  })
  
  # --- Plot: Hospital Beds ---
  output$beds_plot <- renderPlotly({
    df <- filtered_data() %>% 
      filter(`Indicator Name` == "Hospital beds (per 1,000 people)")
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#16A085", linewidth = 1) +
      geom_point(color = "#16A085", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "Beds per 1,000")
    
    ggplotly(p)
  })
  
  # --- Plot: Health Expenditure ---
  output$expenditure_plot <- renderPlotly({
    df <- filtered_data() %>% 
      filter(`Indicator Name` == "Current health expenditure (% of GDP)")
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#8E44AD", linewidth = 1) + 
      geom_point(color = "#8E44AD", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "% of GDP")
    
    ggplotly(p)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
