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
  "Population, total",
  "Life expectancy at birth, male (years)",
  "Life expectancy at birth, female (years)",
  "Mortality rate, infant (per 1,000 live births)",
  "Mortality rate, under-5 (per 1,000 live births)", 
  "Mortality rate, neonatal (per 1,000 live births)",
  "Fertility rate, total (births per woman)",
  "Urban population (% of total population)",   
  "Rural population (% of total population)",
  "Incidence of tuberculosis (per 100,000 people)",
  "Prevalence of overweight, weight for height (modeled estimate, % of children under 5)", 
  "Hospital beds (per 1,000 people)",
  "Current health expenditure (% of GDP)",
  "Immunization, DPT (% of children ages 12-23 months)",
  "Prevalence of anemia among women of reproductive age (% of women ages 15-49)",
  "Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%)",
  "Age dependency ratio, old (% of working-age population)",
  "Sex ratio at birth (male births per female births)"
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
# 2. USER INTERFACE (UI) - FIXED
# ==========================================
ui <- page_navbar(
  title = "United Kingdom Health Trends (1960-2024)",
  id = "main_tabs", 
  theme = bs_theme(bootswatch = "flatly", primary = "#2C3E50"),
  fillable = FALSE,  
  
  footer = p("Data Source: World Bank / UK National Statistics", class = "text-end text-muted mt-3 me-3"),
  
  tags$head(
    tags$style(HTML("
      body, .bslib-page-navbar { background-color: #F0F4F8 !important; }
      .card { 
        border: 1px solid #D1D5DB !important; 
        box-shadow: 0 4px 6px rgba(0,0,0,0.05) !important; 
      }
      .card-header {
        font-weight: bold !important;
        background-color: #FFFFFF !important;
        border-bottom: 1px solid #D1D5DB !important;
        color: #2C3E50 !important; 
      }
      
      .bslib-value-box .value-box-grid {
  padding: 8px 15px !important;
  gap: 0 !important;
}
.bslib-value-box .value-box-title {
  margin-bottom: 0px !important;
  font-size: 0.9rem !important;
}
    "))
  ),
  
  # TAB 1: Demographics (Unchanged)
  nav_panel("Demographics", value = "Demographics",
            layout_columns(
              col_widths = c(4, 8), 
              layout_columns(
                col_widths = 12, 
                value_box(
                  title = "Total Population",
                  value = textOutput("kpi_population"),
                  theme = "primary"
                ),
                card(
                  card_header("Gender Distribution"),
                  plotlyOutput("gender_pie", height = "180px")
                ),
                card(
                  card_header("Pyramid Controls"),
                  sliderInput("pyramid_year", "Select Year:",
                              min = min_year, max = max_year, value = max_year,
                              step = 1, sep = "", animate = animationOptions(interval = 800))
                )
              ),
              card(
                card_header("Population Pyramid"),
                plotlyOutput("pyramid_plot", height = "450px") 
              )
            ),
            layout_columns(
              col_widths = c(6, 6), 
              card(
                card_header("Total Fertility Rate"), 
                plotlyOutput("fertility_plot", height = "250px")
              ),
              card(
                card_header("Urban vs. Rural Population"), 
                plotlyOutput("urban_rural_plot", height = "250px") # <-- NEW PLOT ID
              )
            ),
            
            layout_columns(
              col_widths = c(6, 6), 
              card(
                card_header("Age Dependency Ratio (Old)"), 
                plotlyOutput("dependency_plot", height = "250px"),
                card_footer("% of working-age population aged 65+")
              ),
              card(
                card_header("Sex Ratio at Birth"), 
                plotlyOutput("sex_ratio_plot", height = "250px"),
                card_footer("Male births per female birth (Baseline ~1.05)")
              )
            )
  ),
  
  # TAB 2: Mortality & Life Expectancy - FIXED
  nav_panel("Mortality & Life Expectancy", 
            # --- Inside Tab 2: Mortality & Life Expectancy ---
            layout_column_wrap(
              width = 1/5,
              fill = FALSE, 
              
              value_box(
                title = "Life Expectancy (M) (2023)", 
                value = textOutput("kpi_life_male"), 
                theme = value_box_theme(bg = "#0047AB", fg = "#FFFFFF"),
                height = "120px", # <--- ADD THIS
                fill = FALSE     # <--- ADD THIS
              ),
              value_box(
                title = "Life Expectancy (F) (2023)", 
                value = textOutput("kpi_life_female"), 
                theme = value_box_theme(bg = "#C71585", fg = "#FFFFFF"),
                height = "120px",
                fill = FALSE
              ),
              value_box(
                title = "Neonatal Mortality (2023)",
                value = textOutput("kpi_neonatal"),
                theme = value_box_theme(bg = "#27AE60", fg = "#FFFFFF"),
                height = "120px",
                fill = FALSE
              ),
              value_box(
                title = "Infant Mortality (2023)",
                value = textOutput("kpi_infant"),
                theme = "primary",
                height = "120px",
                fill = FALSE
              ),
              value_box(
                title = "Under-5 Mortality (2023)",
                value = textOutput("kpi_under5"),
                theme = "danger",
                height = "120px",
                fill = FALSE
              )
            ),
            
            layout_columns(
              col_widths = c(6, 6),
              card(
                card_header("Life Expectancy at Birth (By Gender)"),
                plotlyOutput("life_expectancy_plot", height = "400px")
              ),
              card(
                card_header("Child Mortality Trends (Under-5, Infant, Neonatal)"),
                plotlyOutput("infant_mortality_plot", height = "400px")
              )
            ),
            
            layout_columns(
              col_widths = 12,
              card(
                card_header("Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70"),
                plotlyOutput("ncd_mortality_plot", height = "350px")
              )
            )
  ),
  
  # TAB 3 & 4 (Keep as you had them, they were structurally sound)
  nav_panel("Epidemiology (Disease)",
            layout_columns(
              col_widths = c(6, 6, 12), 
              card(card_header("Incidence of Tuberculosis"), plotlyOutput("tb_plot")),
              card(card_header("Overweight Children"), plotlyOutput("overweight_plot")),
              card(card_header("Anemia Prevalence among Women of (15-49)"), plotlyOutput("anemia_plot"))
            )
  ),
  nav_panel("Healthcare System",
            layout_columns(
              col_widths = c(6, 6, 12), 
              card(card_header("Hospital Capacity"), plotlyOutput("beds_plot")),
              card(card_header("Health Expenditure"), plotlyOutput("expenditure_plot")),
              card(card_header("Immunization Rate (DPT)"), plotlyOutput("dpt_plot"))
            )
  )
)


# ==========================================
# 3. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  # --- KPI: Neonatal Mortality ---
  output$kpi_neonatal <- renderText({
    val <- clean_data %>% 
      filter(grepl("neonatal", `Indicator Name`, ignore.case = TRUE)) %>%
      filter(Year == max(Year)) %>% 
      pull(Value)
    if(length(val) > 0) paste0(round(val, 2), " per 1000") else "N/A"
  })
  
  # --- KPI: Infant Mortality (Existing, but unique) ---
  output$kpi_infant <- renderText({
    val <- clean_data %>% 
      filter(grepl("infant", `Indicator Name`, ignore.case = TRUE)) %>%
      filter(Year == max(Year)) %>% 
      pull(Value)
    if(length(val) > 0) paste0(round(val, 2), " per 1000") else "N/A"
  })
  
  # --- KPI: Under-5 Mortality ---
  output$kpi_under5 <- renderText({
    val <- clean_data %>% 
      filter(grepl("under-5", `Indicator Name`, ignore.case = TRUE)) %>%
      filter(Year == max(Year)) %>% 
      pull(Value)
    if(length(val) > 0) paste0(round(val, 2), " per 1000") else "N/A"
  })
  
  # --- KPI: Life Expectancy (Male) ---
  output$kpi_life_male <- renderText({
    val <- clean_data %>% 
      filter(`Indicator Name` == "Life expectancy at birth, male (years)") %>%
      filter(Year == max(Year)) %>% pull(Value)
    if(length(val) > 0) paste0(round(val, 1), " Years") else "N/A"
  })
  
  # --- KPI: Life Expectancy (Female) ---
  output$kpi_life_female <- renderText({
    val <- clean_data %>% 
      filter(`Indicator Name` == "Life expectancy at birth, female (years)") %>%
      filter(Year == max(Year)) %>% pull(Value)
    if(length(val) > 0) paste0(round(val, 1), " Years") else "N/A"
  })
  
  # --- KPI: Total Population (Interactive with Pyramid Slider) ---
  output$kpi_population <- renderText({
    val <- clean_data %>% 
      filter(`Indicator Name` == "Population, total") %>%
      filter(Year == input$pyramid_year) %>% 
      pull(Value)
    
    if(length(val) > 0) {
      paste0(round(val / 1000000, 2), " Million")
    } else {
      "N/A"
    }
  })
  
  # --- Plot: Life Expectancy by Gender ---
  output$life_expectancy_plot <- renderPlotly({
    df <- clean_data %>%
      filter(grepl("Life expectancy at birth", `Indicator Name`)) %>%
      mutate(Gender = case_when(
        grepl("female", `Indicator Name`) ~ "Female",
        grepl("male", `Indicator Name`) ~ "Male"
      ))
    
    p <- ggplot(df, aes(x = Year, y = Value, color = Gender)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = c(
        "Female" = "#C71585", 
        "Male" = "#0056B3"    
      )) +
      theme_minimal() +
      labs(x = "Year", y = "Age (Years)") +
      theme(legend.position = "bottom", legend.title = element_blank())
    
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # --- Plot: Combined Mortality Trends ---
  output$infant_mortality_plot <- renderPlotly({
    # Define the group of indicators we want to plot together
    mortality_types <- c(
      "Mortality rate, under-5 (per 1,000 live births)",
      "Mortality rate, infant (per 1,000 live births)",
      "Mortality rate, neonatal (per 1,000 live births)"
    )
    
    df <- clean_data %>%
      filter(`Indicator Name` %in% mortality_types) %>%
      mutate(Type = case_when(
        grepl("under-5", `Indicator Name`, ignore.case = TRUE) ~ "Under-5",
        grepl("infant", `Indicator Name`, ignore.case = TRUE) ~ "Infant",
        grepl("neonatal", `Indicator Name`, ignore.case = TRUE) ~ "Neonatal"
      ))
    
    p <- ggplot(df, aes(x = Year, y = Value, color = Type)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1) +
      scale_color_manual(values = c(
        "Under-5" = "#E74C3C", # Bright Red
        "Infant" = "#F39C12",  # Vivid Orange
        "Neonatal" = "#27AE60" 
      )) +
      theme_minimal() +
      labs(x = "Year", y = "Deaths per 1,000 live births") +
      theme(legend.title = element_blank())
    
    # Place the legend at the bottom so the chart has more room to breathe
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
  })
  
  
  output$ncd_mortality_plot <- renderPlotly({
    df <- clean_data %>% 
      filter(grepl("CVD, cancer, diabetes", `Indicator Name`, ignore.case = TRUE))
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#6C3483", linewidth = 1.3) + # Rich Purple
      geom_point(color = "#6C3483", size = 2) +
      theme_minimal() +
      labs(x = "Year", y = "Percentage (%)") +
      scale_y_continuous(labels = function(x) paste0(x, "%"))
    
    ggplotly(p) %>%
      layout(hovermode = "x unified")
  })
  
  # --- Plot: Gender Distribution Pie Chart ---
  output$gender_pie <- renderPlotly({
    pie_data <- raw_data %>%
      slice(254:297) %>%
      pivot_longer(cols = `1960`:`2024`, names_to = "Year", values_to = "Percentage") %>%
      mutate(Year = as.numeric(Year)) %>%
      filter(Year == input$pyramid_year) %>% 
      filter(!is.na(Percentage)) %>%
      mutate(Gender = ifelse(grepl("female", `Indicator Name`, ignore.case = TRUE), "Female", "Male")) %>%
      group_by(Gender) %>%
      summarise(Total_Percentage = sum(Percentage, na.rm = TRUE))
    
    plot_ly(pie_data, labels = ~Gender, values = ~Total_Percentage, type = 'pie',
            textinfo = 'label+percent',
            hoverinfo = 'text',
            text = ~paste(Gender, ": ", round(Total_Percentage, 1), "%", sep=""),
            marker = list(colors = c("#C71585", "#0056B3"))) %>% 
      layout(showlegend = FALSE, 
             margin = list(t = 10, b = 10, l = 10, r = 10),
             paper_bgcolor = "transparent",
             plot_bgcolor = "transparent")
  })
  
  # --- Plot: Population Pyramid (Single Year) ---
  output$pyramid_plot <- renderPlotly({
    pyramid_data <- raw_data %>%
      slice(254:297) %>%
      pivot_longer(cols = `1960`:`2024`, names_to = "Year", values_to = "Percentage") %>%
      mutate(Year = as.numeric(Year)) %>%
      filter(Year == input$pyramid_year) %>% 
      filter(!is.na(Percentage))
    
    pyramid_data <- pyramid_data %>%
      mutate(
        Gender = ifelse(grepl("female", `Indicator Name`, ignore.case = TRUE), "Female", "Male"),
        Age_Group = str_extract(`Indicator Name`, "^[0-9]+(-[0-9]+|\\+)")
      ) %>%
      filter(!is.na(Age_Group))
    
    age_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                    "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", 
                    "95-99", "100+")
    
    pyramid_data <- pyramid_data %>%
      mutate(Age_Group = factor(Age_Group, levels = age_levels)) %>%
      mutate(Percentage = ifelse(Gender == "Male", -Percentage, Percentage))
    
    p <- ggplot(pyramid_data, aes(x = Age_Group, y = Percentage, fill = Gender)) +
      geom_col() +
      coord_flip() + 
      scale_fill_manual(values = c("Female" = "#C71585", "Male" = "#0056B3")) +
      scale_y_continuous(labels = abs) + 
      theme_minimal() +
      labs(x = "Age Group", y = "Percentage of Population (%)") +
      theme(
        legend.position = "bottom",
        axis.text.y = element_text(size = 9)
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>% 
      layout(hovermode = "y unified")
  })
  
  # --- Plot: Fertility ---
  output$fertility_plot <- renderPlotly({
    df <- clean_data %>% 
      filter(`Indicator Name` == "Fertility rate, total (births per woman)")
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#9B59B6", linewidth = 1) +
      geom_point(color = "#9B59B6", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "Births per woman") 
    
    ggplotly(p)
  })
  
  # --- Plot: Urban vs Rural Population (FIXED) ---
  output$urban_rural_plot <- renderPlotly({
    df <- clean_data %>% 
      # Fixed: Use grepl to catch the strings even with the "(% of total population)" part
      filter(grepl("Urban population|Rural population", `Indicator Name`, ignore.case = TRUE)) %>%
      mutate(
        # Create a clean label for the legend
        Type = ifelse(grepl("Urban", `Indicator Name`, ignore.case = TRUE), "Urban", "Rural"),
        # Fixed: We don't need to divide by 1 million anymore since these are percentages!
        Value_Percentage = Value 
      )
    
    p <- ggplot(df, aes(x = Year, y = Value_Percentage, color = Type)) +
      geom_line(linewidth = 1.3) +
      scale_color_manual(values = c(
        "Urban" = "#8E44AD", # Vibrant Purple
        "Rural" = "#27AE60"  # Vibrant Green
      )) +
      theme_minimal() +
      labs(x = "Year", y = "Population (%)") + # Updated axis label
      theme(legend.title = element_blank())
    
    # Place the legend at the bottom to save chart space
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
  })
  
  # --- Plot: Tuberculosis Incidence ---
  output$tb_plot <- renderPlotly({
    df <- clean_data %>% 
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
    df <- clean_data %>% 
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
    df <- clean_data %>% 
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
    df <- clean_data %>% 
      filter(`Indicator Name` == "Current health expenditure (% of GDP)")
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#8E44AD", linewidth = 1) + 
      geom_point(color = "#8E44AD", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "% of GDP")
    
    ggplotly(p)
  })
  
  # --- Plot: DPT Immunization ---
  output$dpt_plot <- renderPlotly({
    df <- clean_data %>% 
      filter(`Indicator Name` == "Immunization, DPT (% of children ages 12-23 months)")
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#27AE60", linewidth = 1) + 
      geom_point(color = "#27AE60", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "Immunization Rate (%)")
    
    ggplotly(p)
  })
  
  # --- Plot: Anemia Prevalence ---
  output$anemia_plot <- renderPlotly({
    df <- clean_data %>% 
      filter(grepl("anemia among women", `Indicator Name`, ignore.case = TRUE))
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#8E44AD", linewidth = 1) + 
      geom_point(color = "#8E44AD", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "Prevalence (%)")
    
    ggplotly(p)
  })
  
  # --- Plot: Age Dependency Ratio (Old) ---
  output$dependency_plot <- renderPlotly({
    df <- clean_data %>% 
      filter(grepl("Age dependency ratio, old", `Indicator Name`, ignore.case = TRUE))
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      geom_line(color = "#D35400", linewidth = 1.3) + # Deep Amber
      geom_point(color = "#D35400", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "% of Working Age")
    
    ggplotly(p)
  })
  
  # --- Plot: Sex Ratio at Birth ---
  output$sex_ratio_plot <- renderPlotly({
    df <- clean_data %>% 
      filter(grepl("Sex ratio at birth", `Indicator Name`, ignore.case = TRUE))
    
    p <- ggplot(df, aes(x = Year, y = Value)) +
      # Reference line for biological norm
      geom_hline(yintercept = 1.05, linetype = "dashed", color = "gray60") +
      geom_line(color = "#008080", linewidth = 1.3) + # Teal
      geom_point(color = "#008080", size = 1.5) +
      theme_minimal() +
      labs(x = "Year", y = "Male per Female Births")
    
    ggplotly(p)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
