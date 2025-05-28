# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)


data <- read.csv("Global_Space_Exploration_Dataset.csv")
data <- data %>% distinct()
data$Country <- str_trim(data$Country)
data$Year <- as.numeric(data$Year)


# Tab 1: Investment & Strategic Focus
budget_by_country_mission_type <- data %>%
  group_by(Country, Mission.Type) %>%
  summarise(Total_Budget = sum(Budget..in.Billion..., na.rm = TRUE), .groups = 'drop') %>%
  
  # Filter for top countries by overall budget
  group_by(Country) %>%
  mutate(Country_Overall_Budget = sum(Total_Budget)) %>% # Calculate overall budget per country
  ungroup() %>%
  arrange(desc(Country_Overall_Budget))

# Get the list of countries that are in the top overall budget
top_countries_for_select <- budget_by_country_mission_type %>%
  distinct(Country, Country_Overall_Budget) %>%
  top_n(10, Country_Overall_Budget) %>%
  arrange(desc(Country_Overall_Budget)) %>%
  pull(Country)


# Tab 2: Technology, Success & Sustainability

# Avg Success Rate by Technology
avg_success_rate_by_tech <- data %>%
  group_by(Technology.Used) %>%
  summarise(Average_Success_Rate = mean(Success.Rate...., na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Average_Success_Rate))

# Environmental Impact by Technology
env_impact_by_tech <- data %>%
  group_by(Technology.Used, Environmental.Impact) %>%
  summarise(Number_of_Missions = n(), .groups = 'drop') %>%
  arrange(Technology.Used, desc(Number_of_Missions))

# Get a list of all technologies for dropdown
technologies <- sort(unique(data$Technology.Used))


# Tab 3: Mission Profiles: Complexity, Duration & Footprint

# Average Duration by Mission Type
avg_duration_by_mission_type <- data %>%
  group_by(Mission.Type) %>%
  summarise(Average_Duration_Days = mean(Duration..in.Days., na.rm = TRUE), .groups = 'drop')

# Environmental Impact Distribution by Mission Type
env_impact_distribution_by_mission_type <- data %>%
  group_by(Mission.Type, Environmental.Impact) %>%
  summarise(Number_of_Missions = n(), .groups = 'drop') %>%
  mutate(Percentage = Number_of_Missions / sum(Number_of_Missions) * 100) # For percentage plot


data$Environmental.Impact <- factor(data$Environmental.Impact,
                                    levels = c("Low", "Medium", "High", "Critical"))


# --- User Interface (UI) ---
ui <- fluidPage(
  titlePanel("Global Space Strategies: Investment, Innovation, and Impact"),
  
  tabsetPanel(
    id = "story_tabs", # Give the tabset an ID for server-side logic if needed
    
    # --- Tab 1: Investment & Strategic Focus ---
    tabPanel("1. Investment & Strategic Focus",
             h3("The Architects of Space: Investment & Strategic Focus"),
             p("Who leads in space? It's not just about total spending, but how budgets are allocated. Do top nations prioritize high-stakes manned missions or widespread unmanned exploration? Their choices reveal their core strategy."),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "num_top_countries",
                   label = "Show Top Countries (by total budget):",
                   choices = c(5, 10),
                   selected = 10 # Default to top 10
                 ),
                 p("Select how many of the top budgeting countries to analyze.")
               ),
               mainPanel(
                 h4("Strategic Budget Allocation by Mission Type"),
                 p("This shows how the budget of leading spacefaring nations is split between manned and unmanned missions. It reveals their strategic priorities."),
                 plotOutput("budgetAllocationPlot")
               )
             )
    ),
    
    # --- Tab 2: Technology, Success & Sustainability ---
    tabPanel("2. The Edge of Innovation: Technology, Success & Sustainability",
             h3("The Edge of Innovation: Technology, Success & Sustainability"),
             p("Beyond investment, **innovation drives progress**. We'll investigate if cutting-edge technologies like Reusable Rockets lead to better mission success rates. Crucially, how do these technological choices affect the environmental footprint of space missions?"),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "selected_tech_impact",
                   label = "Focus on Technology:",
                   choices = technologies,
                   selected = "Reusable Rocket"
                 ),
                 p("Select a technology to see its success rate and environmental impact.")
               ),
               mainPanel(
                 h4("Technology's Payoff: Success Rates and Environmental Footprint"),
                 p("These charts examine if specific technologies lead to higher mission success and what environmental impact profile they typically have."),
                 fluidRow(
                   column(6,
                          h5("Average Success Rate by Technology"),
                          p("This chart compares average success rates across technologies. A higher bar means more successful missions."),
                          plotOutput("techSuccessPlot")
                   ),
                   column(6,
                          h5("Environmental Impact by Technology"),
                          p("This chart shows environmental impact levels for missions using various technologies. It helps identify greener or more impactful options."),
                          plotOutput("techEnvImpactPlot")
                   )
                 )
               )
             )
    ),
    
    # --- Tab 3: Mission Profiles: Complexity, Duration & Footprint ---
    tabPanel("3. Mission Profiles: Complexity, Duration & Footprint",
             h3("Mission Profiles: Complexity, Duration & Footprint"),
             p("Manned and unmanned missions are fundamentally different, representing distinct approaches to space with unique complexities. How do these differences play out in mission duration and environmental impact? This tab reveals the trade-offs inherent in each mission type."),
             hr(),
             mainPanel(
               fluidRow(
                 column(6,
                        h4("Average Mission Duration by Type"),
                        p("This chart shows the typical lifespan of manned versus unmanned missions. Longer duration might indicate sustained scientific objectives."),
                        plotOutput("durationByMissionTypePlot")
                 ),
                 column(6,
                        h4("Environmental Impact Distribution by Mission Type"),
                        p("This chartt breaks down environmental impact levels for manned and unmanned missions. This can highlight if one type carries a higher environmental cost."),
                        plotOutput("envImpactByMissionTypePlot")
                 )
               )
             )
    )
  )
)


# --- Server Logic ---
server <- function(input, output) {
  
  # --- Tab 1 Server Logic ---
  output$budgetAllocationPlot <- renderPlot({
    num_countries <- as.numeric(input$num_top_countries)
    

    plot_data <- budget_by_country_mission_type %>%
      distinct(Country, Country_Overall_Budget) %>%
      top_n(num_countries, Country_Overall_Budget) %>%
      pull(Country) %>%
      { . } -> top_n_countries_list # Get the list of top N countries
    
    filtered_data <- budget_by_country_mission_type %>%
      filter(Country %in% top_n_countries_list) %>%

      group_by(Country) %>%
      mutate(Current_Overall_Budget = sum(Total_Budget)) %>%
      ungroup() %>%
      arrange(desc(Current_Overall_Budget))
    
    # Bar chart for budget allocation
    ggplot(filtered_data, aes(x = reorder(Country, Current_Overall_Budget), y = Total_Budget, fill = Mission.Type)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = paste0("Budget Allocation by Mission Type for Top ", num_countries, " Countries"),
        x = "Country",
        y = "Total Budget (in Billion $)",
        fill = "Mission Type"
      ) +
      scale_fill_manual(values = c("Manned" = "#FF7F50", "Unmanned" = "#6495ED")) + # Custom colors
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_flip() # Horizontal bars for better readability
  })
  
  # --- Tab 2 Server Logic ---
  output$techSuccessPlot <- renderPlot({
    
    ggplot(avg_success_rate_by_tech, aes(x = reorder(Technology.Used, Average_Success_Rate), y = Average_Success_Rate, fill = Technology.Used == input$selected_tech_impact)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "#28a745")) + # Highlight color for selected tech
      labs(
        title = "Average Mission Success Rate by Technology Used",
        x = "Technology Used",
        y = "Average Success Rate (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip()
  })
  
  output$techEnvImpactPlot <- renderPlot({

    plot_data <- env_impact_by_tech %>%
      filter(Technology.Used == input$selected_tech_impact)
    
    if (nrow(plot_data) == 0) {

      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No environmental impact data for this technology.", size = 5) +
        theme_void()
    } else {

      plot_data$Environmental.Impact <- factor(plot_data$Environmental.Impact,
                                               levels = c("Low", "Medium", "High", "Critical"))
      
      ggplot(plot_data, aes(x = Technology.Used, y = Number_of_Missions, fill = Environmental.Impact)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(
          title = paste0("Environmental Impact for Missions Using ", input$selected_tech_impact),
          x = "Technology Used",
          y = "Number of Missions",
          fill = "Impact Level"
        ) +
        scale_fill_manual(values = c("Low" = "#90EE90", "Medium" = "#FFD700", "High" = "#FF8C00", "Critical" = "#DC143C")) + # Custom colors
        theme_minimal() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) # Remove x-axis text/ticks as it's a single bar
    }
  })
  
  # --- Tab 3 Server Logic ---
  output$durationByMissionTypePlot <- renderPlot({
    ggplot(avg_duration_by_mission_type, aes(x = Mission.Type, y = Average_Duration_Days, fill = Mission.Type)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Average Mission Duration by Mission Type",
        x = "Mission Type",
        y = "Average Duration (Days)"
      ) +
      scale_fill_manual(values = c("Manned" = "#FF7F50", "Unmanned" = "#6495ED")) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$envImpactByMissionTypePlot <- renderPlot({

    plot_data <- env_impact_distribution_by_mission_type
    plot_data$Environmental.Impact <- factor(plot_data$Environmental.Impact,
                                             levels = c("Low", "Medium", "High", "Critical"))
    
    ggplot(plot_data, aes(x = Mission.Type, y = Percentage, fill = Environmental.Impact)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = "Environmental Impact Distribution by Mission Type",
        x = "Mission Type",
        y = "Percentage of Missions (%)",
        fill = "Impact Level"
      ) +
      scale_fill_manual(values = c("Low" = "#90EE90", "Medium" = "#FFD700", "High" = "#FF8C00", "Critical" = "#DC143C")) + # Custom colors
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)