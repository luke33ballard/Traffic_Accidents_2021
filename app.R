# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)  # Load shinydashboard package
library(leaflet)

drugs <- read.csv("FARS2021NationalCSV/drugs.csv")

drugs <- drugs %>% filter(DRUGRES > 1) %>% select(DRUGRESNAME, ST_CASE)

data <- read.csv("FARS2021NationalCSV/accident.csv")

data <- merge(data, drugs, by = "ST_CASE", all.x = TRUE)

# Count the frequency of each category
category_counts <- data %>%
  group_by(DRUGRESNAME) %>%
  summarise(Count = n())

# Define a threshold count (e.g., 15)
threshold <- 150

# Rename categories with count less than the threshold
data <- data %>%
  left_join(category_counts, by = "DRUGRESNAME") %>%
  mutate(DRUGRESNAME = ifelse(Count < threshold, "Other Drug", DRUGRESNAME)) %>%
  select(-Count)

# Fill NA values in the "DRUGRESNAME" column with "None"
data$DRUGRESNAME[is.na(data$DRUGRESNAME)] <- "None"
# Assuming 'DRUGRESNAME' is the column you want to update
data$DRUGRESNAME <- ifelse(data$DRUGRESNAME == "Tested For Drugs, Drugs Found, Type unknown/Positive", "Other Drugs", data$DRUGRESNAME)
# Assuming 'DRUGRESNAME' is the column you want to update
data$DRUGRESNAME <- ifelse(data$DRUGRESNAME == "Test For Drug, Results Unknown" | data$DRUGRESNAME == "Reported as Unknown if Tested for Drugs", "Unknown Drug Result", data$DRUGRESNAME)


data <- data %>% select(-ST_CASE, -STATE, -TWAY_ID2, -HARM_EV, -SP_JUR, 
                        -FUNC_SYS, -RUR_URB, -ROUTE, -NHS, -WEATHER, -MAN_COLL, -RELJCT1,
                        -WRK_ZONE, -SCH_BUS, -RAIL, -NOT_HOUR, -ARR_HOUR, -ARR_MIN, -HOSP_HR, -COUNTY, 
                        -COUNTYNAME, -CITY, -DAYNAME, -MINUTENAME, -SP_JURNAME, -NHSNAME, -MILEPT,
                        -MILEPTNAME, -LATITUDENAME, -LONGITUDNAME, -RELJCT1, -RELJCT2, -LGT_COND,
                        -YEAR, -DAY_WEEK, -MONTH, -NOT_HOUR, -NOT_MIN, -REL_ROAD, -TYP_INT, -HOURNAME,
                        -HOSP_MN, -CITYNAME, -TWAY_ID, -DAY, -NOT_HOURNAME, -NOT_MINNAME, -ARR_HOURNAME,
                        -ARR_MINNAME, -HOSP_HRNAME, -HOSP_MNNAME, -RD_OWNER, -HARM_EVNAME, -RAILNAME, -MINUTE,
                        -PEDS, -VE_TOTAL, -VE_FORMS, -PERNOTMVIT, -PVH_INVL)
# Define columns to exclude from factor conversion
columns_to_exclude <- c("LONGITUD", "LATITUDE")

# Check and convert the necessary columns to factors
for (col_name in names(data)) {
  if (col_name %in% columns_to_exclude) {
    next  # Skip this column
  }
  
  if (is.numeric(data[[col_name]])) {
    data[[col_name]] <- as.factor(data[[col_name]])
  } else if (is.character(data[[col_name]])) {
    data[[col_name]] <- as.factor(data[[col_name]])
  }
}

# Define the UI
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bar Chart", tabName = "barChart", icon = icon("bar-chart")),
      menuItem("Interactive Map", tabName = "mapTab", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "barChart",
        fluidPage(
          titlePanel("Bar Chart for Lethal Traffic Accidents"),
          sidebarLayout(
            sidebarPanel(
              selectInput("x_axis", "X-Axis Column", ""),
              selectInput("group", "Group Column", ""),
              conditionalPanel(
                condition = "input.group != '' && input.group != null",
                checkboxInput("relative", "Relative Bar Chart", value = FALSE)
              )
            ),
            mainPanel(
              plotOutput("barChart", height = "600px")
            )
          )
        )
      ),
      tabItem(
        tabName = "mapTab",
        fluidPage(
          titlePanel("Interactive US Map"),
          sidebarLayout(
            sidebarPanel(
              selectInput("group_column", "Group Column", choices = c("", setdiff(names(data), c("LATITUDE", "LONGITUD", "STATENAME")))),
              actionButton("clear_highlight", "Clear Highlight")
            ),
            mainPanel(
              leafletOutput("map", height = "700px")
            )
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "x_axis", choices = setdiff(names(data), c("LATITUDE", "LONGITUD")))
    updateSelectInput(session, "group", choices = c("", setdiff(names(data), c("LATITUDE", "LONGITUD")))
    )
  })
  
  # Generate the bar chart
  output$barChart <- renderPlot({
    x_axis_col <- input$x_axis
    group_col <- input$group
    relative <- input$relative
    
    if (group_col != "") {
      p <- ggplot(data, aes_string(x = x_axis_col, group = group_col, fill = group_col)) +
        geom_bar(position = ifelse(relative, "fill", "dodge")) +
        labs(x = "", y = "", fill = group_col, title = "") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          panel.background = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent")
        )
      
      p <- p + theme(legend.position = "bottom")
    } else {
      p <- ggplot(data, aes_string(x = x_axis_col, fill = x_axis_col)) +
        geom_bar(position = ifelse(relative, "fill", "dodge")) +
        labs(x = "", y = "", title = "") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 13, face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          panel.background = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent"),
          
        )
      
      p <- p + theme(legend.position = "none")
    }
    
    p
  })
  
  # Initialize a leaflet map
  output$map <- renderLeaflet({
    leaflet(data) %>% addTiles() %>% setView(lng = -95.7129, lat = 37.0902, zoom = 4)
  })
  
  # Initialize a reactive variable to store the selected group value
  selected_group_value <- reactiveVal()
  
  # Highlight markers based on the selected group
  observeEvent(input$group_column, {
    if (input$group_column != "") {
      selected_group <- input$group_column
      selected_group_value(NULL)
      
      # Define a custom color palette with distinct colors
      custom_color_palette <- colorFactor(
        palette = c("blue", "green", "red", "purple", "orange", "yellow", "brown", "pink", "cyan", "gray"),
        domain = unique(data[[selected_group]])
      )
      
      # Modify the code to use the custom color palette
      leafletProxy("map", data = data) %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(
          lng = ~LONGITUD, lat = ~LATITUDE,
          color = ~custom_color_palette(get(selected_group)),  # Use the custom color palette
          radius = 0.1,
          fillOpacity = 0.8
        ) %>%
        addLegend("topright",
                  pal = custom_color_palette,  # Use the custom color palette
                  values = unique(data[[selected_group]]),
                  title = selected_group
        )
      
    }
  })
  
  # Update the map when a legend value is clicked
  observe({
    if (!is.null(selected_group_value())) {
      selected_group <- input$group_column
      selected_value <- selected_group_value()
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(
          lng = ~LONGITUD, lat = ~LATITUDE, 
          color = ~color_palette(get(selected_group)),
          radius = 0.1,
          fillOpacity = 0.8,
          data = filter(data, !!as.name(selected_group) == selected_value)
        )
    }
  })
  
  # Store the selected group value when a legend value is clicked
  observeEvent(input$map_legend_click, {
    selected_group_value(input$map_legend_click$id)
  })
  
  # Clear the highlighted markers
  observeEvent(input$clear_highlight, {
    selected_group_value(NULL)
    leafletProxy("map") %>%
      clearMarkers()
  })
}

# Run the Shiny app
shinyApp(ui, server)
