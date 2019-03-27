##-----------------------------------------------------------
## Packages
##-----------------------------------------------------------

library(tidyverse)
library(shiny)
library(igraph)
library(tidygraph)
library(ggraph)
library(maps)
library(countrycode)
library(shinythemes)
library(DT)

##-----------------------------------------------------------
## Data
##-----------------------------------------------------------

source_info <- read_csv("data/source_info.csv")
target_info <- read_csv("data/target_info.csv")
gdp <- readRDS("data/wdi_data.RDS") %>% 
  select(country, continent, year, NY.GDP.MKTP.CD) %>%
  rename(gdp = NY.GDP.MKTP.CD) %>% 
  ungroup()

target_info <- target_info %>%
  left_join(rename(gdp, target = country)) %>% 
  rename(target_gdp = gdp, target_continent = continent) %>% 
  drop_na() %>% 
  left_join(rename(gdp, source = country)) %>% 
  rename(source_gdp = gdp, source_continent = continent) %>% 
  drop_na()

source_info <- source_info %>%
  left_join(rename(gdp, target = country)) %>% 
  rename(target_gdp = gdp, target_continent = continent) %>% 
  drop_na() %>% 
  left_join(rename(gdp, source = country)) %>% 
  rename(source_gdp = gdp, source_continent = continent) %>% 
  drop_na()

##-----------------------------------------------------------
## Map and graph data
##-----------------------------------------------------------

mapcoords <- coord_fixed(xlim = c(-170, 180), ylim = c(-55, 80))

world_data <- map_data("world") %>% 
  filter(region != "Antartica") %>% 
  mutate(iso = maps::iso.alpha(region, n = 3))

node_info <- read_delim("data/nodes.txt", delim = " ")

##-----------------------------------------------------------
## Functions
##-----------------------------------------------------------

get_iso <- function(x) {
  countrycode::countrycode(
    x, origin = "country.name", 
    destination = "iso3c")
}

make_flows <- function(country = "Colombia", y = 2005, info = "target", direction = "inflow") {
  theme_map <- theme_void(base_family = "Palatino") + 
    theme(plot.background = element_rect(fill = "white", colour = "white"),
          plot.title = element_text(hjust = 0.5))
  
  if (direction == "inflow") {
    type <- quote(target)
    other <- quote(source)
  }
  
  if (direction == "outflow") {
    type <- quote(source)
    other <- quote(target)
  }
  
  if (info == "target") {
    df <- target_info %>% 
      filter(!!type == country, year == y, flow != 0) %>% 
      mutate(iso = get_iso(!!other),
             edge_weight = abs(flow) * 1e5 / 
               ifelse(type == "inflow", target_gdp, source_gdp)
      ) 
  }
  if (info == "source") {
    df <- source_info %>% 
      filter(!!type == country, year == y, flow != 0) %>% 
      mutate(iso = get_iso(!!other),
             edge_weight = abs(flow) * 1e5 / 
               ifelse(type == "inflow", target_gdp, source_gdp)
      ) 
  }
  
  Gt <- graph_from_data_frame(df) %>% 
    as_tbl_graph() %>% 
    activate(nodes) %>% 
    left_join(node_info) %>% 
    activate(edges) %>% 
    mutate(active = case_when(
      flow < 0 ~ "negative",
      flow > 0 ~ "positive"
    ))
  
  lay <- create_layout(
    graph = Gt, 
    layout = "manual",
    node.positions = rename(
      as_data_frame(Gt, what = "vertices"), 
      x = lon, y = lat)
  )
  
  world_df <- world_data %>% 
    left_join(df) %>% 
    mutate(active = case_when(
      flow < 0 ~ "negative",
      flow > 0 ~ "positive",
      iso == get_iso(country) ~ "focus"
    ))
  
  country_shapes <- geom_polygon(
    aes(x = long, y = lat, group = group, 
        fill = active, alpha = edge_weight),
    data = world_df,
    color = "grey",
    size = 0.1,
    show.legend = FALSE)
  
  g <- ggraph(lay) + 
    country_shapes +
    geom_edge_arc(aes(color = active, alpha = edge_weight), 
                  curvature = 1/3, show.legend = FALSE,
                  arrow = arrow(length = unit(0.1, "cm")),
                  end_cap = circle(0.1, "cm"),
                  start_cap = circle(0, "cm"),
                  width = 0.8) +
    theme_map + mapcoords
  
  if (any(df$flow < 0, na.rm = TRUE) & any(df$flow > 0)) {
    g <- g + 
      scale_edge_color_manual(values = c("red", "steelblue1")) +
      scale_fill_manual(values = c("grey", "red", "steelblue1"))
  } else if (any(df$flow > 0)) {
    g <- g + 
      scale_edge_color_manual(values = c("steelblue1")) +
      scale_fill_manual(values = c("grey", "steelblue1"))
  } else if (any(df$flow < 0)) {
    g <- g +
      scale_edge_color_manual(values = c("red")) +
      scale_fill_manual(values = c("grey", "red"))
  }
  return(g)
}

make_table <- function(country, y, info, direction) {
  if (direction == "inflow") {
    type <- quote(target)
    other <- quote(source)
  }
  
  if (direction == "outflow") {
    type <- quote(source)
    other <- quote(target)
  }
  
  if (info == "target") {
    output <- target_info %>% 
      filter(!!type == country, year == y, flow != 0) %>% 
      select(!!other, !!type, flow) %>% 
      arrange(-abs(flow))
  }
  
  if (info == "source") {
    output <- source_info %>% 
      filter(!!type == country, year == y) %>% 
      select(!!other, !!type, flow) %>% 
      arrange(-abs(flow))
  }
  return(output)
}


##-----------------------------------------------------------
## User Interface (UI)
##-----------------------------------------------------------

library(shiny)

ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),
  titlePanel("Foreign Direct Investment Network Explorer", 
             windowTitle = "FDI Net Explorer"),
  
  # Inputs
  fluidRow(
    # Select country
    column(3, selectInput(inputId = "country",
                          label = "Country: ",
                          choices = unique(c(target_info$target, source_info$target)),
                          selected = "Colombia")),
    
    # Select direction of FDI flows
    column(3, radioButtons(inputId = "direction",
                           label = "Direction of FDI flows: ",
                           choices = c("Inflows" = "inflow",
                                       "Outflows" = "outflow"),
                           selected = "inflow")),
    
    # Select year
    column(3, sliderInput(inputId = "year",
                          label = "Year: ",
                          min = 2001, max = 2012,
                          value = 2007,
                          step = 1, 
                          sep = "", ticks = FALSE)),
    
    # Select information source
    column(3, radioButtons(inputId = "info",
                           label = "Source of information: ",
                           choices = list("Target countries" = "target",
                                          "Source countries" = "source"),
                           selected = "target"))
    ),
                
  # Outputs
  fluidRow(
    column(10, offset = 1,
           plotOutput(outputId = "map"),
           DT::dataTableOutput(outputId = "df"))
    ),
  
  # Text
  fluidRow(
    column(10, offset = 1, hr(),
           HTML("Foreign direct investments are investments that take the form of <i>controlling ownership</i> of corporations in one country by corporations based in another country. These ownership relationships get recorded in FDI statistics at the country level."),
           br(), br(),
           HTML("As such, each country reports two types of information:"), 
           br(), br(),
           HTML("<ul><li>",
                "<strong> Outward FDI </strong>. These are direct investments abroad made by investors in the reporting country. In other words, they represent transactions made by domestic investors that increase their investment in corporations based in a foreign country. If the transactions actually decrease their investment, then they are recorded as a <i> negative transaction</i>.",
                "</li><br><li>",
                "<strong> Outward FDI </strong>. These are direct investments inside the reporting country that come from foreign investors. These can also be positive or negative numbers. </li></ul>"),
           br(),
           HTML("In other words, the arrows <i>do not</i> represent simple flows of money. Instead, they represent investment decisions taken by corporations in the <i>source</i> country inside the <i>target</i> country."),
           br(), br(),
           HTML("Source of data: <a href='https://unctad.org/en/Pages/DIAE/FDI%20Statistics/FDI-Statistics-Bilateral.aspx'> UNCTAD Bilateral FDI Statistics</a>"),
           br(), br()
           )
  )
)


##-----------------------------------------------------------
## Server
##-----------------------------------------------------------

server <- function(input, output) {
  output$map <- renderPlot({
    make_flows(country = input$country, y = input$year, 
               info = input$info, direction = input$direction)
  })
  
  output$df <- DT::renderDataTable({
    make_table(country = input$country, y = input$year, 
               info = input$info, direction = input$direction) %>% 
      mutate(flow = round(flow, 2)) %>% 
      rename(Source = source, Target = target, 
             "FDI (Millions of US Dollars)" = flow) %>% 
      DT::datatable(rownames = FALSE)
  })
  
}

shinyApp(ui, server)