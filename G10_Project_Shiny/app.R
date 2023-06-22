pacman::p_load(jsonlite, tidygraph, ggraph, visNetwork, graphlayouts, ggforce, skimr, tidytext, tidyverse, DT, ggiraph, tm, topicmodels, shiny, shinythemes, bslib, ggplot2, shinycssloaders)

revenue_quartiles <- readRDS("data/revenue_quartiles.rds")
mc3_graph <- readRDS("data/mc3_graph.rds")

one <- revenue_quartiles[2]
two <- revenue_quartiles[3]
three <- revenue_quartiles[5]

create_network <- function(mygraph) {
  
  g <- ggraph(mygraph,
              layout = "nicely") +
    geom_edge_link(aes(),
                   color = "black",
                   alpha = 0.8) +
    geom_point_interactive(
      aes(
        x = x,
        y = y,
        tooltip = paste0(
          "Name:  ", id,
          ifelse(is.na(country), "", paste0("\nCountry:  ", country)),
          ifelse(is.na(type), "", paste0("\nType:  ", type)),
          ifelse(is.na(revenue_omu), "", paste0("\nRevenue:  ", round(revenue_omu, 2))),
          ifelse(is.na(product_services), "", paste0("\nProduct services:  ", product_services)),
          ifelse(is.na(product_type), "", paste0("\nProduct type:  ", product_type))
        ),
        data_id = type,
        size = ifelse(is.na(revenue_omu),
                      0,
                      revenue_omu), 
        fill = type
      ),
      colour = "grey20",
      shape = 21,
      alpha = 1
    ) +
    scale_fill_discrete(name = "Node type") +
    scale_size_continuous(name = "Revenue") +
    theme_graph(foreground = "grey20", ) +
    labs(title = "") +
    theme(plot.title = element_text(size = 11))
  
  girafe(
    ggobj = g,
    options = list(
      opts_hover(css = "fill:;"),
      opts_hover_inv(css = "opacity: 0.2;"),
      opts_selection(
        type = "multiple",
        only_shiny = FALSE,
        css = "opacity:1;"
      ),
      opts_selection_inv(css = "opacity:0;")
    )
  )
  
}

# SHINY APP UI

cards_overview <- list(
  card(
    full_screen = FALSE,
    card_header(
      HTML("<b>Overview of Project - Tackling Mini-Challenge 3 of VAST Challenge 2023</b>")
      ),
    card_body(
      HTML("FishEye International, a non-profit focused on countering illegal, unreported, and unregulated (IUU) fishing, has been given access to an international finance corporation’s database on fishing related companies. In the past, FishEye has determined that companies with anomalous structures are far more likely to be involved in IUU (or other “fishy” business). FishEye has transformed the database into a knowledge graph. It includes information about companies, owners, workers, and financial status. FishEye is aiming to use this graph to identify anomalies that could indicate a company is involved in IUU.
      <br><br>
      FishEye analysts have attempted to use traditional node-link visualizations and standard graph analyses, but these were found to be ineffective because the scale and detail in the data can obscure a business’s true structure. Can you help FishEye develop a new visual analytics approach to better understand fishing business anomalies?"
           )
      )
  ),
  card(
    full_screen = FALSE,
    card_header(
      HTML("<b>Questions to be addressed</b>")
    ),
    HTML("1. Use visual analytics to identify anomalies in the business groups present in the knowledge graph.<br>"),
    HTML("2. Develop a visual analytics process to find similar businesses and group them. This analysis should focus on a business’s most important features and present those features clearly to the user.<br>"),
    HTML("3. Measure similarity of businesses that you group in the previous question. Express confidence in your groupings visually.<br>"),
    HTML("4. Based on your visualizations, provide evidence for or against the case that anomalous companies are involved in illegal fishing. Which business groups should FishEye investigate further?")
  )
)

ui <- page_navbar(
  title = "IUU FISHING BUSTERS",
  theme = bs_theme(bootswatch = "pulse"),
  nav_panel("Overview",
            layout_sidebar(
              sidebar = sidebar(card_image(file = "images/fisheye.png"),
                                open = "always",
                                card_body(HTML("<br>Sanctioned by FishEye International"))),
              cards_overview[[1]],
              cards_overview[[2]]
            )),
  nav_panel(
    "1. Identifying Anomalies",
    icon = icon("magnifying-glass"),
    layout_sidebar(sidebar = "Inputs here",
                   card("Card one"),
                   card("Card two"))
  ),
  nav_panel(
    "2. Grouping Similar Businesses",
    icon = icon("object-group"),
    layout_sidebar(sidebar = "Inputs here",
                   card("Card one"),
                   card("Card two"))
  ),
  nav_panel(
    "3. Measuring Similarity in Groupings",
    icon = icon("equals"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Select Inputs for Network Plot",
        selectInput("product_type", "Product Type:",
                    c(0, 1, 2, 3, 4, 5, 6, 7, 8)),
        selectInput(
          "filter", "Percentile by Revenue:",
          list("0 - 25th" = "revenue_omu <= one",
               "25 - 50th" = "(revenue_omu > one & revenue_omu <= two)",
               "50 - 75th" = "(revenue_omu > two & revenue_omu <= three)",
               "75 - 100th" = "revenue_omu > three"
          )
        )
      ), 
        card(card_header("Network Plot"),
             girafeOutput("network_plot",
                          height = 1000) %>% withSpinner(color = getOption("spinner.color", default = "#263238")))
    )
  ), 
  nav_panel(
    "4. Detecting Suspicious Businesses",
    icon = icon("handcuffs"),
    layout_sidebar(sidebar = "Inputs here",
                   card("Card one"),
                   card("Card two"))
  ), 
  nav_spacer(),
  nav_item(
    tags$a("G10 Website (Opens in new page)",
           href = "https://isss608-vaa-apr2023-g10.netlify.app/")
  )

)

server <- function(input, output) {
  output$network_plot <- renderGirafe({
    create_network(
      mc3_graph %>%
        activate(nodes) %>%
        filter(eval(parse(text = input$filter)) | is.na(revenue_omu)) %>%
        filter(product_type == input$product_type | node_type == "target") %>%
        filter(!node_is_isolated())
    )
  })
}

shinyApp(ui, server)