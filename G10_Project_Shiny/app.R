pacman::p_load(jsonlite, tidygraph, ggraph, visNetwork, graphlayouts, ggforce, skimr, tidytext, tidyverse, DT, ggiraph, tm, topicmodels, plotly, shiny, shinythemes, bslib, ggplot2, shinycssloaders, scales, BWStest, PMCMRplus, Rmpfr, SuppDists, gmp, kSamples, multcompView, topicdoc)

#read rds from data_preparation
dtm <- readRDS("data/dtm.rds")
filtered_mc3_nodes <-  readRDS( "data/filtered_mc3_nodes.rds")
sub_edge <- readRDS("data/sub_edge.rds")
sub_node <- readRDS("data/sub_node.rds") 
revenue_quartiles <- readRDS("data/revenue_quartiles.rds")
mc3_graph <- readRDS("data/mc3_graph.rds")
mc3_data <- fromJSON("data/MC3.json")
mc3_nodes <- readRDS("data/mc3_nodes.rds")
mc3_edges_cleaned <- readRDS("data/mc3_edges_cleaned.rds")
select_country <- readRDS("data/select_country.rds")

num_types <- c("Beneficial owner + Company Contacts" = "num_beneficial_owner+num_company_contacts",
               "Beneficial owner" = "num_beneficial_owner",
               "Company Contacts" = "num_company_contacts"
)

one <- revenue_quartiles[2]
two <- revenue_quartiles[3]
three <- revenue_quartiles[5]

create_network_revenue <- function(mygraph) {
  
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
      shape = 21,
      alpha = 1,
      colour = "grey20"
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

create_network_country <- function(mygraph) {
  
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
        fill = type,
        colour = country
      ),
      shape = 21,
      alpha = 1,
      stroke = 0.8
    ) +
    scale_fill_discrete(name = "Node type") +
    scale_color_discrete(name = "Country") +
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
    layout_sidebar(
      height = 400,
      sidebar = sidebar(
        title = "Investigation on Abnormal Number of Beneficial Owners / Company Contacts",
        radioButtons("num_type", "Category:",num_types)),
      layout_column_wrap(
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          card(card_header("Table"),
               div(DT::dataTableOutput("sus_company_table"), style = "font-size: 85%; width: 85%; height: 85%") %>% withSpinner(color = getOption("spinner.color", default = "#263238")))
        ),
        width = 1/2,
        card(
          card_header("Plot"),
          # height = 250,
          full_screen = TRUE,
          card_body(plotlyOutput("sus_company_graph"))
        )
      )
    ),
    
    layout_sidebar(
      height = 400,
      sidebar = sidebar(
        title = "Investigation on Abnormal Number of Registraion in Different Countries",
        selectInput(inputId = "q1_threshold", 
                    label="Number of countries of registration equal to or more than:",
                    c(3, 4, 5),
                    selected = "3")),
      layout_column_wrap(
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          card(card_header("Table"),
               div(DT::dataTableOutput("company_mt2_table"), style = "font-size: 85%; width: 85%; height: 85%") %>% withSpinner(color = getOption("spinner.color", default = "#263238")))
        ),
        width = 1/2,
        card(
          card_header("Plot"),
          full_screen = TRUE,
          card_body(
            girafeOutput("company_mt2_ct_graph")
          ),
          width = "600px", height = "500px"
        )
      ))
  ),
  nav_panel(
    "2. Grouping Similar Businesses",
    icon = icon("object-group"),
    layout_sidebar(
      sidebar = sidebar(
        title = "LDA Topic Modelling",
        selectInput(inputId = "num_topic", 
                    label="Number of topics (LDA modelling might take a while):",
                    c(3, 4, 5, 6, 7, 8),
                    selected = "5"),
        sliderInput(inputId="num_words",
                    label = "Number of top words shown for each topic",
                    min=2,
                    max=10,
                    value=5),
        radioButtons("layout", "Layout:",
                     choices = c("layout_in_circle", "layout_with_fr", "layout_with_sugiyama","layout_with_kk"),
                     selected = "layout_with_fr"),
        radioButtons("node_size", "Centrality Measure for node size:",
                     choices = c("betweenness_centrality", "degree_centrality", "eigenvector_centrality","closeness_centrality"),
                     selected = "betweenness_centrality")
      ),
      layout_column_wrap(
        layout_column_wrap(
          width = 1, 
          heights_equal = "row",
          card(card_header("Topics with Top Terms"),tableOutput("topic_table") %>% withSpinner(color = getOption("spinner.color", default = "#263238"))),
          card(card_header("Topics Distribution"),plotlyOutput(("barPlot"),width = "550px", height = "450px") %>% withSpinner(color = getOption("spinner.color", default = "#263238")))
        ),
        width = 1/2, 
        card(card_header("Network Visualisation (subset of beneficial owners with >= 5 companies)"),visNetworkOutput(("Q2_network"),height="1000px") %>% withSpinner(color = getOption("spinner.color", default = "#263238")))
      )
    )
  ),
  
  nav_panel(
    "3. Measuring Similarity in Groupings",
    icon = icon("equals"),
    navset_tab(
      nav_panel(title = "Filter by Revenue",
                layout_sidebar(
                  sidebar = sidebar(
                    title = "Select Inputs for Network Plot",
                    selectInput("product_type_revenue", "Product Type (based on Topic Modelling in [2], no. of product types should not exceed the chosen number of topics):",
                                c(1, 2, 3, 4, 5, 6, 7, 8),
                                selected = 1),
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
                       girafeOutput("network_plot_revenue",
                                    height = 800) %>% withSpinner(color = getOption("spinner.color", default = "#263238"))),
                  layout_column_wrap(
                    width = 1/2,
                    card(card_header("Boxplot of Revenue by Product Type (with Outliers)"),
                         plotOutput("stats_revenue",
                                    height = 200) %>% withSpinner(color = getOption("spinner.color", default = "#263238"))),
                    card(card_header("Boxplot of Revenue by Product Type (without Outliers)"),
                         plotOutput("stats_revenue_no_outliers",
                                    height = 200) %>% withSpinner(color = getOption("spinner.color", default = "#263238")))
                  )
                  
                )),
      nav_panel(title = "Filter by Country",
                layout_sidebar(
                  sidebar = sidebar(
                    title = "Select Inputs for Network Plot",
                    selectInput("product_type_country", "Product Type (based on Topic Modelling in [2], no. of product types should not exceed the chosen number of topics):",
                                c(1, 2, 3, 4, 5, 6, 7, 8),
                                selected = 1),
                    selectInput(
                      "country", "Country (select based on histogram below):",
                      select_country
                    )
                  ),
                  card(card_header("Network Plot"),
                       girafeOutput("network_plot_country",
                                    height = 800) %>% withSpinner(color = getOption("spinner.color", default = "#263238"))),
                  card(card_header("Histogram of Country for Selected Product Type"),
                       plotOutput("histogram",
                                  height = 200) %>% withSpinner(color = getOption("spinner.color", default = "#263238")))
                  )
      )
    )
  ), 
  nav_panel(
    "4. Detecting Suspicious Businesses",
    icon = icon("handcuffs"),
    layout_column_wrap(
      width = 1/2,
      # height = 300,
      card(full_screen = TRUE,
           card_header("Detect Suspicious Company through Anomalies"),
           card_body(
             HTML("<b>Company:<ul>
                  <li> Aqua Aura SE Marine Life </li>
                  </ul></b>"),
             card_image(file = "images/Q1_1.png", width = 400, height = 400*0.8, min_height = 190),
             HTML("<b>Approach 1: Identifying Companies with Abnormal Structure.</b>Companies with an abnormal number of beneficial owners or company contacts relative to their declared revenue are considered suspicious. Such companies may raise red flags for potential involvement in IUU fishing.<br>Aqua Aura SE Marine Life has emerged as the most suspicious company. It is associated with a total of 33 beneficial owners and 8 company contacts, indicating a significant deviation from the norm.<br>"),
             card_image(file = "images/Q1_2.png", width = 400, height = 400*0.7, min_height = 190),
             HTML("<b>Approach 2: Identifying Companies Registered in Multiple Countries.</b>Companies that are registered in more than three countries are considered suspicious. The rationale behind this is that companies operating in multiple countries might have complex ownership structures, which could make it easier for them to engage in IUU fishing activities across various jurisdictions.<br>Aqua Aura SE Marine Life is flagged again in this approach as it has operations in 9 countries."),
           )
      ),
      card(full_screen = TRUE,
           card_header("Detect Suspicious Company through Anomalies"),
           card_body(
             HTML("<b>Company:<ul>
                  <li> BlueTide GmbH & Co.KG </li>
                  <li> Mar del Oeste </li>
                  <li> West Fish GmbH Transport </li>
                  </ul></b>"),
             card_image(file = "images/Q4_1.png", width = 400, height = 400),
             HTML("<b>Approach 3: Identifying beneficial owners who own multiple companies.</b> A person who owns multiple companies might be involved in IUU fishing. We plotted network focusing on the beneficial owners who have >= 5 companies. From the network graph, we can observe a big cluster includes several high betweenness companies. Beneficial owner “Jessica Brown” is the essential linkage in this cluster. Therefore her 1-hop neighbour companies are likely to be involved in IUU fishing."),
             card_image(file = "images/Q4_2.png", width = 400, height = 320)
           )
      )
    )
  ), 
  nav_spacer(),
  nav_item(
    tags$a("G10 Website",
           href = "https://isss608-vaa-apr2023-g10.netlify.app/")
  )
  
)

server <- function(input, output) {
  
  tp_modelling <- reactive({ 
    lda <- LDA(dtm, control=list(seed=1), k = as.numeric(input$num_topic))
    lda
  })
  
  # Q2 outputs
  output$topic_table <- renderTable({
    lda <- tp_modelling()
    top_words <- terms(lda, input$num_words)
    top_words_table <- as.data.frame(top_words, stringsAsFactors = FALSE)
    top_words_table
  })
  
  output$barPlot <- renderPlotly({
    lda <- tp_modelling()
    filtered_mc3_nodes$product_type <- topics(lda, 1)
    topic_dist <- filtered_mc3_nodes %>%
      mutate(product_type = as.character(product_type)) %>%
      group_by(product_type) %>%
      summarise(Count = n()) %>%
      ungroup()
    p <- ggplot(topic_dist, aes(x = product_type, y = Count, fill=product_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("") +
      theme(axis.text.x = element_text(hjust = 1))
    ggplotly(p, tooltip = c("x","y"))
  })
  
  output$Q2_network <- renderVisNetwork ({ 
    lda <- tp_modelling()
    filtered_mc3_nodes$product_type <- topics(lda, 1)
    sub_node <- left_join(sub_node,filtered_mc3_nodes,by = "id",unmatched = "drop")%>%  
      distinct(id,.keep_all = TRUE)
    set.seed(1)
    
    #build graph object and calculate centrality measures
    mc3_graph <- tbl_graph(nodes = sub_node,
                           edges = sub_edge,
                           directed = FALSE) %>%
      mutate(betweenness_centrality = centrality_betweenness())%>%
      mutate(degree_centrality = centrality_degree())%>%
      mutate(closeness_centrality = centrality_closeness())%>%
      mutate(eigenvector_centrality = centrality_eigen())
    
    #store centrality values
    sub_node$value <- mc3_graph %>%
      pull(input$node_size)
    
    #store attributes with respective column name that is readable by visnetwork
    sub_node$group <- sub_node$product_type
    sub_node$group <- ifelse(is.na(sub_node$product_type),"unknown",sub_node$product_type)
    sub_node$shape <- ifelse(sub_node$type=="Company","star","dot")
    sub_edge$value <- sub_edge$weights
    
    network <- visNetwork(sub_node,sub_edge) %>%
      visIgraphLayout(layout = input$layout) %>%
      visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE,selectedBy = "product_type") %>%
      visLegend(position = "right")
    network
  })
  
  #Q3
  
  output$network_plot_revenue <- renderGirafe({
    
    lda <- tp_modelling()
    filtered_mc3_nodes$product_type <- topics(lda, 1)
    
    mc3_nodes <- left_join(mc3_nodes, filtered_mc3_nodes, by = names(mc3_nodes))
    mc3_nodes$product_type <-
      ifelse(is.na(mc3_nodes$product_type),
             0,
             mc3_nodes$product_type)
    
    id1 <- mc3_edges_cleaned %>%
      select(source) %>%
      rename(id = source) %>%
      mutate(type = "Company", node_type = "source")
    id2 <- mc3_edges_cleaned %>%
      select(target, type) %>%
      rename(id = target) %>%
      mutate(node_type = "target")
    
    mc3_nodes_combined <- rbind(id1, id2) %>%
      distinct() %>%
      left_join(mc3_nodes,
                by = "id")
    mc3_nodes_combined$type <-
      ifelse(
        is.na(mc3_nodes_combined$type.y), # if type of id is not defined in mc3_nodes
        mc3_nodes_combined$type.x, # then use the value that was generated on top within rbind(id1, id2)
        mc3_nodes_combined$type.y # else, just use the type of id defined in mc3_nodes
      )
    mc3_nodes_combined <- mc3_nodes_combined %>%
      select(id, country, type, revenue_omu, product_services, product_type, node_type)
    
    mc3_nodes_combined$product_type <-
      ifelse(mc3_nodes_combined$node_type == "source" & is.na(mc3_nodes_combined$product_type),
             0,
             mc3_nodes_combined$product_type)
    
    mc3_graph <- tbl_graph(nodes = mc3_nodes_combined,
                           edges = mc3_edges_cleaned,
                           directed = FALSE)
    
    create_network_revenue(
      mc3_graph %>%
        activate(nodes) %>%
        filter(eval(parse(text = input$filter)) | is.na(revenue_omu)) %>%
        filter(product_type == input$product_type_revenue | node_type == "target") %>%
        filter(!node_is_isolated())
    )
    
  })
  
  output$network_plot_country <- renderGirafe({
    
    lda <- tp_modelling()
    filtered_mc3_nodes$product_type <- topics(lda, 1)
    
    mc3_nodes <- left_join(mc3_nodes, filtered_mc3_nodes, by = names(mc3_nodes))
    mc3_nodes$product_type <-
      ifelse(is.na(mc3_nodes$product_type),
             0,
             mc3_nodes$product_type)
    
    id1 <- mc3_edges_cleaned %>%
      select(source) %>%
      rename(id = source) %>%
      mutate(type = "Company", node_type = "source")
    id2 <- mc3_edges_cleaned %>%
      select(target, type) %>%
      rename(id = target) %>%
      mutate(node_type = "target")
    
    mc3_nodes_combined <- rbind(id1, id2) %>%
      distinct() %>%
      left_join(mc3_nodes,
                by = "id")
    mc3_nodes_combined$type <-
      ifelse(
        is.na(mc3_nodes_combined$type.y), # if type of id is not defined in mc3_nodes
        mc3_nodes_combined$type.x, # then use the value that was generated on top within rbind(id1, id2)
        mc3_nodes_combined$type.y # else, just use the type of id defined in mc3_nodes
      )
    mc3_nodes_combined <- mc3_nodes_combined %>%
      select(id, country, type, revenue_omu, product_services, product_type, node_type)
    
    mc3_nodes_combined$product_type <-
      ifelse(mc3_nodes_combined$node_type == "source" & is.na(mc3_nodes_combined$product_type),
             0,
             mc3_nodes_combined$product_type)
    
    mc3_graph <- tbl_graph(nodes = mc3_nodes_combined,
                           edges = mc3_edges_cleaned,
                           directed = FALSE)
    
    create_network_country(
      mc3_graph %>%
        activate(nodes) %>%
        filter(country == input$country | is.na(country)) %>%
        filter(product_type == input$product_type_country | node_type == "target") %>%
        filter(!node_is_isolated())
    )
    
  })
  
  output$stats_revenue <- renderPlot({
    
    lda <- tp_modelling()
    filtered_mc3_nodes$product_type <- topics(lda, 1)
    
    mc3_nodes <- left_join(mc3_nodes, filtered_mc3_nodes, by = names(mc3_nodes))
    mc3_nodes$product_type <-
      ifelse(is.na(mc3_nodes$product_type),
             0,
             mc3_nodes$product_type)
    
    id1 <- mc3_edges_cleaned %>%
      select(source) %>%
      rename(id = source) %>%
      mutate(type = "Company", node_type = "source")
    id2 <- mc3_edges_cleaned %>%
      select(target, type) %>%
      rename(id = target) %>%
      mutate(node_type = "target")
    
    mc3_nodes_combined <- rbind(id1, id2) %>%
      distinct() %>%
      left_join(mc3_nodes,
                by = "id")
    mc3_nodes_combined$type <-
      ifelse(
        is.na(mc3_nodes_combined$type.y), # if type of id is not defined in mc3_nodes
        mc3_nodes_combined$type.x, # then use the value that was generated on top within rbind(id1, id2)
        mc3_nodes_combined$type.y # else, just use the type of id defined in mc3_nodes
      )
    mc3_nodes_combined <- mc3_nodes_combined %>%
      select(id, country, type, revenue_omu, product_services, product_type, node_type)
    
    mc3_nodes_combined$product_type <-
      ifelse(mc3_nodes_combined$node_type == "source" & is.na(mc3_nodes_combined$product_type),
             0,
             mc3_nodes_combined$product_type)
    
    mc3_graph <- tbl_graph(nodes = mc3_nodes_combined,
                           edges = mc3_edges_cleaned,
                           directed = FALSE)
    
    nodes <- mc3_graph %>% activate(nodes) %>% data.frame() %>% filter(is.na(product_type) == FALSE) %>% filter(product_type != 0)
    nodes$product_type <- nodes$product_type %>% factor()
    
    ggplot(nodes,
           aes(y = revenue_omu,
               x = product_type,
               fill = product_type)) +
      geom_boxplot(notch = TRUE) +
      geom_point(position = "jitter",
                 size = 0.5) +
      stat_summary(geom = "point",
                   fun = "mean",
                   colour = "red",
                   size = 2) +
      scale_fill_discrete(guide = "none") +
      theme_classic()
    
  })
  
  output$stats_revenue_no_outliers <- renderPlot({
    
    lda <- tp_modelling()
    filtered_mc3_nodes$product_type <- topics(lda, 1)
    
    mc3_nodes <- left_join(mc3_nodes, filtered_mc3_nodes, by = names(mc3_nodes))
    mc3_nodes$product_type <-
      ifelse(is.na(mc3_nodes$product_type),
             0,
             mc3_nodes$product_type)
    
    id1 <- mc3_edges_cleaned %>%
      select(source) %>%
      rename(id = source) %>%
      mutate(type = "Company", node_type = "source")
    id2 <- mc3_edges_cleaned %>%
      select(target, type) %>%
      rename(id = target) %>%
      mutate(node_type = "target")
    
    mc3_nodes_combined <- rbind(id1, id2) %>%
      distinct() %>%
      left_join(mc3_nodes,
                by = "id")
    mc3_nodes_combined$type <-
      ifelse(
        is.na(mc3_nodes_combined$type.y), # if type of id is not defined in mc3_nodes
        mc3_nodes_combined$type.x, # then use the value that was generated on top within rbind(id1, id2)
        mc3_nodes_combined$type.y # else, just use the type of id defined in mc3_nodes
      )
    mc3_nodes_combined <- mc3_nodes_combined %>%
      select(id, country, type, revenue_omu, product_services, product_type, node_type)
    
    mc3_nodes_combined$product_type <-
      ifelse(mc3_nodes_combined$node_type == "source" & is.na(mc3_nodes_combined$product_type),
             0,
             mc3_nodes_combined$product_type)
    
    mc3_graph <- tbl_graph(nodes = mc3_nodes_combined,
                           edges = mc3_edges_cleaned,
                           directed = FALSE)
    
    nodes <- mc3_graph %>% activate(nodes) %>% data.frame() %>% filter(is.na(product_type) == FALSE) %>% filter(product_type != 0)
    nodes$product_type <- nodes$product_type %>% factor()
    
    ggplot(nodes,
           aes(y = revenue_omu,
               x = product_type,
               fill = product_type)) +
      geom_boxplot(notch = TRUE,
                   outlier.shape = NA) +
      geom_point(position = "jitter",
                 size = 0.5) +
      stat_summary(geom = "point",
                   fun = "mean",
                   colour = "red",
                   size = 2) +
      scale_fill_discrete(guide = "none") +
      scale_y_continuous(limits = c(0, 100000)) +
      theme_classic()
    
  })
  
  output$histogram <- renderPlot({
    
    lda <- tp_modelling()
    filtered_mc3_nodes$product_type <- topics(lda, 1)
    
    mc3_nodes <- left_join(mc3_nodes, filtered_mc3_nodes, by = names(mc3_nodes))
    mc3_nodes$product_type <-
      ifelse(is.na(mc3_nodes$product_type),
             0,
             mc3_nodes$product_type)
    
    id1 <- mc3_edges_cleaned %>%
      select(source) %>%
      rename(id = source) %>%
      mutate(type = "Company", node_type = "source")
    id2 <- mc3_edges_cleaned %>%
      select(target, type) %>%
      rename(id = target) %>%
      mutate(node_type = "target")
    
    mc3_nodes_combined <- rbind(id1, id2) %>%
      distinct() %>%
      left_join(mc3_nodes,
                by = "id")
    mc3_nodes_combined$type <-
      ifelse(
        is.na(mc3_nodes_combined$type.y), # if type of id is not defined in mc3_nodes
        mc3_nodes_combined$type.x, # then use the value that was generated on top within rbind(id1, id2)
        mc3_nodes_combined$type.y # else, just use the type of id defined in mc3_nodes
      )
    mc3_nodes_combined <- mc3_nodes_combined %>%
      select(id, country, type, revenue_omu, product_services, product_type, node_type)
    
    mc3_nodes_combined$product_type <-
      ifelse(mc3_nodes_combined$node_type == "source" & is.na(mc3_nodes_combined$product_type),
             0,
             mc3_nodes_combined$product_type)
    
    mc3_graph <- tbl_graph(nodes = mc3_nodes_combined,
                           edges = mc3_edges_cleaned,
                           directed = FALSE)
    
    nodes <- mc3_graph %>% activate(nodes) %>% data.frame() %>% filter(is.na(country) == FALSE) %>% filter(product_type == input$product_type_country)
    
    ggplot(nodes, aes(x = country)) +
      geom_histogram(
        stat = "count",
        color = "black",
        fill = "grey"
      ) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  # Observe changes in num_type and print corresponding names in num_types vector
  observe({
    x1 <- input$num_type
    print(names(which(num_types == x1)))
  })
  
  ####
  # Q1: Compare max revenue vs. number of important people
  
  # Process edges data to get a tibble with unique links and corresponding weights
  mc3_edges <- as_tibble(mc3_data$links) %>% 
    distinct() %>%
    mutate(source = as.character(source),
           target = as.character(target),
           type = as.character(type)) %>%
    group_by(source, target, type) %>%
    summarise(weights = n()) %>%
    filter(source != target) %>%
    ungroup()
  
  # Process nodes data to get a tibble with necessary attributes
  mc3_nodes <- as_tibble(mc3_data$nodes) %>%
    mutate(country = as.character(country),
           id = as.character(id),
           product_services = as.character(product_services),
           revenue_omu = as.numeric(as.character(revenue_omu)),
           type = as.character(type)) %>%
    select(id, country, type, revenue_omu, product_services)
  
  # Join edges and nodes tibbles to get aggregated data
  id1 <- mc3_edges %>%
    select(source) %>%
    rename(id = source)
  id2 <- mc3_edges %>%
    select(target) %>%
    rename(id = target)
  mc3_nodes1 <- rbind(id1, id2) %>%
    distinct() %>%
    left_join(mc3_nodes, unmatched = "drop")
  
  mc3_nodes2 <- mc3_nodes1
  mc3_edges2 <- mc3_edges
  
  # Aggregate data based on node id to get maximum revenue for each company
  mc3_nodes2 <- mc3_nodes2 %>%
    mutate(revenue_omu = replace_na(revenue_omu, 0))
  
  mc3_nodes2_agg2 <- mc3_nodes2 %>%
    group_by(id) %>%
    summarize(max_revenue = max(revenue_omu))
  
  # Aggregate edge data to count the number of company contacts and beneficial owners,
  # then filter companies with more than one of each type
  mc3_edges2_agg <- mc3_edges2 %>%
    group_by(source) %>%
    summarize(
      num_company_contacts = sum(type == 'Company Contacts', na.rm = TRUE),
      num_beneficial_owner = sum(type == 'Beneficial Owner', na.rm = TRUE)
    ) %>%
    filter(num_company_contacts > 1, num_beneficial_owner > 1) %>%
    ungroup()
  
  # Left join aggregated edge data with the aggregated node data to get final dataset
  mc3_edges2_agg1 <- left_join(mc3_edges2_agg, mc3_nodes2_agg2, by = c("source" = "id")) %>%
    select(source, num_company_contacts, num_beneficial_owner, max_revenue) %>%
    mutate_all(~ replace(., is.na(.), 0))
  
  # Filter out specific company (if required)
  mc3_edges2_agg1 <- mc3_edges2_agg1 %>%
    filter(source != 'Mar de la Luna LLC' &
             source != 'Jones LLC' &
             source != 'Smith PLC' &
             source != 'Wright and Sons' )
  
  # Render a DataTable for displaying aggregated data
  output$sus_company_table <- DT::renderDataTable({
    datatable(mc3_edges2_agg1,
              colnames = c("Company", "Number of Company Contacts", "Number of Beneficial Owners", "Max Revenue"),
              class = "compact")
  })
  
  # Render a Plotly plot to compare max revenue vs. number of important people
  output$sus_company_graph <- renderPlotly({
    if (input$num_type == "num_beneficial_owner+num_company_contacts") {
      x_cols <- mc3_edges2_agg1$num_beneficial_owner + mc3_edges2_agg1$num_company_contacts
    } else if (input$num_type == "num_beneficial_owner") {
      x_cols <- mc3_edges2_agg1$num_beneficial_owner
    } else if (input$num_type == "num_company_contacts") {
      x_cols <- mc3_edges2_agg1$num_company_contacts
    }
    plot_ly(mc3_edges2_agg1,
            x = x_cols,
            y = ~max_revenue,
            text = ~paste(source, "<br>"
                          ,"Num Com Contacts:", num_company_contacts, "<br>"
                          ,"Num Ben Owners:", num_beneficial_owner, "<br>"
                          ,"Max Registered Revenue", round(max_revenue/1000, digits = 0), "k"),
            type = 'scatter',
            mode = 'markers'
    ) %>% layout(title = list(text = 'Max Revenue vs Number of Important People', y = 0.99),
                 xaxis = list(title = input$num_type, title_standoff = 25, range = c(0, 50)),
                 yaxis = list(title = 'Max Revenue')
    )
  })
  
  
  #####
  # Q1: Companies registered in more than a certain number of countries
  
  # Reactive function to filter companies based on the threshold input value
  filter_mask <- reactive({
    mc3_nodes2_agg <- mc3_nodes2 %>%
      filter(type == "Company") %>%
      select(id, country) %>%
      distinct() %>%
      group_by(id) %>%
      summarise(Weight = n()) %>%
      filter(Weight >= input$q1_threshold) %>%
      ungroup()%>%
      arrange(desc(Weight))
    
    return(mc3_nodes2_agg)
  })
  
  # Reactive function to get filtered data based on the above filter
  filtered_data <- reactive({
    mc3_nodes2_mt3 <- mc3_nodes2[mc3_nodes2$id %in% filter_mask()$id, c("id", "country", "type")] %>%
      filter(type == "Company") %>%
      select(id, country)
    mc3_nodes2_mt3$id <- gsub("'", "", mc3_nodes2_mt3$id)
    
    mc3_nodes2_mt3$tooltip <- paste0("Company = ", mc3_nodes2_mt3$id)
    return(mc3_nodes2_mt3)
  })
  
  # Reactive function to generate a ggplot with filtered data
  q1_p <- reactive({
    gg_bar <- ggplot(data = filter_mask(), 
                     aes(x = reorder(id, -Weight),
                         y = Weight,
                         tooltip = paste0("Company: ", id, "<br>Weight: ", Weight))
    ) +
      geom_bar(stat = "identity", 
               fill = "light blue", 
               color = "black"
      ) +
      geom_text(aes(label = Weight), 
                vjust = -0.5, 
                size = 3,
                color = "black"
      ) +
      theme_minimal() +
      labs(title = "Companies registered in multiple countries",
           x = "Company",
           y = "Weight") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
            axis.text.y = element_text(size = 9),
            plot.margin = margin(0, 0, 0, 1, "cm") # Adjust the left margin to 4cm
      ) +
      scale_y_continuous(breaks= pretty_breaks(), limits = c(0, 11))
    return(gg_bar)
  })
  
  # Render a Girafe plot using the above ggplot
  output$company_mt2_ct_graph <- renderGirafe({
    girafe(ggobj = q1_p(),
           width_svg = 8,
           height_svg = 8 * 0.47)
  })
  
  # Render a DataTable for displaying filtered company data
  output$company_mt2_table <- DT::renderDataTable({
    datatable(filtered_data(), class = "compact")
  })
  
}

shinyApp(ui, server)