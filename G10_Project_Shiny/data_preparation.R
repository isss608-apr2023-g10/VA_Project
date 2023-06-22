pacman::p_load(jsonlite, tidygraph, ggraph, visNetwork, graphlayouts, ggforce, skimr, tidytext, tidyverse, DT, ggiraph, tm, topicmodels, shiny, shinythemes, bslib, ggplot2)

json_file_path <- "data/MC3.json"
mc3_file_path <- "data/mc3.rds"

if (!file.exists(mc3_file_path)) {
  mc3_data <- fromJSON(json_file_path)
  saveRDS(mc3_data, mc3_file_path)
} else {
  mc3_data <- readRDS(mc3_file_path)
}

mc3_edges <- as_tibble(mc3_data$links) %>% 
  distinct() %>%
  mutate(source = as.character(source),
         target = as.character(target),
         type = as.character(type)) %>%
  group_by(source, target, type) %>%
  summarise(weights = n()) %>%
  filter(source!=target) %>%
  ungroup()

mc3_nodes <- as_tibble(mc3_data$nodes) %>%
  mutate(country = as.character(country),
         id = as.character(id),
         product_services = as.character(product_services),
         revenue_omu = as.numeric(as.character(revenue_omu)),
         type = as.character(type)) %>%
  select(id, country, type, revenue_omu, product_services)

# Change "character(0)" to "Unknown"
mc3_nodes$product_services <-
  ifelse(mc3_nodes$product_services == "character(0)",
         "Unknown",
         mc3_nodes$product_services)

# Exclude "Unknown" from topic modelling
filtered_mc3_nodes <- mc3_nodes %>%
  filter(product_services != "Unknown")

# Preprocessing
corpus <- Corpus(VectorSource(filtered_mc3_nodes$product_services))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)

# Text Transformation
dtm <- DocumentTermMatrix(corpus)

# Build LDA model
num_topics = 8
lda <- LDA(dtm, k = num_topics)

# Assigning topics as product_type
topics <- topics(lda, 1)  # Get the topic probabilities for each document
filtered_mc3_nodes$product_type <- topics

mc3_nodes <- left_join(mc3_nodes, filtered_mc3_nodes, by = names(mc3_nodes))
mc3_nodes$product_type <-
  ifelse(is.na(mc3_nodes$product_type),
         0,
         mc3_nodes$product_type)

mc3_edges_cleaned <- mc3_edges %>%
  filter(source %in% mc3_nodes$id)

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
                       directed = FALSE) %>%
  mutate(betweenness_centrality = centrality_betweenness(),
         degree_centrality = centrality_degree())

revenue_quartiles <- summary(mc3_nodes_combined$revenue_omu)
one <- revenue_quartiles[2]
two <- revenue_quartiles[3]
three <- revenue_quartiles[5]

saveRDS(revenue_quartiles, file = "data/revenue_quartiles.rds")
saveRDS(mc3_graph, file = "data/mc3_graph.rds")