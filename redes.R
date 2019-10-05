library(dplyr)
library(readr)
library(stringr)
library(tidytext)
library(economiccomplexity)
library(igraph)
library(ggraph)
library(purrr)

tweets_latinr <- read_csv("tweets_latinr.csv")

mentions <- tweets_latinr %>% 
  select(user = nombre_usuario, text = tweet) %>% 
  unnest_tokens("words", "text", "tweets") %>% 
  filter(str_detect(words, "^@")) %>% 
  mutate(from_name = user,
         to_name = str_remove(words, "@")) %>% 
  count(from_name, to_name) %>% 
  arrange(desc(n)) %>% 
  group_by(from_name, to_name) %>% 
  summarise(n = sum(n)) %>% 
  ungroup()

names(mentions) <- c("country","product","value")

rca_tbl <- ec_rca(
  data = mentions,
  tbl = T
)

cm_fitness <- ec_complexity_measures(
  rca = rca_tbl,
  method = "fitness",
  tbl = T
)

pro <- ec_proximity(
  rca = rca_tbl,
  d = cm_fitness$diversity,
  u = cm_fitness$ubiquity,
  tbl = T
)

set.seed(200100)

crear_red <- function(corte_proximidad = 0.4) {
  net <- ec_networks(
    pc = pro$proximity_c,
    pp = pro$proximity_p,
    cutoff_p = corte_proximidad,
    tbl = T,
    compute = "product"
  )
  
  g <- net$network_p %>%
    graph_from_data_frame(directed = F)
  
  E(g)$weight <- 0.05
  
  g <- g %>%
    ggraph(layout = "fr") +
    geom_edge_link(edge_colour = "#d3d3d3") +
    geom_node_point(color = "darkslategray4", size = 4) +
    geom_node_text(aes(label = name), vjust = 2.2, size = 3) +
    ggtitle(sprintf("Grafo de interacciones en Twitter durante LatinR (Filtro: proximidad \u2265 %s)", corte_proximidad)) +
    theme_void()
  
  if (corte_proximidad == 1) {
    ggsave(sprintf("red_latinr_2019_proximidad_%s.png", 10 * corte_proximidad), width = 16, height = 9)
  } else {
    ggsave(sprintf("red_latinr_2019_proximidad_0%s.png", 10 * corte_proximidad), width = 16, height = 9)
  }
}

map(seq(1:10) / 10, crear_red)
