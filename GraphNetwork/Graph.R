library('tidyverse')
library('ggraph')
library('tidygraph')


# 1 Data preparation ----
df <- read_csv2('Nodedata.csv')

graph <- as_tbl_graph(df) %>%
  mutate(Popularity = centrality_degree(mode = 'in'))

# 2 Visual ----

ggraph(graph, layout = 'kk') +
  geom_edge_fan(aes(alpha=stat(index)), show.legend = F) + 
  geom_node_point()

View(highschool)

# Create graph of highschool friendships
graph <- as_tbl_graph(highschool) %>% 
  mutate(Popularity = centrality_degree(mode = 'in'))

# plot using ggraph
ggraph(graph, layout = 'kk') + 
  geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) + 
  geom_node_point(aes(size = Popularity)) + 
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
