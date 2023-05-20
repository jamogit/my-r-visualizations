library(spotifyr)
library(ggplot2)
library(dplyr)
library(purrr)
library(knitr)
library(lubridate)
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)



# Get data from Spotify ----
access_token <- get_spotify_access_token()

# Suomen Top 50 - 37i9dQZEVXbMxcczTSoGwZ
# Belgian Top 50 - 37i9dQZEVXbJNSeeHswcKB
# Saksan Top 50 - 37i9dQZEVXbJiZcmkrIHGU
# Intian Top 50 - 37i9dQZEVXbLZ52XmnySJg


playlist_username <- 'spotify'
playlist_uris <- c('37i9dQZEVXbMxcczTSoGwZ', '37i9dQZEVXbJNSeeHswcKB', '37i9dQZEVXbJiZcmkrIHGU', '37i9dQZEVXbLZ52XmnySJg?si=b0a41d3a7ab848a4')
playlist_audio_features <- get_playlist_audio_features(playlist_username, playlist_uris)

df <- playlist_audio_features |> 
  transmute(
    playlist_name,
    Title = paste(map_chr(track.artists, function(x) x$name[1]),
                  "-", track.name),
    track.popularity
  ) |> distinct()

# create a data frame giving the hierarchical structure of your individuals
set.seed(1234)
d1 <- data.frame(from= df$, to=paste("group", seq(1,10), sep=""))
d2 <- data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
edges <- rbind(d1, d2)
edges <- df |> select(from = playlist_name, to = Title) |> distinct()

# create a dataframe with connection between leaves (individuals)
# all_leaves <- paste("subgroup", seq(1,100), sep="_")
all_leaves <- df |> select(Title) |> distinct()

# connect <- rbind( 
#   data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), 
#   data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), 
#   data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), 
#   data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) )
# connect$value <- runif(nrow(connect))
connect <- df |> 
  select(
    from = playlist_name,
    to = Title,
    value = track.popularity
  )




# create a vertices data.frame. One line per object of our hierarchy
# vertices  <-  data.frame(
#   name = unique(c(as.character(edges$from), as.character(edges$to))) , 
#   value = runif(111)
# ) 
vertices <- connect |> 
  select(
    name = unique(c(as.character(connect$from), as.character(connect$to))),
    value
  )
# Let's add a column with the group of each name. It will be useful later to color points
vertices$group  <-  edges$from[ match( vertices$name, edges$to ) ]


#Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the labels
vertices$id <- NA
myleaves <- which(is.na( match(vertices$name, edges$from) ))
nleaves <- length(myleaves)
vertices$id[ myleaves ] <- seq(1:nleaves)
vertices$angle <- 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust <- ifelse( vertices$angle < -90, 1, 0)

# flip angle BY to make them readable
vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

mygraph <- igraph::graph_from_data_frame( edges, vertices=vertices )

# The connection object must refer to the ids of the leaves:
from  <-  match( connect$from, vertices$name)
to  <-  match( connect$to, vertices$name)

ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", width=0.9) +
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust), size=1.5, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))


library(arcdiagram)


# create a star graph with 10 nodes
star_graph = graph.star( nrow(all_leaves), mode="out")

# extract edgelist
star_edges = get.edgelist(star_graph)

# inspect star_edges
star_edges

# plot 1: default arc diagram
arcplot(star_edges)
