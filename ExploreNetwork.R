# Explore network properties of the city council ballots

source('DataSources.R')

candidates = candidates()
ballots = all_ballots() %>% 
  mutate(second = map_chr(choices, 2, .default=NA)) %>% 
  filter(!is.na(first), first %in% candidates$code,
         !is.na(second), second %in% candidates$code)

lookup = deframe(candidates[, c(1, 3)])

top10 = ballots %>% 
  #mutate(name=lookup[first]) %>% 
  count(first, sort=TRUE) %>% 
  top_n(10, n) %>% 
  pull(first)

edges = ballots %>% 
  #filter(first %in% top10, second %in% top10) %>% 
  select(first, second) %>% 
  group_by(first, second) %>% 
  summarize(weight=n()) %>% 
  filter(weight >=50)

vertices = candidates %>% 
  filter(code %in% edges$first || code %in% edges$second)

library(igraph)

ig = graph_from_data_frame(edges, vertices=vertices) %>% 
  add_layout_(with_kk())
plot(ig, 
     #layout = layout_with_kk, 
     edge.arrow.size = 0.5,
     edge.width=edges$weight/50,
     vertex.label=vertices$last_name)


# Make an undirected graph for clustering
net <- as.undirected(ig, mode= "collapse",
                    edge.attr.comb=list(weight="sum", "ignore"))
ceb = cluster_edge_betweenness(ig)
clp <- cluster_label_prop(net)
cfg = cluster_fast_greedy(net)
plot(cfg, net, vertex.label=vertices$last_name)

cw = cluster_walktrap(ig)
plot(cw, ig, vertex.label=candidates$last_name)

library(visNetwork)
visNetwork(vertices %>%
             select(id=code, label=last_name),
           edges %>% 
             rename(from=first, to=second, value=weight),
           width=800, height=800) %>% 
  visIgraphLayout('layout_in_circle') %>% 
  visNodes(font=list(size=32)) %>% 
  visEdges(arrows=list(to=TRUE),
           color=list(highlight='red'))

           