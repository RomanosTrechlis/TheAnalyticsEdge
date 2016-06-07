setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit7 - Visualization")

# Problem 1.1 - Summarizing the Data
edges = read.csv("data\\edges.csv")
users = read.csv("data\\users.csv")
str(users)
2*146/59

# Problem 1.2 - Summarizing the Data
table(users$locale)

# Problem 1.3 - Summarizing the Data
table(users$locale, users$gender)

# Problem 2.1 - Creating a Network
library(igraph)
?graph.data.frame

# Problem 2.2 - Creating a Network
#' In this graph, there are a number of groups of nodes 
#' where all the nodes in each group are connected but the 
#' groups are disjoint from one another, forming "islands" 
#' in the graph. Such groups are called "connected components," 
#' or "components" for short.
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)

# Problem 2.3 - Creating a Network
#' In our graph, the "degree" of a node is its number of friends
degree(g) 

# Problem 2.4 - Creating a Network
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
max(V(g)$size)
min(V(g)$size)

# Problem 3.1 - Coloring Vertices
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

# Problem 3.2 - Coloring Vertices
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

# Problem 3.3 - Coloring Vertices
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

# Problem 4 - Other Plotting Options
?igraph.plotting
plot(g, vertex.label=NA, edge.width = 2)
