library(RNeo4j)
library(visNetwork)

graph <- startGraph("http://localhost:7474/db/data/", username="neo4j", password="test01")
clear(graph)

nicole = createNode(graph, "Person", name="Nicole")
greta = createNode(graph, "Person", name="Greta")
kenny = createNode(graph, "Person", name="Kenny")
adam = createNode(graph, "Person", name="Adam")

neo4j = createNode(graph, "Company", name="Neo4j")
digital = createNode(graph, "Company", name="Digital Insights")
docker = createNode(graph, "Company", name="Docker")

createRel(nicole, "WORKS_FOR", neo4j)
createRel(greta, "WORKS_FOR", neo4j)
createRel(kenny, "WORKS_FOR", digital)
createRel(adam, "WORKS_FOR", docker)

createRel(greta, "KNOWS", adam)
createRel(nicole, "KNOWS", kenny)
createRel(kenny, "KNOWS", adam)

node_query = "
MATCH (n)
RETURN n.name AS id,
n.name AS label,
LABELS(n)[0] AS group
"

edge_query = "
MATCH (n)-[r]->(m)
RETURN n.name AS from,
m.name AS to,
TYPE(r) AS label
"

nodes <- cypher(graph, node_query)
edges <- cypher(graph, edge_query)

head(nodes)
head(edges)

visNetwork(nodes, edges)

# Nodes can also be sized by a value:
neo4j = updateProp(neo4j, employees=1)
digital = updateProp(digital, employees=2)
docker = updateProp(docker, employees=3)

node_query = "
MATCH (n)
RETURN n.name AS id,
n.name AS label,
LABELS(n)[0] AS group,
n.employees AS value
"
nodes = cypher(graph, node_query)
nodes[is.na(nodes)] = 1 # Person no have "value", assign value 1 a items with value = NULL

nodes

visNetwork(nodes, edges)

# --------------------------------------------------
# Use sample database "movies"

importSample(graph, "movies", input=F)

LIMIT = 30

node_query = "
MATCH (n)
WITH n, RAND() AS random
ORDER BY random
LIMIT {limit}
RETURN ID(n) AS id, 
       COALESCE(n.name, n.title) AS label,
       LABELS(n)[0] AS group
"

nodes = cypher(graph, node_query, limit=LIMIT)
head(nodes)

nrow(nodes)

edge_query = "
MATCH (n)-[r]->(m)
WHERE ID(n) IN {ids} AND ID(m) IN {ids}
RETURN ID(n) AS from,
ID(m) AS to,
TYPE(r) AS label
"
edges <- cypher(graph, edge_query, ids=nodes$id)
head(edges)

visNetwork(nodes, edges)

nodes$connected <- nodes$id %in% c(edges$from, edges$to)
nodes <- nodes[nodes$connected, ]

visNetwork(nodes, edges)

# --------------------

nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
edges <- data.frame(from = c(1,2), to = c(2,3))
visNetwork(nodes, edges) %>%
  visGroups(groupname = "A", shape = "icon", icon = list(code = "f0c0", size = 75)) %>%
  visGroups(groupname = "B", shape = "icon", icon = list(code = "f007", color = "red")) %>% addFontAwesome()
nodes <- data.frame(id = 1:3)
edges <- data.frame(from = c(1,2), to = c(1,3))
visNetwork(nodes, edges) %>%
  visNodes(shape = "icon", icon = list( face ='FontAwesome', code = "f0c0")) %>%
  addFontAwesome()


nb <- 10
nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb),
                    group = sample(LETTERS[1:3], nb, replace = TRUE), value = 1:nb,
                    title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), stringsAsFactors = FALSE)

edges <- data.frame(from = c(8,2,7,6,1,8,9,4,6,2),
                    to = c(3,7,2,7,9,1,5,3,2,9),
                    value = rnorm(nb, 10), label = paste("Edge", 1:nb),
                    title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))

visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
  visOptions(highlightNearest = TRUE) %>%
  visLayout(randomSeed = 123)

# ----------------------
library(networkD3)
data(MisLinks, MisNodes)
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4)

# ----------------------
library(DiagrammeR)
grViz("
  digraph {
    layout = twopi
    node [shape = circle]
    A -> {B C D} 
  }")
