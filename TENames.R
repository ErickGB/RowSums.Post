# --------------------------------------------------------------------------
# Created by: Erick Gordon
# Date: 31-Oct-2016
# Modified: 31-Oct-2016
# Description: Baby names in Panama since 2000 to 2016
# --------------------------------------------------------------------------
# Load libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
# --------------------------------------------------------------------------
# thema basico, con fondo blanco pero con líneas divisorias grises, sin legenda
theme._basic <- theme_bw() + 
  theme(panel.border=element_blank()) + 
  theme(axis.line=element_line(colour = "black")) + 
  theme(axis.title.y=element_text(size = 12)) + #, family="san-serif" 
  theme(legend.position = "none") + 
  theme(axis.text.x=element_text(size=13)) + 
  theme(axis.text.y=element_text(size=13)) 
# --------------------------------------------------------------------------
# consumer <- read.csv("./Data/Consumer_Complaints.csv", header = TRUE, stringsAsFactors = FALSE)
# head(consumer)
# Lodad data
names <- read.csv("./Data/PanNamesUTF_8.csv", header = FALSE, sep = ",",  encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(names) <- c("firstname", "lastname", "year", "sex", "count")

x <- names$firstname
Encoding(x) <- "latin1"
x

#x <- c("Ekstr\xf8m", "J\xf6reskog", "bi\xdfchen Z\xfcrcher")
#Encoding(x) <- "latin1"
#x


names$firstname <- ifelse(toupper(names$firstname)==toupper("JosŽ"), toupper("José"), toupper(names$firstname))
names$firstname <- ifelse(toupper(names$firstname)==toupper("Mar’a"), toupper("María"), toupper(names$firstname))
names$firstname <- ifelse(names$firstname=="Mar’a", as.character("María"), names$firstname)
names$firstname <- ifelse(names$firstname=="SOF’A", as.character("SOFÍA"), names$firstname)
names$firstname <- ifelse(names$firstname=="INŽS", as.character("INÉS"), names$firstname)
names$firstname <- ifelse(names$firstname=="JOSUŽ", as.character("JOSUÉ"), names$firstname)

names$firstname <- ifelse(names$firstname=="ANDRŽS", as.character("ANDRÉS"), names$firstname)
names$firstname <- ifelse(names$firstname=="JEFTŽ", as.character("JEFTÉ"), names$firstname)
names$firstname <- ifelse(names$firstname=="ADRI‡N", as.character("ADRIÁN"), names$firstname)
names$firstname <- ifelse(names$firstname=="SIANNŽ", as.character("SIANNÉ"), names$firstname)
names$firstname <- ifelse(names$firstname=="MOISŽS", as.character("MOISÉS"), names$firstname)
names$firstname <- ifelse(names$firstname=="MAIRŽ", as.character("MAIRÉ"), names$firstname)
names$firstname <- ifelse(names$firstname=="LUC’A", as.character("LUCÍA"), names$firstname)

names$lastname <- ifelse(toupper(names$lastname)==toupper("JosŽ"), toupper("José"), toupper(names$lastname))
names$lastname <- ifelse(toupper(names$lastname)==toupper("Mar’a"), toupper("María"), toupper(names$lastname))
names$lastname <- ifelse(names$lastname=="Mar’a", as.character("María"), names$lastname)
names$lastname <- ifelse(names$lastname=="SOF’A", as.character("SOFÍA"), names$lastname)
names$lastname <- ifelse(names$lastname=="INŽS", as.character("INÉS"), names$lastname)
names$lastname <- ifelse(names$lastname=="JOSUŽ", as.character("JOSUÉ"), names$lastname)

names$lastname <- ifelse(names$lastname=="ANDRŽS", as.character("ANDRÉS"), names$lastname)
names$lastname <- ifelse(names$lastname=="JEFTŽ", as.character("JEFTÉ"), names$lastname)
names$lastname <- ifelse(names$lastname=="ADRI‡N", as.character("ADRIÁN"), names$lastname)
names$lastname <- ifelse(names$lastname=="SIANNŽ", as.character("SIANNÉ"), names$lastname)
names$lastname <- ifelse(names$lastname=="MOISŽS", as.character("MOISÉS"), names$lastname)
names$lastname <- ifelse(names$lastname=="MAIRŽ", as.character("MAIRÉ"), names$lastname)
names$lastname <- ifelse(names$lastname=="LUC’A", as.character("LUCÍA"), names$lastname)
names$lastname <- ifelse(names$lastname=="", "-", names$lastname)

only.FirstName <- names %>%
  mutate(firstname=toupper(firstname)) %>%
  filter(year==2015) %>%
  group_by(firstname) %>%
  summarize(Total=sum(count))
rownames(only.FirstName) <- NULL

temp <- names %>%
  filter(year==2015) %>% 
  mutate(firstname=toupper(firstname), lastname=toupper(lastname)) 
rownames(temp) <- NULL

only.FirstName %>%
  filter(firstname=="ANDREW")

head(only.FirstName)
head(temp)
write.csv(only.FirstName, "./Data/names2016.csv")
write.csv(temp, "./Data/namesComplete2016.csv")
rm(temp, only.FirstName)

colnames(names) <- c("first.name", "second.name", "year", "sex", "count")
head(names)

# --------------------------------------------------------------------------
# Exploratory analisys

# total names
nrow(names) # 601,904 panamenians TE registered since Jan 2000 to Sep 2016
table(names$sex) # F - 345173 ; M - 256730; 345173/256730 = 1.344 

names %>% 
  group_by(sex) %>% 
  summarize(Total=n())


names %>% 
  group_by(sex) %>% 
  summarize(Total=sum(count))

total.female <- 615533
total.male <- 644832
  
total.female / (total.female + total.male) # F
total.male / (total.female + total.male) # M

## First Name
# Top 10 baby female names 
top.10.female <- names %>%
  filter(sex=="F") %>%
  group_by(first.name) %>%
  summarize(Total=sum(count)) %>%
  arrange(desc(Total)) %>% head (10) 
top.10.female$first.name <- as.character(top.10.female$first.name)
top.10.female$Percentaje <- round( (total.female / top.10.female$Total), digits=2) 
write.csv(top.10.female, paste0("./Data/femaleTotal.csv"))
top.10.female

top.10.female$first.name <- factor(top.10.female$first.name, 
                                   c("Alexandra","Genesis", "Emily", "Gabriela", "Daniela", "Andrea", 
                                     "Milagros", "María", "Maria", "Ana"),   ordered = TRUE)

top.10.female %>%
  mutate(Total=round(Total/1000, digits=1)) %>%
  ggplot(aes(x=(first.name), y=(Total))) +  
  geom_bar(stat = "identity") + #geom_text(aes(label=Total), vjust=1.5, colour="white") + 
  coord_flip() + 
  labs(y=NULL, x="\n", title="Top 10 nombres de niñas más populares") + 
  theme._basic


# ----------------------------
# Top 10 baby males names 
top.10.male <- names %>%
    filter(sex=="M") %>%
  group_by(first.name) %>%
    summarize(Total=sum(count)) %>%
    arrange(desc(Total)) %>% head (10) 
top.10.male$first.name <- as.character(top.10.male$first.name)
top.10.male$Percentaje <- round( (total.male / top.10.male$Total), digits=2) 
write.csv(top.10.male, paste0("./Data/maleTotal.csv"))
top.10.male

top.10.male$first.name <- factor(top.10.male$first.name, 
                                   c("Kevin","David", "Jorge", "Daniel", "Angel", "Jose", 
                                     "José", "Juan", "Carlos", "Luis"),   ordered = TRUE)

top.10.male %>%
  mutate(Total=round(Total/1000, digits=1)) %>%
  ggplot(aes(x=(first.name), y=(Total))) +  
  geom_bar(stat = "identity") + #geom_text(aes(label=Total), vjust=1.5, colour="white") + 
  coord_flip() + 
  labs(y=NULL, x="\n", title="Top 10 nombres de niños más populares") + 
  theme._basic


# ----------------------------
# Cantidad de nombres diferentes por sexo por año

male <- names %>%
  filter(sex=="M" & year < 2016) %>%
  group_by(first.name, sex, year) %>%
  summarize(Total=n()) %>%
  arrange(desc(Total))

male <- male %>%
  group_by(sex, year) %>%
  summarize(Total=sum(Total)) %>%
  arrange(desc(year))

# female
female <- names %>%
  filter(sex=="F" & year < 2016) %>%
  group_by(first.name, sex, year) %>%
  summarize(Total=n()) %>%
  arrange(desc(Total))

female <- female %>%
  group_by(sex, year) %>%
  summarize(Total=sum(Total)) %>%
  arrange(desc(year))

# bind
total <- rbind(male, female)
write.csv(total, "./Data/total.csv")

# adultos e inmaduros
my.colors <- c("#969696","#525252") 
cortes <- seq(2000, 2015, by=1)
total %>%
  mutate(Total=(Total/1000)) %>%
  ggplot(aes(x=year, y=Total, group=sex, colour=sex)) + 
  geom_line(size=0.90) + 
  scale_x_continuous(breaks=cortes) + 
  scale_color_manual(values=my.colors) +
  labs(y=NULL, x="\n", title="Nombes diferentes de niños y niñas por año\n") + 
  theme_bw() + 
  theme(panel.border=element_blank()) + 
  theme(axis.line=element_line(colour = "black")) + 
  theme(axis.title.y=element_text(size = 12)) + #, family="san-serif" 
  theme(axis.text.x=element_text(size=13)) + 
  theme(axis.text.y=element_text(size=13)) 

# -----------------------------  
# getTop3.byYear("F", 2015)
getTop3.byYear <- function(p.sex, p.year) {
  top3 <- names %>%
    filter(sex==p.sex & year == p.year) %>%
    group_by(first.name, year) %>%
    summarize(Total=sum(count)) %>% 
    arrange(desc(Total)) %>% head(3)  
  
  top3$no <- seq(1, 3, 1)
  top3$sex <- p.sex
  return (top3)
}

df.male <- data.frame()
df.female <- data.frame()
for(i in 2000:2015) {
  temp.male <- getTop3.byYear("M", i)
  temp.femmale <- getTop3.byYear("F", i)
  
  if(i==2000)
  {
    df.male <- temp.male
    df.female <- temp.femmale
  } else {
    df.male <- rbind(df.male, temp.male)
    df.female <- rbind(df.female, temp.femmale)
  }
}
df.female$leyenda <- paste(df.female$first.name, ":", df.female$Total)
df.male$leyenda <- paste(df.male$first.name, ":", df.male$Total)
df.female$first.name <- NULL
df.female$Total <- NULL
df.male$first.name <- NULL
df.male$Total <- NULL

df.male <- data.frame(df.male)
df.female <- data.frame(df.female)

df.Total <- rbind(df.male, df.female)

df.Total <- spread(df.Total, no, leyenda)
colnames(df.Total) <- c("year", "sex", "Top.1", "Top.2", "Top.3")
df.Total$key <- paste0(df.Total$year, df.Total$sex)
total$key <- paste0(total$year, total$sex)

totalv2 <- inner_join(total, df.Total, by="key") 
total <- totalv2
totalv2$sex.x <- NULL
totalv2$year.x <- NULL
totalv2$key <- NULL
colnames(totalv2) <- c("Total", "year", "sex", "Top.1", "Top.2", "Top.3")
totalv2
totalv2$Tops <- paste0(totalv2$Top.1, "; ", totalv2$Top.2, "; ", totalv2$Top.3)
write.csv(totalv2, "./Data/final.csv", fileEncoding="UTF-8")

my.colors <- c("#969696","#525252") 
cortes <- seq(2000, 2015, by=1)
g <- totalv2 %>%
  mutate(Total=(Total/1000)) %>%
  ggplot(aes(x=year, y=Total, group=sex, colour=sex)) + 
  geom_line(size=0.90) + 
  scale_x_continuous(breaks=cortes) + 
  scale_color_manual(values=my.colors) +
  labs(y=NULL, x="\n", title="Nombes diferentes de niños y niñas por año\n") + 
  theme_bw() + 
  theme(panel.border=element_blank()) + 
  theme(axis.line=element_line(colour = "black")) + 
  theme(axis.title.y=element_text(size = 12)) + #, family="san-serif" 
  theme(axis.text.x=element_text(size=13)) + 
  theme(axis.text.y=element_text(size=13)) 
ggplotly(g)

# ----------------------------------------------------------------------
# create word cloud 
# ----------------------------------------------------------------------
library(tm)
library(wordcloud)

# 1) Create a corpus using the Tweet variable
word.cloud <- names %>%
  group_by(first.name, sex) %>%
  summarize(Total=sum(count)) %>%
  arrange(desc(Total)) %>% head(1000)
word.cloud$first.name <- as.character(word.cloud$first.name)

# What is the most common word across all the tweets 
wordcloud(word.cloud$first.name, word.cloud$Total,
          scale=c(5,0.5),min.freq=1999,colors=brewer.pal(9, "Greys"))

# ----------------------------------------------------------------------
# -----------------------------  
## Second Names
# Top 10 baby female names 
top.10.female <- names %>%
  filter(sex=="F") %>%
  group_by(second.name) %>%
  summarize(Total=n()) %>%
  arrange(desc(Total)) %>% head (10) 

# Top 10 baby males names 
top.10.male <- names %>%
  filter(sex=="M") %>%
  group_by(second.name) %>%
  summarize(Total=n()) %>%
  arrange(desc(Total)) %>% head (10) 

## Second Names
# Top 10 baby female names 
top.10.female <- names %>%
  filter(sex=="F") %>%
  group_by(second.name) %>%
  summarize(Total=n()) %>%
  arrange(desc(Total)) %>% head (10) 

## Complete Names
# Male
top.10.male <- names %>%
  filter(sex=="M" & (!(second.name=="")) ) %>%
  mutate(complete=paste0(first.name, ", ", second.name)) %>%
  group_by(complete) %>%
  summarize(Total=n()) %>%
  arrange(desc(Total)) %>% head (10) 
# Female
top.10.male <- names %>%
  filter(sex=="F" & (!(second.name=="")) ) %>%
  mutate(complete=paste0(first.name, ", ", second.name)) %>%
  group_by(complete) %>%
  summarize(Total=n()) %>%
  arrange(desc(Total)) %>% head (10) 


## hombres y mujeres por año
g <- names %>%
  filter(year < 2016) %>%
  group_by(year, sex) %>%
  summarize(total=sum(count)) %>%
  ggplot(aes(x=factor(year), y=total, 
                     group=sex, color=sex)) + geom_line()
ggplotly(g)


r <- names %>%
  # filter(sex==p.sex & year == p.year) %>%
  group_by(first.name) %>%
  summarize(Total=sum(count)) %>% 
  arrange((Total)) %>% head(500)  
View(r)


# -------------------------------------------
library(RNeo4j)
library(visNetwork)

graph <- startGraph("http://localhost:7474/db/data/", username="neo4j", password="Nisgua")

# return nodes 
node_query = "
MATCH (n)
RETURN n.name AS id,
n.name AS label,
LABELS(n)[0] AS group
"
# return edges 
edge_query = "
MATCH (f:FirstName)-[r:WITH]->(m) 
WHERE f.name IN ['JUAN','LUIS'] AND r.count > 10  
RETURN f.name AS from,
m.name AS to,
TYPE(r) AS label, r.count AS cantidad
"

nodes.n <- cypher(graph, node_query)
edges.n <- cypher(graph, edge_query)

nodes.n <- nodes.n %>% 
  filter( (id %in% c("JUAN", "LUIS") & group=="FirstName") | (group=="LastName" & id %in% edges.n$to) ) 

edges.n$label <- paste0(edges.n$label, ": ", edges.n$cantidad)
nodes.n$title <- paste0(nodes.n$label)

head(nodes.n)
head(edges.n)

visNetwork(nodes.n, edges.n) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
  visGroups(groupname = "FirstName", color = "#999999") %>%
  visLayout(randomSeed = 123)  %>%
  visGroups(groupname = "LastName", color = "#67a9cf") %>%
  visLegend(position = "right", main = "Group")





