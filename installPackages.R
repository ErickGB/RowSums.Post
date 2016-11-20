# install packages
# https://nicolewhite.github.io/2015/06/18/visualize-your-graph-with-rneo4j-and-visNetwork.html
install.packages('devtools')
install.packages("DiagrammeR")
install.packages("visNetwork")
install.packages("networkD3")
install.packages("RNeo4j")

# twitter
install.packages("twitteR")
install.packages("tm")
install.packages("sna")
install.packages("qdap")
install.packages("ROAuth")
install.packages("qdapRegex")
install.packages("topicmodels")
install.packages("cowplot")

# source("http://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
# install.packages('tm')
# install.packages('wordcloud')

# Execute in terminal before: xcode-select --install
download.file("http://cran.cnr.berkeley.edu/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz", "Rstem_0.4-1.tar.gz") 
install.packages("Rstem_0.4-1.tar.gz", repos=NULL, type="source")

download.file("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", "sentiment.tar.gz")
install.packages("sentiment.tar.gz", repos=NULL, type="source")

# graph 
install.packages("graph")
install.packages("Rgraphviz")

# json
install.packages("jsonlite")