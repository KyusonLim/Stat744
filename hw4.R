# Title: Understanding overall trend in COVID-19 by data mining for both qualitative and quantitative data
# Part 1. Modelling in Text mining for LDA(latent dirichlet distribution) and wordcloud
# Part 2. Modelling using shiny for COVID-19 death for provinces

rm(list=ls())

## load the packages that are installed to help you run the coding
library(rvest)
library(dplyr)
library(devtools)
library(wordcloud2)
library(plyr)
library(tokenizers)
library(lda)
library(LDAvis)
library(tm)

library(shiny)
library(leaflet)
library(RColorBrewer)
library(rvest)
library(RColorBrewer)
library(sp)     
library(leaflet.extras)      
library(raster)      
library(rgdal) 


# this is the website, you are setting the website as a variable
nur1<-"https://www150.statcan.gc.ca/n1/pub/45-28-0001/452800012020001-eng.htm"


# Now, you are crawling the website, and 'lin' gives you list of headlines.
lin<-NULL
pa0<-read_html(nur1, encoding="UTF-8")
lin<-c(lin, pa0%>%
         html_nodes('.container')%>%
         html_nodes('.col-md-12')%>%
         html_nodes('.list-group')%>%
         html_nodes('.list-group-item')%>%
         html_nodes('.mrgn-tp-0')%>%
         html_text())
pa0<-NULL
lin		### there would be 90 headlines, all printed


## this is wordcloud visualization for top 50 words
wordcloud2(w7,size=2.5,color=c('skyblue'),gridSize=20,shape='circle',ellipticity=0.42)



# pre-processing:
lin <- gsub("'", "", lin); lin <- gsub("[[:punct:]]", " ", lin) 
lin <- gsub("[[:cntrl:]]", " ", lin); lin <- gsub("^[[:space:]]+", "", lin)
lin <- gsub("[[:space:]]+$", "", lin); lin <- tolower(lin)  # force to lowercase

# tokenize on space and output as a list:
doc.list <- strsplit(lin, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stopwords("english") | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)



# Compute some statistics related to the data set:
D <- length(documents)  # number of documents
W <- length(vocab)  # number of terms in the vocab 
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
N <- sum(doc.length)  # total number of tokens 
term.frequency <- as.integer(term.table)  # frequencies of terms 


## some self-filering 
w7<-as.data.frame(term.table)[-1,]
## this is wordcloud visualization 
wordcloud2(w7,size=2.5,color=c('skyblue'),gridSize=20,shape='circle',ellipticity=0.42)



# MCMC and model tuning parameters:
K <- 10;G <- 5000;alpha <- 0.02;eta <- 0.02

# Fit the model:
set.seed(744)
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
        num.iterations = G, alpha = alpha, eta = eta, initial = NULL, burnin = 0, compute.log.likelihood = TRUE)


# Visualizing the fitted model with LDAvis
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

lin2 <- list(phi = phi, theta = theta, doc.length = doc.length,
                     vocab = vocab,term.frequency = term.frequency)


# create the JSON object to feed the visualization:
json <- createJSON(phi = lin2$phi, theta = lin2$theta, doc.length = lin2$doc.length, 
                   vocab = lin2$vocab, term.frequency = lin2$term.frequency)

## LDA 'NLP' visualization
serVis(json, out.dir = 'vis', open.browser = T)




# This shows from the statistics Canada headlines for qualitative point of view how we are to understand the issues of COVID-19 in Canada.
# The topic modelling conveys popular topics of 10 keywords with explained sub-key words for each of the topics. 
# Also, the wordcloud conveys from frequency based visualization for intuitive view to readers for simple and explicit words counts from the headlines. 
# From both graphs, we are to observe the ideas and issues from official website for COVID-19 about what keywords and important ideas people have in past year 2020.
# However, with shiny interactive app from official statistics Canada for how many COVID-19 people dies we could understand quantitively for the effect of COVID-19 by the data mining models and techniques. 



# Mining in Geospatial analysis
# fetch data
nur1<-"https://www.covid-19canada.com/"


# Now, you are crawling the website
lin<-NULL
pa0<-read_html(nur1, encoding="UTF-8")
lin<- pa0 %>%
  html_nodes("table")%>%
  html_table(fill = TRUE)


## extract table, conversion for analysis
tab<-as.data.frame(lin[[2]][2:11,])


# function for change
to_numeric<-function(da){
  dd<-rownames(da)
  da %>%
    readr::parse_number(as.character())
}

tab[,2]<-to_numeric(tab[,2]);tab[,3]<-to_numeric(tab[,3])
tab[,4]<-to_numeric(tab[,4]);tab$`Total Deaths`<-to_numeric(tab$`Total Deaths`)


# lat, long
nur2<-"https://www.latlong.net/category/provinces-40-60.html"


# Now, you are crawling the website
lin2<-NULL
pa0<-read_html(nur2, encoding="UTF-8")
lin2<- pa0 %>%
  html_nodes("table")%>%
  html_table(fill = TRUE)
tabl<-lin2[[1]][-9,]


## short name
tabl$`Place Name`<-c('NL', 'SK', 'PEI', 'ON', 'NS', 'AB', 'BC', 'MB', 'NB', 'QC')

## combination
colnames(tabl)<-c('Prov.','lat', 'lng')
fin <- merge(tabl, tab, by=c('Prov.'))



# shiny
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Total Death", min(fin$`Total Deaths`), 11500,
                            value = range(fin$`Total Deaths`), step = 500),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
                checkboxInput("legend", "Show legend", TRUE)))

server <- function(input, output, session) {
  filteredData <- reactive({
    fin[fin$`Total Deaths` >= input$range[1] & fin$`Total Deaths` <= input$range[2],]})
  
  # This reactive expression represents the palette function
  colorpal <- reactive({colorNumeric(input$colors, fin$`Total Deaths`)})
  
  output$map <- renderLeaflet({
    leaflet(fin) %>% addTiles() %>%
      fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))})
  
  # Incremental changes to the map 
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
       #Layers control
      addCircles(radius = ~`Total Deaths`*70, weight = 1, color = "#FFFFCC",
                fillColor = ~pal(`Total Deaths`), fillOpacity = 0.5, popup = ~paste(`Total Deaths`))})
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = fin)
    
    # Remove any existing legend
    proxy %>% clearControls()
    if (input$legend) {pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~`Total Deaths`)}})
}

shinyApp(ui, server)


# From both quantitive graph and qualitative graphs of COVID-19, we are able to comprehend how the effect was for people by the COVID-19.
# The quantitative shiny graph delivers for interactive graphs of how many peple died from con-current point of view which delivers the influence and hazard frequencies of COVID-19.
# However, the qualitative graph of LDA for topic modeling in key words and sub-words explains the impact of COVID-19 to mass media and people for the issues.
# Combined for both modeling, the model emphasizes the keen and comprehensive understand of on-going hazard information for COVID-19 to people by the method of data mining. 


