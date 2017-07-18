library(shiny)
library(RColorBrewer)
library(maptools)
library(ggplot2)
library(ggjoy)
library(caTools)
library(party)
library(e1071)
library(rpart)
library(MASS)
library(tidyverse)

data = read.csv('winemag-data_first150k.csv')

#Sample Size
set.seed(101) 
sample = sample.split(data$country, SplitRatio = .05)
data.train = subset(data, sample == TRUE)
data.test  = subset(data, sample == FALSE)
fit = glm(points ~ country, data = data.train)
fit2 = glm(points ~ variety, data = data.train)
summary(fit)
summary(fit2)




###########

topN = function(df,VAR,nCutoff){
  tmp = df %>%
    group_by_(VAR) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  colnames(tmp) = c('N','n')
  tmpN = tmp$N[1:nCutoff]
  tmpN = factor(tmpN,levels=tmpN)
  return(tmpN)
}

mround <- function(x,base){ 
  base*round(x/base) 
} 

varieties = topN(data,'variety',50)
countries = topN(data,'country',50)

df = data %>% 
  na.omit() %>%
  dplyr::mutate(priceBucket = mround(price,5),Quality=ifelse(points>=90,'Great','Okay')) %>%
  dplyr::select(country,designation,province,region_1,variety,winery,priceBucket,Quality)
df$priceBucket = factor(df$priceBucket,levels=unique(df$priceBucket))
df$Quality = factor(df$Quality,levels=c('Great','Okay'))

set.seed(101) 
sample = sample.split(df$country, SplitRatio = .05)
df.train = subset(df, sample == TRUE)
df.test  = subset(df, sample == FALSE)

model.tree = rpart(Quality ~ ., method = 'class', data = df.train)
printcp(model.tree)
plot(model.tree)
text(model.tree)
##############

server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

shinyApp(ui = ui, server = server)