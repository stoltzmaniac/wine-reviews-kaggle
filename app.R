library(shiny)
library(RColorBrewer)
library(maptools)
library(ggplot2)
library(ggjoy)
library(caTools)
library(randomForest)  
library(caret)
library(e1071)
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

varieties = data %>%
  group_by(variety) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
topVarieties = varieties$variety[1:50]

df = data %>% 
  na.omit() %>%
  filter(country != '') %>%
  filter(variety %in% topVarieties) %>%
  mutate(Quality = if_else(points>=90,1,0)) %>%
  select(Quality,country,designation,price,province,region_1,variety,winery,points)
  #select(Quality,country,designation,price,province,region_1,variety,winery)
#df$Quality = factor(df$Quality,levels=c('Good','Bad'))
df$variety = factor(df$variety,levels=topVarieties)

set.seed(101) 
sample = sample.split(df$country, SplitRatio = .30)
df.train = subset(df, sample == TRUE)
df.test  = subset(df, sample == FALSE)

table(df$Quality)/nrow(df)  
table(df.train$Quality)/nrow(df.train)
table(df.test$Quality)/nrow(df.test)

#Fit Random Forest Model
rf = randomForest(points ~ variety + country,  
                  ntree = 100,
                  data = df.train)
plot(rf)  
print(rf)

varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")


var.imp = data.frame(importance(rf, type=2))
var.imp$Variables = row.names(var.imp) 

df.train$predicted.response = predict(rf , df.train)
df.train = df.train %>% mutate(error = predicted.response-price, error.pct = (predicted.response-price)/price)


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