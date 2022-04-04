library(ggplot2)
library(shiny)
library(dplyr)
#install.packages("scales")
library(scales)
#install.packages("lubridate")
library(lubridate)
library(plotly)
#install.packages("shinydashboard")
library(shinydashboard)


#dataset
bank = read.csv(file.choose(), header = T)
head(bank)
summary(bank)

###################################################################
#data Preparation
###################################################################
bank$Date.Joined <-parse_date_time(bank$Date.Joined, "ymd")
bank$Date.Joined<-as.Date(bank$Date.Joined)
bank$Age<-as.factor(bank$Age)

#line chart data
line_chart<-bank %>% 
  group_by(Date.Joined)%>% 
  summarize(Balance=sum(Balance))

#time series ggplot
line<-ggplot(line_chart, aes(x=Date.Joined, y=Balance))+ 
  geom_line(color="steelblue") + geom_point()+
  xlab("Year of New Account Created")+
  ylab("Balance in $ Million")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2015-01-01"),as.Date("2022-02-11")))+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))

#piechart data
pie<-bank %>% 
  group_by(Gender) %>% 
  summarise(count = n())

#tree map of customers
treemap<-bank %>% 
  group_by(Region) %>% 
  summarise(count = n())

###################################################################
#UI
###################################################################
ui <-
  dashboardPage(
  dashboardHeader(title = "UK Bank Customers"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(align="center",
             box(title = "UK BANK NEEDS MORE CASH DEPOSITS",
                 status = "warning"  , solidHeader = T, collapsible = F, width = 12),
    # top row row with key metric
    fluidRow(
      # 3 Info Box
      infoBox("Current Cash in Bank: Jan'22","536,000", icon = icon("credit-card")),
      infoBox("Highest Cash in Bank: May'19","1.9 Million", icon = icon("list")),
      infoBox("Highest Personal Bank Balance", "183,000", icon = icon("thumbs-up", lib = "glyphicon")),
     
       #second row
      fluidRow(align="center",
        box(title = "Account Balance Distribution", plotOutput("plot1", height=250),
                 status = "primary"  , solidHeader = T, collapsible = F),  
               box(title = "Bank Customer Distribution by Age", plotOutput("plot2", height=250),  status = "primary"  , solidHeader = T, collapsible = F)
        )
), 
   #third Row
fluidRow(align="center",
         box(title = "Time Series of Cash in Bank", plotlyOutput("plot3", height=250),
             status = "primary"  , solidHeader = T, collapsible = F), 
      
                  box(title = "Gender Distribution of Customers", plotlyOutput("plot4", height=250),
                      status = "primary"  , solidHeader = T, collapsible = F)
         )
)
))
###################################################################
#Server Function
###################################################################
server <- function(input, output, session) {
  
  #output plot 1- Account Balance Distribution
  
  output$plot1<-renderPlot({
    ggplot(bank, aes(x=Balance, fill = ..count..))+
      geom_histogram(binwidth = 3000)+
      scale_x_continuous(labels = label_number(suffix = " K", scale = 1e-3))+xlab("Balance in Customer Account")+ylab("Number of Customers")
  })
  
  #Age distribution
    output$plot2<-renderPlot({
      
      ggplot(bank, aes(x=Age, fill = Gender))+
        geom_bar()+ylab("Number of Customers")+xlab("Age of the Customer")
    })
    
    #Time Series 
    output$plot3<-renderPlotly({
      ggplotly(line)
      
    })
    
    output$plot4<-renderPlotly({
      
      plot_ly(pie, labels = ~Gender, values = ~count,  textinfo='label+percent', insidetextorientation='radial') %>% 
        add_pie(hole = 0.6) %>% 
        layout(title = "Account Holders by Gender",  showlegend = F,
               xaxis = list(showgrid = T, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = T, zeroline = FALSE, showticklabels = FALSE))
      
    })
  
}

shinyApp(ui, server)

