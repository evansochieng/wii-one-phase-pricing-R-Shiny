library(shiny)
library(RColorBrewer)
ui<-fluidPage (
    titlePanel="PIXEL524507 FABA BEANS EMERGENCE PHASE DROUGHT COVER",
    
    fluidRow(
        h2(strong("Customer Personal Information")),
        column(6,
               textInput(inputId="name", "Enter your name", value = "")
        ),
        column(6,
               numericInput("contact", "Enter your Phone number", value = "")
        )
    ),
              
    fluidRow(
        h2(strong("Crop Specifications")),
        column(8,
               sliderInput(inputId="dateRange", label="Cover Period", 1, 366, value=c(153, 162)),
               selectInput("trigger", "Phase trigger", choices=seq(15,25,1)),
               selectInput("exit", "Phase exit", seq(5,10,1))
        ),
        column(4,
               numericInput("pickNumber",
                            label="Area under cultivation",
                            min=1, max=20, value=1)
               
        )
    ),
    
    fluidRow(
        h2(strong("Pricing Factors")),
        column(6,
               numericInput("amount","Sum Insured", 0, 50000, 10000)
        ),
        column(3,
               selectInput(inputId="type",
                           label="Deductible type",
                           choices=c(1,2), selected = 2),
               helpText("Note: 1= Franchise deductible",
                        "2= Ordinary deductible")
        ),
        column(3,
               radioButtons(inputId="deductible",
                            label="Deductible level",
                            choices=c(10.0, 15, 20.0)),
               helpText("Note: These are values of deductibles",
                        "      Pick one that best suits you")
        )
    ),
    
    fluidRow(
        column(6,
              h3("Premium"),
              tableOutput("table")
        ),
        column(6,
               h4("Historical Payout"),
               plotOutput("plot", width="100%"),
               actionButton("click", "SUBMIT")
        )
        
    )
)

server<-function(input,output) {
    library(dplyr)
    
    setwd("C:/Users/HP/Desktop/Syngenta/Aug-Dec 2020/RShiny/R Shiny")
    data<-read.csv("Pixel524507.csv", header=TRUE)
    data[data<0]<-NA
    new.data<-select(data, -X)
    colnames(new.data)<-substr(colnames(new.data),2,nchar(colnames(new.data)))
    
    rainfall_slider<-reactive({
        rain_received<-data.frame(data=NA, nrow=1, ncol=ncol(new.data))
        for(i in 1:ncol(new.data)){rain_received[i]<-sum(new.data[input$dateRange[1]:input$dateRange[2], i], na.rm = TRUE)}
        rain_received })
    
    trigger_slider<-reactive({
        tr<-as.numeric(input$trigger) })
    
    exit_slider<-reactive({
        ex<-as.numeric(input$exit) })
    
    payout_slider<-reactive({
        
        rain.received<-rainfall_slider()
        per.trigger<-as.numeric(trigger_slider())
        per.exit<-as.numeric(exit_slider())
        
        weight<-0.25
        tick<-weight/(per.trigger - per.exit)
        
        payout<-data.frame(data=NA, nrow=1, ncol=ncol(new.data))
        for(i in 1:ncol(new.data)){payout[i]<-
            if(rain.received[i]>per.exit & rain.received[i]<=per.trigger){
                payout[i]<-abs((rain.received[i] - per.trigger))*tick
            }else if(rain.received[i]<=per.exit){
                payout[i] <-weight
            }else{
                payout[i] <-0
            }
        }
        colnames(payout)<-colnames(new.data)
        payout
        })
    
 output$plot <-renderPlot({
     payouts<-payout_slider()
     payout.mat<-as.matrix(payouts)
     colou<-brewer.pal(1:ncol(payout.mat),"Accent")
     barplot(height=payout.mat, names.arg = colnames(payout.mat), col = colou, main = "Historical Payout", xlab = "Years", ylab = "Payout")
     
 })
    
    average_payout <- reactive ({
      payouts<-payout_slider()
      payouts<-as.matrix(payouts)
        mean(payouts,na.rm = TRUE)
    })
    
    payout_factor <- reactive ({
        av.payout <- average_payout()
        
        if(input$type ==1) {
            if(av.payout >= input$deductible){
                compens<- av.payout
            }
            else{
                compens<- 0
            }
        }else{
            if(av.payout <= input$deductible){
                compens<- 0
            }
            else{
                compens<- av.payout - input$deductible
            }
            
        }
       av.payout 
    })
    
    
    output$table <- renderTable ({
        payout.factor<-payout_factor()
        payout.factor * as.numeric(input$amount)
    })
    
    

}
shinyApp(ui=ui,server=server)
