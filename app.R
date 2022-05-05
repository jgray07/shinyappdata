#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
sale.data <- read.csv('https://media.githubusercontent.com/media/jgray07/shinyappdata/main/data/train.csv') 
library(shiny)
library(fpp2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Future Sales Forecasting"),
    
    # Sidebar with a slider input for the store number 
    sidebarLayout(
      
                   sidebarPanel(
            
                   numericInput("store", 
                                h3("Store Number: [1 - 54]"), 
                                value = 1),
                   
               selectInput("family", h3("Department"), 
                           choices = list("Automotive" = "AUTOMOTIVE", 
                                          "Baby Care" = "BABY CARE" ,
                                          "Beauty" = "BEAUTY",
                                          "Beverages" = "BEVERAGES",
                                          "Books" = "BOOKS", 
                                          "Bread / Bakery" = "BREAD/BAKERY",
                                          "Celebration" = "CELEBRATION",
                                          "Cleaning" = "CLEANING",
                                          "Dairy" = "DAIRY",
                                          "Deli" = "DELI",
                                          "Eggs" = "EGGS",
                                          "Frozen Foods" = "FROZEN FOODS",
                                          "Grocery I" =  "GROCERY I" , 
                                          "Grocery II" =  "GROCERY II", 
                                          "Hardware" = "HARDWARE",  
                                          "Home and Kitchen I" = "HOME AND KITCHEN I",
                                          "Home and Kitchen II" =  "HOME AND KITCHEN II",
                                          "Home Appliances" = "HOME APPLIANCES" , 
                                          "Home Care" =  "HOME CARE", 
                                          "Ladies Wear" =  "LADIESWEAR",               
                                          "Lawn and Garden" =  "LAWN AND GARDEN",
                                          "Lingerie" = "LINGERIE", 
                                          "Liquor, Wine, Beer" = "LIQUOR,WINE,BEER",
                                          "Magazines" = "MAGAZINES",                 
                                          "Meats" = "MEATS", 
                                          "Personal Care" = "PERSONAL CARE", 
                                          "Pet Supplies" =  "PET SUPPLIES", 
                                          "Players and Electronics" = "PLAYERS AND ELECTRONICS",
                                          "Poultry" = "POULTRY",
                                          "Prepared Foods" =  "PREPARED FOODS",
                                          "Produce" =  "PRODUCE",
                                          "School and Office Supplies" = "SCHOOL AND OFFICE SUPPLIES",
                                          "Seafood" =  "SEAFOOD" ), selected = 1),
               
               radioButtons("model", h3("Select a Model Type"),
                            choices = list("ARIMA" = 1, "ETS" = 2
                                           ),selected = 1),
               
               radioButtons("time", h3("How Would You Like to Forecast?"),
                            choices = list("Daily" = 1, "Weekly" = 2
                            ),selected = 1),
               
               
               sliderInput("year",
                           h3("How many years of past data would you like to use?"),
                           min = 1,
                           max = 4,
                           value = 4),
               
               helpText("Note: If you are using weekly data,", 
                        "you should select 3 or 4 years of past data."),
               
               sliderInput("h",
                           "Future Forecast length:",
                           min = 1,
                           max = 30,
                           value = 1)
        
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("barPlot"),
            plotOutput("barPlot2"),
            plotOutput("tsPlot"),
            tableOutput("predictions")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  
  output$barPlot <- renderPlot({
    x <- data.frame(sale.data)
    require(scales)
    ggplot(data = x, aes(store_nbr, sales)) +
      #stat_summary(geom = 'bar') +
      geom_bar(stat = 'identity', aes(fill = family)) +
      labs(title = 'Store Overview', y = 'Total Sales', x = 'Store') +
      scale_x_continuous(breaks = 1:54) +
      scale_y_continuous(labels = comma)
  })
  
  #-------------------------------------------------------------------------------------------------------------------------------------------------        
  output$barPlot2 <- renderPlot({
    x <- data.frame(sale.data)
    store.df1 <- data.frame((sale.data[(sale.data$store_nbr == round(input$store)),]))
    require(scales)
    ggplot(data = store.df1, aes(family, sales)) +
      #stat_summary(geom = 'bar') +
      geom_bar(stat = 'identity', aes(fill = family), show.legend = FALSE) +
      labs(title = 'Sales by Department at Selected Store', y = 'Total Sales', x = 'Department') +
      #scale_x_continuous() +
      scale_y_continuous(labels = comma) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))
  })
  #-------------------------------------------------------------------------------------------------------------------------------------------------        
    output$tsPlot <- renderPlot({
        store.df <- data.frame((sale.data[(sale.data$store_nbr == round(input$store)) & (sale.data$family == input$family),]))
        # generate bins based on input$bins from ui.R
        if(input$time == 1){
            store.df <- store.df[,'sales']
            store.fore <- ts(store.df, frequency = 7)
            if(input$year == 4){store.fore <- tail(store.fore, 1469)}
            else if(input$year == 3){store.fore <- tail(store.fore, 1095)}
            else if(input$year == 2){store.fore <-tail(store.fore, 730)}
            else if (input$year == 1){store.fore <- tail(store.fore, 365)}}
         else if(input$time == 2){
             store.df <- store.df[,c('date','sales')]
             store.df$date <- as.Date(store.df$date) 
             
             week.num <- as.numeric(store.df$date - store.df$date[1]) %/% 7
             week <- store.df$date[match(week.num, week.num)]
             store.df.week <- aggregate(sales ~ week, store.df, sum)
             store.df.week.full <- head(store.df.week, -1)
             store.fore <- ts(store.df.week.full[,'sales'], start = c(2013,1), frequency = 52)
             if(input$year == 4){store.fore<- tail(store.fore, 208)}
             else if(input$year == 3){store.fore<- tail(store.fore, 156)}
             else if(input$year == 2){store.fore <-tail(store.fore, 104)}
             else if (input$year == 1){store.fore <- tail(store.fore, 52)}}

        #num <- seq(min(x), max(x), length.out = input$num + 1)
        #h <- seq(min(x), max(x), length.out = input$h + 1)
        # draw the histogram with the specified number of bins
        if(input$time == 1){
        if(input$model == 1){autoplot(stlf(store.fore, method = 'arima', h = input$h)) + xlab ('Days') + ylab('Sales')}
        else{autoplot(stlf(store.fore, method = 'ets', h = input$h)) + xlab ('Days') + ylab('Sales')}}
        else{if(input$model == 1){autoplot(stlf(store.fore, method = 'arima', h = input$h)) + xlab ('Weeks') + ylab('Sales')}
            else{autoplot(stlf(store.fore, method = 'ets', h = input$h)) + xlab ('Weeks') + ylab('Sales')}}
    })

    #-------------------------------------------------------------------------------------------------------------------------------------------------            
    output$predictions <- renderTable({ 
        
        store.df <- data.frame((sale.data[(sale.data$store_nbr == round(input$store)) & (sale.data$family == input$family),]))
        if(input$time == 1){
            store.df <- store.df[,'sales']
            store.fore <- ts(store.df, frequency = 7)
            if(input$year == 4){store.fore <- tail(store.fore, 1469)}
            else if(input$year == 3){store.fore <- tail(store.fore, 1095)}
            else if(input$year == 2){store.fore <-tail(store.fore, 730)}
            else if (input$year == 1){store.fore <- tail(store.fore, 365)}}
        else if(input$time == 2){
            store.df <- store.df[,c('date','sales')]
            store.df$date <- as.Date(store.df$date) 
            
            week.num <- as.numeric(store.df$date - store.df$date[1]) %/% 7
            week <- store.df$date[match(week.num, week.num)]
            store.df.week <- aggregate(sales ~ week, store.df, sum)
            store.df.week.full <- head(store.df.week, -1)
            store.fore <- ts(store.df.week.full[,'sales'], start = c(2013,1), frequency = 52)
            if(input$year == 4){store.fore<- tail(store.fore, 208)}
            else if(input$year == 3){store.fore<- tail(store.fore, 156)}
            else if(input$year == 2){store.fore <-tail(store.fore, 104)}
            else if (input$year == 1){store.fore <- tail(store.fore, 52)}}
        
        #mat1 <- matrix(NA, nrow = 15, ncol = 2, byrow = TRUE)
        #colnames(mat1) <- c('Future_Forecast_Values 1-15', 'Future_Forecast_Values 16-30')
        #rownames(mat1) <- c('1|16','2|17','3|18','4|19','5|20','6|21','7|22','8|23','9|24','10|25', '11|26','12|27','13|28','14|29','15|30')
        
        mat1 <- matrix(NA, nrow = 10, ncol = 3, byrow = TRUE)
        colnames(mat1) <- c('Future_Forecast_Values 1-10', 'Future_Forecast_Values 11-20', 'Future_Forecast_values 21-30')
        rownames(mat1) <- c('1|11|21','2|12|22','3|13|23','4|14|24','5|15|25','6|16|26','7|17|27','8|18|28','9|19|29','10|20|30')
        
        if(input$model == 1){mat1[is.na(mat1)][1:input$h] <- round(stlf(store.fore, method = 'arima', h = input$h)$mean, 2)}
            else{mat1[is.na(mat1)][1:input$h] <- round(stlf(store.fore, method = 'ets', h = input$h)$mean, 2)}
        t(mat1)}, rownames = TRUE, colnames = TRUE
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
