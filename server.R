
# install.packages("googleAnalyticsR")
getwd()
# setwd("C:/Users/CarolinaFagundes/Desktop/trainning_shinny")


#### FIRST TRY with ga data ####
  # building the dataframe with the ga data
  ####################################

  library(googleAnalyticsR)
  library(ggplot2)
  library(stringr)
  library(dplyr)


# #EASY AUTHOTIZATION ON ANALYTICS:
ga_auth()

#
# ##MUITO BOM: CONSIGO VER TODAS AS CONTAS REGISTRADAS NO ANALYTICS
# ## NO IESB SÃO 60:
my_accounts <- google_analytics_account_list()
# #View(my_accounts)
#
# #Use my_accounts to find the viewId
my_id <- "167966494"
#
#
# #set date variables for DYNAMIC date range
# ########### CHANGE THAT ##

    start_date <- "2018-10-01"
    end_date <- "2019-02-25"
#
# #Session Query - Uses start_date and end_date
# #### the final data frame for FIRST METRICS
df1 <- google_analytics_4(my_id,
                          date_range = c(start_date, end_date),
                          metrics = c("users", "sessions", "bounceRate", "uniquepageviews"),
                          dimensions = c("date","ga:deviceCategory"))

### the data frame for pageviews
  df2 <- google_analytics(my_id,
                            date_range = c(start_date, end_date),
                            metrics = c("users","sessions", "bounceRate", "uniquepageviews"),
                            dimensions = c("date",
                                         "hour","ga:deviceCategory", "ga:pagePath"),
                                         max = -1)
  df2$weekday <- weekdays(as.Date(df2$date))
  
  df2$pagePath_2 <-  str_split(df2$pagePath, pattern = fixed("/"), n=5)
  
  x <- do.call(rbind, df2$pagePath_2)
  colnames(x) <- LETTERS[1:ncol(x)]
  df2<- cbind(df2, x)
  
  #limpeza de string:
  df2$B <- gsub("\\#.*","",df2$B)
  df2$B <- gsub("\\_.*","",df2$B)
  df2$B <- gsub("\\?.*","",df2$B)
  df2$B <- gsub("[\\^0-9.]","",df2$B)
  df2$B <- gsub("\\-.*","",df2$B)
  df2$B <- gsub("\\..*","",df2$B)
  df2$B <- gsub("\\ .*","",df2$B)
  df2$B <- tolower(df2$B)
  
  df2$A <- grepl("vestibularead", df2$B, fixed = TRUE)
  df2[which(df2$A == TRUE), 12] <- "ead"

  
    df2$A <- grepl("vestibular", df2$B)
    df2[which(df2$A == TRUE), 12] <- "vestibular"
    
    df2$A <- grepl("cricao", df2$B)
    df2[which(df2$A == TRUE), 12] <- "inscricao"
    
    df2$A <- grepl("graduacao", df2$B)
    df2[which(df2$A == TRUE), 12] <- "graduacao"
    
    df2$A <- grepl("resultado", df2$B)
    df2[which(df2$A == TRUE), 12] <- "graduacao"
  
  unique(df2$B)

############################
library(dplyr)
library(ggplot2)
library(tidyr)

server <- function(input, output) {

  output$value <- renderPrint({ input$date })
  #first: reactive dataset
  #df_graph <- select(df1,date, deviceCategory, n = sessions)
   df_graph <- reactive({
   select(df1,date, deviceCategory, n = input$n)
     
  })

  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    paste(input$n, "por device")
  })
  
  output$graph <- renderPlot({  
    req(input$date)
    selected <- df1 %>%
      # convert thtr_rel_date to Date format:
      filter(date >= input$date[1] & date <= input$date[2])
    
    df_graph <- reactive({
      select(selected,date, deviceCategory, n = input$n)
    })
    
    ggplot(data = df_graph() , aes(x = date, y = n,
                                     color = deviceCategory)) +
      geom_line() + scale_x_date(date_minor_breaks = "1 day", date_labels = "%d/%m") +
      labs(x ="data", y = "variável de escolha")
  })
  
  output$graph2 <- renderPlot({ 
    req(input$date)
    selected <- df2 %>%
      filter(date >= input$date[1] & date <= input$date[2])
    
    df_graph2 <- reactive({
      select(selected,hour, deviceCategory, n = input$n)
    })

  ggplot(data = df_graph2()) +
    geom_line(aes(x=hour, y=n,color = deviceCategory)) +
    labs(x ="hora", y = "variável de escolha")
  
  })

  output$graph3 <- renderPlot({ 
    req(input$date)
    selected <- df2 %>%
      filter(date >= input$date[1] & date <= input$date[2])
    
    df_graph3 <- reactive({
      select(selected,weekday, deviceCategory, n = input$n)
    })
    
    ggplot(data = df_graph3()) +
      geom_line(aes(x=weekday, y=n,color = deviceCategory)) +
      labs(x ="dia da semana", y = "variável de escolha")
  
  })
  
  output$graph4 <- renderPlot({ 
    req(input$date)
    selected <- df2 %>%
      filter(date >= input$date[1] & date <= input$date[2])
    
    df_graph4 <- reactive({
      select(selected,B, deviceCategory, n = input$n)
    })
    
    ggplot(data = df_graph4()) +
      geom_line(aes(x=B, y=n,color = deviceCategory)) +
      labs(x ="pagePath", y = "variável de escolha")
    
  })
  
}

