library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)

# --------------- data manipulation ----------------------------------------------------------------------------------------------------
raw = read.csv('imdb_top_1000.csv')

#Organize data
movies_raw = raw %>% select(Series_Title, Released_Year, Runtime, Genre, IMDB_Rating, Director, Star1, Star2, Star3, Star4, No_of_Votes, Gross)
movies_raw$Gross = as.numeric(gsub(',','',movies_raw$Gross))
movies_raw$Runtime = as.numeric(gsub(' min','',movies_raw$Runtime))

#Replace error in year, duplicate genre names
movies_raw$Released_Year[movies_raw$Released_Year == 'PG'] <- 1995
movies_raw$Genre = gsub(' ','', movies_raw$Genre)

#Separate data between reported and non-reported gross profit
no_gross = movies_raw %>% filter(movies_raw$Gross == '')

movies = movies_raw %>% filter(movies_raw$Gross != '')

#----------Time Page Data Manipulation----------
#Expanded Genres
genres = movies %>% separate_longer_delim(Genre, delim = ',')

#Genre count and list for bar chart
genre_count = genres %>% count(Released_Year, Genre, name = 'Count')
genres_unique = unique(genres$Genre)


#----------Actor Page Data Manipulation----------

#Aggregate actor columns
actors = movies
actors$Genres = actors$Genre
actors$Stars = paste(movies$Star1,movies$Star2,movies$Star3,movies$Star4, sep=',')
actors = actors %>% separate_longer_delim(Stars, delim = ',') %>% separate_longer_delim(Genre, delim=',')

actors_unique = unique(actors$Stars)
directors_unique = unique(movies$Director)


actor_count = actors %>% count(IMDB_Rating, Stars, name = 'Count')

#count of genre by actor
genre_list = c(unique(actors$Genre))
#actor_genre = 

#Actor credits dataframe
actor_credits = actors%>%select(Series_Title,Released_Year,Genres,Gross,IMDB_Rating,Director,Star1,Star2,Star3,Star4,Runtime,Stars)
actor_credits$check = paste(actor_credits$Series_Title,actor_credits$Stars)
actor_credits = actor_credits %>%distinct(check, .keep_all = TRUE)

# --------------- UI ------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  dashboardPage(
    skin = 'blue',
    dashboardHeader(title = 'IMDB Top 1000 Films'),
    dashboardSidebar(
      sidebarMenu(id = 'tabs',
        menuItem('Overview', tabName = 'overview', icon = icon('globe')), 
        menuItem('Genre Performance', tabName = 'gen', icon = icon('list'),
                 menuItem('Film Count', tabName = 'genFreq', icon = icon('bars')),
                 menuItem('Gross Profit', tabName = 'gross', icon = icon('coins')),
                 menuItem('Ratings', tabName = 'rating', icon = icon('star-half-stroke')),
                 menuItem('Top Film Search', tabName = 'movie', icon = icon('film'))
                 ),
        menuItem('Actors and Actresses', tabName = 'actors', icon = icon('star'),
                 menuItem('Credits', tabName = 'Acredits', icon = icon('file-signature')),
                 menuItem('Statistics', tabName = 'Astats', icon = icon('percent')),
                 menuItem('Top Performance Search', tabName = 'Atopten', icon = icon('ranking-star'))
                 ),
        menuItem('Directors', tabName = 'directors', icon = icon('user'),
                 menuItem('Credits', tabName = 'Dcredits', icon = icon('file-signature')),
                 menuItem('Statistics', tabName = 'Dstats', icon = icon('percent')),
                 menuItem('Top Performance Search', tabName = 'Dtopten', icon = icon('ranking-star'))
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'overview',
                fluidPage(
                  titlePanel(
                    h1('Welcome to the IMDB Top 1000 Films Diver', align = 'center')
                    ),
                  mainPanel(
                    br(),
                    h2('Background'),
                    h4('The purpose of this app is to help users investigate trends in the top rated films found on the popular critic website, IMDB.
                       The data used in this app consists of the films with the top 1000 IMDB ratings from the years 1920-2020. Information is provided for each film,
                       including the genre(s), director name, actor/actress names, IMDB rating, and the gross profit made by each film.'),
                    br(),
                    h2('How to Navigate the App'),
                    h4('This app consists of three sections containing pages meant to help users identify key information about the dataset. Information about each section can be found below.'),
                    h3('Genre Performance'),
                    h4('The Genre Performance section will help any users looking to find information about the popularity and profitability of genres over time.'),
                    h4('For each of the first three pages within this section, select as many genres as you would like to compare at a given time, and a timeframe
                       over which you would like to investigate. From there, the built in visualizations can help you discover trends within the dataset.'),
                    h4('The final page within this section allows users to search by individual genre over a time period to collect information about each film
                       that fits within the parameters supplied.'),
                    h3('Actor/Actress Investigation'),
                    h4('The Actor/Actress Investigation section is divided into three pages. The first page will allow you to view all films listed for each actor or actress.'),
                    h4('The second page will show basic statistics for the selected actor or actress, and also shows charts of their films success throughout their career span.'),
                    h4('The final page contains search features that will allow you to view the rankings of ratings and gross profit each actor or actress had attained
                       over a given time period. The tables can be viewed from either the most or least successful per statistic by filtering the column headers.') ,
                    h3('Director Investigation'),
                    h4('The Director Investigation page has an identical layout to the Actor/Actress Investigation section, and can be utilized in the same manner.')
                  ,width = 12)
                )),

        tabItem(tabName = 'genFreq',
                fluidPage(
                  titlePanel(
                    h1('Count of Films Per Genre over Time', align = 'center')
                    ),
                  mainPanel(
                    br(),
                    fluidRow(
                      column(2,
                             checkboxGroupInput(inputId = 'G1',label = 'Genres',
                                                choices = sort(genres_unique),
                                                selected = 'Action'),
                             sliderInput(inputId = 'GS1', label = 'Year Range',
                                         min = 1920, max = 2020, value = c(1920,2020), sep = '')
                             ),
                      column(10,
                             plotOutput('plotG1', height = '700px')
                             )
                    ), width = 12
                      )
                )),
        tabItem(tabName = 'gross',
                fluidPage(
                  titlePanel(
                    h1('Gross Profit of Films over Time', align = 'center')
                    ),
                  mainPanel(
                    br(),
                    fluidRow(
                      column(2,
                             checkboxGroupInput(inputId = 'G2',label = 'Genres',
                                                choices = sort(genres_unique),
                                                selected = 'Action'),
                             sliderInput(inputId = 'GS2', label = 'Year Range',
                                         min = 1920, max = 2020, value = c(1920,2020), sep = '')
                             ),
                      column(10,
                              plotOutput('plotG2', height = '700px')#, hover = 'hov1'),
                             #dataTableOutput(outputId = 'hovt1'))
                             ), 
                  ),width =12
                  )
                )),
        tabItem(tabName = 'rating',
                fluidPage(
                  titlePanel(
                    h1('IMDB Rating of Films over Time',align = 'center')
                    ),
                  mainPanel(
                    br(),
                    fluidRow(
                      column(2,
                             checkboxGroupInput(inputId = 'G3',label = 'Genres',
                                                choices = sort(genres_unique),
                                                selected = 'Action'),
                             sliderInput(inputId = 'GS3', label = 'Year Range', 
                                         min = 1920, max = 2020, value = c(1920,2020), sep = '')
                             ),
                      column(10,
                             plotOutput('plotG3', height ='700px'))), width = 12
                    )
                )
                ),
        tabItem(tabName = 'movie',
                fluidPage(
                  titlePanel(
                    h1('Top Film Search', align = 'center')
                  ),
                  mainPanel(
                    br(),
                    fluidRow(
                      column(6,
                             selectInput(inputId = 'G4',label = 'Genres',
                                         choices = sort(genres_unique),
                                         selected = 'Action')
                      ),
                      column(6,
                             sliderInput(inputId = 'G5', label = 'Year Range',
                                         min = 1920, max = 2020, value = c(1920,2020), sep = ''))
                    ),
                    fluidRow(
                      column(12,
                             dataTableOutput('tableG1'))
                      ), width = 12
                    )
                  )
                ),
        tabItem(tabName = 'Acredits',
                fluidPage(
                  titlePanel(
                    h1('Actor/Actress Film Credit List', align = 'center')
                  ),
                  mainPanel(
                    br(),
                    fluidRow(
                      column(4,
                             selectInput('A1', 'Select Actor/Actress',
                                         choices = sort(actors_unique)
                                         ))
                    ),width = 12,
                    fluidRow(
                      column(12,
                             dataTableOutput('tableA1'))
                    )
                  )
                  )
                ),
        tabItem(tabName = 'Astats',
                fluidPage(
                  titlePanel(
                    h1('Actor/Actress Statistics', align = 'center')
                  ),
                  mainPanel(
                    br(),
                     fluidRow(
                       column(4,
                              #selectizeInput('A2',choices = NULL)
                    
                              #selectizeInput('A2', 'Select Actor/Actress',
                             #                choices = sort(actors_unique),
                             #                options = list(create = TRUE)),
                    
                              selectInput('A2','Select Actor/Actress',
                                          choices = sort(actors_unique))
                             )), width =12,
                     fluidRow(
                       'General Statistics'),
                     fluidRow(
                       column(6,
                              infoBoxOutput('boxA1', width = 12)),
                       column(6,
                              infoBoxOutput('boxA2', width = 12))
                     ),
                     fluidRow(
                       column(6,
                              valueBoxOutput('boxA3', width = 12)),
                       column(6,
                              valueBoxOutput('boxA4', width = 12))
                     ),
                     fluidRow(
                       column(6,
                              infoBoxOutput('boxA5', width = 12)),
                       column(6,
                              infoBoxOutput('boxA6', width = 12))
                     ),
                     fluidRow(
                       column(6,
                              plotOutput('plotA1')),
                       column(6,
                              plotOutput('plotA2'))
                     )
                    )
                  )
                ),
        tabItem(tabName = 'Atopten',
                fluidPage(
                  titlePanel(
                    h1('Top Actor/Actress Search', align = 'center')
                  ),
                  mainPanel(
                    br(),
                    fluidRow(
                      column(6,
                             selectInput(inputId = 'A3',label = 'Genres',
                                         choices = sort(genres_unique),
                                         selected = 'Action')
                      ),
                      column(6,
                             sliderInput(inputId = 'A4', label = 'Year Range',
                                         min = 1920, max = 2020, value = c(1920,2020), sep = ''))
                    ),
                    fluidRow(
                      column(3,
                             dataTableOutput('tableA2')
                             ),
                      column(3,
                             dataTableOutput('tableA3')
                             ),
                      column(3,
                             dataTableOutput('tableA4')
                      ),
                      column(3,
                             dataTableOutput('tableA5'))
                    ), width = 12
                    )
                  )
                ),
        tabItem(tabName = 'Dcredits',
                fluidPage(
                  titlePanel(
                    h1('Director Film Credit List', align = 'center')
                  ),
                  mainPanel(
                    br(),
                    fluidRow(
                      column(4,
                             selectInput('D1', 'Select Director',
                                   choices = sort(directors_unique)
                             ))),width = 12,
                    fluidRow(
                      column(12,
                             dataTableOutput('tableD1'))
                    )
                  )
                )
        ),
        tabItem(tabName = 'Dstats',
                fluidPage(
                  titlePanel(
                    h1('Director Statistics', align = 'center')
                  ),
                  mainPanel(
                    br(),
                    fluidRow(
                      column(4,
                             selectInput('D2','Select Director',
                                         choices = sort(directors_unique))
                      )), width =12,
                    fluidRow(
                      'General Statistics'),
                    fluidRow(
                      column(6,
                             infoBoxOutput('boxD1', width = 12)),
                      column(6,
                             infoBoxOutput('boxD2', width = 12))
                    ),
                    fluidRow(
                      column(6,
                             valueBoxOutput('boxD3', width = 12)),
                      column(6,
                             valueBoxOutput('boxD4', width = 12))
                    ),
                    fluidRow(
                      column(6,
                             infoBoxOutput('boxD5', width = 12)),
                      column(6,
                              infoBoxOutput('boxD6', width = 12))
                    ),
                    fluidRow(
                      column(6,
                             plotOutput('plotD1')),
                      column(6,
                             plotOutput('plotD2'))
                    )
                  )
                )
        ),
        tabItem(tabName = 'Dtopten',
                fluidPage(
                  titlePanel(
                    h1('Top Director Search', align = 'center')
                  ),
                  mainPanel(
                    br(),
                    fluidRow(
                      column(6,
                             selectInput(inputId = 'D3',label = 'Genres',
                                         choices = sort(genres_unique),
                                         selected = 'Action')
                      ),
                      column(6,
                             sliderInput(inputId = 'D4', label = 'Year Range',
                                         min = 1920, max = 2020, value = c(1920,2020), sep = ''))
                    ),
                    fluidRow(
                      column(3,
                             dataTableOutput('tableD2')
                      ),
                      column(3,
                             dataTableOutput('tableD3')
                      ),
                      column(3,
                             dataTableOutput('tableD4')
                      ),
                      column(3,
                             dataTableOutput('tableD5'))
                    ), width = 12
                  )
                )
        )
      )
    )
  ))


# --------------- Server ------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  #leave room for first tab

  #G1 Bar Plot
    output$plotG1 <- renderPlot({
      genre_count%>% filter(Genre %in% input$G1 & Released_Year >= input$GS1[1] & Released_Year <= input$GS1[2])%>%
      ggplot(aes(as.numeric(Released_Year), Count)) +
      geom_col(aes(fill = Genre), position = 'dodge') +
      theme_bw() +
      labs(x= 'Release Year',y='Count')
      })
      
  #G2 Scatterplot of gross profit vs release year by genre
    output$plotG2 <- renderPlot({
      genres %>% filter(Genre %in% input$G2 & Released_Year >= input$GS2[1] & Released_Year <= input$GS2[2])%>%
        ggplot(aes(as.numeric(Released_Year), as.numeric(Gross))) +
        geom_point(aes(color=Genre), size = 4) + 
        scale_y_continuous(labels = scales::comma)+
        labs(x= 'Release Year',y='Gross Profit (USD)')
      })
    #output$hovt1 <- renderDataTable({
    #  nearPoints(genres, input$hov1, xvar = 'Released_Year', yvar = 'Genre') %>% select(Series_Title)
      
    #})
    
  #G3 Scatterplot of rating vs release year by genre
      output$plotG3 <- renderPlot({
        genres %>% filter(Genre %in% input$G3 & Released_Year >= input$GS3[1] & Released_Year <= input$GS3[2])%>%
          ggplot(aes(as.numeric(Released_Year), as.numeric(IMDB_Rating))) +
          geom_point(aes(color=Genre), size = 4) +
          scale_y_continuous(labels = scales::comma)+
          labs(x= 'Release Year',y='IMDB Rating')
      })
  #Top Film
      output$tableG1 <- renderDataTable({
        req(input$G4)
        req(input$G5)
        a_ttrat <- unique(actors %>% filter(Genre == input$G4 & actors$Released_Year >= input$G5[1] & actors$Released_Year <= input$G5[2]) %>% select('Film Title' = Series_Title,'Released Year' = Released_Year,'Gross Profit (USD)' = Gross,'IMDB Rating' = IMDB_Rating,Director,Star1,Star2,Star3,Star4))
        a_ttrat = datatable(a_ttrat) %>% formatCurrency('Gross Profit (USD)')
        })
      
  #A1
      output$tableA1 <- renderDataTable({
        req(input$A1)
        actor_table <- actor_credits%>%filter(Stars == input$A1) %>% 
          select('Film Title' = Series_Title, 'Released Year' = Released_Year,Genres,'Gross Profit (USD)' = Gross, 'IMDB Rating' = IMDB_Rating,Director,Star1,Star2,Star3,Star4,'Runtime (min)'=Runtime)
        actor_table = datatable(actor_table) %>% formatCurrency('Gross Profit (USD)')
        actor_table
      })
  
  #A boxes
      output$boxA1 <- renderValueBox({
        a_count = sum(actor_credits$Stars == input$A2, na.rm = TRUE)
        valueBox(value =  a_count, subtitle = 'Number of Credits')
      })
      output$boxA2 <- renderInfoBox({
        a_gen_max = names(which.max(table(actors$Genre[actors$Stars == input$A2])))
        infoBox(title = 'Top Genre', value = a_gen_max)
      })
      
      output$boxA3 <- renderValueBox({
        a_mean_rat = round(mean(actors$IMDB_Rating[actors$Stars == input$A2]), digits = 2)
        valueBox(value = a_mean_rat, subtitle = 'Average Rating')
      })
      output$boxA4 <- renderValueBox({
        a_mean_gross = round(mean(actors$Gross[actors$Stars == input$A2]), digits = 2)
        valueBox(value = scales::dollar(as.numeric(a_mean_gross)), subtitle = 'Average Gross Profit (USD)')
      })
      
      output$boxA5 <- renderInfoBox({
        a_top_rat = max(actors$IMDB_Rating[actors$Stars == input$A2])
        a_top_rat_title = actors$Series_Title[actors$IMDB_Rating == a_top_rat & actors$Stars == input$A2][1]
        infoBox(title = 'Top Rating', value = a_top_rat, subtitle = a_top_rat_title)
      })
      output$boxA6 <- renderInfoBox({
        a_top_gross = max(actors$Gross[actors$Stars == input$A2])
        a_top_gross_title = actors$Series_Title[actors$Gross == a_top_gross & actors$Stars == input$A2][1]
        infoBox(title = 'Top Grossing Film (USD)',value = scales::dollar(as.numeric(a_top_gross)), subtitle = a_top_gross_title)
          
      })
      
  #A2 & A3
       output$plotA1 <- renderPlot({
         actor_credits %>% filter(Stars %in% input$A2)%>%
           ggplot(aes(as.numeric(Released_Year), as.numeric(Gross))) +
           geom_point(size = 4) + 
           scale_y_continuous(labels = scales::comma)+
           labs(x= 'Release Year',y='Gross Profit (USD)',title = 'Gross Profit vs Release Year')
         })
       
       output$plotA2 <- renderPlot({
         actor_credits %>% filter(Stars %in% input$A2)%>%
           ggplot(aes(as.numeric(Released_Year), as.numeric(IMDB_Rating))) +
           geom_point(size = 4) +
           scale_y_continuous(labels = scales::comma)+
           labs(x= 'Release Year',y='IMDB Rating',title = 'IMDB Rating vs Release Year')
       })
       
  #Actor Top Tens
       output$tableA2 <- renderDataTable({
         req(input$A3)
         req(input$A4)
         a_top_count = actors %>% filter(Genre == input$A3 & Released_Year >= input$A4[1] & Released_Year <= input$A4[2]) %>% count(Stars) %>% rename('Total Film Credits' = n)
         a_top_count
       }, options = list(lengthChange = FALSE, searching = FALSE))
       
       output$tableA3 <- renderDataTable({
         req(input$A3)
         req(input$A4)
         a_top_avg = actors %>% filter(Genre == input$A3 & Released_Year >= input$A4[1] & Released_Year <= input$A4[2]) %>% group_by(Stars) %>% summarise('Average Film Rating' = round(mean(IMDB_Rating),digits = 1))
         a_top_avg
       }, options = list(lengthChange = FALSE, searching = FALSE))
       
       output$tableA4 <- renderDataTable({
         req(input$A3)
         req(input$A4)
         a_top_gross_avg = actors %>% filter(Genre == input$A3 & Released_Year >= input$A4[1] & Released_Year <= input$A4[2]) %>% group_by(Stars) %>% summarise('Average Gross (USD)' = round(mean(Gross),digits = 2))
         a_top_gross_avg = datatable(a_top_gross_avg, options = list(lengthChange = FALSE,searching = FALSE)) %>% formatCurrency('Average Gross (USD)')
         a_top_gross_avg
       })#, options = list(lengthChange = FALSE, searching = FALSE))
       
       output$tableA5 <- renderDataTable({
         req(input$A3)
         req(input$A4)
         a_toptop_gross = actors %>% filter(Genre == input$A3 & Released_Year >= input$A4[1] & Released_Year <= input$A4[2]) %>% group_by(Stars) %>% summarise('Total Gross (USD)' =sum(Gross))
         a_toptop_gross = datatable(a_toptop_gross, options = list(lengthChange = FALSE,searching = FALSE)) %>% formatCurrency('Total Gross (USD)')
         a_toptop_gross
       })#, options = list(lengthChange = FALSE, searching = FALSE))

    
       
       
       #D1 Table
       output$tableD1 <- renderDataTable({
         req(input$D1)
         director_table <- movies%>%filter(Director == input$D1)%>%
           select('Film Title' = Series_Title,
                  'Released Year' = Released_Year,
                  Genre,
                  'Gross Profit (USD)' = Gross,
                  'IMDB Rating' = IMDB_Rating,
                  Director,
                  Star1,Star2,Star3,Star4,
                  Runtime)
       })
       
       #D boxes
       output$boxD1 <- renderValueBox({
         d_count = sum(movies$Director == input$D2, na.rm = TRUE)
         valueBox(value = d_count, subtitle = 'Number of Credits')
       })
       output$boxD2 <- renderInfoBox({
         d_gen_max = names(which.max(table(actors$Genre[actors$Director == input$D2])))
         infoBox(title = 'Top Genre', value = d_gen_max)
       })
       
       output$boxD3 <- renderValueBox({
         d_mean_rat = round(mean(movies$IMDB_Rating[movies$Director == input$D2]), digits = 2)
         valueBox(value = d_mean_rat, subtitle = 'Average Rating')
       })
       output$boxD4 <- renderValueBox({
         d_mean_gross = round(mean(movies$Gross[movies$Director == input$D2]), digits = 2)
         valueBox(value = scales::dollar(as.numeric(d_mean_gross)), subtitle = 'Average Gross Profit (USD)')
       })
       
       output$boxD5 <- renderInfoBox({
         d_top_rat = max(movies$IMDB_Rating[movies$Director == input$D2])
         d_top_rat_title = movies$Series_Title[movies$IMDB_Rating == d_top_rat & movies$Director == input$D2][1]
         infoBox(title = 'Top Rating', value = d_top_rat, subtitle = d_top_rat_title)
       })
       output$boxD6 <- renderInfoBox({
         d_top_gross = max(movies$Gross[movies$Director == input$D2])
         d_top_gross_title = movies$Series_Title[movies$Gross == d_top_gross & movies$Director == input$D2][1]
         infoBox(title = 'Top Grossing Film (USD)',value = scales::dollar(as.numeric(d_top_gross)), subtitle = d_top_gross_title)
         
       })
       
       #D2 & D3
       output$plotD1 <- renderPlot({
         movies %>% filter(Director %in% input$D2)%>%
           ggplot(aes(as.numeric(Released_Year), as.numeric(Gross))) +
           geom_point(size = 4) + 
           scale_y_continuous(labels = scales::comma)+
           labs(x= 'Release Year',y='Gross Profit (USD)',title = 'Gross Profit vs Release Year')
       })
       
       output$plotD2 <- renderPlot({
         movies %>% filter(Director %in% input$D2)%>%
           ggplot(aes(as.numeric(Released_Year), as.numeric(IMDB_Rating))) +
           geom_point(size = 4) +
           scale_y_continuous(labels = scales::comma)+
           labs(x= 'Release Year',y='IMDB Rating',title = 'IMDB Rating vs Release Year')
       })
       
       #Actor Top Tens
       output$tableD2 <- renderDataTable({
         req(input$D3)
         req(input$D4)
         d_top_count = actors %>% filter(Genre == input$D3 & Released_Year >= input$D4[1] & Released_Year <= input$D4[2]) %>% count(Director) %>% rename('Total Film Credits' = n)
         d_top_count
       }, options = list(lengthChange = FALSE, searching = FALSE))
       
       output$tableD3 <- renderDataTable({
         req(input$D3)
         req(input$D4)
         d_top_avg = actors %>% filter(Genre == input$D3 & Released_Year >= input$D4[1] & Released_Year <= input$D4[2]) %>% group_by(Director) %>% summarise('Average Film Rating' = round(mean(IMDB_Rating),digits = 1))
         d_top_avg
       }, options = list(lengthChange = FALSE, searching = FALSE))
       
       output$tableD4 <- renderDataTable({
         req(input$D3)
         req(input$D4)
         d_top_gross_avg = actors %>% filter(Genre == input$D3 & Released_Year >= input$D4[1] & Released_Year <= input$D4[2]) %>% group_by(Director) %>% summarise('Average Gross (USD)' = round(mean(Gross),digits = 2))
         d_top_gross_avg = datatable(d_top_gross_avg, options = list(lengthChange = FALSE, searching = FALSE)) %>% formatCurrency('Average Gross (USD)')
         d_top_gross_avg
       })#, options = list(lengthChange = FALSE, searching = FALSE))
       
       output$tableD5 <- renderDataTable({
         req(input$D3)
         req(input$D4)
         d_toptop_gross = actors %>% filter(Genre == input$D3 & Released_Year >= input$D4[1] & Released_Year <= input$D4[2]) %>% group_by(Director) %>% summarise('Total Gross (USD)' =sum(Gross))
         d_toptop_gross = datatable(d_toptop_gross, options = list(lengthChange = FALSE, searching = FALSE)) %>% formatCurrency('Total Gross (USD)')
         d_toptop_gross
       })#, options = list(lengthChange = FALSE, searching = FALSE))
}
#Run
shinyApp(ui = ui, server = server)

