library(shiny)
library(readr)
library(dplyr)
library(tidyverse)
library(srvyr)
library(knitr)
library(reshape2)
library(magrittr)
library(lubridate)
library(readxl)
library(maps)

# read data

fema <- read.csv("shiny_data.csv")
fema %<>% mutate(Year = year(date))

# Cost of damage in different damage category
category_amount <- fema%>%
    group_by(damage_category)%>%
    srvyr::summarize(project_amount=sum(project_amount)/1000000,
              federal_share=sum(federal_share)/1000000)

final_hurr_category <- melt(category_amount,id.vars = c("damage_category"), variable.name = "damage_cost", value.name= "cost_dollar")

# Mapping for damage times and cost
hurr_federal <- fema%>%
    group_by(region,subregion,Year)%>%
    srvyr::summarize(federal_sum=sum(federal_share),count=n())

hurr_federal$federal_sum_million <- hurr_federal$federal_sum/1000000
map_states <- map_data("county", unique(hurr_federal$region))
map_states_border <- map_data("state",unique(hurr_federal$region))
final_hurr <- merge(hurr_federal,map_states,by=c("region","subregion"))

# The number of hurricanes
## bar chart
hurr_bar <- fema %>% 
    group_by(region, Year) %>%
    summarize(hurricanes=n_distinct(hurricane_id)) %>%
    arrange(desc(hurricanes))

hurr_bar$Year<- as.numeric(hurr_bar$Year)

## map
hurr_map<- fema %>% 
    group_by(region,subregion,Year) %>%
    summarize(hurricanes=n_distinct(hurricane_id)) %>%
    arrange(desc(hurricanes))

### load data
MainStates <- map_data("state")
AllCounty <- map_data("county")
hurr_map <- left_join(hurr_map, AllCounty, by = c("subregion"))
hurr_map$Year <- as.numeric(hurr_map$Year)

#shiny app UI
ui <- navbarPage("Hurricanes Gallery",
               
               navbarMenu("Frequency",
                          
                          tabPanel("Bar Chart",
                                   br(),
                                   titlePanel("Hurricane Declaration Year"),
                                   sidebarPanel(
                                       selectInput(
                                           inputId = "from",
                                           label = "From:",
                                           choices = sort(unique(hurr_bar$Year))
                                       ),
                                       uiOutput('ui_to')
                                       
                                   ),
                                   mainPanel(
                                       h3('The Frequencies of Hurricanes in Each State'),
                                       plotOutput('bar')
                                   )
                          ),
                          
                          
                          tabPanel("Map",
                                   br(),
                                   titlePanel("Hurricanes Declaration Year"),
                                   sidebarPanel(
                                       selectInput(
                                           inputId = "from1",
                                           label = "From:",
                                           choices = sort(unique(hurr_map$Year))
                                       ),
                                       uiOutput('ui_to_map')
                                   ),
                                   mainPanel(
                                       h3('The Number of Hurricanes in Each County'),
                                       plotOutput('map1')
                                   )
                          )
                          
               ),
               
               tabPanel("Project Amount Details",
                        br(),
                        sidebarPanel(
                            uiOutput('ui_year')
                        ),
                        mainPanel(
                            h3('Expenses for Different Years'),
                            plotOutput('map')
                        ),
                        mainPanel(
                            
                            plotOutput('map_count')
                        )
               ),
               

               tabPanel("DataTable",
                        fluidRow(
                            column(4,
                                   selectInput("disasterNumber",
                                               "Hurricane Id:",
                                               c("All",
                                                 unique(as.character(fema$hurricane_id))))
                            ),
                            column(4,
                                   selectInput("declarationYear",
                                               "Hurricane Declaration Year:",
                                               c("All",
                                                 unique(as.character(fema$Year))))
                            ),
                         
                            column(4,
                                   selectInput("damageCategory",
                                               "Damage Category:",
                                               c("All",
                                                 unique(as.character(fema$damage_category))))
                            )
                            
                        ),
                        DT::dataTableOutput("table")
               )
               
)



server <- function(input,output){
    plot_bar=reactive({
      
        bar_data<-hurr_bar%>%
            filter((Year>=input$from) & (Year<=input$to))%>%
            group_by(region) %>%
            summarize(frequency=sum(hurricanes))
      
        bar_plot<-ggplot(bar_data[rev(order(bar_data$frequency))[1:5],], 
                         aes(x=reorder(region,frequency), y=frequency,fill = region))+
            geom_bar(stat = "identity") +
            scale_fill_hue(c = 40) +
            ggtitle("The Frequencies of Hurricanes in Top 5 Severe States")+
            xlab("State")+ylab("Frequency")+
            theme(plot.title = element_text(hjust = 0.5),legend.position = "none") +
            coord_flip()
        
        ggpubr::ggarrange(bar_plot,ncol = 1)
        
    })
    output$bar=renderPlot(plot_bar())
    
    plot_map=reactive({
        hurr_map_data <- hurr_map%>%
            filter((Year>=input$from1) & (Year<=input$to_map))%>%
            group_by(subregion, long,lat,group)%>%
            summarize(hurricanes=sum(hurricanes))
        
        hurr_map3 <- ggplot() + 
            geom_polygon(data=AllCounty, aes(x=long, y=lat, group=group),
                         color="gray", fill="white", size = .1 ) + 
            geom_polygon(data = hurr_map, aes(x = long, y = lat, group = group,fill = `hurricanes`), color = "grey", size = 0.2, alpha = 1.6) +
            scale_fill_gradient(low="steelblue",high="blue")+
            geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),color="black", fill="white",  size = .5, alpha = .3) +
            ggtitle("The Number of Hurricanes in Each County") +
            xlab("Longtitude")+ylab("Latitude")+labs(fill="Count")+
            theme(plot.title = element_text(hjust = 0.5))
        ggpubr::ggarrange(hurr_map3,ncol = 1)
        
    })
    output$map1=renderPlot(plot_map())
    
    
    s_year=reactive({
        sort(unique(final_hurr$Year))
    })
    output$ui_year=renderUI({
        checkboxGroupInput(
            inputId = "year",
            label = "Year",
            choices = s_year(),
            selected = s_year()
        )
    })
    
    output$ui_to=renderUI({
        selectInput(
            inputId = "to",
            label = "To:",
            choices = as.character(subset(sort(unique(as.numeric(final_hurr$Year))),sort(unique(as.numeric(final_hurr$Year)))>input$from))
        )
        
    })
    
    
    output$ui_to_map=renderUI({
        selectInput(
            inputId = "to_map",
            label = "To:",
            choices = as.character(subset(sort(unique(as.numeric(final_hurr$Year))),sort(unique(as.numeric(final_hurr$Year)))>input$from1))
        )
        
    })
    
    plot_damage=reactive({
        options(
            ggplot2.continuous.colour = "viridis",
            ggplot2.continuous.fill = "viridis"
        )
        x1 <- filter(final_hurr,Year%in%input$year)
        p1 <- ggplot()+
            geom_polygon(x1, mapping = aes(x=long,y=lat,group=group, fill=federal_sum_million))+
            geom_path(map_states, mapping=aes(x=long, y=lat, group=group),color="grey")+
            geom_path(map_states_border, mapping=aes(x=long, y=lat, group=group),color="black")+
            labs(fill="Expense/million")+
            xlim(min(map_states$long),max(map_states$long))+
            ylim(min(map_states$lat),max(map_states$lat))+
            xlab("Longtitude")+ylab("Latitude")+
            ggtitle("Total Obligated Expenses in Each Area")+
            theme(plot.title = element_text(hjust = 0.5))
        
        ggpubr::ggarrange(p1,ncol = 1)
        
    })
    output$map=renderPlot(plot_damage())
    
    output$table <- DT::renderDataTable(DT::datatable({
        data <- fema
        if (input$disasterNumber != "All") {
            data <- data[data$disasterNumber == input$disasterNumber,]
        }
        if (input$declarationYear != "All") {
            data <- data[data$declarationYear == input$declarationYear,]
        }
        if (input$damageCategory != "All") {
            data <- data[data$damageCategory == input$damageCategory,]
        }
        data
    }))
    
    
}

shinyApp(ui = ui, server = server)
