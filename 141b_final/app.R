library(httr)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(tidyr)
library(shiny)
library(DT)       

#############summary of all countries############################################################################
endpoint = ("https://api.covid19api.com/countries")
r = GET(endpoint)
stop_for_status(r)
json <- content(r, as = "text", encoding = "UTF-8")
summary <- fromJSON(json) 
country_list = sort(unique(summary$Slug))

#function of check by 1 days #################################################################################################

lsq_1 = function(country){  
    lsq_1_10 = c("01","02","03","04","05","06","07","08","09")
    lsq_2_10 = c("02","03","04","05","06","07","08","09")
    lsq1 = c()
    lsq2 = c()
    lsq3 = c()
    lsq_date = c()
    #check by 5 days
    check_by_5__dd1 = lsq_1_10
    check_by_5_dd2 = lsq_2_10
    check_by_5__dd1 = append(check_by_5__dd1, as.character(c(10:28)))
    check_by_5_dd2 = append(check_by_5_dd2, as.character(c(10:28)))
    #loop
    dd2_index = 1
    for (i in 2:5){
        
        for (dd1 in check_by_5__dd1){
            mm2 = i
            dd2 = check_by_5_dd2[dd2_index]
            dd2_index = dd2_index+1
            if (dd1 == "28"){
                mm2 = i+1
                dd2 = "01"
                dd2_index = 1
            }
            
            endpoint = str_glue(("https://api.covid19api.com/country/{input}?from=2020-0{m1}-{dd1}T00:00:00Z&to=2020-0{m2}-{d2}T00:00:00Z"), 
                                input = country, m1 = i, m2 = mm2, d1 = dd1, d2 = dd2)
            #print(endpoint)}}
            r = GET(endpoint)
            stop_for_status(r)
            json <- content(r, as = "text", encoding = "UTF-8")
            summary <- fromJSON(json) %>%as.data.frame()
            lsq = summary %>% 
                group_by(Province)%>%
                summarise(confirmed = max(Confirmed),
                          dead = max(Deaths),
                          Recovered = max(Recovered))
            date = paste("2020-0",as.character(i),"-",as.character(dd1), "to", "2020-0",as.character(mm2),"-",as.character(dd2),sep= "")
            sum1 = sum(lsq$confirmed)
            sum2 = sum(lsq$dead)
            sum3 = sum(lsq$Recovered)
            lsq_date = append(lsq_date, date)
            lsq1 = append(lsq1, sum1)
            lsq2 = append(lsq2, sum2)
            lsq3 = append(lsq3, sum3)
        }
        
    }
    lsq_final = tibble( date = lsq_date,confirmed = lsq1, death = lsq2, recovered = lsq3)%>%as.data.frame()
    lsq_final 
}
#function that returns the summary of all countries##############################################################################################################
lsq_summary_all = function(){
    endpoint = ("https://api.covid19api.com/summary")
    r = GET(endpoint)
    stop_for_status(r)
    json <- content(r, as = "text", encoding = "UTF-8")
    summary <- fromJSON(json) 
    summary}
temp = lsq_summary_all()
summary_all_country = temp$Global%>%as.data.frame()
country_table = temp$Countries

#Function that returns input country's summary data ########################################################################################################

lsq_by_all = function(country){
    endpoint = str_glue("https://api.covid19api.com/total/country/{input}", input = country)
    r = GET(endpoint)
    stop_for_status(r)
    json <- content(r, as = "text", encoding = "UTF-8")
    summary <- fromJSON(json) %>%as.data.frame()
    summary = summary %>% arrange(desc(Confirmed))
    summary[1,]
}


#UI##########################################################################################################

ui <- fluidPage(
    
    headerPanel('COVID-19'),
    sidebarLayout( 
        sidebarPanel(                   
            selectInput('worc', "Country or world (change to country to see each country's infomation)", c("world","country"), selected = "world" ) ,
            selectInput('country', 'select country', c("-",country_list), selected = "-" ) ,
            selectInput('type', 'summary of the country or detail of the country', c("-","summary", "detail"), selected = "-" ),
            selectInput('dorp', 'data or plots', c("-", "data","trends"), selected = "-" ),
            "Some countries might not have records, try countries like China, Canada, or United States.",
            "Detailed data and plot of countries might take a few seconds to load, if you see ERROR pops out, please wait"
            
            
        ),
        
        mainPanel(   
            conditionalPanel(
                condition = "input.worc == 'world'",
                span(textOutput("text1"), style="color:blue"),
                plotOutput("piechart"),
                span(textOutput("text2"), style="color:blue"),
                DT::dataTableOutput("summary_all_country"),
                span(textOutput("text3"), style="color:blue"),
                DT::dataTableOutput("country_table")
                
            ),
            conditionalPanel(
                condition = "input.worc == 'country' & input.type == 'detail' & input.dorp == 'data'",
                span(textOutput("text5"), style="color:blue"),
                DT::dataTableOutput("result_detail")
            ),
            conditionalPanel(
                condition = "input.worc == 'country' & input.type == 'detail' & input.dorp == 'trends'",
                span(textOutput("text6"), style="color:blue"),
                plotOutput("lsq_by_5_plot1"),
                span(textOutput("text7"), style="color:blue"),
                plotOutput("lsq_by_5_plot2"),
                span(textOutput("text8"), style="color:blue"),
                plotOutput("lsq_by_5_plot3")
            ),
            conditionalPanel(
                condition = " input.worc == 'country' & input.type == 'summary' & input.dorp == 'data'",
                span(textOutput("text4"), style="color:blue"),
                DT::dataTableOutput("result_sum")
            ),
            conditionalPanel(
                condition = " input.worc == 'country' & input.type == 'summary' & input.dorp == 'trends'",
                span(textOutput("text9"), style="color:blue")
            )
        )
    ) 
)

#SERVER#####################################################################################################

server <- function(input, output) {
    ################TEXT################
    output$text1 = renderText("The top 10 countries that has largest new confirmed cases")
    output$text2 = renderText("The total number of cases in the world")
    output$text3 = renderText("The total number of cases in each countries")
    output$text4 = renderText("The summary data of cases(confirmed, death, and recovered)")
    output$text5 = renderText("The detailed data from Feb to present (by 1 day, might take few seconds to load, please be patient)")
    output$text6 = renderText("Trend plot of confirmed cases (might take few seconds to load, please be patient)")
    output$text7 = renderText("Trend plot of death cases")
    output$text8 = renderText("Trend plot of recovered cases")
    
    
    #################### DATA ##################################
    
    data1 = reactive({
        lsq_1(input$country)
    })
    
    data2 = reactive({
        lsq_by_all(input$country)
    })
    
    ######################### DETAIL COUNTRY ###############################
    
    #data detail text output
    output$result_detail =
        DT::renderDataTable({
            data1()
        })
    #plot output, by status
    output$lsq_by_5_plot1 = renderPlot({   
        ggplot(data1(), aes(date,confirmed)) +
            geom_point()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 20))
    })
    
    output$lsq_by_5_plot2 = renderPlot({   
        ggplot(data1(), aes(date,death)) +
            geom_point()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 20))
    })
    output$lsq_by_5_plot3 = renderPlot({   
        ggplot(data1(), aes(date, recovered)) +
            geom_point()+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 20))
    })
    ##The trend
    
    
    ################# SUMMARY WORLD #########################
    output$piechart = renderPlot({
        df = country_table %>% arrange(desc(NewConfirmed))
        df = df[1:5,]
        
        bp = ggplot(df, aes(x = 2, y = NewConfirmed, fill = Country)) +
            geom_bar( stat = "identity", color = "white") +
            coord_polar("y", start = 0)+
            xlim(0.5, 2.5)
        bp
    })
    
    output$summary_all_country = DT::renderDataTable({
        summary_all_country
    })
    
    output$country_table = DT::renderDataTable({
        country_table
    })
    
    ################# SUMMARY country ####################### 
    ########DATA ###########
    #data summary text output
    output$result_sum = DT::renderDataTable({
        data2()%>% select(Country,Confirmed,Deaths,Recovered,Active,Date)
    })
    
    ####### PLOT##########
    
    output$text9 = renderText("There's too little data to plot, check the detail plot :)")
    
}

####################### More #############################


############################ RUN ##################################################################################

shinyApp(ui = ui, server = server)

