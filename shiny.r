## Application shiny
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(RCurl)

library(viridis)
library(tidyverse)


variable <-F
URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
data <- read.csv(text = URL, check.names = F)
names(data)
url <- "https://twitter.com/intent/tweet?text=Hello%20world&url=https://shiny.rstudio.com/gallery/widget-gallery.html/"


# https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/

# countries <- readOGR(dsn ="ne_50m_admin_0_countries", 
#                      layer = "ne_50m_admin_0_countries", 
#                      encoding = "utf-8",use_iconv = T,
#                      verbose = FALSE)

#save(countries, file="shapeFile.RData")
load("shapeFile.RData")

data$`Country/Region`<-as.character(data$`Country/Region`)
data$`Country/Region`[data$`Country/Region`=="Macau"]<- "Macao"
data$`Country/Region`[data$`Country/Region`=="Mainland China"]<- "China"
data$`Country/Region`[data$`Country/Region`=="South Korea"]<- "South Korea"
data$`Country/Region`[data$`Country/Region`=="North Macedonia"]<- "Macedonia"
data$`Country/Region`[data$`Country/Region`=="Czech Republic"]<- "Czechia"
data$`Country/Region`[data$`Country/Region`=="Dominican Republic"]<- "Dominican Rep."
data$`Country/Region`[data$`Country/Region`=="UK"]<- "United Kingdom"
data$`Country/Region`[data$`Country/Region`=="Gibraltar"]<- "United Kingdom"
data$`Country/Region`[data$`Country/Region`=="US"]<- "United States"
data$`Country/Region`[data$`Country/Region`=="Saint Barthelemy"]<- "St-Barthélemy"

data$`Country/Region`[data$`Country/Region`=="Faroe Islands"]<- "Faeroe Is."
data$`Country/Region`[data$`Country/Region`=="Bosnia and Herzegovina"]<- "Bosnia and Herz."
data$`Country/Region`[data$`Country/Region`=="Vatican City"]<- "Vatican"

data$`Country/Region`[data$`Country/Region`=="Republic of Ireland"]<- "Ireland"


data$Pays<-as.character(countries$NAME[charmatch(data$`Country/Region`,countries$NAME)])





data$`Country/Region`[is.na(data$Pays)]

population<- read.csv2("pop.csv",stringsAsFactors = F)

data$Pays[! data$Pays%in%population$pays]

#https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population

population$pays<-as.character(countries$NAME[charmatch(population$Country,countries$NAME)])

dataPays<- data%>%dplyr::select(-`Province/State`, -Lat, -Long,-`Country/Region`)%>%group_by(Pays)%>%summarise_each(sum)




jour<-names(dataPays%>%select(contains( "/")))
jourDate<- as.Date(jour, "%m/%d/%y")
names(dataPays)[str_detect(names(dataPays), "/")]<-format.Date(jourDate, "%m/%d/%y")




dataPays$Pays<-as.character(dataPays$Pays)
dataPays<-left_join(data.frame(Pays = countries$NAME%>%as.character(), Pop =countries$POP_EST%>%as.character()%>%as.numeric()),dataPays)


maxTotal<- max(dataPays%>%select(-Pop)%>%select_if(is.numeric), na.rm = T)
maxTotalPrevalence<- max(dataPays%>%select(-Pop)%>%select_if(is.numeric)%>%mutate_all(function(x) x/dataPays$Pop*100000), na.rm = T)

dataPays[is.na(dataPays)]<- 0



arrondi<- function(x) 10^(ceiling(log10(x)))


ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}",
               HTML(  ".panel-default {background-color: rgb(256, 256, 256,0.5);
               padding : 10px;;}
               .panel-title {background-color: rgb(256, 256, 256,0.8);
               padding : 10px;
               border-style: solid;
               border-color: grey;}
               .panel-credits {background-color: rgb(256, 256, 256,1);
               padding : 15px;
               border-style: solid;
               border-color: black;}
               ")
               
),
leafletOutput("map", width = "100%", height = "93%"),
column(10,HTML("<b><a href='https://www.linkedin.com/in/thibaut-fabacher'>Thibaut FABACHER</a></b></br>
               <i>Groupe Methode en Recherche Clinique (Pr. MEYER), Laboratoire de Biostatistique (Pr. SAULEAU)</br> CHU STRASBOURG</i>")), 
column(2, br(),checkboxInput("credits", "Credits", FALSE)),


absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
              
              uiOutput("Slider"),
              helpText("The detail of each country can be obtained by clicking on it."), 
              radioButtons("variable", choices =  c("Total cases", 'Total cases/population',"New cases over period","New cases over period/population" ), label = "Indicator"),
              
              checkboxInput("legend", "Show legend", TRUE)
),
uiOutput("Credits"),
absolutePanel(id = "name",class = "panel panel-title",top  = 10, left  = 100, HTML("<h1>COVID-19 outbreak</h1>"),draggable = T)
)

server <- function(input, output, session) {
    
    # Reactive expression for the data subsetted to what the user selected
    
    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    
    
    
    
    output$map <- renderLeaflet({
      
      
      
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        
        leaflet(data = countries) %>%
            
            setView(0, 30, zoom = 3)
        
        
    })
    
    
    pal <- colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), domain = c(0,log(arrondi(maxTotal))))
    
    pal2 <- colorNumeric(c("#FFFFFFFF" ,rev(inferno(256))), domain = c(0,log(arrondi(maxTotalPrevalence))))
    
    observe({
        
    if (!is.null(input$day1)) {
      indicator<-format.Date(input$day1, "%m/%d/%y")
      
    }else{
      indicator = format.Date(max(jourDate), "%m/%d/%y")
    }
        
    
      if (!is.null(input$day2)) {
        indicator2<-format.Date(input$day2-c(1,0), "%m/%d/%y")

      }else{
        indicator2 =format.Date(c(min(jourDate)-1,max(jourDate)), "%m/%d/%y")
      }

        
        variable<- input$variable

        if(variable =="Total cases/population"){
          # nCases
          countries2 <- merge(countries, 
                              dataPays, 
                              by.x = "NAME", 
                              by.y = "Pays",                    
                              sort = FALSE)
            country_popup <- paste0("<strong>Country: </strong>",
                                    countries2$NAME,
                                    "<br><strong>",
                                    "Total cases/population :",


                                    " </strong>",
                                    round(countries2[[indicator]]/countries2$Pop*100000,2)," /100 000")

            
            leafletProxy("map", data = countries2)%>%
                addPolygons(fillColor = pal2(log((countries2[[indicator]]/countries2$Pop*100000)+1)),
                            fillOpacity = 1,
                            color = "#BDBDC3",
                            weight = 1,
                            popup = country_popup)

        }else if(variable =="Total cases"){
          countries2 <- merge(countries, 
                              dataPays, 
                              by.x = "NAME", 
                              by.y = "Pays",                    
                              sort = FALSE)
          country_popup <- paste0("<strong>Country: </strong>",
                                  countries2$NAME,
                                  "<br><strong>",
                                  "Total cases :",
                                  
                                  
                                  " </strong>",
                                  round(countries2[[indicator]],2))
          
          
          leafletProxy("map", data = countries2)%>%
            addPolygons(fillColor = pal(log((countries2[[indicator]])+1)),
                        fillOpacity = 1,
                        color = "#BDBDC3",
                        weight = 1,
                        popup = country_popup)
          
          
        }else if(variable =="New cases over period"){
          
          dataPaysSel<-dataPays%>%select(Pays, Pop)
          if(indicator2[1] == format.Date(min(jourDate)-1, "%m/%d/%y")){
            
            dataPaysSel$ncases<-dataPays[,indicator2[2]]
          }else{
          dataPaysSel$ncases<-dataPays[,indicator2[2]]-dataPays[,indicator2[1]]
          
          }
          
          # nCases
          countries2 <- merge(countries, 
                              dataPaysSel, 
                              by.x = "NAME", 
                              by.y = "Pays",                    
                              sort = FALSE)
            country_popup <- paste0("<strong>Country: </strong>", 
                                    countries2$NAME, 
                                    "<br><strong>", 
                                    "New cases over period :", 
                                    
                                    
                                    " </strong>", 
                                    countries2$ncases)
            
            leafletProxy("map", data = countries2)%>%
                addPolygons(fillColor = pal(log(countries2$ncases+1)), 
                            fillOpacity = 1, 
                            color = "#BDBDC3", 
                            weight = 1, 
                            popup = country_popup)
        }else{
          
          dataPaysSel<-dataPays%>%select(Pays, Pop)
          if(indicator2[1] == format.Date(min(jourDate)-1, "%m/%d/%y")){
            
            dataPaysSel$ncases<-dataPays[,indicator2[2]]
          }else{
            dataPaysSel$ncases<-dataPays[,indicator2[2]]-dataPays[,indicator2[1]]
            
          }
          
          # nCases
          countries2 <- merge(countries, 
                              dataPaysSel, 
                              by.x = "NAME", 
                              by.y = "Pays",                    
                              sort = FALSE)
          country_popup <- paste0("<strong>Country: </strong>", 
                                  countries2$NAME, 
                                  "<br><strong>", 
                                  "New cases over period / population :", 
                                  
                                  
                                  " </strong>", 
                                  round(countries2$ncases/countries2$Pop*100000,2))
          
          leafletProxy("map", data = countries2)%>%
            addPolygons(fillColor = pal2(log(countries2$ncases/countries2$Pop*100000+1)), 
                        fillOpacity = 1, 
                        color = "#BDBDC3", 
                        weight = 1, 
                        popup = country_popup)
          
          
          
        }
        
        
        
        
        
        }
    
        
        )
    
    
    
    # Use a separate observer to recreate the legend as needed.
    observe({
     
        
        
        variable<- input$variable
        proxy <- leafletProxy("map", data = countries)
     
            
            # Remove any existing legend, and only if the legend is
            # enabled, create a new one.
            proxy %>% clearControls()
            if (input$legend) {
                if(variable %in% c("Total cases/population","New cases over period/population")){
                proxy %>% addLegend(position = "bottomright",
                                    pal = pal2,opacity = 1,
                                    bins = log(10^(seq(0,log10(arrondi(maxTotalPrevalence)),0.5))),
                                    value = log(1:10^(log10(arrondi(maxTotalPrevalence)))),
                                    data =log(1:10^(log10(arrondi(maxTotalPrevalence)))),
                                    labFormat = labelFormat(transform = function(x) round(exp(x)) ,suffix = " /100 000")
                                    
                )
            
        }else{
            
     
                
                proxy %>% addLegend(position = "bottomright",
                                    pal = pal,opacity = 1,
                                    bins = log(10^(0:log10(arrondi(maxTotal)))),
                                    value = log(1:10^(log10(arrondi(maxTotal)))),
                                    data = log(10^(0:log10(arrondi(maxTotal)))),
                                    labFormat = labelFormat(transform =  exp )
                                    
                )
            }       
        }
    

    })
    
    output$Slider<-renderUI({  
      if(input$variable %in% c("Total cases", "Total cases/population")){
      sliderInput("day1", "Day", min(jourDate), max(jourDate),
                value =  c(max(jourDate)),animate = T, step = 1
                
                #min(jourDate), 
      )}else{
        sliderInput("day2", "Day", min(jourDate), max(jourDate),
                    value =  c(max(jourDate)-7,max(jourDate)),animate = F, step = 1
                    
                    #min(jourDate), 
        )
        
      }
        
        })
    output$Credits <- renderUI({
        if (input$credits) {
            tagList(
                absolutePanel(
                    id = "name",
                    class = "panel panel-credits",
                    top = "45%",
                    left  = "45%",
                    HTML(
                        "<h1> Data Source : </h1>
                                                                                                 <p>
                                                                                                 <li>COVID-19 Cases : <a href='https://github.com/CSSEGISandData/COVID-19'>Johns Hopkins University</a></li>
  <li>World population : <a href='https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population'>Wikipedia</a></li>
  <li>Shapefile : <a href='https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/'>Natural Earth Data</a></li>

                                                                                                   </p>"
                    ),
                    draggable = T
                )
            )
            
        }
        
    })
    
}

shinyApp(ui, server)
