library(rgdal)
library(leaflet)
library(readxl)
library(tmap)
library(tmaptools)
library(sf)
library(shiny)
library(tidyverse)
library(htmlwidgets)
library(tigris)

#read data of incident cases at each council
region_data<-data.frame(read_excel("Data_Tables_LGA_Family_Incidents_Year_Ending_June_2021.xlsx",sheet=2))

#drop off unnecessary column Year.ending as they are the same value
region_data<-region_data[,!(names(region_data)%in%c("Year.ending"))]

region_data<-subset(region_data,Local.Government.Area!="Total")

#read suburb and council mapper
councilmapper <- read_excel("df_suburb.xlsx")

combine_df<-merge(x =councilmapper, y =region_data ,   
                  by.x = "Council", by.y = "Local.Government.Area",all = FALSE)  

#subset_df=combine_df[sample(nrow(combine_df), 10000), ]


suburb_shp<-st_read("vic_localities/vic_localities.shp")

suburb_shp<-suburb_shp[c("LOC_NAME","geometry")]

all_data <- geo_join(suburb_shp,combine_df, 
                     "LOC_NAME", "Suburb",how="inner")


ui<-fluidPage(
  titlePanel("Family Incident Cases and Rate for Victorian Suburbs from 2017 to 2021"),
  sidebarLayout(
    sidebarPanel(
      tags$a(h5("Family incident in Victoria"),
             selectInput("Selected_Year",
                         'Select the year you would like to observe:',
                         choices=c('2017','2018','2019','2020','2021'))
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Number of Cases",leafletOutput('cases')),
        tabPanel('Incident Rates',leafletOutput("rates"))
      )
    )
  )
)

server<-function(input,output){
  
  year_data<-reactive({
    w<-all_data%>% filter(Year==input$Selected_Year)
    return(w)
  })
  
  output$cases<-renderLeaflet({
    pal<-colorBin(palette='YlGn',4,domain=all_data$Family.Incidents)
    
    labels<-paste0(
      "<strong> Year: <strong> ",
      year_data()$Year,"<br/> ",
      "<strong> Suburb: <strong> ", 
      year_data()$LOC_NAME,"<br/> ",
      "<strong> Local Council: <strong> ", 
      year_data()$Council,"<br/> ",
      "<strong> Police Region: <strong> ", 
      year_data()$Police.Region,"<br/> ",
      "<strong> Incident Cases: <strong> ",
      year_data()$Family.Incidents,"<br/> ")%>%
      lapply(htmltools::HTML)
    
    year_data()%>%
      #st_transform(crs="+init=epsg:4326")%>%
      leaflet()%>%
      addProviderTiles(provider="CartoDB.Positron")%>%
      setView(144.964600,-37.020100,zoom=7)%>%
      addPolygons(label=labels,
                  stroke =FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal(year_data()$Family.Incidents),
                  highlightOptions = highlightOptions(weight=5,
                                                      fillOpacity = 1,
                                                      color="black",
                                                      opacity = 1,
                                                      bringToFront = TRUE))%>%
      
      addLegend("topright",
                pal=pal,
                values=~Family.Incidents,
                title="Incident Number",
                opacity = 0.7)
  })
  
   output$rates<-renderLeaflet({
     pal<-colorBin(palette='OrRd',4,domain=all_data$Rate.per.100.000.population)
  
     labels<-paste0(
       "<strong> Year: <strong> ",
       year_data()$Year,"<br/> ",
       "<strong> Suburb: <strong> ",
       year_data()$LOC_NAME,"<br/> ",
       "<strong> Local Council: <strong> ",
       year_data()$Council,"<br/> ",
       "<strong> Police Region: <strong> ",
       year_data()$Police.Region,"<br/> ",
       "<strong> Incident Rate: <strong> ",
       year_data()$Rate.per.100.000.population,"<br/> ")%>%
       lapply(htmltools::HTML)

     year_data()%>%
       #st_transform(crs="+init=epsg:4326")%>%
       leaflet()%>%
       addProviderTiles(provider="CartoDB.Positron")%>%
       setView(	144.964600,-37.020100,zoom=7)%>%
       addPolygons(label=labels,
                   stroke =FALSE,
                   smoothFactor = 0.5,
                   opacity = 1,
                   fillOpacity = 0.7,
                   fillColor = ~pal(year_data()$Rate.per.100.000.population),
                   highlightOptions = highlightOptions(weight=5,
                                                       fillOpacity = 1,
                                                       color="black",
                                                       opacity = 1,
                                                       bringToFront = TRUE))%>%
  
       addLegend("topright",
                 pal=pal,
                 values=~Rate.per.100.000.population,
                 title="Rate Per 100,000 Population",
                 opacity = 0.7)
   })
}

shinyApp(ui=ui,server = server)

