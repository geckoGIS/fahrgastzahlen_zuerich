library(shiny)
#install.packages("DT") 
#install.packages("rgdal")
#install.packages("leaflet")
#install.packages("tidyverse")
#install.packages('rsconnect')
library(DT)
library(rgdal)
library(leaflet)
#library(dplyr)
library(ggplot2)
library(tidyverse)


library(rsconnect)

rsconnect::setAccountInfo(name='gecko-gis',
                          token='75307464DF8EF5A8837A64607AB25A6C',
                          secret='oa4YRnIxiL0re8rWfDIUxS3GD74IJ3K+EL8LKVyS')

data <- read.csv("fahrgastzahlen_zuerich_2018.csv")



# ui object
ui <- fluidPage(
  titlePanel(p("Fahrgastzahlen in Zürich 2018", style="color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      img(src = "gecko_logo.png", width = "40%", height = "40%"),
      p("Created by:", a("gecko-GIS", href = "http://www.gecko-gis.com")),
      p("Data from:", a("Open Data - Stadt Zürich", href = "https://data.stadt-zuerich.ch/dataset/vbz_fahrgastzahlen_ogd")),
      p("Infos:", a("vbzonline", href = "https://vbzonline.ch/zahlen-bitte/")),
      
      helpText("Fahrgastzahlen (Tram, Bus) in Zürich pro Haltestelle, 2018"),
      
      selectInput(inputId = "var", 
                  label = "Wähle aus:",
                  choices = c("Summe Ein-/Ausstiege werktags (DWV)", "Summe Ein-/Ausstiege (DTV)",
                              "Einstiege werktags (DWV)", "Ausstiege werktags (DWV)",
                              "Einstiege (DTV)", "Ausstiege (DTV)"),
                  selected = "Summe Ein-/Ausstiege werktags (DWV)"),
      helpText(HTML("<b>Abkürzungen:</b></br>"),
               HTML("<b>DWV</b> = Durchschnittlicher Werktagverkehr</br>"),
               HTML("<b>DTV</b> = Durchschnittlicher täglicher Verkehr</br>"))
      ),
    mainPanel(
      tabsetPanel(tabPanel("map", leafletOutput("map",height = 500)),
                  tabPanel("data", DTOutput(outputId = "table")),
                  tabPanel("top10", plotOutput("top10")))
    )
  )
  
)





# server()
server <- function(input, output){
  output$table <- renderDT(data)
  output$map <- renderLeaflet({
    if(input$var=="Summe Ein-/Ausstiege werktags (DWV)"){yvar <- data$SUM_DWV}
    if(input$var=="Summe Ein-/Ausstiege (DTV)"){yvar <- data$SUM_DTV}
    if(input$var=="Einstiege werktags (DWV)"){yvar <- data$EIN_DWV}
    if(input$var=="Ausstiege werktags (DWV)"){yvar <- data$AUS_DWV}
    if(input$var=="Einstiege (DTV)"){yvar <- data$EIN_DTV}
    if(input$var=="Ausstiege (DTV)"){yvar <- data$AUS_DTV}
    
    color_pal <- colorNumeric(palette = ("RdPu"), 
                              domain = yvar, 
                              reverse = FALSE)

    legend.title <- paste(paste0(input$var, " ("), 
                          round(min(yvar, na.rm=T), 2), " - ", 
                          round(max(yvar, na.rm=T), 2), ")", sep="")

    m <- leaflet(data) %>%
      addProviderTiles('OpenStreetMap.Mapnik', group='OpenStreetMap Mapnik') %>%
      addProviderTiles('Esri.WorldImagery',group='Esri Imagery') %>%
      addLayersControl(baseGroups=c('OpenStreetMap Mapnik','Esri Imagery'),
                       options = layersControlOptions(collapsed = F, autoZIndex =
                                                        T)) %>% 
      setView(lng=8.55, lat=47.36 , zoom=12)%>% 
      addCircles(~lng, ~lat, 
                 popup = paste("<strong>",data$CHSTNAME,"</strong>", "<br>", input$var,": ",round(yvar)), weight = 1.5, radius= ~ sqrt(yvar)/1.2, fillColor=~color_pal(yvar), color="black", stroke=TRUE,  fillOpacity = 0.8)%>% 
      addLegend("bottomright", pal = color_pal, values = ~yvar, 
              title = legend.title, opacity = 1)
    
    m 
  })
  
  # TOP10 Ein und Ausstiege pro Haltestelle
  # Die Haltestellen-Hitparade zeigt die grössten Begegnungszonen der Stadt
  
  output$top10 <- renderPlot({
    if(input$var=="Summe Ein-/Ausstiege werktags (DWV)"){yvar <- "SUM_DWV"}
    if(input$var=="Summe Ein-/Ausstiege (DTV)"){yvar <- "SUM_DTV"}
    if(input$var=="Einstiege werktags (DWV)"){yvar <- "EIN_DWV"}
    if(input$var=="Ausstiege werktags (DWV)"){yvar <- "AUS_DWV"}
    if(input$var=="Einstiege (DTV)"){yvar <- "EIN_DTV"}
    if(input$var=="Ausstiege (DTV)"){yvar <- "AUS_DTV"}
    

    top10 <- data %>%
      arrange(!!as.name(yvar)) %>%
      mutate(HST_KURZ=factor(HST_KURZ, levels=HST_KURZ)) %>%
      select(!!as.name(yvar), HST_KURZ,  CHSTNAME)%>%
      top_n(10,!!as.name(yvar))
    ggplot(top10, aes(x=HST_KURZ, y=!!as.name(yvar))) + geom_bar(stat='identity', position = 'dodge') + coord_flip() + theme_light()+
      geom_text(aes(label = CHSTNAME), color = "white", hjust=2) + geom_text(aes(label = round(!!as.name(yvar), digits = 0)), color = "blue", hjust=-0.02)
    
    

  })

}


# shinyApp()
shinyApp(ui = ui, server = server)
