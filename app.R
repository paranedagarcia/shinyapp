
library(shiny)
library("plyr")
library("dplyr")
library(plotly)
library(leaflet)
library(geojson)
library(geojsonio)
library(shiny)
library("rgdal")
library("DT")

#clg_data <- read.csv("ChG_countydata.csv")

# Read data
load("geodata.RData") # Hack: load comunas and regiones saved as R objects from a different server
# comunas <- geojsonio::geojson_read("comunas.json", what = "sp")
# regiones <- geojsonio::geojson_read("regiones.json", what = "sp")
ancestry <- read.csv("ChG_countydata_ancestry.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "estilos.css")
    ),
    # Application title
    titlePanel(""),
    navbarPage(
        title = 'Ancestry',
        
        tabPanel('Load data',
            sidebarLayout(
                sidebarPanel(
                    checkboxInput('header', 'First row as header', TRUE),
                    
                    radioButtons('sep', 'Separator field character',
                         c(Comma=',',
                           Semicolon=';',
                           Tab='\t'),
                         ','),
                    
                    radioButtons('quote', 'Text quote',
                      c(None='',
                        'Double Quote'='"',
                        'Single Quote'="'"),
                      '"'),
                    
                    fileInput('datafile', 'Choose file to upload',
                    accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain',
                        '.csv',
                        '.tsv')
                    ),
                    tags$p("Select file to upload, this must be contain a county column."),
                    tags$p("Wait for 'Upload complete' message."),
                    tags$p("Once loaded you can select Plot, table or visualize the maps."),
                    tags$p("To visualize the results, you must first upload the data file.")
                    ),
                
                mainPanel(
                    br(),
                    tags$img(src="images/frontis.jpg", height="150px"),
                    tags$h1("Ancestry"),
                    tags$h3("We are working to understand the diversity of the chilean genomes."),
                    br(),
                    tags$p(paste0(
                        "This site shows ancestry estimated by county and regio of Chile. Optionally, you can display your own data with our ancestry estimates."
                    )),
                    tags$p(paste0(
                        "Note: Load data in '.csv' format. The first column must be named 'County' and should have exact county names as shown in this file:"
                    )),
                    tags$a(href="Division-Politico-Administrativa.xls", "Territorial codes (Excel format)"),br(),br(),
                    tags$p("Download this test file with sample data.",
                    tags$a(href="ChG_countydata_userdata.csv", "ChG_countydata_userdata.csv (csv format).")),
                    br(), br(),tags$hr(), br(),
                    tags$img(src="images/logofondef.jpg", height="100px")
                    )
                )),
        
        tabPanel('Plots',
            sidebarLayout(
                sidebarPanel(width = 3,
                    uiOutput("titulo"),
                    uiOutput("fromCol"),
                    uiOutput("namex"),
                    uiOutput("toCol"),
                    uiOutput("namey")
                    #uiOutput("symbolxy")
                    #uiOutput("colorxy")
                    ),
                mainPanel(
                    plotlyOutput("xy", height = 650)
                    )
                )),
        
        tabPanel('Table', DT::dataTableOutput('table')),
        
        tabPanel('County',
           sidebarLayout(
               sidebarPanel(
                   uiOutput("mapvar"),
                   #uiOutput("maptags"),
                   #uiOutput("maptheme"),
                   uiOutput("mapcolor")
                   #actionButton("gomap", "See map")
                   ),
               mainPanel(
                   tags$p(" "),
                   leafletOutput("mymap", width = "100%", height = "650")
                   )
               ))
    )

)

# dictionary
# ancestry : archivo de datos
# filedata : objeto de datos como función
# dfile : objeto de datos
# titulo : 

# Define server logic required to draw a histogram
server <- function(input, output, ...) {

    options(shiny.sanitize.errors=TRUE)
   
    
    #-----------------------------------------------
    # capture file
    #-----------------------------------------------
    filedata <- reactive({
        infile <- input$datafile
        if (is.null(infile)) {
            outdata <- ancestry
        } else {
           outdata <- read.csv(infile$datapath)
          # outdata <- merge(ancestry, userdata, by=1, all=T)
        }
           return(outdata)
    })

    #-----------------------------------------------
    # PLOT
    #-----------------------------------------------
    output$titulo <- renderUI({
        dfile <-filedata()
        if (is.null(dfile)) return(NULL)
        textInput(inputId = "titulo", "Graph title", input$titulo, placeholder = "Graph title")
    })

    output$fromCol <- renderUI({
       dfile <-filedata() %>% select(AFR, EUR, EAS, AMR, AYM, MAP)
       if (is.null(dfile)) return(NULL)
       items=colnames(dfile)
       selectInput(inputId = "from", "X Variable from file:", items, selected="EUR")
    })

    output$toCol <- renderUI({
        dfile <-filedata() %>% select(AFR, EUR, EAS, AMR, AYM, MAP)
        if (is.null(dfile)) return(NULL)
        items=colnames(dfile)
        selectInput(inputId = "to", "Y Variable from file:",items, selected="AMR")
    })

    output$namex <- renderUI({
        dfile <-filedata()
        if (is.null(dfile)) return(NULL)
        textInput(inputId = "namex", "Nombre de variable X", input$from)
    })

    output$namey <- renderUI({
        dfile <-filedata()
        if (is.null(dfile)) return(NULL)
        textInput(inputId = "namey", "Nombre de variable Y", input$to)
    })

    output$symbolxy <- renderUI({
        dfile <-filedata()
        if (is.null(dfile)) return(NULL)
        simbolos <- c("circle","square","diamond","cross","hexagon","star")
        selectInput(inputId = "simbol", "Symbol:",simbolos, selected="circle")
    })

    output$colorxy <- renderUI({
        dfile <-filedata()
        if (is.null(dfile)) return(NULL)
        colores <- c("red", "blue", "green")
        selectInput(inputId = "color", "Color:",colores, selected="green")
    })
    #-----------------------------------------------
    # XY PLOT inicial
    #-----------------------------------------------
    output$xy <- renderPlotly({
        dfile <-filedata()
        if (is.null(dfile)) return(NULL)
        

        xvar <- dfile[,input$from]
        yvar <- dfile[,input$to]
        xylab <- as.character(dfile[,"County"])

        if(is.null(input$namex))
            xname = input$from
        else
            xname = input$namex
        if(is.null(input$namey))
            yname = input$to
        else
            yname = input$namey


        tipo <- "scatter"
        if(class(xvar)=="factor") {
            tipo <- "bar"
            yvar <- tapply(yvar, xvar, mean)
            xylab <- tapply(xylab, xvar, paste, collapse=", ")
            xvar <- as.factor(levels(xvar))
            dfile <- dfile[!duplicated(dfile[,input$from]), ,drop=T]
        }

        micolor <- c("red", "blue", "green")

        dfile %>%
            plot_ly(x = ~xvar, y= ~yvar, type = ~tipo, height = 650,
                     text = ~xylab, color=dfile, colors=micolor, mode="markers",
                     marker = list(size = 10,
                               color = 'rgba(255, 182, 193, .9)',
                               line = list(color = 'rgba(152, 0, 0, .8)',
                                           width = 1))) %>%
            layout(title=input$titulo, margin=list(b=150),
                   xaxis = list(title=xname),
                   yaxis = list(title=yname))
    })

    #-----------------------------------------------
    # TABLE
    #-----------------------------------------------
    output$table <- DT::renderDataTable({
        dfile <-filedata()
        if (is.null(dfile)) return(NULL)

       # tabla <- read.csv(inFile$datapath, header=input$header, sep=input$sep,  dec = input$dec)
        DT::datatable(
            dfile, options = list(
                lengthMenu = list(c(10, 15, 25, 50, 100), c('10', '15', '25', '50', '100')),
                pageLength = 15
            )
        )
    })

    #-----------------------------------------------
    # Output para mapa una vez cargado el archivo
    #-----------------------------------------------
    output$mapvar <- renderUI({
        dfile <-filedata() %>% select(AFR, EUR, EAS, AMR, AYM, MAP)
       # if (is.null(dfile)) return(NULL)
        items = colnames(dfile)
        selectInput(inputId="mapvar", "Variable:",items, selected="AMR")
    })

    output$maptags <- renderUI({
        dfile <-filedata()
        if (is.null(dfile)) return(NULL)
        items=colnames(dfile)
        selectInput(inputId = "mavtags", "Tags:",items, selected="AMR", multiple = TRUE)
    })

    output$mapcolor <- renderUI({
        dfile <-filedata()
        if (is.null(dfile)) return(NULL)
        colors <- c("Blues", "Reds", "Greens", "viridis", "magma", "inferno", "plasma")
        selectInput(inputId="mapcolor", "Palette of colors:",colors, selected="Blues")
    })

    output$maptheme <- renderUI({
        dfile <-filedata()
        if (is.null(dfile)) return(NULL)
        maptemas <- c("CartoDB.Positron", "MapBox","Stamen.Watercolor","Stamen.TonerLines", "Hydda.Base")
        selectInput(inputId="maptheme", "Theme:",maptemas, selected="Hydda.Base")
    })



    #-----------------------------------------------
    # Map chart
    # usa comunas.json con geojsonio
    #-----------------------------------------------

    output$mymap <- renderLeaflet({

 if(is.null(input$mapvar)) return()
        dfile <-filedata()
        # variable
        mapvar <- "AMR"
        if(!is.null(input$mapvar)) {
          mapvar <- input$mapvar
        }
        midata <- data.frame(dfile[,"County"], dfile[,"N"], dfile[,mapvar])

        minewdata <- midata

      if(length(colnames(minewdata))==0) return()
        colnames(minewdata) <- c("NOM_COM", "N", "variable")

        tema <- "Hydda.Base"

        datos <- merge(comunas, minewdata, by.x='NOM_COM', by.y='NOM_COM', all=FALSE)

        pal <- colorNumeric(input$mapcolor, domain = datos$variable,na.color="#808080")

     showNotification("Loading map...", duration=25)
     
        labels <- sprintf(
                 "<strong>%s</strong><br/>%s: %g<br/>%s: %g ",
                 datos$NOM_COM, "Samples", datos$N, mapvar , datos$variable
            ) %>% lapply(htmltools::HTML)

        leaflet(datos) %>%
            setView(lng = -70.8067798, lat = -33.5107981, zoom = 6) %>%
            addProviderTiles(tema) %>%
            addPolygons(
                fillColor= ~pal(variable),
                stroke = TRUE, weight = 1, color="#999",
                smoothFactor = .5, fill = TRUE,
                fillOpacity = 1,
                dashArray = "3",
                highlight = highlightOptions(
                    weight = 1,
                    color = "#DDD",
                    dashArray = "",
                    fillOpacity = 0.4,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
                ) %>%
            addPolygons(data = regiones,
                stroke = TRUE, weight = 1.5,
                smoothFactor = 0.5, fill = FALSE,
                fillOpacity = 0.8,
                color = "#000") %>%
            addLegend(pal = pal, values = ~variable, opacity = 0.7, title = NULL, position = "bottomright")
    })
    # fin mapa
}

# Run the application
shinyApp(ui = ui, server = server)
