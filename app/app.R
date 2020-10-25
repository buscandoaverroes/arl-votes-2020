# app.R

library(tidyverse)
library(sf)
library(leaflet)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(simplevis)
library(shinydashboard)
library(plotly)
library(RColorBrewer)
library(lwgeom)
library(markdown)
library(knitr)

# opening
#root.data     <- "/Volumes/la-republic"
#setwd()
load("data/arl-vote2020.Rdata")


vote <- vote.data
vote.pr <- vote %>%
  filter(`Precinct Name` != "Arlington Totals")

# update
up.date <- "17 Oct, 2020"



# define basemap 
# basemap <- leaflet() %>% 
#   addTiles() %>% 
#   setView(-77.1, 38.88, zoom = 12)

# misc values 


# define popup vars to display 
# later, make this in to interactive select multiple.
popupvars <- c("Precinct Name", "Total Votes", "Mail Received", "Early Voted", 
               "Active Turnout", "Turnout by Mail", "Turnout by Early Voting",
               "Mail Ballot Return Rate", "Early to Mail Ratio", "Early to Mail Turnout Ratio")

popupvaroptions <-c("Precinct Name", "Total Votes", "Mail Received", "Mail Ballots Outstanding",
                    "Mail Ballots Counted", "Early Voted", 
                    "Active Turnout", "Turnout by Mail", "Turnout by Early Voting",
                    "Mail Ballot Return Rate", "Early to Mail Ratio", "Early to Mail Turnout Ratio",
                    "Mail Ballots Requested", "Percent Mail Counted",
                    "Precinct Share of All Votes", "Precinct Share of Early Votes",
                    "Precinct Share of Oustanding Mail Votes", "Precinct Share of Mail Ballots Received",
                    "Active Registered")

arltotalvars <- c("`Active Turnout`", "`Turnout by Mail`", "`Turnout by Early Voting`",
                  "`Mail Ballot Return Rate`", "`Early to Mail Ratio`", "`Early to Mail Turnout Ratio`",
                  "`Mail Ballots Counted`", "`Early Voted`")

polarplot <- c("Active Turnout", "Turnout by Mail", "Turnout by Early Voting",
               "Mail Ballot Return Rate", "Early to Mail Ratio", "Early to Mail Turnout Ratio")

heatmapvars <- c("Total Votes", "Mail Received", "Early Voted",
                 "Active Registered", "Military", "Overseas", "Federal",
                 "Mail Ballots Requested", "Early to Mail Ratio")




pop.filter <- vote[vote$date %in% "2020-10-18",] %>%
  select(popupvars)

# # forheatmap
# vote.heatmap <- vote %>%
#   select(heatmapvars) %>%
#   as.data.frame()
# 
# rownames(vote.heatmap) <- pop.filter$`Precinct Name`



# color 'n-tiles'
ntiles <- c(0, 1/6, 1/3, 0.5, 2/3, 5/6, 1)

# other color settings 
color.av <- '#778899'
color.1  <- 'Aqua' #
color.1mk<- 'Teal' 
color.2  <- 'Violet' #
color.2mk<- 'Indigo'
color.3  <- 'Salmon' #
color.3mk<- 'DarkRed'
color.blk<- '#000000'

a.sp = 0.3   # alpha for scatter polar
w.sp = 2      # width of scatter polar boundary lines

# hovertext settings
ht.polar <- paste("%{theta}",
                  "<br>Normalized Value: %{r:.2f}")

# correlation data 
# ## from https://stackoverflow.com/a/13112337/4747043
# r <- cor(vote.heatmap)
# 
# cor.test.p <- function(x) {
#   FUN <- function(x, y) cor.test(x, y)[["p.value"]]
#   z <- outer (
#     colnames(x), 
#     colnames(x),
#     Vectorize(function(i,j) FUN(x[,i], x[,j]))
#   )
#   dimnames(z) <- list(colnames(x), colnames(x))
#   z
# }
# 
# p <- cor.test.p(vote.heatmap)










# - - - - - - - - - - - - - - - - - - - - - - - - - -    UI    - - - - - -  ----



# Define UI for application that draws a histogram
ui <- navbarPage(
        theme = shinytheme('superhero'),
        collapsible = TRUE,
        'ArlVotes2020', 
        id = 'nav',
        header = tagList(useShinydashboard()),
        
        
        tabPanel(
          "Timeline", 
          fluidPage(
            fluidRow(
                    valueBox(paste0(arl.tot$`Active Turnout`, '%'), 'Estimated Turnout', width = 4, color = 'yellow'),
                    valueBox(prettyNum(arl.tot$`Total Votes`, big.mark = ','),
                             'Total Votes', width = 4, color = 'yellow'),
                    valueBox(up.date, "Data Update", color = 'fuchsia')),
            fluidRow(plotlyOutput('timeline1')),
            fluidRow(plotlyOutput('timeline2')),
              
              
              # timeline input panel ---- 
              absolutePanel(
                id    = 'controlpanel1',
                #class = "panel panel-default",
                bottom = 50, 
                right  = 10,
                width  = 200,
                fixed  = TRUE,
                draggable = TRUE,
                height = 'auto',
                cursor = "move",
                style = "opacity: 0.9",
                
                wellPanel(
                  
                  HTML(
                    markdownToHTML(fragment.only = TRUE,
                                   text = c("Select Precints"))
                  ),
                  
                  pickerInput(
                    'timeline.in1',
                    choices = arltotalvars,
                    selected = "`Total Votes`",
                    multiple = FALSE,
                    width = 'auto',
                    options = list(
                      liveSearch = TRUE,
                      liveSearchNormalize = TRUE,
                      liveSearchStyle = 'contains',
                      selectOnTab = TRUE,
                      showTick = TRUE,
                      title = "Title",
                      virtualScroll = TRUE,
                      width = 'auto',
                      dropupAuto = TRUE
                    )
                  ),
                  
                  pickerInput(
                    'timeline.in2',
                    choices = sort(unique(vote$`Precinct Name`)),
                    selected = "Abingdon",
                    multiple = FALSE,
                    width = 'auto',
                    options = list(
                      liveSearch = TRUE,
                      liveSearchNormalize = TRUE,
                      liveSearchStyle = 'contains',
                      selectOnTab = TRUE,
                      showTick = TRUE,
                      title = "Title",
                      virtualScroll = TRUE,
                      width = 'auto',
                      dropupAuto = TRUE
                    )
                  ),
                  
                    pickerInput(
                    'timeline.in3',
                    choices = sort(unique(vote$`Precinct Name`)),
                    selected = "Ballston",
                    multiple = FALSE,
                    width = 'auto',
                    options = list(
                      liveSearch = TRUE,
                      liveSearchNormalize = TRUE,
                      liveSearchStyle = 'contains',
                      selectOnTab = TRUE,
                      showTick = TRUE,
                      title = "Title",
                      virtualScroll = TRUE,
                      width = 'auto',
                      dropupAuto = TRUE
                    )
                  ),
                
                  
                ) # end wellpanel
              ) # end absolute panel
              
          ) # end fluidpage
          
          
        ), # end tab panel timeline
        
        
        # tabPanel( # map (coming soon)  ----
        #     "Map",
        #     fluidRow(
        #       valueBox(paste0(rate, '%'), 'Estimated Turnout', width = 4, color = 'yellow'),
        #       valueBox(prettyNum(vote.turnout$tot.votes, big.mark = ','),
        #                'Total Votes', width = 4, color = 'yellow'),
        #       valueBox(up.date, "Data Update", color = 'fuchsia')),
        #     
        #     
        #     leafletOutput(
        #       "map",
        #       width = '100%', 
        #       height = 600
        #       ),
        #     
        #     absolutePanel(
        #       id    = 'controlpanel',
        #       #class = "panel panel-default",
        #       top   = 220, 
        #       left  = 70,
        #       width = 250,
        #       fixed = TRUE,
        #       draggable = TRUE,
        #       height = 'auto',
        #       cursor = "move",
        #       wellPanel(
        #         HTML(
        #           markdownToHTML(fragment.only = TRUE,
        #                          text = c("Map Options"))
        #         ),
        #         
        #         pickerInput( 
        #           inputId =  'plotvar',
        #           label  =  'Variable to Plot',
        #           choices = list(Votes = c("Total Votes" = "Total Votes",
        #                                    "Mail Ballots" = "Mail Received",
        #                                    "Mail Ballots Counted" = "Mail Ballots Counted",
        #                                    "Mail Ballots Outstanding" = "Mail Outstanding",
        #                                    "Early Voting Ballots" = "Early Voted"
        #                                    ),
        #                          Turnout = c("Active Voter Turnout" = "Active Turnout",
        #                                      "Turnout via Mail" = "Turnout by Mail",
        #                                      "Turnout via Early Voting" = "Turnout by Early Voting"),
        #                          Ratios = c("Mail Ballot Return Rate" = "Mail Ballot Return Rate", 
        #                                     "Early-to-Mail Ratio" = "Early to Mail Ratio", 
        #                                     "Early-to-Mail Turnout Ratio" = "Early to Mail Turnout Ratio",
        #                                     "Share of All Outstanding Mail Ballots"="Precinct Share of Oustanding Mail Votes",
        #                                     "Share of All Early Votes" = "Precinct Share of Early Votes",
        #                                     "Percent Received Mail Counted" = "Percent Mail Counted")),
        #           selected = "Total Votes", 
        #           multiple = FALSE,
        #           width = 215
        #         ),
        #         
        #       
        #         pickerInput(
        #           'poplbls', 
        #           "Select Popup Information",
        #           choices = popupvaroptions,
        #           selected = c("Precinct Name", "Total Votes", "Mail Received", "Early Voted"),
        #           multiple = TRUE,
        #           width = 'auto',
        #           options = list(
        #             liveSearch = TRUE,
        #             liveSearchNormalize = TRUE,
        #             liveSearchStyle = 'contains',
        #             selectOnTab = TRUE,
        #             showTick = TRUE,
        #             title = "Title",
        #             virtualScroll = TRUE,
        #             width = 'auto',
        #             dropupAuto = TRUE
        #           )
        #         ),
        #         
        #         
        #         style = "opacity: 0.9",
        #         
        #         # output 
        #        # plotlyOutput('polar')
        #       ) # close well panel
        #       
        #       
        #       
        #       # secondary absolute panel for additional graphs, info, etc?
        #     ) # close absolute panel
        #     
        #     ), # close map tab panel 
        
        tabPanel( # stats ----
          "Stats", 
          fluidPage(
            tags$h2("Key Voting Statistics by Precinct"),
            tags$h5("Select up to three precincts to display normalized statistics. Click on the precinct 
                    names in the legend to toggle individual polygons."),
            tags$br(),
            
            
            plotlyOutput('polar', width = '100%', height = '600px'),
            

              
              absolutePanel(
                id    = 'controlpanel2',
                #class = "panel panel-default",
                bottom = 50, 
                right  = 10,
                width  = 200,
                fixed  = TRUE,
                draggable = TRUE,
                height = 'auto',
                cursor = "move",
                style = "opacity: 0.9",
                
                wellPanel(
                  
                  HTML(
                    markdownToHTML(fragment.only = TRUE,
                                   text = c("Select Precints"))
                  ),
                  
                  pickerInput(
                    'stat.in1',
                    choices = sort(unique(sp.norm$`Precinct Name`)),
                    selected = "Arlington Average",
                    multiple = FALSE,
                    width = 'auto',
                    options = list(
                      liveSearch = TRUE,
                      liveSearchNormalize = TRUE,
                      liveSearchStyle = 'contains',
                      selectOnTab = TRUE,
                      showTick = TRUE,
                      title = "Title",
                      virtualScroll = TRUE,
                      width = 'auto',
                      dropupAuto = TRUE
                    )
                  ),
                  
                  pickerInput(
                    'stat.in2',
                    choices = sort(unique(sp.norm$`Precinct Name`)),
                    selected = "Abingdon",
                    multiple = FALSE,
                    width = 'auto',
                    options = list(
                      liveSearch = TRUE,
                      liveSearchNormalize = TRUE,
                      liveSearchStyle = 'contains',
                      selectOnTab = TRUE,
                      showTick = TRUE,
                      title = "Title",
                      virtualScroll = TRUE,
                      width = 'auto',
                      dropupAuto = TRUE
                    )
                  ),
                  
                  
                  
                  pickerInput(
                    'stat.in3',
                    choices = sort(unique(sp.norm$`Precinct Name`)),
                    selected = "Ballston",
                    multiple = FALSE,
                    width = 'auto',
                    options = list(
                      liveSearch = TRUE,
                      liveSearchNormalize = TRUE,
                      liveSearchStyle = 'contains',
                      selectOnTab = TRUE,
                      showTick = TRUE,
                      title = "Title",
                      virtualScroll = TRUE,
                      width = 'auto',
                      dropupAuto = TRUE
                    )
                  ),
                  
                  
                  sliderInput(
                    'sp.alpha',
                    "Fill Opacity",
                    min = 0.1,
                    max = 1,
                    value = 0.4,
                    step = 0.1,
                    ticks = FALSE
                  )
                  
                  
                )
              ),
            
            tags$h2("On Interpretation"), 
            tags$h6("Normalization transforms each dimension independently such that highest value in the data
                    is 1 and the lowest value is zero.
                    Normalized numbers facilitate comparison across precincts and indicators; but they are not the raw
                    data. It's important to know how to interpret a single point.
                    If, for example, Abigndon had a value of 0.6 for Voter Turnout, you could interpret this as: "), 
              tags$h6(tags$i("Abigdon's voter turnout is about 60% of the highest Voter Turnout rate.")),
            tags$h6("However, the polygon points are best analyzed holistically with other precincts.
                    Abingdon's relative score on all six dimensions compared to those in another precinct may indicate 
                    differences in aggregate patterns across geography.")
            
            
            )
           
            
          ),
              
              

    
        
        tabPanel(   # about ----
          "About",  
          fluidPage(
            includeMarkdown("README.md")
           
          )
        )
            
            
        
          
        
        )
        
  



# - - - - - - - - - - - - - - - - - - - - - - - - - -    SERVER    ----- 


# Define server logic 
server <- function(input, output, session) {
 
  
  
  # for timelines ----
  
  ## reactive values 
  t1.in <- reactive({input$timeline.in1})
  
  
  output$timeline1 <- renderPlotly({
  t1 <-  ggplot(vote[vote$`Precinct Name` %in% "Arlington Totals",], aes(date, `Total Votes`)) +
      geom_line() +
      geom_point() +
      theme_minimal()
  ggplotly(t1)
  })
  
  output$timeline2 <- renderPlotly({
  t2 <-  ggplot(vote.pr, aes(date, `Total Votes`)) +
      geom_line(aes(color = `Precinct Name`)) +
      geom_point(aes(color = `Precinct Name`)) +
      theme_minimal()
  ggplotly(t2)
  })
  
  
  
  
  # observeEvent(input$map_shape_click, { # for map ----
  #   p <- input$map_shape_click
  #   print(p)
  #   
  #   # store coordinates 
  #   click.lat <- p$lat
  #   click.lng <- p$lng
  # 
  #   click.point <- data.frame(lat = click.lat, lng = click.lng) %>%
  #     st_as_sf(coords = c( "lat", "lng"))
  #   
  #   st_crs(click.point) <- st_crs(vote.data)
  #     
  #   print(click.point)
  #   
  #   # return overlapping feature
  #   ret_precinct <- st_join(click.point, vote.data
  #                           ) 
  #     #select(`Precinct Name`)
  #   
  #   print(ret_precinct)
  #   
  #   
  #   
  #   
  # })
  
 
  # reactive rendering of selected info   ----
  ## Make concatenated popupvarlist 
  sel.popvars <- reactive({
    union(as.vector(input$poplbls), as.vector(input$plotvar))
  })
  
  #union(as.vector(popupvaroptions), as.vector(popupvars))
  
  
  sel.pop.filter <- reactive({
    vote %>% select(sel.popvars())
  })

  
  
  # map - - - - ----
  
  # # output 
  # output$map <-leaflet::renderLeaflet({
  # 
  #   basemap
  #   # part of map that won't change dynamically (polygons)
  # })
  # 
  # 
  # # draw map 
  # draw_map <- function() {
  #   
  # # convert selected variable 
  # selected_var <- input$plotvar
  # 
  # # convert nbins slider to variable 
  # # selected_bins <- reactive({input$select.bins}
  # # denom   <- reactive({(1 / selected_bins())}) # determine step
  # 
  # col_cut <- as.vector(seq(0, 1, by = 0.25)) # formerly input$select.bins
  # 
  #   
  # # title 
  # title <- paste0(selected_var)
  # 
  # #leaftlet call 
  # leaflet_sf_col(
  #   data = vote.data, # this is not reactive object since not change
  #   col_var = selected_var, # variable to set color features?
  #   label_var = "Precinct Name",
  #   col_method = "quantile", # if "quantile", use "ntile" object for col_cuts
  #   opacity = 0.8,
  #   col_cuts = col_cut,
  #   title = title,
  #   legend_digits = 2,
  #   popup = leafpop::popupTable(sentence_spaced_colnames(sel.pop.filter()),
  #                               row.numbers = FALSE,
  #                               feature.id = FALSE)
  #   ) 
  #   
  # }
  # 
  # #observe 
  # observe({
  #   req(input$map_zoom) # wait for zoom before plotting 
  #   
  #   withProgress(
  #     message = "Loading", {
  #       draw_map()
  #     })
  # })
  
  
  
  # Heatmap ----
  
  # output$heatmap1 <- renderPlotly( # heatmap ----
  #   heatmaply(
  #     normalize(vote.heatmap),
  #   #  xlab = "Stats", 
  #     ylab = "Precincts",
  #     main = "Normalized Heatmap",
  #     show_dendrogram = FALSE,
  #     label_names = c("Precinct", "Indicator", "Value"),
  #     showticklabels = c(TRUE, FALSE),
  #     labRow = vote.heatmap$`Precinct Name`,
  #     dynamicTicks = TRUE
  #   )
  # )
  
   # output$heatmap2 <- renderPlotly(
   #   heatmaply_cor(
   #     r,
   #     main = "Correlation Map",
   #     node_type = "scatter", 
   #     point_size_mat= -log10(p),
   #     point_size_name= "-log10(p-value)",
   #     label_names = c("x", "y", "correlation"),
   #     show_dendrogram = FALSE
   #   )
   # )
 

  # polar plot ----
   
   ## first make reactive expressions
   polar1 <- reactive({
     sp.norm %>% 
     filter(`Precinct Name` == input$stat.in1) %>% 
     select(-`Precinct Name`) %>%
     gather()
   })
   
   polar2  <- reactive({
     sp.norm %>% 
     filter(`Precinct Name` == input$stat.in2) %>% 
     select(-`Precinct Name`) %>%
     as.vector() %>%
     gather()
   })
   
   
   polar3  <- reactive({
     sp.norm %>% 
       filter(`Precinct Name` == input$stat.in3) %>% 
       select(-`Precinct Name`) %>%
       as.vector() %>%
       gather()
   })
   
   
   sp.alpha <- reactive({input$sp.alpha})
   
   # then generate plots 
   output$polar <- renderPlotly(
     
     plot_ly(
       type = 'scatterpolar',
       fill = 'toself',
       hovertemplate = ht.polar
       
     )  %>%
       add_trace(
         mode = "lines+markers+text",
         r = as.vector(polar1()$value), # values
         theta = polar1()$key, # dimensions
         name = as.character(input$stat.in1),
         opacity = sp.alpha(),
         marker = list(color = color.1mk),
         line = list(width = w.sp, color = color.1mk),
         marker = list(
           colorbar = list(
             ticks = 'inside'
           )
         ),
         fillcolor = color.1
       ) %>%
       add_trace(
         mode = "lines+markers+text",
         r = as.vector(polar2()$value), # county values
         theta = polar2()$key, # dimensions
         name = as.character(input$stat.in2),
         opacity = sp.alpha(),
         marker = list(color = color.2mk),
         line = list(width = w.sp, color = color.2mk),
         fillcolor = color.2
       )  %>%
       add_trace(
         mode = "lines+markers+text",
         r = as.vector(polar3()$value), # county values
         theta = polar3()$key, # dimensions
         name = as.character(input$stat.in3),
         opacity = sp.alpha(),
         marker = list(color = color.3mk),
         line = list(width = w.sp, color = color.3mk),
         fillcolor = color.3
       )  %>%
       layout(
         paper_bgcolor = "", #'rgba(0,0,0,0)',
         plot_bgcolor  = "", #'rgba(0,0,0,0.5)',
         polar = list(
           radialaxis = list(
             visible = T,
             range = c(0, 1.0)
           )
         ),
         legend = list(
           orientation = 'h',
           itemclick = 'toggleothers'
           
         )
       )
   )

   
   
  
   

}  
   
    
    
   



# Run the application 
app <- shinyApp(ui, server)
