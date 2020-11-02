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
library(scales)

# opening
#root.data     <- "/Volumes/la-republic"
#setwd()
load("data/arl-vote2020.Rdata")




# update
up.date <- "Oct 31"
date.data <- "2020-10-31"

# format date and make data changes.
vote <- vote.data

# 
# vote.tot <- vote %>%
#   filter(Precinct.Name == "Arlington Totals" & date == date.data)


# define basemap
# basemap <- leaflet() %>%
#   addTiles() %>%
#   setView(-77.1, 38.88, zoom = 12)

# misc values


# define popup vars to display
# later, make this in to interactive select multiple.
popupvars <- c("Precinct.Name", "Total.Votes", "Mail.Received", "Early.Voted",
               "Active.Turnout", "Turnout.by.Mail", "Turnout.by.Early.Voting",
               "Mail.Ballot.Return.Rate", "Early.to.Mail.Ratio", "Early.to.Mail.Turnout.Ratio")

popupvaroptions <-c("Precinct.Name", "Total.Votes", "Mail.Received", "Mail.Ballots.Outstanding",
                    "Mail.Ballots.Counted", "Early.Voted",
                    "Active.Turnout", "Turnout.by.Mail", "Turnout.by.Early.Voting",
                    "Mail.Ballot.Return.Rate", "Early.to.Mail.Ratio", "Early.to.Mail.Turnout.Ratio",
                    "Mail.Ballots.Requested", "Percent.Mail.Counted",
                    "Precinct.Share.of.All.Votes", "Precinct.Share.of.Early.Votes",
                    "Precinct.Share.of.Oustanding.Mail.Votes", "Precinct.Share.of.Mail.Ballots.Received",
                    "Active.Registered")

arltotalvars <- c("Total Votes" = "Total.Votes",
                  "Total In-person Early Voting" = "Early.Voted",
                 "Mail Ballots Received" = "Mail.Received",
                 "Mail Ballots Counted" = "Mail.Ballots.Counted",
                 "Mail Ballots Outstanding" = "Mail.Outstanding",
                 "Percent of Received Mail Ballots Counted" = "Percent.Mail.Counted",
                 "Active Voter Turnout" =  "Active.Turnout",
                 "Active Turnout via Mail" = "Turnout.by.Mail",
                 "Active Turnout via Early Voting" = "Turnout.by.Early.Voting",
                 "Mail Ballot Return Rate" = "Mail.Ballot.Return.Rate",
                 "Early Voting-to-Mail Ratio" = "Early.to.Mail.Ratio"
                  )

polarplot <- c("Active.Turnout", "Turnout.by.Mail", "Turnout.by.Early.Voting",
               "Mail.Ballot.Return.Rate", "Early.to.Mail.Ratio", "Early.to.Mail.Turnout.Ratio")

heatmapvars <- c("Total.Votes", "Mail.Received", "Early.Voted",
                 "Active.Registered", "Military", "Overseas", "Federal",
                 "Mail.Ballots.Requested", "Early.to.Mail.Ratio")




pop.filter <- vote[vote$date %in% "2020-10-31",] %>%
  select(popupvars)

# # forheatmap
# vote.heatmap <- vote %>%
#   select(heatmapvars) %>%
#   as.data.frame()
#
# rownames(vote.heatmap) <- pop.filter$Precinct.Name



# color 'n-tiles'
ntiles <- c(0, 1/6, 1/3, 0.5, 2/3, 5/6, 1)



# SETTINGS          - - - - - - - - - - - - - - - - - -- - - ----
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

# heights
t1height = 500 # timeline 1 height 
t2height = 500 # timeline 2 height 
s2height = 700 # timeline 2 height 



# hovertext settings
ht.polar <- paste("%{theta}",
                  "<br>Normalized Value: %{r:.2f}")

# hovertext settings
ht.timeline <- paste("%{x}",
                     "<br>%{y:.1f}") #  <extra></extra>

ht.scatter <- paste("<b>%{text}</b>",
                    "<br>Mail: %{x:,f}",
                    "<br>Early: %{y:,f}",
                    "<br>Turnout: %{marker.color:.1f}%",
                    "<extra></extra>") #  

ht.scatter2 <- paste("<b>%{text}</b>",
                    "<br>Early-to-Mail Ratio: %{x:.1f}",
                    #"<br>Date: %{meta[i]}", # access this in scatter>meta?
                    "<br>Turnout: %{marker.color:.1f}%",
                    "<extra></extra>") # 

ht.scatter3 <- paste("<b>%{text}</b>",
                    #"<br> Active Registered: %{marker.size:,f}",
                    "<br>Mail Ballot Return Rate: %{x:.2f}",
                    "<br>Outstanding Votes: %{y:,f}",
                    "<br>Turnout: %{marker.color:.1f}%",
                    "<extra></extra>") #  

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
        theme = shinytheme('cosmo'),
        collapsible = TRUE,
        'ArlVotes2020',
        id = 'nav',
        header = tagList(useShinydashboard()),


        tabPanel(
          "Home",
          fluidPage(
            
            fluidRow(
              column( 12, align = 'left',
              tags$h1(tags$b("Arlington Votes 2020")),
              tags$h4(tags$b("Tracking the latest voting totals for Arlington County, Virginia.")),
            )), tags$br(),
            
            # column(4, align = 'center', 
            #        tags$br(),
            #        tags$h3(paste(up.date, ": Update")))
             
            
            # valueboxrow
            fluidRow(
              column(3, align = 'center',
                     valueBox(up.date, "Data Update", color = 'black', width = NULL)),
              
              # Total Votes
              column(3, align = 'center',
                     valueBox(prettyNum(vote.tot$Total.Votes, big.mark = ','),
                              "Total Votes", color = 'purple', width = NULL)),
              
              
              # Mail to Early Ratio
              column(3, align = 'center',
                     valueBox(vote.tot$Early.to.Mail.Ratio,
                              "Early to Mail Ratio", color = 'purple', width = NULL)),
              
              # Mail Ballot Return Rate 
              column(3, align = 'center',
                     valueBox( paste(100*vote.tot$Mail.Ballot.Return.Rate, "%"),
                              "Mail Vote Return Rate", color = 'purple', width = NULL))
              
            ),
            
            
            
            tags$br(),

            
            fluidRow(
                    column(6, align = 'center',
                           tags$h3("Estimated Turnout"),
                          plotlyOutput('gauge1', height = "150px")
                    ),
                    column(6, align = 'center',
                           tags$h3("Mail Ballots Counted"),
                          plotlyOutput('gauge2', height = "150px")
                    )
                   
            ),
            
            tags$h2(tags$b("Voting Totals Animations")),
            tags$body("In each chart, tap 'play' to show changes over time.
                      You can adjust the animiation settings for all charts below.
                      The dot size is proportionate
                      to active registered voter counts in each precinct."),
            tags$br(), tags$br(),
            
            
            
          # Plotly time sliders ----   
            fluidRow(
              
              # slider 1
              column(4, align = 'center',
                     sliderInput('in_pause_dur',
                                 "Pause Duration",
                                 min = 0,
                                 max = 1,
                                 value = 0.1,
                                 ticks = FALSE,
                                 step = 0.1,
                                 width = '75%'
                                 #post = " (s)"
                     )
                     ),
            
              
              # slider 2 --- 
              column(4, align = 'center',
                     sliderInput('in_trans_dur',
                                 "Transition Duration",
                                 min = 0,
                                 max = 1,
                                 value = 0.4,
                                 ticks = FALSE,
                                 step = 0.1,
                                 width = '75%'
                               #  post = "()"
                                 )
                     
              ),
              
              # checkbox 1 ---
              column(4, align = 'center',
                     radioButtons(
                       'in_ease_type',
                       "Animation Type",
                       choices = c("Linear" = "linear",
                                   "Elastic" = "elastic"),
                       inline = FALSE
                       
                     )
              ),
              
              
            ),
            
          
          
          # scatter 1 output ----
            fluidRow(column(12, align = 'center', tags$h3(tags$b("Votes by Method")))),
            fluidRow(plotlyOutput('scatter1', width = '100%', height = t1height)),
            tags$br(),tags$br(),
          
          # scatter 2 output ----
          fluidRow(column(12, align = 'center', tags$h3(tags$b("Early Voting-to-Mail Ratio and Active Turnout")),
                          tags$body("A higher ratio indicates that a greater share of votes came from in-person
                                    early voting."))),
          fluidRow(tags$br(),tags$br(),plotlyOutput('scatter2', width = '100%', height = s2height)),
          tags$br(),tags$br(),
          
          # scatter 3 output ----
          fluidRow(column(12, align = 'center', tags$h3(tags$b("Mail Ballot Return Rate and Oustanding Votes")),
                          tags$body("The return rate is the fraction of mail ballots returned to Arlington over the number
                                    of mail ballots requested; 1 indicates that all ballots are returned.
                                    Oustanding Votes is the remainder of Active Registered Voter minus all mail and early votes
                                    received by precinct."))),
          fluidRow(tags$br(),tags$br(),plotlyOutput('scatter3', width = '100%', height = t1height)),
          tags$br(),tags$br(),
          

          ) # end fluidpage
        ), # end tab panel home


        # tabPanel( # map (coming soon)  ----
        #     "Map",
        #     fluidRow(
        #       valueBox(paste0(rate, '%'), 'Estimated Turnout', width = 4, color = 'yellow'),
        #       valueBox(prettyNum(vote.turnout$tot.votes, big.mark = ','),
        #                'Total.Votes', width = 4, color = 'yellow'),
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
        #           choices = list(Votes = c("Total.Votes" = "Total.Votes",
        #                                    "Mail Ballots" = "Mail.Received",
        #                                    "Mail.Ballots.Counted" = "Mail.Ballots.Counted",
        #                                    "Mail Ballots Outstanding" = "Mail.Outstanding",
        #                                    "Early Voting Ballots" = "Early.Voted"
        #                                    ),
        #                          Turnout = c("Active Voter Turnout" = "Active.Turnout",
        #                                      "Turnout via Mail" = "Turnout.by.Mail",
        #                                      "Turnout via Early Voting" = "Turnout.by.Early.Voting"),
        #                          Ratios = c("Mail.Ballot.Return.Rate" = "Mail.Ballot.Return.Rate",
        #                                     "Early-to-Mail Ratio" = "Early.to.Mail.Ratio",
        #                                     "Early-to-Mail Turnout Ratio" = "Early.to.Mail.Turnout.Ratio",
        #                                     "Share of All Outstanding Mail Ballots"="Precinct.Share.of.Oustanding.Mail.Votes",
        #                                     "Share of All Early Votes" = "Precinct.Share.of.Early.Votes",
        #                                     "Percent Received Mail Counted" = "Percent.Mail.Counted")),
        #           selected = "Total.Votes",
        #           multiple = FALSE,
        #           width = 215
        #         ),
        #
        #
        #         pickerInput(
        #           'poplbls',
        #           "Select Popup Information",
        #           choices = popupvaroptions,
        #           selected = c("Precinct.Name", "Total.Votes", "Mail.Received", "Early.Voted"),
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

        
        tabPanel( # Timeline tab ----
          "Timeline",
          fluidPage(
            
            tags$h2(tags$b("Voting Timelines")),
            tags$body("Select a stat from the dropdown menu.
                      You can zoom via click-and-drag. To isolate a single precinct,
                      double click its name in the legend below. Then single click other precincts to compare."),
            tags$br(), tags$br(), tags$br(),
            fluidRow(
              column(12, align = 'center',
                     # timeline input panel ----
                     pickerInput(
                       'timeline.in1',
                       "Graph:",
                       choices = arltotalvars,
                       selected = "Total.Votes",
                       multiple = FALSE,
                       width = '350px',
                       inline = TRUE,
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
                     )
                     
              )),
            
            tags$br(),tags$br(),
            
            
            fluidRow(column(12, align = 'center',
                            tags$h3(tags$b("Arlington Totals")))),
            fluidRow(plotlyOutput('timeline1', width = '100%', height = t1height)),
            tags$br(),tags$br(),
            fluidRow(column(12, align = 'center',
                            tags$h3(tags$b("By Precinct")))), 
            fluidRow(tags$br(),plotlyOutput('timeline2', width = '100%', height = t2height)),
            
            
          ) # end fluidpage
        ), #end tabpanel timeline
        
        
        
        tabPanel( # stats ----
          "Stats",
          fluidPage(
            tags$h2(tags$b("Key Voting Statistics by Precinct")),
            tags$body("Select up to three precincts to display normalized statistics. Clicking on the precinct
                    names in the legend will toggle individual polygons."),
            tags$br(), tags$br(),
            fluidRow(

              column(4, align = 'center',
              pickerInput(
                'stat.in1',
                label = "Polygon 1",
                choices = sort(unique(sp.norm$Precinct.Name)),
                selected = "Dominion Hills",
                multiple = FALSE,
                width = '200px',
                options = list(
                  liveSearch = TRUE,
                  liveSearchNormalize = TRUE,
                  liveSearchStyle = 'contains',
                  selectOnTab = TRUE,
                  showTick = TRUE,
                  title = "Title",
                  virtualScroll = TRUE,
                  width = 'auto',
                  dropupAuto = TRUE,
                  mobile = TRUE
                )
              )
              ),

              column(4, align = 'center',
              pickerInput(
                'stat.in2',
                label = "Polygon 2",
                choices = sort(unique(sp.norm$Precinct.Name)),
                selected = "Rosslyn",
                multiple = FALSE,
                width = '200px',
                options = list(
                  liveSearch = TRUE,
                  liveSearchNormalize = TRUE,
                  liveSearchStyle = 'contains',
                  selectOnTab = TRUE,
                  showTick = TRUE,
                  title = "Title",
                  virtualScroll = TRUE,
                  width = 'auto',
                  dropupAuto = TRUE,
                  mobile = TRUE
                )
                )
              ),


              column(4, align = 'center',
              pickerInput(
                'stat.in3',
                label = "Polygon 3",
                choices = sort(unique(sp.norm$Precinct.Name)),
                selected = "Arlington Average",
                multiple = FALSE,
                width = '200px',
                options = list(
                  liveSearch = TRUE,
                  liveSearchNormalize = TRUE,
                  liveSearchStyle = 'contains',
                  selectOnTab = TRUE,
                  showTick = TRUE,
                  title = "Title",
                  virtualScroll = TRUE,
                  width = 'auto',
                  dropupAuto = TRUE,
                  mobile = TRUE
                )
                )
              ),

              fluidRow(
                column(12, align = 'center',
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
              )
            ),
            
            # polar output
            fluidRow(column(12, align = 'center',
               plotlyOutput('polar', width = '100%', height = '600px')
                )),



              # absolutePanel(
              #   id    = 'controlpanel2',
              #   #class = "panel panel-default",
              #   bottom = 10,
              #   right  = 10,
              #   width  = 180,
              #   fixed  = TRUE,
              #   draggable = TRUE,
              #   height = 'auto',
              #   cursor = "move",
              #   style = "opacity: 0.9",
              #
              #   wellPanel(
              #
              #     HTML(
              #       markdownToHTML(fragment.only = TRUE,
              #                      text = c("Select Precincts"))
              #     ),
              #
              #
              #
              #
              #   )
              # ),

            tags$h2("On Interpretation"),
            tags$body("Normalization transforms each dimension independently such that highest value in that dimension
                    is 1 and the lowest value is zero.
                    Normalized numbers facilitate comparison across precincts and indicators; but they are not the raw
                    data -- so it's important to know what a single point actually means in this format.
                    If, for example, Abigndon had a value of 0.6 for Voter Turnout, you could interpret this as: "),
            tags$br(),  tags$br(),
              tags$body(tags$i("Abingdon's voter turnout is about 60% of the highest Voter Turnout rate of all precincts.")),
            tags$br(), tags$br(),
            tags$body("However, the polygon points are best analyzed holistically with other precincts.
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

  # status gauges ----
  output$gauge1 <- renderPlotly({
    
    plot_ly(
      type = 'indicator',
      mode = 'gauge+number+delta',
      number= list(
        valueformat = "%"
      ),
      value= round(vote.tot$Active.Turnout / 100, 3),
      domain= list(x = c(0,1), y = c(0,1)),
      delta= list(
        reference = vote.tot.yest$Active.Turnout / 100,
        valueformat= "%"
      ),
      gauge = list(
        shape= 'angular',
        axis = list(
          range = list(NULL, 1),
          tickmode = 'array',
          tickvals = c(0,0.20,0.40,0.60,0.80, 1),
          tickformat = "%"
        ),
        steps= list(
          list(range = c(0,50), color = '#ffffff'), # currently set to white, 
          list(range = c(50,80), color= '#ffffff'),
          list(range = c(80,100), color= '#ffffff')
          ),
        bar = list(
          color = "#d9f0a3",
          thickness = 0.75,
          line = list(
            width = 0
          )
        )
        )
      ) %>%
      layout(
        margin= list(l=20, r=30)
      )
    
  })
  
  
  # total votes 
  output$gauge2 <- renderPlotly({
    
    plot_ly(
      type = 'indicator',
      mode = 'gauge+number+delta',
      number= list(
        valueformat = "%"
      ),
      value= round(vote.tot$Percent.Mail.Counted / 100, 3),
      domain= list(x = c(0,1), y = c(0,1)),
      delta= list(
        reference = vote.tot.yest$Percent.Mail.Counted / 100,
        valueformat= "%"
      ),
      gauge = list(
        shape= 'angular',
        axis = list(
          range = list(NULL, 1),
          tickmode = 'array',
          tickvals = c(0,0.20,0.40,0.60,0.80, 1),
          tickformat = "%",          
          tickangle = 0
        ),
        steps= list(
          list(range = c(0,50), color = '#ffffff'), # currently set to white, 
          list(range = c(50,80), color= '#ffffff'),
          list(range = c(80,100), color= '#ffffff')
          ),
        bar = list(
          color = "#78c679",
          thickness = 0.75,
          line = list(
            width = 0
          )
        )
        )
      ) %>%
      layout(
        margin= list(l=20, r=30)
      )
    
  })
  
  
  
  # scatter ----
  
  ## define reactive sum of two input values
  frame_trans <- reactive({ (1000 * input$in_trans_dur) })
  frame_dur <- reactive({ frame_trans() + (1000 * input$in_pause_dur)  })
  # ease <- reactive({ if_else(input$in_ease_type == TRUE, 
  #                            true = "elastic",
  #                            false= "linear")  })
  
  output$scatter1 <- renderPlotly({
    
    scatter <- plot_ly() %>%
      add_trace(
      data = vote.pr,
      type = 'scatter', 
      mode = 'markers',
      x = ~Mail.Received, # Mail.Ballot.Return.Rate
      y = ~Early.Voted, # Active.Turnout
      size = ~Active.Registered,
      color = ~Active.Turnout, # Early.to.Mail.Ratio
      frame = ~date,
      text = ~Precinct.Name
    ) %>%
    layout(
      title = list(
        text = "",
        font = list(
          family = c("Arial", "Droid Sans", "Times New Roman"),
          size = 18
        )
      ),
      yaxis = list(
        title = list(
          text = "Early In-Person Votes",
          font = list(
            family = c("Arial", "Droid Sans", "Times New Roman"),
            size = 17
          )
        )
      ),
      xaxis = list(
        title = list(
          text = "Mail-in Votes",
          font = list(
            family = c("Arial", "Droid Sans", "Times New Roman"),
            size = 17
          )
        )
      )
    ) %>% # end layout
    colorbar(
      title = paste("% Active", "<br>Turnout"),
      x = 0.90,
      y = 0.5,
      len = 0.5,
      lenmode = 'percent',
      thickness = 12
      ) %>%
    animation_opts(
        frame = frame_dur(),
        transition = frame_trans() ,
        easing = input$in_ease_type,
        redraw = FALSE
      ) %>%
    animation_slider(
      currentvalue = list(prefix = "Date ", font = list(color='red')),
      y = -0.1
    ) %>%
      style(
        hovertemplate = ht.scatter
      ) %>%
      config(
        displayModeBar = FALSE
      ) 
    
  })
  

  # scatter 2: Early to Mail Ratio to Active Turnout Overtime 
  output$scatter2 <- renderPlotly({
    
    plot_ly() %>%
      add_trace(
        data = vote.pr,
        type = 'scatter', 
        mode = 'markers',
        x = ~Early.to.Mail.Ratio, # Mail.Ballot.Return.Rate
        y = ~Precinct.Name, # Active.Turnout
        size = ~Active.Registered,
        color = ~Active.Turnout, # Early.to.Mail.Ratio
        frame = ~date,
        text = ~Precinct.Name
      ) %>%
      layout(
        title = "",
        xaxis = list(
          title = "Early Voting-to-Mail Ratio"
        ),
        yaxis = list(
          showgrid = FALSE,
          title = "Precinct",
          showticklabels = FALSE
        ),
        margin = list(l = 100)
      ) %>% # end layout
      colorbar(
        title = paste("% Active", "<br>Turnout"),
        x = 0.90,
        y = 0.5,
        len = 0.5,
        lenmode = 'percent',
        thickness = 12
      ) %>%
      animation_opts(
        frame = frame_dur(),
        transition = frame_trans(),
        easing = input$ease_type,
        redraw = FALSE
      ) %>%
      animation_slider(
        currentvalue = list(prefix = "Date ", font = list(color='red')),
        y = -0.1
      ) %>%
      style(
        hovertemplate = ht.scatter2
      ) %>%
      config(
        displayModeBar = FALSE
      ) 

    
  })
  
  
  # scatter 3 
  output$scatter3 <- renderPlotly({
    
    scatter <- plot_ly() %>%
      add_trace(
        data = vote.pr,
        type = 'scatter', 
        mode = 'markers',
        x = ~Mail.Ballot.Return.Rate, 
        y = ~Outstanding.Votes, 
        size = ~Active.Registered,
        color = ~Active.Turnout,
        frame = ~date,
        text = ~Precinct.Name
      ) %>%
      layout(
        title = list(
          text = "",
          font = list(
            family = c("Arial", "Droid Sans", "Times New Roman"),
            size = 18
          )
        ),
        yaxis = list(
          rangemode = 'tozero',
          title = list(
            text = "Oustanding Votes",
            font = list(
              family = c("Arial", "Droid Sans", "Times New Roman"),
              size = 17
            )
          )
        ),
        xaxis = list(
          title = list(
            text = "Mail Ballot Return Rate",
            font = list(
              family = c("Arial", "Droid Sans", "Times New Roman"),
              size = 17
            )
          )
        )
      ) %>% # end layout
      colorbar(
        title = paste("% Active", "<br>Turnout"),
        x = 0.90,
        y = 1.3,
        len = 0.7,
        lenmode = 'percent',
        thickness = 12
      ) %>%
      animation_opts(
        frame = frame_dur(),
        transition = frame_trans() ,
        easing = input$in_ease_type,
        redraw = FALSE
      ) %>%
      animation_slider(
        currentvalue = list(prefix = "Date ", font = list(color='red')),
        y = -0.1
      ) %>%
      style(
        hovertemplate = ht.scatter3
      ) %>%
      config(
        displayModeBar = FALSE
      ) 
    
    
    
  })
  
  

  # timelines ----

  ## reactive values
  t1.in <- reactive({ input$timeline.in1 })
  t1.in.lab <- reactive({ str_replace_all(input$timeline.in1, "\\.", " ") })

  output$timeline1 <- renderPlotly({

  t1 <-
    ggplot(vote[vote$Precinct.Name %in% "Arlington Totals",],
                aes(x = date,
                    y =  eval(as.name(t1.in() ))
                    )) + # input$timeline.in1
    geom_line(aes(color = Precinct.Name)) +
    geom_area(alpha = 0.1, fill = '#ffa500') +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.2)) ) +
    scale_x_date(labels = date_format("%d-%b"), breaks = unique(vote$date)) +
    labs(color = "", y = t1.in.lab(), x = "Date") +
    theme(legend.position = 'bottom') +
    theme_classic()


  ggplotly(t1, height = t1height) %>%
    layout(
      yaxis = list(
        type = 'linear',
        tickmode = 'auto',
        title = list(
          text = t1.in.lab(),
          font = list(
            family = c("Arial", "Droid Sans", "Times New Roman"),
            size = 14
          )  
        )
      ),
      showlegend = FALSE,
      # title = list(
      #   text = paste("Arlington Overall:", t1.in.lab() ),
      #   font = list(
      #     family = c("Arial", "Droid Sans", "Times New Roman"),
      #     size = 17
      #   ),
      #   y = 0.98
      # ),
      xaxis = list(
        title = list(
          text = ""
        ),
        tickangle = 45
      ),
      legend = list(
        title = "Precinct",
        itemclick = 'toggleothers',
        itemdoubleclick = 'toggle',
        orientation = 'v'
      )
    ) %>%
    style(
      hovertemplate = ht.timeline
    ) %>%
    config(
      displayModeBar = FALSE
    ) 

  }) # end render plotly

  output$timeline2 <- renderPlotly({
  t2 <-
    ggplot(vote.pr, aes(x = date,
                        y = eval(as.name(t1.in() )) )) +
    geom_line(aes(color = Precinct.Name)) +
    #geom_point(aes(color = Precinct.Name)) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0,0.5)) ) +
    scale_x_date(labels = date_format("%d-%b"), breaks = unique(vote$date)) +
    labs(color = "", y = t1.in.lab(), x = "") +
    guides(color = guide_legend(nrow = 10, byrow = TRUE)) + # pltly won't render this?
    theme(legend.position = 'bottom') +
    theme_classic()

  ggplotly(t2, height = t2height)  %>%
    layout(
      # title = list(
      #   text= paste("By Precinct:", t1.in.lab() ),
      #   font = list(
      #     family = c("Arial", "Droid Sans", "Times New Roman"),
      #     size = 17
      #   ),
      #   y = 0.95,
      # ),
      hovertemplate = ht.timeline,
      legend = list(
        orientation = 'h',
        y = -0.28,
        x = 0,
        title = list(
          text = "Precincts: double or single click",
          side = 'top',
          font = list(
            family = c("Arial", "Droid Sans", "Times New Roman"),
            size = 16
          )
        )
      ),
      height = 700,
      xaxis = list(
        title = list(
          text = ""
        ),
        tickangle = 45
      ),
      yaxis = list(
        type = 'linear',
        tickmode = 'auto',
        nticks = 6,
          title = list(
            text = t1.in.lab(),
            font = list(
              family = c("Arial", "Droid Sans", "Times New Roman"),
              size = 14
            )  
          )
      ),
      legend = list(
        title = "Precinct",
        itemclick = 'toggleothers',
        itemdoubleclick = 'toggle',
        orientation = 'v'
      )
    ) %>%
    style(
      hovertemplate = ht.timeline
    ) %>%
    config(
      displayModeBar = FALSE
    )
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
  #     #select(Precinct.Name)
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
  #   label_var = "Precinct.Name",
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
  #     labRow = vote.heatmap$Precinct.Name,
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
       filter(Precinct.Name == input$stat.in1) %>%
       select(-Precinct.Name) %>%
       rename(
         `Active Turnout` = "Active.Turnout",  `Mail Ballot Return Rate` = "Mail.Ballot.Return.Rate",
         `Early to Mail Ratio` = "Early.to.Mail.Ratio", `Mail Ballots Requested` = "Mail.Ballots.Requested",
         `Outstanding Votes` = "Outstanding.Votes", `Total Votes` = "Total.Votes"
       ) %>%
       as.vector() %>%
       gather()
   })

   polar2  <- reactive({
     sp.norm %>%
       filter(Precinct.Name == input$stat.in2) %>%
       select(-Precinct.Name) %>%
       rename(
         `Active Turnout` = "Active.Turnout",  `Mail Ballot Return Rate` = "Mail.Ballot.Return.Rate",
         `Early to Mail Ratio` = "Early.to.Mail.Ratio", `Mail Ballots Requested` = "Mail.Ballots.Requested",
         `Outstanding Votes` = "Outstanding.Votes", `Total Votes` = "Total.Votes"
       ) %>%
       as.vector() %>%
       gather()
   })


   polar3  <- reactive({
    sp.norm %>%
       filter(Precinct.Name == input$stat.in3) %>%
       select(-Precinct.Name) %>%
       rename(
        `Active Turnout` = "Active.Turnout",  `Mail Ballot Return Rate` = "Mail.Ballot.Return.Rate",
        `Early to Mail Ratio` = "Early.to.Mail.Ratio", `Mail Ballots Requested` = "Mail.Ballots.Requested",
        `Outstanding Votes` = "Outstanding.Votes", `Total Votes` = "Total.Votes"
       ) %>%
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
         margin = list(
           l = 120,
           r = 110
         ),
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
