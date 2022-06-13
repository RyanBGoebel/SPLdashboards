
# ALY 6070
# Ryan Goebel
# Assignment 3
# R Shiny Dashboard

###############################################################################
# Load required libraries
###############################################################################
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(lubridate)



## Set working directory to current folder where this app.R file is located.
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###############################################################################
# Load previously cleaned/sorted/wrangled data
# SPL-RG.csv
###############################################################################
SPL <- read.csv("SPL-RG.csv", header = TRUE, stringsAsFactors = FALSE) 


###############################################################################
# Further data prep / subsetting / defining add'l variables
###############################################################################

# Beginning of each year
date.breaks <- as.Date(c("2017-01-01", "2018-01-01","2019-01-01","2020-01-01",
                         "2021-01-01","2022-01-01","2023-01-01","2024-01-01"))

# Define scale in hundreds of thousands for checkouts
yscale <- c(0, 100, 200, 300, 400, 500, 600)


# All digital vs. all physical checkouts by month
checkoutTrends <- SPL %>%
  select(Month, AllDigital, AllPhysical) %>%
  pivot_longer(!Month, names_to = "UsageClass", values_to = "Checkouts")

# Checkouts by Material Type
MT <- SPL %>%
  select(Month, Ebook, Audiobook, OtherDigital,
         Book, VideoDisc, SoundDisc, OtherPhysical) %>%
  pivot_longer(!Month, names_to = "MaterialType", values_to = "Checkouts")

# Re-order material type factors
MT$MaterialType <- factor(MT$MaterialType, 
                           levels=c("Ebook", "Audiobook", "OtherDigital",
                                    "Book", "VideoDisc",
                                    "SoundDisc", "OtherPhysical"))
# Reverse order of Material Type factors
MT$MaterialType <- factor(MT$MaterialType, levels=rev(levels(MT$MaterialType))
                          )
####### NOTE TO SELF: #####
# Data for timeline.  Had to settle for some hard coding with text annotations 
# here, since dates and line breaks were causing problems.  Will try to fix if 
# time allows. If not, may consider fixing in the future.
###########################

#EventDates <- as.Date(c("2017-01-01", "2017-04-01", "2019-05-01", "2020-03-01", 
#                        "2021-05-01", "2022-10-01", "2022-12-01", "2024-01-01"))

EventDates <- c(0, 3, 28, 39, 52, 69, 71, 84) # count of months since 01-01-2017

Events <- seq(1,8,1) # 8 highlighted events


# Events <- c("Digital checkouts account\nfor 30% of all checkouts.",
#             "Ebooks become 2nd most popular checked out material type in a 
#             month for 1st time.",
#             "Audiobooks become 3rd most popular material type\nchecked out in a 
#             month for 1st time.",
#             "SPL closes to the public due to COVID-19.",
#             "Regardless of effects of the pandemic, digital checkouts 
#             were\npredicted to equal physical checkouts for 1st time",
#             "Digital checkouts are predicted to remain the predominant 
#             checkout\ntype going forward",
#             "Ebooks are predicted to become the most popular material 
#             type\nchecked out in a month for the 1st time",
#             "Digital checkouts are predicted to account for 61% of all checkouts."
# )

# Combine vectors into dataframe
TimelineDF <- cbind.data.frame(EventDates, Events)





###############################################################################
# 
# D E F I N E    U I
# 
###############################################################################

ui <- fluidPage(
  
# Title bar
  titlePanel(
    fluidRow(column(2, div(img(src="SPL-logo-white.jpg", height=140, width=140)), 
                    style="text-align: center;"
                    ), # END COLUMN
             column(9, HTML("<h1><b>
             Where should the Seattle Public Library focus their future material 
             purchasing budget?
             </b></h1>
                              <h3><b>
                              <span style=\"color:#9F8F12\">DIGITAL CHECKOUTS</span> 
                             are predicted to surpass 
                             <span style=\"color:#A39FC9\">PHYSICAL CHECKOUTS</span> 
                             by the end of 2022*</b></h3>")
                    ) # END COLUMN
             ), # END FLUID ROW
    ), # END TITLE PANEL
  
  fluidRow(
    column(1),
    column(10,
      HTML("<hr><h4>A prediction model was built to estimate future checkouts at the
      Seattle Public Library (SPL).  The COVID-19 pandemic forced SPL to 
      close all branches in March 2020, and then only limited curbside 
      services were offered from Aug 2020 through April 2021. Due to the 
      anomalous nature of this event, it is assumed that library activity
      will return to pre-pandemic trends once all branches re-open to the public
      this summer.  Therefore, the prediction model was built using checkout data
      from January 2017 through February 2020.<hr><br>"
      # To see the actual checkout numbers duing the pandemic, click the box to
      # the right:  (CHECKBOX)???<br><br><br>"
      
      ####### NOTE TO SELF: #####
      # May want to try creating this checkbox to display actual pandemic numbers
      # Would just require adding the appropriate data to existing dataframe,
      # or making an additional dataframe with the data.
      ###########################      
      )
    ),
      ), # END FLUID ROW
  
  
  fluidRow(
    column(4, 
           # DROP DOWN INPUT MENU
           # MOVES SLIDER AND ADJUSTS VLINE POSITION ON LINE GRAPH AND FILTER 
           # FOR MATERIAL TYPES BAR GRAPH
           div(align="center",
           HTML("<H2>KEY EVENTS:</H2>"),
           selectInput(
             "dateList", "Choose Event Date to Update Plots",
             c("Jan 2017" = format(as.Date("2017-01-10"), "%b %Y"),
               "Apr 2017" = format(as.Date("2017-04-01"), "%b %Y"),
               "May 2019" = format(as.Date("2019-05-01"), "%b %Y"),
               "Mar 2020" = format(as.Date("2020-03-01"), "%b %Y"),
               "May 2021" = format(as.Date("2021-05-01"), "%b %Y"),
               "Oct 2022" = format(as.Date("2022-10-01"), "%b %Y"),
               "Dec 2022" = format(as.Date("2022-12-01"), "%b %Y"),
               "Jan 2024" = format(as.Date("2024-01-01"), "%b %Y")
             )
             ),
           
          plotOutput("TimelinePlot",
                     width=500, height=900)),
           
          # If Timeline plot causes problems:
          # div(img(src="timeline.png", # static image version of timeline
           # style="max-width: 100%;
            #height: auto;"
    #) 
    #), # END div(img
    style="text-align: center;"
    ),  # END COLUMN
    
    column(8,  # THIS IS START OF MAIN RIGHT COLUMN
      fluidRow(  # FLUID ROW WITHIN RIGHT COLUMN    
        column(1, h4("")),
        column(10,      
           HTML("<H2>PREDICTION MODEL FOR<br>
           <span style=\"color:#9F8F12\">DIGITAL</SPAN> 
           VS. <span style=\"color:#A39FC9\">PHYSICAL</span> 
                CHECKOUTS:</H2>",
                ), #  END HTML 
           style="text-align: center;",
           plotOutput("checkoutTrendsPlot", height = 400),
           #plotOutput("DigVsPhy", height=250), # not used in final version
           HTML("<br><br>"),
        ), # END COLUMN (size 9)
      ), # END FLUID ROW WITHIN RIGHT COLUMN
           
    fluidRow(  # FLUID ROW WITHIN RIGHT COLUMN
      column(2, h4("")),
      column(9, 
           tags$style((".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, 
                       .js-irs-0 .irs-bar {background: #FF6600}")),
           sliderTextInput("ChooseDate",
                           "",
                           choices = format(as.Date(unique(SPL$Month)), "%b %Y"),
                           selected = format(as.Date("2017-01-01"), "%b %Y"), 
                           grid=TRUE,
                           width="100%"
           ), # END SLIDER INPUT
           style="text-align: center;",
           HTML("<h3><em>Use slider above to see estimated monthly checkouts by 
                material type:</em></h3>"),
    )), # END FLUID ROW WITHIN COLUMN
  
  fluidRow(   # FLUID ROW WITHIN RIGHT COLUMN
    column(2, h4("")),
    column(9,
    plotOutput("MonthlyMaterialTypesPlot", height=250),
    )
   ), # END FLUID ROW WITHIN COLUMN
  fluidRow(    # FLUID ROW WITHIN RIGHT COLUMN
    column(2, h4("")),
    column(9,
           HTML("<hr><center><h3><b><em>Recommendation:</em><br>SPL should focus 
           the purchasing budget on 
           <span style=\"color:#9F8F12\">e-books</span>
                and <span style=\"color:#9F8F12\">audiobooks</span>, 
                since they are the only material types
                exhibiting continual checkout growth.</b></h3></center><hr><br><br>")
    ) # END COLUMN           
    ) # END FLUID ROW
    ),   # FLUID ROW WITHIN RIGHT COLUMN

  
  # THESE RADIO BUTTONS WORKED TO UPDATE SLIDER,
  # BUT COULDN'T FIND A WAY TO LINE THEM UP WITH THE TIMELINE AND LOOK NICE
  # fluidRow( # fluid row for testing
  #   column(2),
  #   column(8,
  #   radioButtons(
  #     inputId = "button",
  #     label = "Update choices:",
  #     choices = as.yearmon(c("2017-01-01", "2017-04-01", "2019-05-01",
  #                            "2024-01-01"))
  #   ),
  #   verbatimTextOutput("buttonSelection"),
  #   )
  # ) # end fluid row for testing
  
  
  ), # END RIGHT COLUMN
  
  fluidRow(
    
    HTML("<p>
    <center><b>Sources:</b><br> <a href='https://data.seattle.gov/Community/Checkouts-by-Title/tmmm-ytt6'>
         Seattle,  <em>Checkouts by Title</em>, 2021</a><br>
         <a href=''>
         Seattle Public Library, <em>Road to Reopening</em>, 2021</a><br>
         <br>
         <em>
         <b>* Notes:</b><br>  All statistics are based on a 2% sample of checkouts from Jan 2017 
         through Feb 2020, with predictions made using a Holt-Winters exponential 
         smoothing model.<br>Predicted checkouts by material type were normalized
         to match the total digital and physical checkout predictions by month. 
         <br><br>
         R Shiny Dashboard created by 
         <a href='https://www.linkedin.com/in/ryangoebel/'>Ryan Goebel</a> 
         for ALY 6070, Spring 2021 
         Term B, at the Roux Institute at Northeastern University in Portland, 
         Maine.</em>
         </p>")
    
  ), # END FLUID ROW
  
  ) # END FLUID PAGE


###############################################################################
# 
# D E F I N E    S E R V E R
# 
###############################################################################

server <- function(input, output, session) {
  
  
  
### PRIMARY PLOT OF PREDICTION OF DIGITAL VS PHYSICAL CHECKOUTS
  
  output$checkoutTrendsPlot <- renderPlot({
    
    tDate <- input$ChooseDate[1]
    tDate <- paste(tDate, "01")
    tDate <- as.Date(tDate, format="%b %Y %d")
    
    ChosenDate <- as.Date(as.Date(tDate), format="%b %Y")
    #ChosenDate <- as.Date(as.Date(input$ChooseDate[1]), format="%b %Y")
    #print(paste("Slider says ", ChosenDate))  # FOR TESTING
    
    di <- SPL[SPL$Month == ChosenDate, "AllDigital"]
    ph <- SPL[SPL$Month == ChosenDate, "AllPhysical"]
   
        ggplot(checkoutTrends, aes(x=as.Date(Month), y=Checkouts, 
                                  group=UsageClass, color=UsageClass)) +
          geom_line(size=1.2) +
            labs(title="",
             x="", y="Monthly Checkouts") +
            scale_y_continuous(limits=c(0,630000), 
                         labels = function(n) {
                           ifelse(n == 0, 0, paste0(yscale,"K")) 
                         },
                         breaks=yscale*1000, expand=c(0,0)) +
            scale_x_date(date_breaks="1 year", date_labels="%Y") +
            scale_color_manual(values=c("#9F8F12", "#A39FC9")) +
            ggplot2::annotate("rect", xmin=as.Date("2020-03-01"), 
                              xmax=as.Date("2024-02-01"), 
                 ymin=-Inf, ymax=Inf, 
                 fill="gray60", alpha=0.1) +
            ggplot2::annotate("text",
                    label="*Checkouts from March 2020 onward are predicted values",
                    x=as.Date("2024-01-01"), y=20000, hjust=1, size=5) +
            geom_vline(xintercept=as.Date(ChosenDate), linetype="dotted",
                     color="#FF6600", size=1) +
            ggplot2::annotate("text",
                              label=paste0(as.character(ChosenDate, format="%B %Y"), ":"),
                              x=as.Date("2024-01-01"), y=170000, 
                              hjust=1, size=6, fontface="bold") +
            ggplot2::annotate("text", 
                          label=paste0("DIGITAL CHECKOUTS: ", 
                                       round(100*di/(di+ph), 2),"%"),
                          x=as.Date("2024-01-01"), y=130000, 
                          hjust=1, size=6, color="#9F8F12", fontface="bold") +
            ggplot2::annotate("text", 
                              label=paste0("PHYSICAL CHECKOUTS: ", 
                                           round(100*ph/(di+ph), 2),"%"),
                              x=as.Date("2024-01-01"), y=90000, 
                              hjust=1, size=6, color="#A39FC9", fontface="bold") +
            ggplot2::annotate("point", x=as.Date(ChosenDate), y=di, 
                              size=8, shape=1, color="#FF6600") +
            ggplot2::annotate("point", x=as.Date(ChosenDate), y=ph, 
                            size=8, shape=1, color="#FF6600") +
            theme(
              axis.text.y = element_text(size=20),
              axis.text.x = element_text(size=20, face="bold"),
              axis.title.y = element_text(size=24),
              panel.border = element_rect(color="white", fill=NA, size=1),
              panel.background = element_blank(),
              legend.position = "none",
              plot.margin = unit(c(0,1,0,1), "pt")
          ) # end of ggplot for checkoutTrendsPlot
  }) # end of output$checkoutTrendsPlot

  
### PLOT OF MONTHLY CHECKOUTS BY MATERIAL TYPE
  
  output$MonthlyMaterialTypesPlot <- renderPlot({
    
    tDate <- input$ChooseDate[1]
    tDate <- paste(tDate, "01")
    tDate <- as.Date(tDate, format="%b %Y %d")
    
    ChosenDate <- as.Date(as.Date(tDate), format="%b %Y")
  
    #ChosenDate <- as.Date(as.yearmon(input$ChooseDate[1]))

    ChosenDateMT <- filter(MT, as.Date(Month) == as.Date(ChosenDate))
    RefDateData <- filter(MT, as.Date(Month) == as.Date("2020-01-01"))
    
    ggplot(ChosenDateMT, aes(x=MaterialType, y=Checkouts, fill=unique(MaterialType))) +
        coord_flip() +
      geom_bar(stat="identity", width=0.75) +
      labs(title="", x="", y="(Dots indicate Jan 2020 levels)") +
      scale_fill_manual(values=c("#9F8F12","#9F8F12","#9F8F12",
                                 "#A39FC9","#A39FC9","#A39FC9","#A39FC9")) +
      geom_text(aes(label=ifelse(Checkouts < 1000,"< 1K",
                      paste0(round(Checkouts/1000), "K"))), 
                    hjust=-0.15,
                size=6, color="black", fontface="bold") +
      scale_y_continuous(limits=c(0,440000), expand=c(0,0)) +
      ggplot2::annotate("text", x=0.5, y=440000, 
                          label=ifelse(as.Date(ChosenDate) >= as.Date("2020-03-01"), 
                                       "*Checkouts currently displayed are predicted values",
                                       " "),
                          size=5, hjust=1) +
      geom_point(data=RefDateData, aes(x=MaterialType, y=Checkouts),
                 color="#c9b69f", size=8, alpha=0.35) +
      
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_text(size=14, 
                                    face="bold.italic", color="#c9b69f"),
        axis.text.y = element_text(size=16, face="bold"),
        axis.ticks = element_blank(),
        panel.border = element_rect(color="white", fill=NA, size=1),
        panel.background = element_blank(),
        legend.position = "none") +
  
       if(as.Date(ChosenDate) >= as.Date("2020-03-01")){ 
         (ggplot2::annotate("rect", 
                           xmin=0, xmax=Inf, 
                           ymin=0, ymax=Inf, 
                           fill="gray60", alpha=0.1)) 
         }
}) # end output MonthlyMaterialsPlot
  
# TIMELINE PLOT
  
  ####### NOTE TO SELF: #####
  # May want to improve this by using nearPoints() so hovering or clicking on
  # either dots or text makes selection instead of pulldown menu.
  # Would need to modify the input dataframe to have extra columns for the 
  # displayed dots and text locations, since they differ slightly from
  # the actual dates.  (the axis and segment starting points are the actual
  # dates.)
  ###########################  

  output$TimelinePlot <- renderPlot({
    
    ggplot(TimelineDF, aes(y=EventDates, x=0), color=EventDates) + 
      scale_y_reverse() +
      geom_segment(aes(x=0, xend=0, y=0, yend=84), size=1, color="gray") +
      geom_segment(aes(x=0, 
                       xend=10, 
                       yend=c(-0.7, 3.5, 28, 39, 52, 65, 73, 84)
      ), size=1,
      color=c("#A39FC9", "#A29CAE", "#A19A94", "#A1987A", "#A09560", "#A09346",
              "#9F912C", "#9F8F12")
      ) +
      geom_point(aes(x=10, y=c(-0.7, 3.5, 28, 39, 52, 65, 73, 84)), size=4,
                 color=c("#A39FC9", "#A29CAE", "#A19A94", "#A1987A", "#A09560", 
                         "#A09346", "#9F912C", "#9F8F12")) +
      ggplot2::annotate("text", x=15.2, y=EventDates[1], 
                label="Jan 2017: Digital checkouts account
                  for 30% of all checkouts.", 
                color="#A39FC9", size=5, hjust=0, fontface="bold") +
      ggplot2::annotate("text", x=15.2, y=EventDates[2]+4, 
                label="Apr 2017: Ebooks become 2nd most 
                 popular checked out 
                 material type in a month 
                 for 1st time.", 
                color="#A29CAE", size=5, hjust=0, fontface="bold") +
      ggplot2::annotate("text", x=15.2, y=EventDates[3]+2, 
                label="May 2019: Audiobooks become 3rd 
                  most popular material type 
                  checked out in a month for 1st time.",
                color="#A19A94", size=5, hjust=0, fontface="bold") +
      ggplot2::annotate("text", x=15.2, y=EventDates[4]+1, 
                label="Mar 2020: SPL closes to the public due to 
                  COVID-19.",
                color="#A1987A", size=5, hjust=0, fontface="bold") +
      ggplot2::annotate("text", x=15.2, y=EventDates[5]+3, 
                label="May 2021: Regardless of effects of the 
                  pandemic, digital checkouts 
                  were predicted to equal physical 
                  checkouts for 1st time.",
                color="#A09560", size=5, hjust=0, fontface="bold") +
      ggplot2::annotate("text", x=15.2, y=EventDates[6]-2.1, 
                label="Oct 2022: Digital checkouts are predicted to 
                 remain the predominant checkout 
                 type going forward",
                color="#A09346", size=5, hjust=0, fontface="bold") +
      ggplot2::annotate("text", x=15.2, y=EventDates[7]+5.8, 
                label="Dec 2022: Ebooks are predicted to become 
                 the most popular material type 
                 checked out in a month for the 
                 1st time",
                color="#9F912C", size=5, hjust=0, fontface="bold") +
      ggplot2::annotate("text", x=15.2, y=EventDates[8]+1, 
                label="Jan 2024: Digital checkouts are predicted to 
                 account for 61% of all checkouts.",
                color="#9F8F12", size=5, hjust=0, fontface="bold") +
      coord_cartesian(clip="off") +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        plot.title = element_text(hjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0,15,0,0), "cm"),
        panel.grid = element_blank()
      )
  }) # end output Timeline Plot
  
 # This "plot" was not displayed in the final dashboard, but was a way of 
  # displaying dynamically changing text for the digital and physical
  # checkout percentages.  In the end, decided to just include that info
  # as annotations on the line chart, so the overall page is shorter in length
  # and the user is more likely to see the slider bar and material type
  # bar plot upon opening or soon after a minimal scroll downward.
  
  # output$DigVsPhy <- renderPlot({
  #   
  #   dt <- as.Date(as.yearmon(input$ChooseDate[1]))
  #   print(paste("dt is ", dt))
  #   di <- SPL[SPL$Month == dt, "AllDigital"]
  #   ph <- SPL[SPL$Month == dt, "AllPhysical"]
  #   
  #   ggplot(SPL) +
  #     scale_x_continuous(limits=c(-1,3)) +
  #     scale_y_continuous(limits=c(1,1.5)) +
  #     geom_text(aes(x=1, y=1.37, 
  #                   label=paste0("In ", as.character(dt, format="%B %Y"), ":")),
  #               size=8) +
  #     
  #     geom_text(aes(x=0, y=1.28, 
  #                   label="DIGITAL CHECKOUTS"),
  #               size=6, color="#9F8F12", fontface="bold") +
  #     geom_text(aes(x=0, y=1.22, 
  #                   label=ifelse(as.Date(dt) >= as.Date("2020-03-01"),
  #                     "are predicted to account for",
  #                     "accounted for")),
  #               size=6) +
  #     geom_text(aes(x=0, y=1.1, 
  #                   label=(
  #                     paste0(round(100*di/(di+ph), 2),"%")
  #                   )
  #     ),
  #     size=18, fontface="bold", color="#9F8F12") +
  #     geom_text(aes(x=0, y=1.0, 
  #                   label="of all checkouts at SPL"),
  #               size=6) +
  #     
  #     
  #     geom_text(aes(x=1, y=1.15, label="&"), size=24) +
  #     
  #     
  #     
  #     geom_text(aes(x=2, y=1.28, 
  #                   label="PHYSICAL CHECKOUTS"),
  #               size=6, color="#A39FC9", fontface="bold") +
  #     geom_text(aes(x=2, y=1.22, 
  #                   label=ifelse(as.Date(dt) >= as.Date("2020-03-01"),
  #                                "are predicted to account for",
  #                                "accounted for")),
  #               size=6) +
  #     geom_text(aes(x=2, y=1.1, 
  #                   label=(
  #                     paste0(round(100*ph/(di+ph), 2),"%")
  #                   )
  #     ),
  #     size=18, fontface="bold", color="#9F8F12") +
  #     geom_text(aes(x=2, y=1.0, 
  #                   label="of all checkouts at SPL"),
  #               size=6) +
  #     
  #     theme(
  #       axis.text = element_blank(),
  #       axis.title = element_blank(),
  #       axis.ticks = element_blank(),
  #       panel.border = element_blank(),
  #       panel.background = element_blank()
  #     )
    
    
  #}) # end output Digital vs Physical "plot"
  

  # THESE RADIO BUTTONS WORK, BUT COULDN'T FIND A WAY TO LINE THEM UP WITH THE PLOT
  # WENT WITH PULL DOWN MENU INSTEAD
  # IDEALLY, IT WOULD BE MODIFIED TO USE NEARPOINTS()
  # If selected element changes, then update the slider
  # observeEvent(input$button, {
  #   x = input$button
  #   updateSliderTextInput(session, "ChooseDate", selected = x)
  # })
  
  
  # WHEN EVENT IS CHOSEN IN PULLDOWN INPUT SELECT MENU, SLIDER BAR IS
  # AUTOMATICALLY UPDATED.
  observeEvent(input$dateList, {
    x = input$dateList
    updateSliderTextInput(session, "ChooseDate", selected = x)
  })
  
  # Display to console:
  print("Hello world! Thank you for viewing my R Shiny app.")

} # END SERVER

###############################################################################
# Run the Shiny App
###############################################################################
shinyApp(ui = ui, server = server)

#########  END OF CODE  #########  
