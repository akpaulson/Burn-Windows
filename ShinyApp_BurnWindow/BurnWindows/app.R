#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS) #For the tool tip
library(shinythemes)
library(ggplot2)
library(lubridate)
library(dplyr)

#### Data ####

#For simple burn window: 
all_raws_fig2 <- read.csv("all_raws_fig2.csv") %>% 
    mutate(date = as.Date(ymd(date))) 

all_raws_fig3 <- read.csv("all_raws_fig3_data.csv", 
                          stringsAsFactors = FALSE) %>%
    mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                        ordered = TRUE))

all_raws_fig4 <- read.csv("all_raws_fig4_data.csv") %>%
    mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                          ordered = TRUE))

all_raws_fig5 <- read.csv("all_raws_fig5_data.csv") %>%
    mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                          ordered = TRUE))

# #For custom burn window: 
# 
# custom_bw_data <- read.csv("custom_bw_data.csv", stringsAsFactors = FALSE) %>% 
#     mutate(date = ymd(date))

####UI for application####

# Define UI for application
ui <- fluidPage(

    # Application title
    #titlePanel("Proportion of Burn Window Days"),
    
    #Navigation bar for the app
    navbarPage(title = "Historical Burn Windows", 
               
               #Text for first Background Panel
               tabPanel(title = "Background", 
                        mainPanel(
                            #** Will need to replace image - currently from John W.**
                            div(img(src = "Drip_lines_Klamath_10-19.jpg", 
                                   height = 200, width = 300), 
                               style = "text-align: center;"),
                           h3("What is a burn window?"), #h3 is the header level, h1 is bigger, up to h6 is smaller
                           p("Managers' ability to conduct prescribed fires is 
                             constrained by several factors, such as fire weather, 
                             air quality, and resource availability. Burn windows 
                             occur when all of 
                             these factors are favorable for prescribed burning."),
                           h3("What does this tool do?"),
                           p("This tool provides an assessment of when burn windows took place
                             during the past ~20 years across National Forests in California. 
                             This assessment is based on historical weather data, California Air 
                             Resources Board burn day designations, and state/national preparedness Levels."),
                           h3("What is a simple burn window?"), 
                           p("Simple burn windows are described  in", 
                             tags$a(href = "https://link.springer.com/content/pdf/10.1186/s42408-020-00071-3.pdf", "Striplin et al. (2020)", target = "_blank"), 
                             ", and are based on the coincidence of three criteria:"),
                           tags$ol(
                               tags$li("California Air Resources Board burn day designations"),
                               tags$li("Days when weather falls within burn plan prescription (based on historical RAWS data):"),
                                tags$ul(
                                    tags$li("Minimum relative humidity between 20 and 50%"),
                                    tags$li("1-hour fuel moisture between 7 and 20%"),
                                    tags$li("Maximum wind speeds (6.1 m above ground) < 25 mph")
                                ),
                               tags$li("Operational preparedness level based on the Northern/Southern California Geographic Area and national preparedness levels.")
                           ),
                           h3 ("What are custom burn windows?"),
                           p("Custom burn windows allow the user to have more control
                             over the criteria used to designate a burn window. 
                             We have provided several additional criteria so that you can 
                             customize your burn window. This section of the tool is still
                             under development and we will consider feature requests.")
               )),
               
               ##### Simple Burn Windows UI ####
               tabPanel(title = "Simple Burn Windows", 
                        # Sidebar with a selector input for Forest and District
                        sidebarLayout(
                            #Want to have a panel on the side for the selection of forest/district
                                #these are updated in server function to be reactive to initial
                                # forest/district selection
                            sidebarPanel(
                                
                                #Select snow flag option: 
                                radioButtons(inputId = "snowflag_selection", 
                                              label = "Include Snow Flag?", 
                                              choices = c("Yes" = "yes", 
                                                          "No" = "no"),
                                              inline = TRUE),
                                #Add shiny tool tip: 
                                bsTooltip(id = "snowflag_selection", 
                                          title = "This setting allows you to ignore the snow flag by setting \"No.\". If the snow flag is included (\"Yes\"), then burn days that are actually feasible for pile burning in the winter may be excluded.", 
                                          placement = "right", 
                                          trigger = "hover"),
                                
                                #Select National Forest: 
                                selectInput(inputId = "forest_selection", 
                                            label = "Select National Forest", 
                                            choices = unique(all_raws_fig2$Forest)), 
                                
                                #Select Forest District
                                selectInput(inputId = "district_selection", 
                                            label = "Select Forest District", 
                                            choices = unique(all_raws_fig2$District)),
                                
                                #Select RAWS Station
                                selectInput(inputId = "raws_selection", 
                                            label = "Select RAWS Station", 
                                            choices = unique(all_raws_fig2$RAWS_station)),
                                
                                #Don't make plots until action button is hit:
                                actionButton(inputId = "bw_action_button",
                                             label = "Calculate Burn Window")
                                
                            ), 
                            
                            # Include Plot output: 
                            mainPanel(
                                plotOutput("burn_window_proportion", width = "7in", height = "2in"), 
                                textOutput("fig1_caption"),
                                br(), #line break
                                br(), #line break
                                
                                plotOutput("burn_window_days", width = "7in", height = "2in"),
                                textOutput("fig2_caption"),
                                br(), #line break
                                br(), #line break
                                
                                plotOutput("multiple_day_bw", width = "7in", height = "2in"),
                                textOutput("fig3_caption"),
                                br(), #line break
                                br(), #line break
                                
                                plotOutput("monthly_yearly_bw", width = "7in", height = "6in"),
                                textOutput("fig4_caption")
                            )
                        )), 
               tabPanel("Custom Burn Windows", 
                        mainPanel(
                            p("In Development")
                        )
                        # Sidebar with a selector input for Forest and District
                        # sidebarLayout(
                        #     #Want to have a panel on the side for the selection of forest/district
                        #     #these are updated in server function to be reactive to initial
                        #     # forest/district selection
                        #     sidebarPanel(
                        #         
                        #         #Select National Forest: 
                        #         selectInput(inputId = "forest_selection_custom", 
                        #                     label = "Select National Forest", 
                        #                     choices = unique(custom_bw_data$Forest)), 
                        #         
                        #         #Select Forest District
                        #         selectInput(inputId = "district_selection_custom", 
                        #                     label = "Select Forest District", 
                        #                     choices = unique(custom_bw_data$District)),
                        #         
                        #         #Select RAWS Station
                        #         selectInput(inputId = "raws_selection_custom", 
                        #                     label = "Select RAWS Station",
                        #                     choices = unique(custom_bw_data$RAWS_station)), 
                        #         
                        #         br(), #line break
                        #         h4(strong("Customize Burn Window:")),
                        #         
                        #         #CARB
                        #         checkboxInput(inputId = "carb_selection", 
                        #                       label = strong("CARB Burn Day")), 
                        #         
                        #         #Preparedness Level
                        #         checkboxInput(inputId = "pl_selection", 
                        #                       label = strong("Preparedness Level")),
                        #         
                        #         br(), #line break
                        #         h5(strong("Burn Prescription Weather:")),
                        #         
                        #         
                        #         #Select snow flag option: 
                        #         radioButtons(inputId = "snowflag_selection_custom", 
                        #                      label = "Include Snow Flag?", 
                        #                      choices = c("Yes" = "yes", 
                        #                                  "No" = "no"),
                        #                      inline = TRUE),
                        #         
                        #         #Relative humidity
                        #         checkboxInput(inputId = "rh_selection", 
                        #                       label = strong("Min. relative humidity (%) between:")),
                        #         
                        #         sliderInput(inputId = "rh_scale", 
                        #                     label = NULL, 
                        #                     min = 0, max = 100, 
                        #                     value = c(20, 50), step = 1),
                        #         
                        #         #Max gust speed
                        #         checkboxInput(inputId = "max_gspd_selection", 
                        #                       label = strong("Max. gust speed (mph) less than")), 
                        #         
                        #         sliderInput(inputId = "gsp_scale", 
                        #                     label = NULL, 
                        #                     min = 0, max = 100, 
                        #                     value = 25, step = 1),
                        #         
                        #         #Fuel Moisture 10
                        #         checkboxInput(inputId = "fm10_selection", 
                        #                       label = strong("Min. 10-hour Fuel Moisture (%) between:")), 
                        #         
                        #         sliderInput(inputId = "fm10_scale", 
                        #                     label = NULL, 
                        #                     min = 0, max = 100, 
                        #                     value = c(7, 20), step = 1),
                        #         
                        #         #Fuel Moisture 100
                        #         checkboxInput(inputId = "fm100_selection", 
                        #                       label = strong("Min. 100-hour Fuel Moisture (%) between:")), 
                        #         
                        #         sliderInput(inputId = "fm100_scale", 
                        #                     label = NULL, 
                        #                     min = 0, max = 100, 
                        #                     value = c(7, 20), step = 1),
                        #         
                        #         #Fuel Moisture 1000
                        #         checkboxInput(inputId = "fm1000_selection", 
                        #                       label = strong("Min. 1000-hour Fuel Moisture (%) between:")), 
                        #         
                        #         sliderInput(inputId = "fm1000_scale", 
                        #                     label = NULL, 
                        #                     min = 0, max = 100, 
                        #                     value = c(7, 20), step = 1),
                        #         
                        #         #Burn Index
                        #         checkboxInput(inputId = "bi_selection", 
                        #                       label = strong("Max. burning index less than:")), 
                        #         
                        #         sliderInput(inputId = "bi_scale", 
                        #                     label = NULL, 
                        #                     min = 0, max = 100, 
                        #                     value = 15, step = 1),
                        #         
                        #         #ERC
                        #         checkboxInput(inputId = "erc_selection", 
                        #                       label = strong(HTML(paste0("Max. energy release component (BTUs/ft", tags$sup("2"), ") less than:")))),
                        #         
                        #         sliderInput(inputId = "erc_scale", 
                        #                     label = NULL, 
                        #                     min = 0, max = 100, 
                        #                     value = 18, step = 1),
                        #         
                        #         #Don't make plots until action button is hit:
                        #         actionButton(inputId = "custom_bw_action_button",
                        #                      label = "Calculate Burn Window")
                        #         
                        # 
                        #         ),
                        #     
                        #     mainPanel(
                        #             p("In Development")
                        #         )
                            # )
               ),
               
               
               
               tabPanel("Technical Details", 
                        mainPanel(
                            
                            p("“Burn windows” are days in which conditions are feasible 
                            for conducting prescribed burning.  We consider a burn window 
                            to occur when 1) weather patterns are within prescription, 
                            2) air quality regulators have granted permission to burn, 
                            and 3) resources are available to conduct the prescribed burn. 
                            To understand when burn windows occur most frequently 
                            throughout the year", 
                            tags$a(href = "https://link.springer.com/content/pdf/10.1186/s42408-020-00071-3.pdf", "Striplin et al. (2020)", 
                                   target = "_blank"), 
                            "evaluated patterns of historical burn windows since 1999 
                            in the Lake Tahoe Basin. This online application extends the 
                            burn window assessment and allows users to identify historical 
                            burn window patterns in different US Forest Service 
                            Districts in California."),
                            
                            h3("Air Quality"), #h3 is the header level, h1 is bigger, up to h6 is smaller
                            
                            p("We used designations from the California Air Resources Board (",
                              tags$a(href = "https://www.arb.ca.gov/smp/histor/histor.htm", "CARB Burn Days", target = "_blank"), 
                              ") to classify each day as “Burn” or “No-burn.” These 
                              designations are made by Agricultural Burn Decision staff and 
                              are based on weather and air quality conditions. 
                              The burn day designations are specific to the air
                              basin in which a forest district is located."),
                            
                            h3("Firefighting Resource Availability"),
                            p("We used preparedness level (PL) designations from 
                              the Northern and Southern California Geographic Areas 
                              (NOPS and SOPS) as a proxy indicator for the availability 
                              of operational and contingency resources for 
                              prescribed fire implementation. We considered a 
                              PL < 3 as indicative of prescribed burn feasibility. 
                              When NOPS or SOPS preparedness levels were unavailable, 
                              we substituted national preparedness levels. "),
                           
                            h3("Weather"), 
                            p("We downloaded historical weather data from remote 
                              automated weather stations (RAWS) that were 
                              identified by forest and fire managers in each 
                              forest district. RAWS data were downloaded from 
                              both the National Fire and Aviation Management 
                              Web Applications (", 
                              tags$a(href = "https://fam.nwcg.gov/fam-web/", "FAMWEB", target = "_blank"), 
                              ") and the Program for Climate, Ecosystem, and Fire 
                              Applications (", 
                              tags$a(href = "https://cefa.dri.edu/raws/" , "CEFA", target = "_blank"),
                              "). After download, we conducted quality control 
                              for each weather station using Fire Family Plus 
                              version 5.0.  QA/QC included removing obviously 
                              incorrect temperature readings (e.g. -80 F or 120 
                              F during winter months), wind speeds > 100 mph, and
                              likely erroneous precipitation estimates 
                              (e.g. > 8 inches in an hour). Each RAWS station has a
                              different period of record, often starting 
                              between 1999 – 2010. We downloaded all available 
                              hourly weather observations for each station 
                              for this application."),
                            
                            h4("Snow Flag"),
                            p("For both simple and custom burn windows, users 
                              are able to define whether or not the snow flag 
                              should be considered. A snow flag is recorded in 
                              the RAWS data when there is snow on the ground, 
                              and this flag automatically affects temperature, 
                              relative humidity, and fuel moisture readings such 
                              that they won’t be feasible for a prescribed burn. 
                              Nevertheless, in some areas, it is desirable to 
                              conduct pile burning or other underburning when 
                              there is snow on the ground.  Thus, we have 
                              included a “Snow Flag” option that allows users 
                              to decide whether or not they want to snow flag 
                              considered when assessing historical burn windows. 
                              By selecting “Yes,” the application will determine 
                              that no burning can take place when there is a 
                              snow flag. By selecting “No,” the application will
                              remove the snow flag and there will likely be more
                              feasible days identified for burning during the winter"),
                            
                            h4("Simple Burn Windows"),
                            p("Simple Burn Windows use the fire weather prescription 
                              criteria described by ", 
                              tags$a(href = "https://link.springer.com/content/pdf/10.1186/s42408-020-00071-3.pdf", "Striplin et al. (2020)", target = "_blank"), 
                              ", and are based on the coincidence of three criteria:"),
                            tags$ol(
                                tags$li("California Air Resources Board burn day designations"),
                                tags$li("Days when weather falls within burn plan prescription (based on historical RAWS data):"),
                                tags$ul(
                                    tags$li("Minimum relative humidity between 20 and 50%"),
                                    tags$li("1-hour fuel moisture between 7 and 20%"),
                                    tags$li("Maximum wind speeds (6.1 m above ground) < 25 mph")
                                ),
                                tags$li("Operational preparedness level based on the 
                                        Northern/Southern California Geographic Area 
                                        and national preparedness levels.")
                            ),
                            p("We have already set these criteria for each weather station. 
                              The user simply chooses their weather station of 
                              interest and the application will display graphs 
                              corresponding to the historical burn window occurrences 
                              for that station. you have to do is choose your 
                              weather station of interest."),
                            
                            h4("Custom Burn Window (In Development)"),
                            p("In the Custom Burn Window panel, users are able to 
                              set their own criteria for designating burn windows. 
                              The user must select the checkbox for each criterion 
                              they would like considered for their burn window 
                              (e.g. CARB Burn Day Designation, NOPS/SOPS Preparedness
                              Level, and various weather criteria).  We have currently
                              provided options to control the several different 
                              weather criteria (i.e., snow flag, minimum relative 
                              humidity, maximum wind gust speed, minimum 10/100/1000 
                              hour fuel moisture, maximum burning index, and maximum 
                              energy release component).  The user simply checks the 
                              box corresponding to the criteria they wish to use and 
                              sets the range for each variable as appropriate for 
                              their burn prescription. "),
                            br(), #line break
                            p("This section is still in development and we are more 
                              than happy to accept feature requests.")
                        )), 
               
               
               tabPanel("About", 
                        mainPanel(
                            
                            p("“This application was developed by Alison Paulson 
                              (UC Davis), Randy Striplin (USFS), and Hugh Safford 
                              (USFS/UC Davis) with funding from the Joint Fire 
                              Science Program and CalFire. The application is based
                              on concepts discussed by",
                              tags$a(href = "https://link.springer.com/content/pdf/10.1186/s42408-020-00071-3.pdf", "Striplin et al. (2020)", 
                                     target = "_blank"),
                              "."), 
                              br(), #line break
                            
                            p("The code and data used in this app are available on", 
                              tags$a(href = "https://github.com/akpaulson/Burn-Windows", "GitHub", 
                                     target = "_blank"), 
                              "."),
                            br(), #line break
                            
                            p("Please contact Alison Paulson (akpaulson @ ucdavis.edu)
                            with any feedback, questions, or feature requests.")
                        )),
               
               theme = shinytheme("flatly")),
               # id = NULL, selected = NULL,
               # position = c("static-top", "fixed-top", "fixed-bottom"),
               # header = NULL, footer = NULL, inverse = FALSE,
               # collapsible = FALSE, collapsable, fluid = TRUE, responsive = NULL,
               # theme = NULL, windowTitle = title),
    


)

#### Server Logic ####

# Define server logic
server <- function(input, output, session) {
    
    ##### Server For Simple Burn Windows: ####
    
    #Make Forest Selector respond to snowflag selection:
    observe({x = input$snowflag_selection
    
    updateSelectInput(session, 
                      inputId = 'forest_selection',
                      choices=unique(all_raws_fig2$Forest[all_raws_fig2$snow_flag==input$snowflag_selection]))
    })
    
    #Make District Selector respond to forest selection:
    observe({x = input$forest_selection
        
        updateSelectInput(session, 
                          inputId = 'district_selection',
                          choices=unique(all_raws_fig2$District[all_raws_fig2$Forest==input$forest_selection]))
    })
    
    #Make RAWS station selector respond to both forest/district selection: 
    observe({x = input$district_selection
    
        updateSelectInput(session,
                          inputId = 'raws_selection',
                          choices = unique(all_raws_fig2$RAWS_station[all_raws_fig2$Forest == input$forest_selection &
                                                                 all_raws_fig2$District == input$district_selection]))
    })
    
    #Don't make plots until action button selected: 
    observeEvent(input$bw_action_button, ignoreInit = TRUE, {


    #Code for Figure 2 from striplin et al: 
    output$burn_window_proportion <- renderPlot({
        
        isolate(  #Isolate makes it so the data aren't prepared until the calc 
                        # burn window action button is pressed. 
        #Select data for RAWS of interest:
        
        dat <- all_raws_fig2 %>% 
            filter(snow_flag == input$snowflag_selection, 
                   Forest == input$forest_selection,  
                   District == input$district_selection,
                   RAWS_station == input$raws_selection))
        
        ggplot(dat, aes(x = date, y = prop_PL, group = 1)) +
            geom_line() +
            geom_line(aes(y = prop_BD), color = "#2c7bb6") +
            geom_line(aes(y = prop_inRx), color = "#d73027") +
            geom_area(aes(y = prop_BW), fill = "gray") +
            scale_x_date(date_labels = "%d %b", # change date labelling
                         breaks = dat$date[seq(1, length(dat$date), by = 7)], 
                         #only 7th day displayed
                         expand = c(0,0)) + 
            scale_y_continuous(expand = c(0,0)) +
            labs(y = "Proportion of days")+
            theme_classic() +
            theme(axis.text.x = element_text(angle = 90), 
                  axis.title.x = element_blank())
        })
        
    
    output$fig1_caption <- renderText({
        "Figure 1: Frequency of burn window and burn window components occurring on different days throughout the period of record.
        The black line indicates the frequency of days when the Northern California Geographic
        Area preparedness level was < 3; 
        the blue line represents the frequency of permissible burn days as designated by the California Air Resources Board; 
        the red line indicates the frequency of days that meet prescribed burn criteria. 
        The grey shaded area indicates the frequency of days that meet all three burn window criteria."
    })
    
   # Code for Figure 3 from striplin et al:
    output$burn_window_days <- renderPlot({
        
        isolate(
        #Select data for RAWS of interest:
        dat <- all_raws_fig3 %>%
            filter(snow_flag == input$snowflag_selection,
                   Forest == input$forest_selection,
                   District == input$district_selection,
                   RAWS_station == input$raws_selection) %>%
            #This is to try to control the y-axis limits for a given station:
            mutate(max = max(mean_percent) + max(se_percent)+7))

        ggplot(dat, aes(x = month, y = mean_percent)) +
            geom_bar(stat = "identity", color = "black", fill = "grey") +
            geom_text(aes(label = round(mean_percent, 0), vjust = -4.5)) +
            geom_errorbar(aes(ymin = mean_percent - se_percent,
                              ymax = mean_percent + se_percent), width = 0.2) +
            scale_y_continuous(limits = c(0, dat$max), expand = c(0,0)) +
            labs(y = "Percent days (%)") +
            theme_classic() +
            theme(axis.title.x = element_blank())})
    
    output$fig2_caption <- renderText({
        "Figure 2: Mean percent (+/- SE) of all days in each month that were
        burn windows during the period of record."
    })

    #Code for Figure 4 from striplin et al:
    output$multiple_day_bw <- renderPlot({
        
        
        isolate(
        dat <- all_raws_fig4 %>%
            filter(snow_flag == input$snowflag_selection,
                   Forest == input$forest_selection,
                   District == input$district_selection,
                   RAWS_station == input$raws_selection))

        ggplot(dat, aes(x = month, y = mean_bw,
                                ymin = mean_bw - se_bw,
                                ymax = mean_bw + se_bw,
                                fill = streak_max_chr)) +
            geom_bar(stat = "identity",
                     color = "black",
                     position = position_dodge()) +
            geom_errorbar(position = position_dodge(0.9), width = 0.2) +
            scale_fill_manual(values = c("lightgray", "gray60", "black")) +
            scale_y_continuous(expand = c(0,0)) +
            labs(y = "Mean number of muliple-day \n burn windows") +
            theme_classic() +
            theme(axis.title.x = element_blank(),
                  legend.title = element_blank())})
    
    output$fig3_caption <- renderText({
        "Figure 3: Mean number of multiple-day burn windows per month during the period of record.
        Multiple-day burn windows were consecutive days that met burn-window criteria; classes do
        not include single-day burn window occurrences; larger consecutive-day burn window periods
        exclude shorter periods (i.e. 2-3 days periods are not counted in 4-5 day periods)."
    })


    #Code for Figure 5 from striplin et al:
    output$monthly_yearly_bw <- renderPlot({
        
        isolate(
        dat <- all_raws_fig5 %>%
            filter(snow_flag == input$snowflag_selection,
                   Forest == input$forest_selection,
                   District == input$district_selection,
                   RAWS_station == input$raws_selection))

        ggplot(dat, aes(x = year, y = bw_count)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            facet_wrap(~month, nrow = 4, ncol = 3) +
            scale_x_continuous(breaks = seq(1999, 2019, by = 2),
                               expand = c(0,0)) +
            labs(x = "Year", y = "Number of Days") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90,  vjust=0.5, hjust=1),
                  axis.text = element_text(size = 10))})
    
    output$fig4_caption <- renderText({
        "Figure 4: Frequency of burn window days in each month and year for the period of record."
    })

        }) #End ObserveEvent
    
    
    # #### Server For Custom Burn Windows: ####
    # 
    # #Make District Selector respond to forest selection:
    # observe({x = input$forest_selection_custom
    # 
    # updateSelectInput(session, 
    #                   inputId = 'district_selection_custom',
    #                   choices=unique(custom_bw_data$District[custom_bw_data$Forest==input$forest_selection_custom]))
    # })
    # 
    # #Make RAWS station selector respond to both forest/district selection: 
    # observe({x = input$Select2
    # 
    # updateSelectInput(session,
    #                   inputId = 'raws_selection',
    #                   choices = unique(custom_bw_data$RAWS_station[custom_bw_data$Forest == input$forest_selection_custom &
    #                                                                     custom_bw_data$District == input$district_selection_custom]))
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
