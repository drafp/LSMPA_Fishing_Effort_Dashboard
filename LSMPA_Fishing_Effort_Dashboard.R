#### LSMPA Fishing Effort Dashboard ####
## Licensing and Citation
# The following code relies on data downloaded from Global Fishing Watch
# Accessed under Creative Commons.
#
# Copyright 2021, Global Fishing Watch, Inc. Accessed on 30 June 2021.
# https://globalfishingwatch.org/data-download/datasets/public-fishing-effort.
#
# EEZ Boundary Data: 
# "Flanders Marine Institute (2019). Maritime Boundaries Geodatabase:
# Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. 
# Available online at https://www.marineregions.org/. https://doi.org/10.14284/386",
#
# LSMPA Boundary Data:
# "UNEP-WCMC and IUCN (2021), Protected Planet: The World Database on Protected 
# Areas (WDPA) and World Database on Other Effective Area-based Conservation 
# Measures (WD-OECM) [Online], August 2021, Cambridge, UK: UNEP-WCMC and IUCN. 
# Available at: www.protectedplanet.net."

                          


## Author Comments
# This project explores changes in fishing effort at various timescales in
# regions with established Large Scale Marine Protected Areas (LSMPAs).
#
# by Diego Rafael Perez, diegoraf@stanford.edu


## Load Libraries
library(leaflet)
library(rgdal)
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(shiny)
library(htmltools)
library(sf)
library(ggthemes)
library(sp)
library(maps)
library(ggplot2)
library(dplyr)
library(foreach)
library(shinythemes)
library(shinydashboard)
library(httr)
library(jsonlite)
library(leaflet.providers)
library(leaflet.extras)
library(plotly)
library(shinyWidgets)
library(BAMMtools)
library(gt)
library(lubridate)
library(raster)
library(ggsn)
library(diffeR)

#### DATA SETUP: Import Data 

#setwd("LSMPA_Fishing_Effort_Dashboard/Data Dependencies")

# Load general fishing vessel information
tbl.fishing.vessels <- read_csv("Fishing Vessel Reference Information/fishing-vessels-v2.csv")

#### Load data for each LSMPA/EEZ ####
# load global eezs
global.eez <- read_sf("Geospatial Boundaries/Global Exclusive Economic Zones")

# Filter global EEZs to form Parties to Nauru Agreement boundary
nauru.parties.eez <- global.eez %>% filter(`GEONAME` %in% c("Micronesian Exclusive Economic Zone",
                                             "Kiribati Exclusive Economic Zone (Gilbert Islands)",                        
                                             "Kiribati Exclusive Economic Zone (Line Islands)",
                                             "Kiribati Exclusive Economic Zone (Phoenix Islands)",
                                             "Marshall Islands Exclusive Economic Zone",
                                             "Nauruan Exclusive Economic Zone",
                                             "Palau Exclusive Economic Zone",
                                             "Papua New Guinean Exclusive Economic Zone",
                                             "Solomon Islands Exclusive Economic Zone",
                                             "Tuvaluan Exclusive Economic Zone"))
                                           
### Palau National Marine Sanctuary ###
# load geospatial boundary for Palau National Marine Sanctuary
sf.pnms <- read_sf("Geospatial Boundaries/LSMPAs/PNMS Shapefile")

# load geospatial boundary for Exclusive Economic Zone of Palau
#sf.palau.eez <- read_sf("Geospatial boundaries/EEZs/Palau EEZ Shape")

# load nearby eezs
palau.neighboring.eez <- global.eez %>% filter(`GEONAME` %in% c("Micronesian Exclusive Economic Zone",
                                                                "Palau Exclusive Economic Zone",
                                                                #"Papua New Guinean Exclusive Economic Zone",
                                                                "Indonesian Exclusive Economic Zone",
                                                                #"Guam Exclusive Economic Zone",
                                                                "Philippines Exclusive Economic Zone"
                                                                ))




# Access bounding box of geometry for use in filtering data
bbox.pmns <-st_bbox(sf.pnms)
numeric.bbox.pmns <- as.numeric(bbox.pmns)

# Create buffer established by kconstant in degrees beyond boundary of MPA
kboundarybuffer <- 2
vector.boundary.buffer <- c(-kboundarybuffer, -kboundarybuffer, kboundarybuffer,
                            kboundarybuffer)

# Alter bounding box by buffer
numeric.bbox.pmns <- numeric.bbox.pmns + vector.boundary.buffer

# Load LSMPA Specific Datasets
# MMSI
pnms.mmsi.data.filename <- "LSMPA Specfic Datasets/Filtered For PNMS/MMSI.csv"

pnms.tbl.daily.mmsi.data <- read_csv(
  pnms.mmsi.data.filename, col_types = cols(
    date = col_date(format = ""),
    cell_ll_lat = col_double(),
    cell_ll_lon = col_double(),
    mmsi = col_double(),
    hours = col_double(),
    fishing_hours = col_double()
  )
)

# Create a merged dataset with mmsi and vessel data
pnms.vessel.tbl.daily.mmsi.data <- pnms.tbl.daily.mmsi.data %>% left_join(
  tbl.fishing.vessels, by = c("mmsi"))


# # Fleet -- Deprecated
# pnms.fleet.data.filename <- "LSMPA Specfic Datasets/PNMS/Fleet.csv"
# 
# pnms.tbl.daily.fleet.data <- read_csv(
#   pnms.fleet.data.filename, col_types = cols(
#     date = col_date(format = ""),
#     cell_ll_lat = col_double(),
#     cell_ll_lon = col_double(),
#     flag = col_character(),
#     geartype = col_character(),
#     hours = col_double(),
#     fishing_hours = col_double(),
#     mmsi_present = col_double()
#   )
# )

# Set parameters for mapping: Palau
# Create labels for EEZ objects
# Palau and neighbors
palau.labels <- c("Palau EEZ", "Philippines EEZ", "Micronesia EEZ", "Indonesia EEZ", "High Seas")
palau.lon <- c(134, 128.7, 137, 134, 135)
palau.lat <- c(5, 8, 11, 1, 13)

palau.labels.tbl <- tibble(id = palau.labels, lon = palau.lon, lat = palau.lat)



# ## Niue LSMPA -- include if data for Niue is present
# # load geospatial boundary for Niue
# sf.niue <- read_sf("Geospatial Boundaries/LSMPAs/Niue LSMPA Shapefile")
# 
# # load nearby eezs
# niue.neighboring.eez <- global.eez %>% filter(`GEONAME` %in% c("Tongan Exclusive Economic Zone",
#                                                                "Samoan Exclusive Economic Zone",
#                                                                "American Samoa Exclusive Economic Zone",
#                                                                "Cook Islands Exclusive Economic Zone",
#                                                                "Niue Exclusive Economic Zone"
# ))
# 
# 
# 
# # Load LSMPA Specific Datasets
# # MMSI
# niue.mmsi.data.filename <- "LSMPA Specfic Datasets/niuemmsi.csv"
# 
# niue.tbl.daily.mmsi.data <- read_csv(
#   niue.mmsi.data.filename, col_types = cols(
#     date = col_date(format = ""),
#     cell_ll_lat = col_double(),
#     cell_ll_lon = col_double(),
#     mmsi = col_double(),
#     hours = col_double(),
#     fishing_hours = col_double()
#   )
# )
# 
# # Create a merged dataset with mmsi and vessel data
# niue.vessel.tbl.daily.mmsi.data <- niue.tbl.daily.mmsi.data %>% left_join(
#   tbl.fishing.vessels, by = c("mmsi"))
# 
# # Create Niue Labels
# niue.labels <- c("Niue EEZ", "High Seas")
# niue.lon <- c(-168, -166)
# niue.lat <- c(-20, -22.5)
# 
# niue.labels.tbl <- tibble(id = niue.labels, lon = niue.lon, lat = niue.lat)



#### Establish Non-Reactive Functions ####
## Jenks Functions and Labeling
# Create jenks breaks labels
create_jenks_labels <- function(jenksbreaks) {
  breakslabel <- c()
  
  for (n in 1:(length(jenksbreaks) - 1)) {
    breakslabel <- append(breakslabel, paste(as.character(round(jenksbreaks[n]), digits = 2), as.character(round(jenksbreaks[(n + 1)]), digits = 2), sep = " — "))
  }
  
  breakslabel
}

# Create jenks breaks
add_jenks <- function(values, jenksbreaks) {
  breaks <- c()
  
  for (value in values) {
    for (n in 1:(length(jenksbreaks) - 1)) {
      if (value >= jenksbreaks[n] & value <= jenksbreaks[(n + 1)]) {
        breaks <- append(breaks, as.character(n))
        break
      }
    }
  }
  breaks
}


create_jenks_labels_difference <- function(jenksbreaks, difference) {
  
  if (difference == T) {
    neg <- jenksbreaks[jenksbreaks <= 0]
    pos <- jenksbreaks[jenksbreaks >= 0]
    
    negbreakslabel <- c()
    for (n in 1:(length(neg) - 1)) {
      negbreakslabel <- append(negbreakslabel, paste(as.character(round(neg[n]), digits = 2), as.character(round(neg[(n + 1)]), digits = 2), sep = " — "))
    }
    
    posbreakslabel <- c()
    for (n in 1:(length(pos) - 1)) {
      posbreakslabel <- append(posbreakslabel, paste(as.character(round(pos[n]), digits = 2), as.character(round(pos[(n + 1)]), digits = 2), sep = " — "))
    }
    
    breakslabel <- c(negbreakslabel, "no change (exactly 0)", posbreakslabel)
    
    breakslabel
    
  } else {
    breakslabel <- c()
    
    for (n in 1:(length(jenksbreaks) - 1)) {
      breakslabel <- append(breakslabel, paste(as.character(round(jenksbreaks[n]), digits = 2), as.character(round(jenksbreaks[(n + 1)]), digits = 2), sep = " — "))
    }
    
    breakslabel
  }
}


add_jenks_difference <- function(values, jenksbreaks, difference) {
  breaks.tbl <- tibble(values = values)
  
  for (jenksbreak in (jenksbreaks[1:(length(jenksbreaks) - 1)])) {
    column.name <- as.character(jenksbreak)
    column.values <- (values >= jenksbreak)
    breaks.tbl <- breaks.tbl %>% add_column(!!column.name := column.values)
  }
  
  
  if (difference == T) {
    zerovect <- (values == 0) * -0.5
    breaks.tbl <- breaks.tbl %>% add_column(zero = zerovect)
  }
  
  as.character(rowSums(breaks.tbl %>% dplyr::select(-values)))
}


# Set UI Features
add_disclosure <- function() {
  div(HTML(paste("<em>This dashboard is under development by the</em>", 
                 tags$a(href="https://oceansolutions.stanford.edu/", HTML("<em>Stanford Center for Ocean Solutions</em>")),
                 "<em>through funding from the</em>", 
                 tags$a(href="https://woods.stanford.edu/", HTML("<em>Stanford Woods Institute for the Environment</em>")),
                 "<em>via the</em>", 
                 tags$a(href="https://woods.stanford.edu/educating-leaders/education-leadership-programs/mentoring-undergraduates-interdisciplinary-research", HTML("<em>Mentoring Undergraduate Interdisciplinary Research</em>")),
                 "<em>program. While under development, the dashboard is not intended for resource planning purposes. 
                          Please contact diegoraf@stanford.edu for questions or feedback.</em>")))
}
  



#### SHINY USER INTERFACE
ui <-
  fluidPage(
  navbarPage("Large-Scale Marine Protected Areas Fishing Effort Dashboard", theme = shinytheme("flatly"),
             
             tabPanel("How To Use This Dashboard",
                      mainPanel(
                        fluidRow(
                          column (12,
                                  h1("How To Use This Dashboard:")
                          )
                        ),
                        "This dashboard explores changes in industrial fishing effort at various timescales in regions with established Large-Scale Marine Protected Areas (LSMPAs), and relies on data downloaded from Global Fishing Watch, accessed under Creative Commons. Please see the \"Data Sources\" tab for citations.",
                        
                      
                        h3("What is Fishing Effort?"),
                        "Global Fishing Watch uses a convolutional neural network to classify industrial fishing vessels.",
                        "Global Fishing Watch also classifies vessels by what type of gear they use (fishing method) and the flag state of the vessel, meaning the country that has registered or liscenced the vessel.",
                        "More information on GLobal Fishing Watch's methods and analyses can be found in Kroodsma et al., 2018; see \"Data Sources\" for more information",
                        
                        h3("Select an LSMPA of Interest"),
                        "Each dashboard tab includes a selection box to choose a Large-Scale Marine Protected Area of interest. Choose the LSMPA of interest in each tab to reactively generate outputs according to the selection and explore changes in industrial fishing effort.",
                        
                        h3("Key Terms"),
                        
                        "fishing: a combination of vessels of unknown fishing gear
 - drifting_longlines: drifting longlines
 - seiners: vessels using seine nets, including potential purse seine vessels
   targeting tuna and other species, as well as danish and other seines
     - purse_seines: purse seines, both pelagic and demersal
        - tuna_purse_seines: large purse seines primarily fishing for tuna.
        - other_purse_seines: purse seiners fishing for mackerel, anchovies, etc, often smaller and operating nearer the coast than tuna purse seines.
    - other_seines: danish seines and other seiners not using purse seines.
 - trawlers: trawlers, all types
 - pole_and_line: vessel from which people fish with pole and line.
 - trollers: vessel that tows multiple fishing lines.
 - fixed_gear: a category that includes potential set longlines, set gillnets,  and pots and traps
     - pots_and_traps: vessel that deploys pots (small, portable traps) or traps to
       catch fish
     - set_longlines: vessel that fishes by setting longlines anchored to the
       seafloor. These lines have shorter hooked, typically baited, lines hanging
       from them
     - set_gillnets: vessel that fishes by setting gillnets anchored to the seafloor.
 - dredge_fishing: vessel that tows a dredge the scrapes up edible bottom
   dwellers such as scallops or oysters.
 - squid_jigger: squid jiggers, mostly large industrial pelagic operating vessels
",
                        
                        h3("Dashboard Tab Information"),
                        
                        HTML("<ul><li>LSMPA Information: </li>
                             <li>Map Comparison: </li>
                             <li>Over Time: </li>
                             <li>Map Time Series: </li>
                             <li>Monthly Average Map: </li></ul>"),
                        
                        
                        h3("Key Assumptions"),
                        "Fishing effort data availability near featured Large-Scale Marine Protected Areas has increased over time, particularly as the automatic identification system (AIS), the method used to identify vessels (including by Global Fishing Watch), has become more prevalent in industrial fleets. We recommend comparing similar years (limited year ranges) to decrease uncertainty in multi-year comparisons. Early data (roughly 2012 through 2014) is not congruent with later data and should be compared with caution. Limited data availability does not necessarily mean less industrial fishing effort."
                        # Information: This dashboard relies on fishing effort quantification 
                        # All metrics related to fishing effort and vessel characterisitics are determined 
                        # by Global Fishing Watch according to their analyses. For more information on Global Fishing Watch and their Fishing Effort Calclations, visit
                        
                        # img(src = "oceansolutions.png", height = 100, width = 800, style="display: block; margin-left: auto; margin-right: auto;"),
                        # fluidRow(column(12,HTML("<br><div class='footer'></div><br>")))
                      ),
                      
                      fluidRow(column(12,HTML("<br><div class='footer'></div><br>")))      
             ),
             
             tabPanel("LSMPA Information",
                      sidebarLayout(
                        sidebarPanel(
                          h1("Large-Scale Marine Protected Area Information"),
                        
                          "Explore changes in industrial fishing effort prior to and after the designation of a select LSMPA with this dashboard tab. Please select inputs to filter the data below to reactively display visualizations.",
                          br(),
                          br(),
                          radioButtons(inputId = "lsmpa_summary", "Select LSMPA of interest",
                                       choiceValues = c("pnms", "niue"),
                                       choiceNames =  c("Palau National Marine Sanctuary", "Niue Moana Mahu Marine Protected Area"),
                                       selected = c("pnms")),
                          
                          br(),
                          radioButtons(inputId = "hourtype_summary", "Select Time Parameter",
                                       choiceValues = c("hours", "fishing_hours"),
                                       choiceNames =  c("Total Vessel Hours (includes fishing and transit)", "Fishing Hours"),
                                       selected = c("hours")),
                          br(),
                          ## Vessel Flag Filtering
                          selectizeInput(
                            inputId = "mmsi_flag_layer_summary",
                            label = "Filter by Vessel Flag (Country):",
                            choices = c("All", names(sort(table(pnms.vessel.tbl.daily.mmsi.data$flag_gfw),decreasing=TRUE)[1:4])),
                            selected = c("All"),
                            multiple = T
                          ),
                          br(),
                          ## Vessel Gear-Type Filtering
                          selectizeInput(
                            inputId = "mmsi_gear_layer_summary",
                            label = "Filter by Vessel Class (Gear-Type of Vessel):",
                            choices =  c("All", unique(pnms.vessel.tbl.daily.mmsi.data$vessel_class_gfw)),
                            selected = c("All"),
                            multiple = T
                          ),
                          add_disclosure()
                        ),
                        mainPanel(
                          fluidRow(
                            column (12,
                                    h1(textOutput("lsmpa_title_summary"))
                            )),
                          fluidRow(column(12, uiOutput("lsmpa_link"))),
                          fluidRow(column(12, uiOutput("lsmpa_text"))),
                          br(),


                          # fluidRow(
                          #   column(4, plotOutput("beforemap", width = "100%", height = "600px")),
                          #   column(4, plotOutput("aftermap", width = "100%", height = "600px")),
                          #   column(4, plotOutput("changemap", width = "100%", height = "600px"))
                          # ),

                          tabsetPanel(
                            tabPanel("Before/With", 
                                     fluidRow(column(12, h4("Policy Context"))),
                                     fluidRow(column(12, h5(textOutput("lsmpa_text_beforeafter")))),
                                     fluidRow(
                              column(6, h3(textOutput("before_title"))),
                              column(6, h3(textOutput("after_title")))
                            ),
                            fluidRow(
                              column(6, plotOutput("beforemap", width = "100%", height = "700px")),
                              column(6, plotOutput("aftermap", width = "100%", height = "700px"))
                            ), 
                            fluidRow(
                              column(6, h4("Overall Mean Hours:")),
                              column(6, h4("Overall Mean Hours:"))
                            ),
                            fluidRow(
                              column(6, htmlOutput("summary_text_before_mean")),
                              column(6, htmlOutput("summary_text_after_mean"))
                            ),
                            fluidRow(
                              column(6, h4("Overall Cumulative Days:")),
                              column(6, h4("Overall Cumulative Days:"))
                            ),
                            fluidRow(
                              column(6, htmlOutput("summary_text_before_cumul")),
                              column(6, htmlOutput("summary_text_after_cumul"))
                            )
                            
                            ),
                            tabPanel("Difference", plotOutput("changemap", width = "100%", height = "800px"))
                          )
                        )
                      ),
                      fluidRow(column(12,HTML("<br><div class='footer'></div><br>"))),
                      tags$footer("Early fishing effort data (roughly 2012 through 2014) is not congruent with data for later years and should be compared with caution. 
Limited data availability does not necessarily mean less industrial fishing effort.
", align = "center", style = "
              
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: black;
              padding: 10px;
              background-color: white;
              z-index: 1000;")
             ),
             
             tabPanel("Map Comparison",
                      sidebarLayout(
                        sidebarPanel(
                          h1("Map Comparison"), 
                          
                          "Compare two map inputs with this dashboard tab.  Please independently select filtering inputs below to reactively display the map comparison visualizations.",
                          br(),
                          br(),
                          radioButtons(inputId = "lsmpa_yearly", "Select LSMPA",
                                       choiceValues = c("pnms"),
                                       choiceNames =  c("Palau National Marine Sanctuary"),
                                       selected = c("pnms")),
                          br(),
                          splitLayout(
                            radioButtons(inputId = "hourtype_yearly_1", "Select Time Parameter for Map 1",
                                         choiceValues = c("hours", "fishing_hours"),
                                         choiceNames =  c("Total Vessel Hours (fishing + transit)", "Fishing Hours"),
                                         selected = c("hours")),
                            radioButtons(inputId = "hourtype_yearly_2", "Select Time Parameter for Map 2",
                                         choiceValues = c("hours", "fishing_hours"),
                                         choiceNames =  c("Total Vessel Hours (fishing + transit)", "Fishing Hours"),
                                         selected = c("hours"))
                          ),
                          br(),
                          splitLayout(
                            selectizeInput(
                              inputId = "year_check_yearly_1",
                              label = "Select Year(s) for Map 1:",
                              choices = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
                              selected = c("2020"),
                              multiple = T
                            ),
                            selectizeInput(
                              inputId = "year_check_yearly_2",
                              label = "Select Year(s) for Map 2:",
                              choices = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
                              selected = c("2019"),
                              multiple = T
                            )
                          ),
                          
                          div(HTML("<em>If more than one year is selected, selected years are averaged</em>")),
                          br(),
                          
                          ## Vessel Flag Filtering
                          splitLayout(
                            selectizeInput(
                              inputId = "mmsi_flag_layer_yearly_1",
                              label = "Filter by Vessel Flag/Country (Map 1):",
                              choices = c("All", names(sort(table(pnms.vessel.tbl.daily.mmsi.data$flag_gfw),decreasing=TRUE)[1:4])),
                              selected = c("All"),
                              multiple = T
                            ),
                            selectizeInput(
                              inputId = "mmsi_flag_layer_yearly_2",
                              label = "Filter by Vessel Flag/Country (Map 2):",
                              choices = c("All", names(sort(table(pnms.vessel.tbl.daily.mmsi.data$flag_gfw),decreasing=TRUE)[1:4])),
                              selected = c("All"),
                              multiple = T
                            )
                          ),
                          br(),
                          ## Vessel Gear-Type Filtering
                          splitLayout(
                            selectizeInput(
                              inputId = "mmsi_gear_layer_yearly_1",
                              label = "Filter by Vessel Class/Gear (Map 1):",
                              choices =  c("All", unique(pnms.vessel.tbl.daily.mmsi.data$vessel_class_gfw)),
                              selected = c("All"),
                              multiple = T
                            ),
                            selectizeInput(
                              inputId = "mmsi_gear_layer_yearly_2",
                              label = "Filter by Vessel Class/Gear (Map 2):",
                              choices =  c("All", unique(pnms.vessel.tbl.daily.mmsi.data$vessel_class_gfw)),
                              selected = c("All"),
                              multiple = T
                            )
                          ),
                          
                          tags$head(tags$style(HTML("
                            .shiny-split-layout > div {
                              overflow: visible;
                            }
                          "))),
                          
                          
                          add_disclosure()
                        ),
                        
                        mainPanel(
                          fluidRow(
                            column (12,
                                    h1(textOutput("lsmpa_title_yearly"))
                            )
                          ),
                          fluidRow(
                            column(6, plotOutput("yearlymap1", width = "100%", height = "800px")),
                            column(6, plotOutput("yearlymap2", width = "100%", height = "800px"))
                          ),
                          fluidRow(
                            column(6, h4("Overall Mean Hours:")),
                            column(6, h4("Overall Mean Hours:"))
                          ),
                          fluidRow(
                            column(6, htmlOutput("summary_text_map1_mean")),
                            column(6, htmlOutput("summary_text_map2_mean"))
                          ),
                          fluidRow(
                            column(6, h4("Overall Cumulative Days:")),
                            column(6, h4("Overall Cumulative Days:"))
                          ),
                          fluidRow(
                            column(6, htmlOutput("summary_text_map1_cumul")),
                            column(6, htmlOutput("summary_text_map2_cumul"))
                          )
                        )
                      ),
                      fluidRow(column(12,HTML("<br><div class='footer'></div><br>"))),
                      tags$footer("Early fishing effort data (roughly 2012 through 2014) is not congruent with data for later years and should be compared with caution. 
Limited data availability does not necessarily mean less industrial fishing effort.
", align = "center", style = "
              
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: black;
              padding: 10px;
              background-color: white;
              z-index: 1000;")
             ),
             
             tabPanel("Over Time",
                      sidebarLayout(
                        sidebarPanel(
                          h1("Over Time"), 
                          
                          "Visualize mean and cumulative time series of industrial fishing effort with this dashboard tab. Please select inputs below to reactively filter and display visualizations.",
                          br(),
                          br(),
                          radioButtons(inputId = "lsmpa_timeseries", "Select LSMPA",
                                       choiceValues = c("pnms"),
                                       choiceNames =  c("Palau National Marine Sanctuary"),
                                       selected = c("pnms")),
                          
                          br(),
                          "To limit the calculations to a certain spatial extent, including the boundary of the LSMPA, select the corresponding option below.",
                          br(),
                          br(),
                          radioButtons(inputId = "boundary_limit_timeseries", "Limit Spatial Data Extent:",
                                       choiceValues = c("none", "eez", "lsmpa"),
                                       choiceNames =  c("Not Constrained (includes buffer around EEZ and/or LSMPA boundary", "Constrain Data to EEZ Boundary", "Constrain Data to LSMPA Boundary"),
                                       selected = c("none")),
                          br(),
                          
                          radioButtons(inputId = "hourtype_timeseries", "Select Time Parameter",
                                       choiceValues = c("hours", "fishing_hours"),
                                       choiceNames =  c("Total Vessel Hours (includes fishing and transit)", "Fishing Hours"),
                                       selected = c("hours")),
                          br(),
                          
                          # selectizeInput(
                          #   inputId = "year_check_timeseries",
                          #   label = "Select Year(s):",
                          #   choices = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
                          #   selected = c("2020"),
                          #   multiple = T
                          # ),
                          
                          dateRangeInput(
                            inputId = "year_range_timeseries",
                            label = "Select Date Range:",
                            start = "2019-01-01",
                            end = "2020-12-31",
                            min = "2012-01-01",
                            max = "2020-12-31"
                          ),
                          
                          br(),
                          
                          ## Vessel Flag Filtering
                          selectizeInput(
                            inputId = "mmsi_flag_layer_timeseries",
                            label = "Filter by Vessel Flag (Country):",
                            choices = c("All", names(sort(table(pnms.vessel.tbl.daily.mmsi.data$flag_gfw),decreasing=TRUE)[1:4])),
                            selected = c("All"),
                            multiple = T
                          ),
                          br(),
                          ## Vessel Gear-Type Filtering
                          selectizeInput(
                            inputId = "mmsi_gear_layer_timeseries",
                            label = "Filter by Vessel Class (Gear-Type of Vessel):",
                            choices =  c("All", unique(pnms.vessel.tbl.daily.mmsi.data$vessel_class_gfw)),
                            selected = c("All"),
                            multiple = T
                          ),
                          br(),
                          radioButtons(inputId = "grouping_timeseries", "Group Data",
                                       choiceValues = c("none", "flag", "gear"),
                                       choiceNames =  c("No Grouping", "Group By Selected Vessel Flag", "Group By Selected Vessel Gear-Type"),
                                       selected = c("none")),
                          add_disclosure()
                          # ,
                          # div(HTML("<em>EEZ Data Source: Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. Available online at https://www.marineregions.org/. https://doi.org/10.14284/386</em>"))
                        ),
                        
                        
                        
                        # Main panel: multiple tabs:
                        mainPanel(
                          fluidRow(
                            column (12,
                                    h1(textOutput("lsmpa_title_timeseries"))
                            )
                          ),
                          tabsetPanel(
                            tabPanel("Total", plotOutput("timeseries_cumul", width = "100%", height = "800px")),
                            tabPanel("Mean", plotOutput("timeseries_mean", width = "100%", height = "800px"))
                          )
                        )
                        
                        
                        
                        
                        
                      ),
                      fluidRow(column(12,HTML("<br><div class='footer'></div><br>"))),
                      tags$footer("Early fishing effort data (roughly 2012 through 2014) is not congruent with data for later years and should be compared with caution. 
Limited data availability does not necessarily mean less industrial fishing effort.
", align = "center", style = "
              
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: black;
              padding: 10px;
              background-color: white;
              z-index: 1000;")
             ),
             
             tabPanel("Map Time Series",
                      sidebarLayout(
                        sidebarPanel(
                          h1("Map Time Series"),
                          
                          "Visualize how industrial fishing effort has changed over time, spatially, with this dashboard tab. Please select inputs below to reactively filter and display visualizations. Selecting years and months to display will isolate data to explore seasonal variations.",
                          br(),
                          br(),
                          radioButtons(inputId = "lsmpa_annual", "Select LSMPA",
                                       choiceValues = c("pnms"),
                                       choiceNames =  c("Palau National Marine Sanctuary"),
                                       selected = c("pnms")),

                          br(),
                          
                          radioButtons(inputId = "hourtype_annual", "Select Time Parameter",
                                       choiceValues = c("hours", "fishing_hours"),
                                       choiceNames =  c("Total Vessel Hours (includes fishing and transit)", "Fishing Hours"),
                                       selected = c("hours")),
                          br(),
                          # checkboxGroupInput("year_check_annual", "Select Year(s) to Display:",
                          #                    choices = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
                          #                    selected = c("2020")),
                          # dropdownButton(
                          #   inputId = "dropdown",
                          #   circle = FALSE,
                          #   label = "Select Year(s) to Display:",
                          #   status = "custom",
                          #   checkboxGroupInput(inputId = "year_check_annual", label = NULL,
                          #                      choices = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
                          #                      selected = c("2020")
                          #   )
                          # ),
                          # sliderInput("year_range_annual", "Select Year(s):",
                          #             min = 2012, max = 2020,
                          #             value = c(2019,2020),
                          #             sep = ""),
                          selectizeInput(
                            inputId = "year_dropdown_annual",
                            label = "Select Year(s) to Display (up to 4):",
                            choices = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
                            selected = c("2020"),
                            multiple = T,
                            options = list(maxItems = 4)
                          ),
                          br(),
                          selectizeInput(
                            inputId = "month_check_annual",
                            label = "Select Month:",
                            choices = c("All Months" = 13, "January" = 1, "February" = 2, "March" = 3, "April" = 4, "May"  = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, "October" = 10, "November" = 11, "December" = 12),
                            selected = c(13),
                            multiple = T
                          ),
                          br(),
                          ## Vessel Flag Filtering
                          selectizeInput(
                            inputId = "mmsi_flag_layer_annual",
                            label = "Filter by Vessel Flag (Country):",
                            choices = c("All", names(sort(table(pnms.vessel.tbl.daily.mmsi.data$flag_gfw),decreasing=TRUE)[1:4])),
                            selected = c("All"),
                            multiple = T
                          ),
                          br(),
                          ## Vessel Gear-Type Filtering
                          selectizeInput(
                            inputId = "mmsi_gear_layer_annual",
                            label = "Filter by Vessel Class (Gear-Type of Vessel):",
                            choices =  c("All", unique(pnms.vessel.tbl.daily.mmsi.data$vessel_class_gfw)),
                            selected = c("All"),
                            multiple = T
                          ),
                          add_disclosure()
                        ),
                        mainPanel(
                          fluidRow(
                            column (12,
                                    h1(textOutput("lsmpa_title_annual"))
                                    #     tags$head(tags$style(HTML("
                                    # #final_text {
                                    #   text-align: center;
                                    # }
                                    # div.box-header {
                                    #   text-align: center;
                                    # }
                                    # "))),
                                    #     box(verbatimTextOutput("final_text"), status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 25, title = h1("Palau National Marine Sanctuary"))
                                    #

                            )),
                          fluidRow(column(12, uiOutput("maps")))
                          
                          # ,
                          # fluidRow(
                          #   column (12,
                          #           leafletOutput("annual", width = "100%", height = "900px")
                          #   ))
                          # fluidRow(
                          #   column (12,
                          #           downloadLink("download_ggplot", "Download Plot Output"))
                          # )
                        )
                      ),
                      fluidRow(column(12,HTML("<br><div class='footer'></div><br>"))),
                      tags$footer("Early fishing effort data (roughly 2012 through 2014) is not congruent with data for later years and should be compared with caution. 
Limited data availability does not necessarily mean less industrial fishing effort.
", align = "center", style = "
              
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: black;
              padding: 10px;
              background-color: white;
              z-index: 1000;")
             ),
             

             tabPanel("Monthly Average Map",
                      sidebarLayout(
                        sidebarPanel(
                          h1("Monthly Average Map"), 
                          
                          "Visualize seasonal variation in industrial fishing effort with this dashboard tab. Please select inputs below to reactively filter and display visualizations. Selecting multiple years will create averages for each month.",
                          br(),
                          br(),
                          radioButtons(inputId = "lsmpa", "Select LSMPA",
                                       choiceValues = c("pnms"),
                                       choiceNames =  c("Palau National Marine Sanctuary"),
                                       selected = c("pnms")),
                          
                          br(),
                          
                          radioButtons(inputId = "hourtype", "Select Time Parameter",
                                       choiceValues = c("hours", "fishing_hours"),
                                       choiceNames =  c("Total Vessel Hours (includes fishing and transit)", "Fishing Hours"),
                                       selected = c("hours")),
                          br(),
                          
                          selectizeInput(
                            inputId = "year_check",
                            label = "Select Year(s):",
                            choices = c("All", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
                            selected = c("2020"),
                            multiple = T
                          ),
                          div(HTML("<em>If more than one year is selected, selected years are averaged</em>")),
                          br(),
                          
                          ## Vessel Flag Filtering
                          selectizeInput(
                            inputId = "mmsi_flag_layer",
                            label = "Filter by Vessel Flag (Country):",
                            choices = c("All", names(sort(table(pnms.vessel.tbl.daily.mmsi.data$flag_gfw),decreasing=TRUE)[1:4])),
                            selected = c("All"),
                            multiple = T
                          ),
                          br(),
                          ## Vessel Gear-Type Filtering
                          selectizeInput(
                            inputId = "mmsi_gear_layer",
                            label = "Filter by Vessel Class (Gear-Type of Vessel):",
                            choices =  c("All", unique(pnms.vessel.tbl.daily.mmsi.data$vessel_class_gfw)),
                            selected = c("All"),
                            multiple = T
                          ),
                          add_disclosure()
                          ),
                        
                        mainPanel(
                          fluidRow(
                            column (12,
                                    h1(textOutput("lsmpa_title"))
                            )
                          ),
                          fluidRow(
                            column (12,
                                    plotOutput("monthlymap", width = "100%", height = "800px"))
                          )
                          
                          )
                      ),
                      fluidRow(column(12,HTML("<br><div class='footer'></div><br>"))),
                      tags$footer("Early fishing effort data (roughly 2012 through 2014) is not congruent with data for later years and should be compared with caution. 
Limited data availability does not necessarily mean less industrial fishing effort.
", align = "center", style = "
              
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: black;
              padding: 10px;
              background-color: white;
              z-index: 1000;")
             ),
             
             
             tabPanel("Data Sources",
                      mainPanel(
                        fluidRow(
                          column (12,
                                  h1("Data Sources")
                          )
                        ),
                        h3("Fishing Effort and Vessel Characteristics Data: Global Fishing Watch"),
                        "Copyright 2021, Global Fishing Watch, Inc. Accessed on 30 June 2021.
                          https://globalfishingwatch.org/data-download/datasets/public-fishing-effort.",
                        br(),
                        br(),
                        "Global Fishing Watch data is licensed under a Creative Commons Attribution-ShareAlike 4.0 
                        International license(https://creativecommons.org/licenses/by-sa/4.0/) and code under an 
                        Apache 2.0 license (http://www.apache.org/licenses/LICENSE-2.0)",
                        br(),
                        br(),
                        "D.A. Kroodsma, J. Mayorga, T. Hochberg, N.A. Miller, K. Boerder, F. Ferretti, A. Wilson, 
                        B. Bergman, T.D. White, B.A. Block, P. Woods, B. Sullivan, C. Costello, and B. Worm. 
                        \"Tracking the global footprint of fisheries.\" Science 361.6378 (2018). 
                        (http://science.sciencemag.org/content/359/6378/904)",
                        #"Calculations of vessel class, etc. made by global fishing watch. More information on their methods here:",
                        h3("Exclusive Economic Zone Boundaries: Flanders Marine Institute"),
                        "Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries 
                        and Exclusive Economic Zones (200NM), version 11. Available online at https://www.marineregions.org/. 
                        https://doi.org/10.14284/386",
                        br(),
                        br(),
                        "Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Contiguous Zones (24NM), 
                        version 3. Available online at https://www.marineregions.org/ https://doi.org/10.14284/384",
                        br(),
                        br(),
                        "Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Territorial Seas (12NM),
                        version 3. Available online at https://www.marineregions.org/. https://doi.org/10.14284/387",
                        
                        
                        h3("Large-scale Marine Protected Area Boundaries: Protected Planet"),
                        "UNEP-WCMC and IUCN (2021), Protected Planet: The World Database on Protected Areas (WDPA) 
                        and World Database on Other Effective Area-based Conservation Measures (WD-OECM) [Online], 
                        August 2021, Cambridge, UK: UNEP-WCMC and IUCN. Available at: www.protectedplanet.net.",
                        
                        h3("Stanford Center for Ocean Solutions Report on the Palau National Marine Sanctuary"),
                        "Palau International Coral Reef Center and the Stanford Center for Ocean Solutions, 
                        “Palau’s National Marine Sanctuary: Managing Ocean Change and Supporting Food 
                        Security,” PICRC, December 2019. Available at: http://picrc.org/picrcpage/palau-national-
                          marine-sanctuary and https://oceansolutions.stanford.edu/pnms-report."
                        
                        # Information: This dashboard relies on fishing effort quantification 
                        # All metrics related to fishing effort and vessel characterisitics are determined 
                        # by Global Fishing Watch according to their analyses. For more information on Global Fishing Watch and their Fishing Effort Calclations, visit
                        
                        # img(src = "oceansolutions.png", height = 100, width = 800, style="display: block; margin-left: auto; margin-right: auto;"),
                        # fluidRow(column(12,HTML("<br><div class='footer'></div><br>")))
                      ),
                      
                      fluidRow(column(12,HTML("<br><div class='footer'></div><br>")))
                      
             )
  )
)






#### SHINY SERVER ####
server <- function(input, output) {
  
  
  
  #### Intro Tab ####
  
  ## Reactive Inputs for Intro Tab:
  # Render LSMPA Title
  
  lsmpa_name_summary <- reactive({
    if (input$lsmpa_summary == c("pnms")) {
      "Palau National Marine Sanctuary"
    } else if (input$lsmpa_summary == c("niue")) {
      "Niue Moana Mahu Marine Protected Area"
    }
  })
  
  output$lsmpa_title_summary <- renderText({ 
    lsmpa_name_summary()
  })
  
  # Render Before + LSMPA Title
  output$before_title <- renderText({ 
    paste("Before", lsmpa_name_summary())
  })
  
  # Render With + LSMPA Title
  output$after_title <- renderText({ 
    paste("With", lsmpa_name_summary())
  })
  
  ## Accessing and Filtering Initial Dataset
  
  #Access appropriate LSMPA data based on LSMPA input and MMSI/Fleet input
  data.input.summary <- reactive({
    lsmpa.selection <- input$lsmpa_summary  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      pnms.vessel.tbl.daily.mmsi.data
    } else if (lsmpa.selection == c("niue")) {
      niue.vessel.tbl.daily.mmsi.data
    }
    
  })
  
  # Access appropriate LSMPA Geospatial Boundary
  lsmpa.boundary.input.summary <- reactive({
    lsmpa.selection <- input$lsmpa_summary  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      sf.pnms
    } else if (lsmpa.selection == c("niue")) {
      sf.niue
    }
  })
  
  # Access appropriate EEZ Geospatial Boundary
  eez.boundary.input.summary <- reactive({
    lsmpa.selection <- input$lsmpa_summary  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      # sf.palau.eez
      palau.neighboring.eez
    } else if (lsmpa.selection == c("niue")) {
      niue.neighboring.eez
    }
  })
  
  # Access appropriate Location Labels (EEZ Labels)
  location.labels.summary <- reactive({
    lsmpa.selection <- input$lsmpa_summary  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      palau.labels.tbl
    } else if (lsmpa.selection == c("niue")) {
      niue.labels.tbl
    }
  })
  
  
  # Set Appropriate Date Ranges
  before.date.range <- reactive({
    lsmpa.selection <- input$lsmpa_summary  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      as.vector(c(2015, 2019))
    } else if (lsmpa.selection == c("niue")) {
      as.vector(c(2019, 2019))
    }
    
  })
  
  after.date.range <- reactive({
    lsmpa.selection <- input$lsmpa_summary  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      as.vector(c(2020, 2020))
    } else if (lsmpa.selection == c("niue")) {
      as.vector(c(2020, 2020))
    }
    
  })
  
  # Filter data based on further parameters
  # Flag
  flag.filtered.data.input.summary <- reactive({
    
    if ("All" %in% input$mmsi_flag_layer_summary | length(input$mmsi_flag_layer_summary) == 0) {
      data.input.summary()
    } else {
      data.input.summary() %>% filter(flag_gfw %in% input$mmsi_flag_layer_summary)
    }
    
  })
  
  # Gear
  gear.filtered.data.input.summary <- reactive({
    
    if ("All" %in% input$mmsi_gear_layer_summary | length(input$mmsi_gear_layer_summary) == 0) {
      flag.filtered.data.input.summary()
    } else {
      flag.filtered.data.input.summary() %>% filter(vessel_class_gfw %in% input$mmsi_gear_layer_summary)
    }
    
  })
  
  # No further filtering for now (EXAMPLE)
  filtered.data.summary <- reactive({
    gear.filtered.data.input.summary()
  })
  
  
  
  ## Once Filtered, Set Parameters and Manipulate Dataset for Outputs
  # Set Reactive Labels for Mapping
  # Main Title Label
  main.title.label.summary <- reactive({
    if (input$hourtype_summary == "hours") {
      "Mean Vessel Hours"
    } else if (input$hourtype_summary == "fishing_hours") {
      "Mean Vessel Fishing Hours"
    }
  })
  
  # filter-type Labels
  gear.subtitle.label.summary <- reactive({
    if (!("All" %in% input$mmsi_gear_layer_summary)) {
      subtitle <- paste(input$mmsi_gear_layer_summary, collapse = ", ")
      subtitle <- paste("Vessels with matching gear-type:", subtitle, sep = " ")
    }
  })
  
  flag.subtitle.label.summary <- reactive({
    if (!("All" %in% input$mmsi_flag_layer_summary)) {
      subtitle <- paste(input$mmsi_flag_layer_summary, collapse = ", ")
      subtitle <- paste("Vessels with matching flag code:", subtitle, sep = " ")
    }
  })
  
  filter.subtitle.label.summary <- reactive({
    if (!(is.null(flag.subtitle.label.summary())) && !(is.null(gear.subtitle.label.summary()))) {
      paste(flag.subtitle.label.summary(), gear.subtitle.label.summary(), sep = " & ")
    } else {
      paste(flag.subtitle.label.summary(), gear.subtitle.label.summary())
    }
  })
  
  
  
  
  
  # Main Title Label
  subtitle.title.label.summary.before <- reactive({
    if (length(unique(before.date.range())) == 1) {
      years <- before.date.range()[1]
    } else {
      years <- before.date.range()
      years <- paste(sort(years), collapse = " through ")
    }
    paste("Data Averaged for Years:", years, sep = " ")

  })
  
  # Main Title Label
  subtitle.title.label.summary.after <- reactive({
    if (length(unique(after.date.range())) == 1) {
      years <- after.date.range()[1]
    } else {
      years <- after.date.range()
      years <- paste(sort(years), collapse = " through ")
    }
    paste("Data Averaged for Years:", years, sep = " ")
  })
  
  # Hour-type Label
  time.label.summary <- reactive({
    if (input$hourtype_summary == "hours") {
      "Mean Hours (Total)"
    } else if (input$hourtype_summary == "fishing_hours") {
      "Mean Fishing Hours"
    }
  })
  
  # Filter Before and After Year Ranges
  # Filter data based on year range selection
  before.filtered.data.summary <- reactive({
    filtered.data.summary() %>%
      filter(year(date) >= before.date.range()[1]) %>%
      filter(year(date) <= before.date.range()[2])
  })
  
  after.filtered.data.summary <- reactive({
    filtered.data.summary() %>%
      filter(year(date) >= after.date.range()[1]) %>%
      filter(year(date) <= after.date.range()[2])
  })
  
  
  before.plotting.data.pre <- reactive({
    data <- before.filtered.data.summary() %>% dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype_summary)) %>%
      group_by("lon" = cell_ll_lon, "lat" = cell_ll_lat) %>%
      summarise("mean" = mean(hourtype), .groups = "drop")
  })
  
  after.plotting.data.pre <- reactive({
    data <- after.filtered.data.summary() %>% dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype_summary)) %>%
      group_by("lon" = cell_ll_lon, "lat" = cell_ll_lat) %>%
      summarise("mean" = mean(hourtype), .groups = "drop")
  })
  
  before.plotting.data <- reactive({
    jenksbreaks <- getJenksBreaks(c((before.plotting.data.pre()$mean), (after.plotting.data.pre()$mean)), k = 6)
    values <- (before.plotting.data.pre()$mean)
    
    before.plotting.data.pre() %>% mutate(jenks = add_jenks(values, jenksbreaks))
  })
  
  after.plotting.data <- reactive({
    jenksbreaks <- getJenksBreaks(c((before.plotting.data.pre()$mean), (after.plotting.data.pre()$mean)), k = 6)
    values <- (after.plotting.data.pre()$mean)
    
    after.plotting.data.pre() %>% mutate(jenks = add_jenks(values, jenksbreaks))
  })
  
  
  
  
  
  difference.tbl <- reactive({
    
    
    before.raster <- rasterFromXYZ(before.plotting.data()[,1:2])
    before.raster <- rasterize(before.plotting.data()[,1:2],
                               before.raster, field=before.plotting.data()[,3])
    crs(before.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    
    
    after.raster <- rasterFromXYZ(after.plotting.data()[,1:2])
    after.raster <- rasterize(after.plotting.data()[,1:2],
                              after.raster, field=after.plotting.data()[,3])
    crs(after.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    zero.raster <- raster(ext = extent(after.raster), crs = crs(after.raster), resolution = res(after.raster))
    values(zero.raster) <- 0
    
    before.raster.extended <- mosaic(zero.raster, before.raster, fun = sum)
    
    after.raster.extended <- mosaic(zero.raster, after.raster, fun = sum)
    
    difference.raster <- after.raster.extended - before.raster.extended
    
    difference.tbl <- as_tibble(rasterToPoints(difference.raster))
    colnames(difference.tbl) <- c("lon", "lat", "difference")
    
    difference.tbl
  })
  
  difference.jenks <- reactive({
    values <- difference.tbl()$difference
    values <- c(0, values)
    
    neg_values <- values[which(values <= 0)]
    pos_values <- values[which(values >= 0)]
    neg_breaks <- neg_values %>%
      getJenksBreaks(k = 4) 
    pos_breaks <- pos_values %>%
      getJenksBreaks(k = 4)
    
    breaks <- unique(c(neg_breaks, pos_breaks))
    breaks
  })
  
  
  difference.plotting.data <- reactive({
    difference.tbl() %>% mutate(jenks = add_jenks_difference(difference.tbl()$`difference`, difference.jenks(), difference = T))
  })
  
  
  
  
  # Calculate Totals
  # Map 1 (Before)
  Before.DATA.sf <- reactive({
    st_as_sf(before.plotting.data(), coords = c("lon", "lat"), crs = 4326)
  })
  
  total.before.mean <- reactive({
    DATA <- before.plotting.data() %>%
      dplyr::select(mean)
    
    round(mean(DATA$mean), digits = 1)
  })
  
  eez.total.before.mean <- reactive({
    DATA <- before.plotting.data() %>% mutate(inside = lengths(st_within(Before.DATA.sf(), eez.boundary.input.summary()))) %>% filter(inside == 1) %>%
      dplyr::select(mean)
    
    round(mean(DATA$mean), digits = 1)
  })
  
  lsmpa.total.before.mean <- reactive({
    DATA <- before.plotting.data() %>% mutate(inside = lengths(st_within(Before.DATA.sf(), lsmpa.boundary.input.summary()))) %>% filter(inside == 1) %>%
      dplyr::select(mean)
    
    round(mean(DATA$mean), digits = 1)
  })
  
  total.before.cumul <- reactive({
    DATA <- before.plotting.data() %>%
      dplyr::select(mean)
    
    round((sum(DATA$mean) / 24), digits = 0)
  })
  
  eez.total.before.cumul <- reactive({
    DATA <- before.plotting.data() %>% mutate(inside = lengths(st_within(Before.DATA.sf(), eez.boundary.input.summary()))) %>% filter(inside == 1) %>%
      dplyr::select(mean)
    
    round((sum(DATA$mean) / 24), digits = 0)
  })
  
  lsmpa.total.before.cumul <- reactive({
    DATA <- before.plotting.data() %>% mutate(inside = lengths(st_within(Before.DATA.sf(), lsmpa.boundary.input.summary()))) %>% filter(inside == 1) %>%
      dplyr::select(mean)
    
    round((sum(DATA$mean) / 24), digits = 0)
  })
  
  # Map 2 (After)
  After.DATA.sf <- reactive({
    st_as_sf(after.plotting.data(), coords = c("lon", "lat"), crs = 4326)
  })
  
  total.after.mean <- reactive({
    DATA <- after.plotting.data() %>%
      dplyr::select(mean)
    
    round(mean(DATA$mean), digits = 1)
  })
  
  eez.total.after.mean <- reactive({
    DATA <- after.plotting.data() %>% mutate(inside = lengths(st_within(After.DATA.sf(), eez.boundary.input.summary()))) %>% filter(inside == 1) %>%
      dplyr::select(mean)
    
    round(mean(DATA$mean), digits = 1)
  })
  
  lsmpa.total.after.mean <- reactive({
    DATA <- after.plotting.data() %>% mutate(inside = lengths(st_within(After.DATA.sf(), lsmpa.boundary.input.summary()))) %>% filter(inside == 1) %>%
      dplyr::select(mean)
    
    round(mean(DATA$mean), digits = 1)
  })
  
  total.after.cumul <- reactive({
    DATA <- after.plotting.data() %>%
      dplyr::select(mean)
    
    round((sum(DATA$mean) / 24), digits = 0)
  })
  
  eez.total.after.cumul <- reactive({
    DATA <- after.plotting.data() %>% mutate(inside = lengths(st_within(After.DATA.sf(), eez.boundary.input.summary()))) %>% filter(inside == 1) %>%
      dplyr::select(mean)
    
    round((sum(DATA$mean) / 24), digits = 0)
  })
  
  lsmpa.total.after.cumul <- reactive({
    DATA <- after.plotting.data() %>% mutate(inside = lengths(st_within(After.DATA.sf(), lsmpa.boundary.input.summary()))) %>% filter(inside == 1) %>%
      dplyr::select(mean)
    
    round((sum(DATA$mean) / 24), digits = 0)
  })
  
  
  
  
  
  #### Intro Tab Outputs ####
  
  # Summary Text
  # Render summary text--map 1
  
  output$summary_text_before_mean <- renderUI({
    line1 <- paste("Total Data Extent:", total.before.mean(), "hours")
    line2 <- paste("Exclusive Economic Zone Boundary:", eez.total.before.mean(), "hours")
    line3 <- paste("LSMPA Boundary:", lsmpa.total.before.mean(), "hours")
    HTML(paste(line1, line2, line3, sep = '<br/>'))
  })
  
  output$summary_text_before_cumul <- renderUI({
    line1 <- paste("Total Data Extent:", total.before.cumul(), "days")
    line2 <- paste("Exclusive Economic Zone Boundary:", eez.total.before.cumul(), "days")
    line3 <- paste("LSMPA Boundary:", lsmpa.total.before.cumul(), "days")
    HTML(paste(line1, line2, line3, sep = '<br/>'))
  })
  
  # Render summary text--map 2
  output$summary_text_after_mean <- renderUI({
    line1 <- paste("Total Data Extent:", total.after.mean(), "hours")
    line2 <- paste("Exclusive Economic Zone Boundary:", eez.total.after.mean(), "hours")
    line3 <- paste("LSMPA Boundary:", lsmpa.total.after.mean(), "hours")
    HTML(paste(line1, line2, line3, sep = '<br/>'))
  })
  
  output$summary_text_after_cumul <- renderUI({
    line1 <- paste("Total Data Extent:", total.after.cumul(), "days")
    line2 <- paste("Exclusive Economic Zone Boundary:", eez.total.after.cumul(), "days")
    line3 <- paste("LSMPA Boundary:", lsmpa.total.after.cumul(), "days")
    HTML(paste(line1, line2, line3, sep = '<br/>'))
  })
  
  output$lsmpa_link <- renderUI({
    if (input$lsmpa_summary == c("pnms")) {
      tags$a(href="https://mpatlas.org/sites/68", "Palau National Marine Sanctuary")
    }
  })
  
  output$lsmpa_text <- renderUI({
    if (input$lsmpa_summary == c("pnms")) {
      
      HTML("<ul><li>Country: Palau</li><li>Year designated: 2015 (no-take zone implemented 2020)</li><li>Size: 477148 km2</li><li>Historic uses: Foreign industrial fishing</li><li>Current uses: No fishing is permitted within the bounds of the marine sanctuary</li></ul>")
    }
  })
  
  
  
  # output$lsmpa_text <- renderText({
  #   if (input$lsmpa_summary == c("pnms")) {
  #    
  #     "Dashboard information:
  #       Country: Palau
  #     Year designated: 2015 (no-take zone implemented 2020)
  #     Size: 477148 km2
  #     Historic uses: Foreign industrial fishing
  #     Current uses: No fishing is permitted within the bounds of the marine sanctuary"
  #     
  #   }
  # })
  
  output$lsmpa_text_beforeafter <- renderText({
    
    if (input$lsmpa_summary == c("pnms")) {
      "The Palau National Marine Sanctuary aims to \"foster a stronger domestic pelagic fishery sector to support food security, livelihoods and economic development in Palau. The PNMS Act mandated that from December 2015 to December 2019 there would be a steady reduction in fishing activity inside the area to be included in the Sanctuary, preparatory to full closure in January 2020.\" (For more information, please see: https://oceansolutions.stanford.edu/pnms-report.)"
    }
  })
  
  
  
  
  output$beforemap <- renderPlot({
    jenksbreaks <- getJenksBreaks(c((before.plotting.data()$mean), (after.plotting.data()$mean)), k = 6)
    
    scalebar.coords <- c(max(before.plotting.data()$`lon`) - 0.6, min(before.plotting.data()$`lat`) + 0.5)
    names(scalebar.coords) = c("x","y")
    
    scalebar.data <- before.plotting.data() %>% dplyr::select(long = lon, lat)
    
    minlat <- if (min(before.plotting.data()$`lat`) == 0) {
      0.00001
    } else {
      min(before.plotting.data()$`lat`)
    }
    
    ggplot() +
      #geom_sf(data = lsmpa.boundary.input.summary(), alpha = 0.3, colour = "blue", fill = "green") +
      geom_sf(data = eez.boundary.input.summary(), colour = "black", alpha = 0.5, fill = "dark blue") +
      geom_tile(data = before.plotting.data(), aes(x = lon, y = lat, fill = jenks)) +
      scale_fill_viridis_d(time.label.summary(), option = "C", direction = 1, labels = create_jenks_labels(jenksbreaks)) +
      geom_sf(data = eez.boundary.input.summary(), colour = "black",  alpha = 0, size = 3) +
      #geom_sf(data = lsmpa.boundary.input.summary(), alpha = 0, colour = "blue", size = 3) +
      geom_label(data = location.labels.summary(), aes(label = id, x = lon, y = lat), label.r = unit(0, "lines"), alpha = 0.9) +
      coord_sf(xlim = c(min(before.plotting.data()$`lon`), max(before.plotting.data()$`lon`)),
               ylim = c(minlat, max(before.plotting.data()$`lat`)), expand = FALSE) +
      labs(title = main.title.label.summary(), subtitle = subtitle.title.label.summary.before(), caption = filter.subtitle.label.summary()) +
      theme_classic(base_size = 16) +
      theme(axis.line = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.title = element_text(hjust = 0.5)) + 
      theme(plot.title = element_text(size=22),
            legend.position = "bottom") +
      scalebar(scalebar.data, location = "bottomright", dist = 200, dist_unit = "km",
               transform = TRUE, model = "WGS84", anchor = scalebar.coords, st.color = "white")
  })
  
  
  
  output$aftermap <- renderPlot({
    jenksbreaks <- getJenksBreaks(c((before.plotting.data()$mean), (after.plotting.data()$mean)), k = 6)
    
    scalebar.coords <- c(max(after.plotting.data()$`lon`) - 0.6, min(after.plotting.data()$`lat`) + 0.5)
    names(scalebar.coords) = c("x","y")
    
    scalebar.data <- after.plotting.data() %>% dplyr::select(long = lon, lat)
    
    minlat <- if (min(after.plotting.data()$`lat`) == 0) {
      0.00001
    } else {
      min(after.plotting.data()$`lat`)
    }
    
    ggplot() +
      geom_sf(data = lsmpa.boundary.input.summary(), alpha = 0.3, colour = "blue", fill = "green") +
      geom_sf(data = eez.boundary.input.summary(), colour = "black", alpha = 0.5, fill = "dark blue") +
      geom_tile(data = after.plotting.data(), aes(x = lon, y = lat, fill = jenks)) +
      scale_fill_viridis_d(time.label.summary(), option = "C", direction = 1, labels = create_jenks_labels(jenksbreaks)) +
      geom_sf(data = eez.boundary.input.summary(), colour = "black", alpha = 0, size = 3) +
      geom_sf(data = lsmpa.boundary.input.summary(), alpha = 0, colour = "blue", size = 3) +
      geom_label(data = location.labels.summary(), aes(label = id, x = lon, y = lat), label.r = unit(0, "lines"), alpha = 0.9) +
      coord_sf(xlim = c(min(after.plotting.data()$`lon`), max(after.plotting.data()$`lon`)),
               ylim = c(minlat, max(after.plotting.data()$`lat`)), expand = FALSE) +
      labs(title = main.title.label.summary(), subtitle = subtitle.title.label.summary.after(), caption = filter.subtitle.label.summary()) +
      theme_classic(base_size = 16) +
      theme(axis.line = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.title = element_text(hjust = 0.5)) + 
      theme(plot.title = element_text(size=22),
            legend.position = "bottom") +
      scalebar(scalebar.data, location = "bottomright", dist = 200, dist_unit = "km",
               transform = TRUE, model = "WGS84", anchor = scalebar.coords, st.color = "white")
  })
  
  output$changemap <- renderPlot({
    scalebar.coords <- c(max(difference.plotting.data()$`lon`) - 0.6, min(difference.plotting.data()$`lat`) + 0.5)
    names(scalebar.coords) = c("x","y")
    
    scalebar.data <- difference.plotting.data() %>% dplyr::select(long = lon, lat)
    
    minlat <- if (min(difference.plotting.data()$`lat`) == 0) {
      0.00001
    } else {
      min(difference.plotting.data()$`lat`)
    }
    
    legend.labels <- create_jenks_labels_difference(difference.jenks(), difference = T)
    
    legend.labels <- paste(legend.labels, c(" (Less fishing)", " ", " ", " ", " ", " ", " (More fishing)"))
    
    ggplot() +
      geom_sf(data = lsmpa.boundary.input.summary(), alpha = 0.3, colour = "blue", fill = "green") +
      geom_sf(data = eez.boundary.input.summary(), colour = "black", alpha = 0.5, fill = "dark blue") +
      geom_tile(data = difference.plotting.data(), aes(x = lon, y = lat, fill = jenks)) +
      scale_fill_brewer(paste("Change in", time.label.summary(), sep = " "), type = "div",
                        palette = "RdBu", direction = -1, labels = legend.labels, guide = guide_legend(reverse = TRUE)) +
      geom_sf(data = eez.boundary.input.summary(), colour = "black", alpha = 0, size = 3) +
      geom_sf(data = lsmpa.boundary.input.summary(), alpha = 0, colour = "blue", size = 3) +
      geom_label(data = location.labels.summary(), aes(label = id, x = lon, y = lat), label.r = unit(0, "lines"), alpha = 0.9) +
      coord_sf(xlim = c(min(difference.plotting.data()$`lon`), max(difference.plotting.data()$`lon`)),
               ylim = c(minlat, max(difference.plotting.data()$`lat`)), expand = FALSE) +
      labs(title = paste("Change in", main.title.label.summary(), sep = " "), caption = filter.subtitle.label.summary()) +
      theme_classic(base_size = 16) +
      theme(axis.line = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.title = element_text(hjust = 0.5)) + 
      theme(plot.title = element_text(size=22),
            legend.position = "right") +
      scalebar(scalebar.data, location = "bottomright", dist = 200, dist_unit = "km",
               transform = TRUE, model = "WGS84", anchor = scalebar.coords, st.color = "black")
  })
  
  
  
  
  #### TAB 1: OVER TIME ANNUAL SUMMARY ####
  
  ## Reactive Inputs for Tab 1 - Annual Summary:
  # Render LSMPA Title--Annual
  output$lsmpa_title_annual <- renderText({ 
    if (input$lsmpa_annual == c("pnms")) {
      "Palau National Marine Sanctuary"
    }
  })
  
  ## Accessing and Filtering Initial Dataset
  
  #Access appropriate LSMPA data based on LSMPA input and MMSI/Fleet input
  data.input.annual <- reactive({
    lsmpa.selection <- input$lsmpa_annual  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      pnms.vessel.tbl.daily.mmsi.data
    }
    
  })
  
  # Access appropriate LSMPA Geospatial Boundary
  lsmpa.boundary.input.annual <- reactive({
    lsmpa.selection <- input$lsmpa_annual  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      sf.pnms
    }
  })
  
  # Access appropriate EEZ Geospatial Boundary
  eez.boundary.input.annual <- reactive({
    lsmpa.selection <- input$lsmpa_annual  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      # sf.palau.eez
      palau.neighboring.eez
    }
  })
  
  # Access appropriate Location Labels (EEZ Labels)
  location.labels.annual <- reactive({
    lsmpa.selection <- input$lsmpa_annual  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      palau.labels.tbl
    }
  })
  
  # # Filter Global EEZ
  # global.eez.filtered.annual <- reactive({
  #   global.eez %>% filter(geometry %in% numeric.bbox.pmns)
  # })
  
  # # Filter data based on year range selection
  # year.filtered.data.input.annual <- reactive({
  #   data.input.annual() %>%
  #     filter(year(date) >= input$year_range_annual[1]) %>%
  #     filter(year(date) <= input$year_range_annual[2])
  # })
  
  # Filter data based on year check selection
  year.filtered.data.input.annual <- reactive({
    if (13 %in% input$month_check_annual) {
      data.input.annual() %>%
        filter(year(date) %in% input$year_dropdown_annual)
    } else {
      data.input.annual() %>%
        filter(year(date) %in% input$year_dropdown_annual) %>%
        filter(month(date) %in% input$month_check_annual)
    }
  })
  
  
  # Filter data based on further parameters
  # Flag
  flag.filtered.data.input.annual <- reactive({
    
    if ("All" %in% input$mmsi_flag_layer_annual | length(input$mmsi_flag_layer_annual) == 0) {
      year.filtered.data.input.annual()
    } else {
      year.filtered.data.input.annual() %>% filter(flag_gfw %in% input$mmsi_flag_layer_annual)
    }
    
  })
  
  # Gear
  gear.filtered.data.input.annual <- reactive({
    
    if ("All" %in% input$mmsi_gear_layer_annual | length(input$mmsi_gear_layer_annual) == 0) {
      flag.filtered.data.input.annual()
    } else {
      flag.filtered.data.input.annual() %>% filter(vessel_class_gfw %in% input$mmsi_gear_layer_annual)
    }
    
  })
  
  # No further filtering for now (EXAMPLE)
  filtered.data.annual <- reactive({
    gear.filtered.data.input.annual()
  })
  
  
  
  ## Once Filtered, Set Parameters and Manipulate Dataset for Outputs
  # Set Reactive Labels for Mapping
  # Main Title Label
  main.title.label.annual <- reactive({
    if (input$hourtype_annual == "hours") {
      "Mean Vessel Hours"
    } else if (input$hourtype_annual == "fishing_hours") {
      "Mean Vessel Fishing Hours"
    }
  })
  
  # Hour-type Label
  time.label.annual <- reactive({
    if (input$hourtype_annual == "hours") {
      "Mean Hours (Total)"
    } else if (input$hourtype_annual == "fishing_hours") {
      "Mean Fishing Hours"
    }
  })
  
  
  # filter-type Labels
  gear.subtitle.label.annual <- reactive({
    if (!("All" %in% input$mmsi_gear_layer_annual)) {
      subtitle <- paste(input$mmsi_gear_layer_annual, collapse = ", ")
      subtitle <- paste("Vessels with matching gear-type:", subtitle, sep = " ")
    }
  })
  
  flag.subtitle.label.annual <- reactive({
    if (!("All" %in% input$mmsi_flag_layer_annual)) {
      subtitle <- paste(input$mmsi_flag_layer_annual, collapse = ", ")
      subtitle <- paste("Vessels with matching flag code:", subtitle, sep = " ")
    }
  })
  
  filter.subtitle.label.annual <- reactive({
    if (!(is.null(flag.subtitle.label.annual())) && !(is.null(gear.subtitle.label.annual()))) {
      paste(flag.subtitle.label.annual(), gear.subtitle.label.annual(), sep = " & ")
    } else {
      paste(flag.subtitle.label.annual(), gear.subtitle.label.annual())
    }
  })
  
    
  
  # Create list of data tables separated by year
  filtered.data.annual.list <- reactive({
    filtered.data.annual() %>% 
      mutate(year = year(date)) %>% 
      split(f = as.factor(.$year))
  })
  
  # Mutate list of data tables separated by year
  selected.data.annual.list.pre <- reactive({
    filtered.data.annual.list() %>% lapply(function(data) {
      data %>% dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype_annual)) %>%
        group_by("lon" = cell_ll_lon, "lat" = cell_ll_lat) %>%
        summarise("mean" = mean(hourtype), .groups = "drop")
    })
  })
  
  jenksbreaks.annual <- reactive({
    values <- bind_rows(selected.data.annual.list.pre())$`mean`
    getJenksBreaks(values, k = 6)
  })
  
  selected.data.annual.list <- reactive({
    selected.data.annual.list.pre() %>% lapply(function(data) {
      data %>% mutate(jenks = add_jenks(data$`mean`, jenksbreaks.annual()))
    })
  })
  
   
  

  
  
  
  #### Tab 1: Create Outputs ####
  output$map1 <- renderPlot({
    if (length(input$year_dropdown_annual) >= 1 && length(input$month_check_annual) >= 1) {
      jenksbreaks <- jenksbreaks.annual()
      
      scalebar.coords <- c(max(selected.data.annual.list()[[1]]$`lon`) - 0.6, min(selected.data.annual.list()[[1]]$`lat`) + 0.5)
      names(scalebar.coords) = c("x","y")
      
      scalebar.data <- selected.data.annual.list()[[1]] %>% dplyr::select(long = lon, lat)
      
      minlat <- if (min(selected.data.annual.list()[[1]]$`lat`) == 0) {
        0.00001
      } else {
        min(selected.data.annual.list()[[1]]$`lat`)
      }
      
      gg <- ggplot()
      if ("2020" %in% names(selected.data.annual.list()[1])) {
        gg <- gg + geom_sf(data = lsmpa.boundary.input.annual(), alpha = 0.3, colour = "blue", fill = "green")
      }
      gg <- gg +  
        geom_sf(data = eez.boundary.input.annual(), colour = "black", alpha = 0.5, fill = "dark blue") +
        geom_tile(data = selected.data.annual.list()[[1]], aes(x = lon, y = lat, fill = jenks)) +
        scale_fill_viridis_d(time.label.annual(), option = "C", direction = 1, labels = create_jenks_labels(jenksbreaks)) +
        geom_sf(data = eez.boundary.input.annual(), colour = "black", alpha = 0, size = 3)
      
      if ("2020" %in% names(selected.data.annual.list()[1])) {
        gg <- gg + geom_sf(data = lsmpa.boundary.input.annual(), alpha = 0, colour = "blue", size = 3)
      }
      gg +
        geom_label(data = location.labels.annual(), aes(label = id, x = lon, y = lat), label.r = unit(0, "lines"), alpha = 0.9) +
        coord_sf(xlim = c(min(selected.data.annual.list()[[1]]$`lon`), max(selected.data.annual.list()[[1]]$`lon`)),
                 ylim = c(minlat, max(selected.data.annual.list()[[1]]$`lat`)), expand = FALSE) +
        labs(title = main.title.label.annual(), subtitle = filter.subtitle.label.annual()) +
        theme_classic(base_size = 16) +
        theme(axis.line = element_blank(), axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              plot.title = element_text(hjust = 0.5)) + 
        theme(plot.title = element_text(size=22),
              legend.position = "bottom") +
        scalebar(scalebar.data, location = "bottomright", dist = 200, dist_unit = "km",
                 transform = TRUE, model = "WGS84", anchor = scalebar.coords, st.color = "white")
    }
  })
  
  output$map2 <- renderPlot({
    if (length(input$year_dropdown_annual) >= 2 && length(input$month_check_annual) >= 1) {
      
      jenksbreaks <- jenksbreaks.annual()
      
      scalebar.coords <- c(max(selected.data.annual.list()[[2]]$`lon`) - 0.6, min(selected.data.annual.list()[[2]]$`lat`) + 0.5)
      names(scalebar.coords) = c("x","y")
      
      scalebar.data <- selected.data.annual.list()[[2]] %>% dplyr::select(long = lon, lat)
      
      gg <- ggplot()
      if ("2020" %in% names(selected.data.annual.list()[2])) {
        gg <- gg + geom_sf(data = lsmpa.boundary.input.annual(), alpha = 0.3, colour = "blue", fill = "green")
      }
      gg <- gg +
        geom_sf(data = eez.boundary.input.annual(), colour = "black", alpha = 0.5, fill = "dark blue") +
        geom_tile(data = selected.data.annual.list()[[2]], aes(x = lon, y = lat, fill = jenks)) +
        scale_fill_viridis_d(time.label.annual(), option = "C", direction = 1, labels = create_jenks_labels(jenksbreaks)) +
        geom_sf(data = eez.boundary.input.annual(), colour = "black", alpha = 0, size = 3)
      
      if ("2020" %in% names(selected.data.annual.list()[2])) {
        gg <- gg + geom_sf(data = lsmpa.boundary.input.annual(), alpha = 0, colour = "blue", size = 3)
      }
      gg +
        geom_label(data = location.labels.annual(), aes(label = id, x = lon, y = lat), label.r = unit(0, "lines"), alpha = 0.9) +
        coord_sf(xlim = c(min(selected.data.annual.list()[[2]]$`lon`), max(selected.data.annual.list()[[2]]$`lon`)),
                 ylim = c(min(selected.data.annual.list()[[2]]$`lat`), max(selected.data.annual.list()[[2]]$`lat`)), expand = FALSE) +
        labs(title = main.title.label.annual(), subtitle = filter.subtitle.label.annual()) +
        theme_classic(base_size = 16) +
        theme(axis.line = element_blank(), axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              plot.title = element_text(hjust = 0.5)) + 
        theme(plot.title = element_text(size=22),
              legend.position = "bottom") +
        scalebar(scalebar.data, location = "bottomright", dist = 200, dist_unit = "km",
                 transform = TRUE, model = "WGS84", anchor = scalebar.coords, st.color = "white")
      
    }
  })
  
  output$map3 <- renderPlot({
    if (length(input$year_dropdown_annual) >= 3 && length(input$month_check_annual) >= 1) {
      
      jenksbreaks <- jenksbreaks.annual()
      
      scalebar.coords <- c(max(selected.data.annual.list()[[3]]$`lon`) - 0.6, min(selected.data.annual.list()[[3]]$`lat`) + 0.5)
      names(scalebar.coords) = c("x","y")
      
      scalebar.data <- selected.data.annual.list()[[3]] %>% dplyr::select(long = lon, lat)
      
      gg <- ggplot()
      if ("2020" %in% names(selected.data.annual.list()[3])) {
        gg <- gg + geom_sf(data = lsmpa.boundary.input.annual(), alpha = 0.3, colour = "blue", fill = "green")
      }
      gg <- gg +
        geom_sf(data = eez.boundary.input.annual(), colour = "black", alpha = 0.5, fill = "dark blue") +
        geom_tile(data = selected.data.annual.list()[[3]], aes(x = lon, y = lat, fill = jenks)) +
        scale_fill_viridis_d(time.label.annual(), option = "C", direction = 1, labels = create_jenks_labels(jenksbreaks)) +
        geom_sf(data = eez.boundary.input.annual(), colour = "black", alpha = 0, size = 3)
      
      if ("2020" %in% names(selected.data.annual.list()[3])) {
        gg <- gg + geom_sf(data = lsmpa.boundary.input.annual(), alpha = 0, colour = "blue", size = 3)
      }
      gg +
        geom_label(data = location.labels.annual(), aes(label = id, x = lon, y = lat), label.r = unit(0, "lines"), alpha = 0.9) +
        coord_sf(xlim = c(min(selected.data.annual.list()[[3]]$`lon`), max(selected.data.annual.list()[[3]]$`lon`)),
                 ylim = c(min(selected.data.annual.list()[[3]]$`lat`), max(selected.data.annual.list()[[3]]$`lat`)), expand = FALSE) +
        labs(title = main.title.label.annual(), subtitle = filter.subtitle.label.annual()) +
        theme_classic(base_size = 16) +
        theme(axis.line = element_blank(), axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              plot.title = element_text(hjust = 0.5)) + 
        theme(plot.title = element_text(size=22),
              legend.position = "bottom") +
        scalebar(scalebar.data, location = "bottomright", dist = 200, dist_unit = "km",
                 transform = TRUE, model = "WGS84", anchor = scalebar.coords, st.color = "white")
      
    }
  })
  
  output$map4 <- renderPlot({
    if (length(input$year_dropdown_annual) >= 4 && length(input$month_check_annual) >= 1) {
      
      jenksbreaks <- jenksbreaks.annual()
      
      scalebar.coords <- c(max(selected.data.annual.list()[[4]]$`lon`) - 0.6, min(selected.data.annual.list()[[4]]$`lat`) + 0.5)
      names(scalebar.coords) = c("x","y")
      
      scalebar.data <- selected.data.annual.list()[[4]] %>% dplyr::select(long = lon, lat)
      
      gg <- ggplot()
      if ("2020" %in% names(selected.data.annual.list()[4])) {
        gg <- gg + geom_sf(data = lsmpa.boundary.input.annual(), alpha = 0.3, colour = "blue", fill = "green")
      }
      gg <- gg +
        geom_sf(data = eez.boundary.input.annual(), colour = "black", alpha = 0.5, fill = "dark blue") +
        geom_tile(data = selected.data.annual.list()[[4]], aes(x = lon, y = lat, fill = jenks)) +
        scale_fill_viridis_d(time.label.annual(), option = "C", direction = 1, labels = create_jenks_labels(jenksbreaks)) +
        geom_sf(data = eez.boundary.input.annual(), colour = "black", alpha = 0, size = 3)
      
      if ("2020" %in% names(selected.data.annual.list()[4])) {
        gg <- gg + geom_sf(data = lsmpa.boundary.input.annual(), alpha = 0, colour = "blue", size = 3)
      }
      gg +
        geom_label(data = location.labels.annual(), aes(label = id, x = lon, y = lat), label.r = unit(0, "lines"), alpha = 0.9) +
        coord_sf(xlim = c(min(selected.data.annual.list()[[4]]$`lon`), max(selected.data.annual.list()[[4]]$`lon`)),
                 ylim = c(min(selected.data.annual.list()[[4]]$`lat`), max(selected.data.annual.list()[[4]]$`lat`)), expand = FALSE) +
        labs(title = main.title.label.annual(), subtitle = filter.subtitle.label.annual()) +
        theme_classic(base_size = 16) +
        theme(axis.line = element_blank(), axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              plot.title = element_text(hjust = 0.5)) + 
        theme(plot.title = element_text(size=22),
              legend.position = "bottom") +
        scalebar(scalebar.data, location = "bottomright", dist = 200, dist_unit = "km",
                 transform = TRUE, model = "WGS84", anchor = scalebar.coords, st.color = "white")
      
    }
  })
  
  # Output Map Titles
  output$map1_title <- renderText({ 
    
    if (length(input$year_dropdown_annual) >= 1 && length(input$month_check_annual) >= 1) {

      
      inputmonths <- recode(sort(input$month_check_annual), `1` = "January", `2` = "February", `3` = "March",
                            `4` = "April", `5` = "May", `6` = "June",
                            `7` = "July", `8` = "August", `9` = "September",
                            `10` = "October", `11` = "November", `12` = "December")
      
      
      
      if (13 %in% input$month_check_annual) {
        months <- "(All Months)"
      } else {
        months <- paste(inputmonths, collapse = ", ")
        months <- paste("(", months, ")", sep = "")
      }
      
      paste(names(selected.data.annual.list()[1]), months, sep = " ")
    }
  })
  
  output$map2_title <- renderText({ 
    
    if (length(input$year_dropdown_annual) >= 2 && length(input$month_check_annual) >= 1) {
      
      inputmonths <- recode(input$month_check_annual, `1` = "January", `2` = "February", `3` = "March",
                            `4` = "April", `5` = "May", `6` = "June",
                            `7` = "July", `8` = "August", `9` = "September",
                            `10` = "October", `11` = "November", `12` = "December")
      
      if (13 %in% input$month_check_annual) {
        months <- "(All Months)"
      } else {
        months <- paste(inputmonths, collapse = ", ")
        months <- paste("(", months, ")", sep = "")
      }
      
      paste(names(selected.data.annual.list()[2]), months, sep = " ")
    }
  })
  
  output$map3_title <- renderText({ 
    if (length(input$year_dropdown_annual) >= 3 && length(input$month_check_annual) >= 1) {
      
      inputmonths <- recode(input$month_check_annual, `1` = "January", `2` = "February", `3` = "March",
                            `4` = "April", `5` = "May", `6` = "June",
                            `7` = "July", `8` = "August", `9` = "September",
                            `10` = "October", `11` = "November", `12` = "December")
      
      if (13 %in% input$month_check_annual) {
        months <- "(All Months)"
      } else {
        months <- paste(inputmonths, collapse = ", ")
        months <- paste("(", months, ")", sep = "")
      }
      
      paste(names(selected.data.annual.list()[3]), months, sep = " ")
    }
  })
  
  output$map4_title <- renderText({
    if (length(input$year_dropdown_annual) >= 4 && length(input$month_check_annual) >= 1) {
      
      
      inputmonths <- recode(input$month_check_annual, `1` = "January", `2` = "February", `3` = "March",
                            `4` = "April", `5` = "May", `6` = "June",
                            `7` = "July", `8` = "August", `9` = "September",
                            `10` = "October", `11` = "November", `12` = "December")
      
      
      if (13 %in% input$month_check_annual) {
        months <- "(All Months)"
      } else {
        months <- paste(inputmonths, collapse = ", ")
        months <- paste("(", months, ")", sep = "")
      }
      
      paste(names(selected.data.annual.list()[4]), months, sep = " ")
    }
  })
  
  
  
  
  
  
  # Render UI for panel--titles and maps
  output$maps <- renderUI ({
    if (length(input$year_dropdown_annual) == 1) {
      column(12,
             fluidRow(
               column(12,h3(textOutput("map1_title")))
             ),
             fluidRow(
               column(12,plotOutput("map1", width = "100%", height = "800px"))
             ))
    } else if (length(input$year_dropdown_annual) == 2) {
      fluidRow(
        column(6, h3(textOutput("map1_title")), plotOutput("map1", height = "800px")),
        column(6, h3(textOutput("map2_title")), plotOutput("map2", height = "800px"))
      )
    } else if (length(input$year_dropdown_annual) == 3) {
      column(12,
             fluidRow(
               column(6, h3(textOutput("map1_title")), plotOutput("map1", height = "800px")),
               column(6, h3(textOutput("map2_title")), plotOutput("map2", height = "800px"))
             ),
             fluidRow(
               column(12, h3(textOutput("map3_title")), plotOutput("map3", height = "800px"))
             ))
    } else if (length(input$year_dropdown_annual) >= 4) {
      column(12,
             fluidRow(
               column(6, h3(textOutput("map1_title")), plotOutput("map1", height = "800px")),
               column(6, h3(textOutput("map2_title")), plotOutput("map2", height = "800px"))
             ),
             fluidRow(
               column(6, h3(textOutput("map3_title")), plotOutput("map3", height = "800px")),
               column(6, h3(textOutput("map4_title")), plotOutput("map4", height = "800px"))
             ))
    }
  })
  
  
  
  #### TAB 2: MONTHLY SUMMARY ####
  ## Reactive Inputs for Tab 2: Monthly Summary
  
  # Render LSMPA Title--Monthly
  output$lsmpa_title <- renderText({ 
    if (input$lsmpa == c("pnms")) {
      "Palau National Marine Sanctuary"
    }
  })
  
  ## Accessing and Filtering Initial Dataset

  #Access appropriate LSMPA data based on LSMPA input
  data.input <- reactive({
    lsmpa.selection <- input$lsmpa  # Character LSMPA Label

    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      pnms.vessel.tbl.daily.mmsi.data
    }
  })
  
  # Access appropriate LSMPA Geospatial Boundary
  lsmpa.boundary.input <- reactive({
    lsmpa.selection <- input$lsmpa  # Character LSMPA Label

    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      sf.pnms
    }
  })

  # Access appropriate EEZ Geospatial Boundary
  eez.boundary.input <- reactive({
    lsmpa.selection <- input$lsmpa  # Character LSMPA Label

    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      palau.neighboring.eez
    }
  })
  
  # Access appropriate Location Labels (EEZ Labels)
  location.labels <- reactive({
    lsmpa.selection <- input$lsmpa  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      palau.labels.tbl
    }
  })


  # # Filter data based on year range selection
  # year.filtered.data.input <- reactive({
  #   data.input() %>%
  #   filter(year(date) >= input$year_range[1]) %>%
  #     filter(year(date) <= input$year_range[2])
  # })
  
  # Filter data based on year check selection
  year.filtered.data.input <- reactive({
    
    if (input$year_check != "All") {
      data.input() %>%
        filter(year(date) %in% input$year_check)
    } else {
      data.input()
    }
  })
  
  
  

  # Filter data based on further parameters
  # Flag
  flag.filtered.data.input <- reactive({
    
    if ("All" %in% input$mmsi_flag_layer | length(input$mmsi_flag_layer) == 0) {
      year.filtered.data.input()
    } else {
      year.filtered.data.input() %>% filter(flag_gfw %in% input$mmsi_flag_layer)
    }
    
  })
  
  # Gear
  gear.filtered.data.input <- reactive({
    
    if ("All" %in% input$mmsi_gear_layer | length(input$mmsi_gear_layer) == 0) {
      flag.filtered.data.input()
    } else {
      flag.filtered.data.input() %>% filter(vessel_class_gfw %in% input$mmsi_gear_layer)
    }
    
  })
  


  # No further filtering for now
  filtered.data <- reactive({
    gear.filtered.data.input()
  })
  
  

  ## Once Filtered, Set Parameters and Manipulate Dataset for Outputs

  # Set Reactive Labels for Mapping
  # Main Title Label
  main.title.label <- reactive({
    if (input$hourtype == "hours") {
      "Monthly Mean Vessel Hours"
    } else if (input$hourtype == "fishing_hours") {
      "Monthly Mean Vessel Fishing Hours"
    }
  })

  # Hour-type Label
  time.label <- reactive({
    if (input$hourtype == "hours") {
      "Mean Hours (Total)"
    } else if (input$hourtype == "fishing_hours") {
      "Mean Fishing Hours"
    }
  })
  
  # Years Selected Subtitle
  year.subtitle <- reactive({
    if (input$year_check == "All") {
      years <- 2012:2020
    } else {
      years <- input$year_check
    }
    
    
    if (length(years) > 1) {
      years <- paste(sort(years), collapse = ", ")
      paste("Data Averaged for Years:", years, sep = " ")
    } else if (length(years) == 1) {
      paste("Data for Year", as.character(years), sep = " ")
    }
  })
  
  # filter-type Labels
  gear.subtitle.label <- reactive({
    if (!("All" %in% input$mmsi_gear_layer)) {
      subtitle <- paste(input$mmsi_gear_layer, collapse = ", ")
      subtitle <- paste("Vessels with matching gear-type:", subtitle, sep = " ")
    }
  })
  
  flag.subtitle.label <- reactive({
    if (!("All" %in% input$mmsi_flag_layer)) {
      subtitle <- paste(input$mmsi_flag_layer, collapse = ", ")
      subtitle <- paste("Vessels with matching flag code:", subtitle, sep = " ")
    }
  })
  
  filter.subtitle.label <- reactive({
    if (!(is.null(flag.subtitle.label())) && !(is.null(gear.subtitle.label()))) {
      paste(flag.subtitle.label(), gear.subtitle.label(), sep = " & ")
    } else {
      paste(flag.subtitle.label(), gear.subtitle.label())
    }
  })
  
  
  
  
  
  

  # Isolate essential variables to begin transformation to Raster layer
  selected.data <- reactive({
    filtered.data() %>%
      dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype)) %>%
      group_by("month" = month(`date`), "lon" = cell_ll_lon, "lat" = cell_ll_lat) %>%
      summarise("mean" = mean(hourtype), .groups = "drop")
  })
  
  
  # Get Jenks Breaks
  jenksbreaks <- reactive({
    getJenksBreaks(selected.data()$`mean`, k = 6)
  })
  
  # Ungroup, Recode, Mutate
  selected.data.ungrouped <- reactive({
    data <- selected.data() %>% dplyr::ungroup() %>% dplyr::select(lon, lat, mean, month)
    data <- data %>%
      mutate(month = recode(data$month, `1` = "January", `2` = "February", `3` = "March",
                            `4` = "April", `5` = "May", `6` = "June",
                            `7` = "July", `8` = "August", `9` = "September",
                            `10` = "October", `11` = "November", `12` = "December")) %>%
      mutate(month = factor(month, levels = c("January", "February", "March",
                                              "April", "May", "June",
                                              "July", "August", "September",
                                              "October", "November", "December")))
    
    data %>% mutate(jenks = add_jenks(data$`mean`, jenksbreaks()))
  })

  




  #### Tab 2: Create Outputs ####
  # Monthly Map
  

  
  output$monthlymap <- renderPlot({
    
    minlat <- if (min(selected.data.ungrouped()$`lat`) == 0) {
      0.00001
    } else {
      min(selected.data.ungrouped()$`lat`)
    }
    
    gg <- ggplot()
    if (2020 %in% input$year_check) {
      gg <- gg + geom_sf(data = lsmpa.boundary.input(), alpha = 0.3, colour = "blue", fill = "green", size = 0.25)
    }
    gg <- gg +
      geom_sf(data = eez.boundary.input(), colour = "black", alpha = 0.5, fill = "dark blue", size = 0.25) +
      geom_tile(data = selected.data.ungrouped(), aes(x = lon, y = lat, fill = jenks)) +
      scale_fill_viridis_d(time.label(), option = "C", direction = 1, labels = create_jenks_labels(jenksbreaks())) +
      facet_wrap(~month) +
      geom_sf(data = eez.boundary.input(), colour = "black", alpha = 0, size = 0.8)
    
    if (2020 %in% input$year_check) {
      gg <- gg + geom_sf(data = lsmpa.boundary.input(), alpha = 0, colour = "blue", size = 0.8)
    }
    gg +
      coord_sf(xlim = c(min(selected.data.ungrouped()$`lon`), max(selected.data.ungrouped()$`lon`)),
               ylim = c(minlat, max(selected.data.ungrouped()$`lat`)), expand = FALSE) +
      # geom_label(data = location.labels(), aes(label = id, x = lon, y = lat), label.r = unit(0, "lines"), alpha = 0.9) +
      labs(title = main.title.label(), subtitle = year.subtitle(), caption = filter.subtitle.label()) +
      theme_classic(base_size = 16) +
      theme(axis.line = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.title = element_text(size=22, hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "bottom")
  })
  
  
  #### TAB 3: Map Comparison ####
  
  ## Reactive Inputs for Tab 3: Map Comparison
  
  # Render LSMPA Title--Monthly
  output$lsmpa_title_yearly <- renderText({ 
    if (input$lsmpa_yearly == c("pnms")) {
      "Palau National Marine Sanctuary"
    }
  })
  
  ## Accessing and Filtering Initial Dataset
  
  #Access appropriate LSMPA data based on LSMPA input
  data.input_yearly <- reactive({
    lsmpa.selection <- input$lsmpa_yearly  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      pnms.vessel.tbl.daily.mmsi.data
    }
  })
  
  # Access appropriate LSMPA Geospatial Boundary
  lsmpa.boundary.input_yearly <- reactive({
    lsmpa.selection <- input$lsmpa_yearly  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      sf.pnms
    }
  })
  
  # Access appropriate EEZ Geospatial Boundary
  eez.boundary.input_yearly <- reactive({
    lsmpa.selection <- input$lsmpa_yearly  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      palau.neighboring.eez
    }
  })
  
  # Access appropriate Location Labels (EEZ Labels)
  location.labels_yearly <- reactive({
    lsmpa.selection <- input$lsmpa_yearly  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      palau.labels.tbl
    }
  })
  
  # Filter data based on year check selection
  year.filtered.data.input_yearly_1 <- reactive({
    data.input_yearly() %>%
      filter(year(date) %in% input$year_check_yearly_1)
  })
  year.filtered.data.input_yearly_2 <- reactive({
    data.input_yearly() %>%
      filter(year(date) %in% input$year_check_yearly_2)
  })
  
  
  
  # Filter data based on further parameters
  # Flag
  flag.filtered.data.input_yearly_1 <- reactive({
    
    if ("All" %in% input$mmsi_flag_layer_yearly_1 | length(input$mmsi_flag_layer_yearly_1) == 0) {
      year.filtered.data.input_yearly_1()
    } else {
      year.filtered.data.input_yearly_1() %>% filter(flag_gfw %in% input$mmsi_flag_layer_yearly_1)
    }
    
  })
  flag.filtered.data.input_yearly_2 <- reactive({
    
    if ("All" %in% input$mmsi_flag_layer_yearly_2 | length(input$mmsi_flag_layer_yearly_2) == 0) {
      year.filtered.data.input_yearly_2()
    } else {
      year.filtered.data.input_yearly_2() %>% filter(flag_gfw %in% input$mmsi_flag_layer_yearly_2)
    }
    
  })
  
  # Gear
  gear.filtered.data.input_yearly_1 <- reactive({
    
    if ("All" %in% input$mmsi_gear_layer_yearly_1 | length(input$mmsi_gear_layer_yearly_1) == 0) {
      flag.filtered.data.input_yearly_1()
    } else {
      flag.filtered.data.input_yearly_1() %>% filter(vessel_class_gfw %in% input$mmsi_gear_layer_yearly_1)
    }
    
  })
  gear.filtered.data.input_yearly_2 <- reactive({
    
    if ("All" %in% input$mmsi_gear_layer_yearly_2 | length(input$mmsi_gear_layer_yearly_2) == 0) {
      flag.filtered.data.input_yearly_2()
    } else {
      flag.filtered.data.input_yearly_2() %>% filter(vessel_class_gfw %in% input$mmsi_gear_layer_yearly_2)
    }
    
  })
  
  
  # No further filtering for now
  filtered.data_yearly_1 <- reactive({
    gear.filtered.data.input_yearly_1()
  })
  filtered.data_yearly_2 <- reactive({
    gear.filtered.data.input_yearly_2()
  })
  
  
  
  # Calculate Totals
  # Map 1
  DATA.sf.1 <- reactive({
    st_as_sf(filtered.data_yearly_1(), coords = c("cell_ll_lon", "cell_ll_lat"), crs = 4326)
  })
  
  total.map1.mean <- reactive({
    DATA <- filtered.data_yearly_1() %>%
      dplyr::select(hourtype = all_of(input$hourtype_yearly_1))
    
    round(mean(DATA$hourtype), digits = 1)
  })
  
  eez.total.map1.mean <- reactive({
    DATA <- filtered.data_yearly_1() %>% mutate(inside = lengths(st_within(DATA.sf.1(), eez.boundary.input_yearly()))) %>% filter(inside == 1) %>%
      dplyr::select(hourtype = all_of(input$hourtype_yearly_1))
    
    round(mean(DATA$hourtype), digits = 1)
  })
  
  lsmpa.total.map1.mean <- reactive({
    DATA <- filtered.data_yearly_1() %>% mutate(inside = lengths(st_within(DATA.sf.1(), lsmpa.boundary.input_yearly()))) %>% filter(inside == 1) %>%
      dplyr::select(hourtype = all_of(input$hourtype_yearly_1))
    
    round(mean(DATA$hourtype), digits = 1)
  })
  
  total.map1.cumul <- reactive({
    DATA <- filtered.data_yearly_1() %>%
      dplyr::select(hourtype = all_of(input$hourtype_yearly_1))
    
    round((sum(DATA$hourtype) / 24), digits = 0)
  })
  
  eez.total.map1.cumul <- reactive({
    DATA <- filtered.data_yearly_1() %>% mutate(inside = lengths(st_within(DATA.sf.1(), eez.boundary.input_yearly()))) %>% filter(inside == 1) %>%
      dplyr::select(hourtype = all_of(input$hourtype_yearly_1))
    
    round((sum(DATA$hourtype) / 24), digits = 0)
  })
  
  lsmpa.total.map1.cumul <- reactive({
    DATA <- filtered.data_yearly_1() %>% mutate(inside = lengths(st_within(DATA.sf.1(), lsmpa.boundary.input_yearly()))) %>% filter(inside == 1) %>%
      dplyr::select(hourtype = all_of(input$hourtype_yearly_1))
    
    round((sum(DATA$hourtype) / 24), digits = 0)
  })
  
  # Map 2
  DATA.sf.2 <- reactive({
    st_as_sf(filtered.data_yearly_2(), coords = c("cell_ll_lon", "cell_ll_lat"), crs = 4326)
  })
  
  total.map2.mean <- reactive({
    DATA <- filtered.data_yearly_2() %>%
      dplyr::select(hourtype = all_of(input$hourtype_yearly_2))
    
    round(mean(DATA$hourtype), digits = 1)
  })
  
  eez.total.map2.mean <- reactive({
    DATA <- filtered.data_yearly_2() %>% mutate(inside = lengths(st_within(DATA.sf.2(), eez.boundary.input_yearly()))) %>% filter(inside == 1) %>%
      dplyr::select(hourtype = all_of(input$hourtype_yearly_2))
    
    round(mean(DATA$hourtype), digits = 1)
  })
  
  lsmpa.total.map2.mean <- reactive({
    DATA <- filtered.data_yearly_2() %>% mutate(inside = lengths(st_within(DATA.sf.2(), lsmpa.boundary.input_yearly()))) %>% filter(inside == 1) %>%
      dplyr::select(hourtype = all_of(input$hourtype_yearly_2))
    
    round(mean(DATA$hourtype), digits = 1)
  })
  
  total.map2.cumul <- reactive({
    DATA <- filtered.data_yearly_2() %>%
      dplyr::select(hourtype = all_of(input$hourtype_yearly_2))
    
    round((sum(DATA$hourtype) / 24), digits = 0)
  })
  
  eez.total.map2.cumul <- reactive({
    DATA <- filtered.data_yearly_2() %>% mutate(inside = lengths(st_within(DATA.sf.2(), eez.boundary.input_yearly()))) %>% filter(inside == 1) %>%
      dplyr::select(hourtype = all_of(input$hourtype_yearly_2))
    
    round((sum(DATA$hourtype) / 24), digits = 0)
  })
  
  lsmpa.total.map2.cumul <- reactive({
    DATA <- filtered.data_yearly_2() %>% mutate(inside = lengths(st_within(DATA.sf.2(), lsmpa.boundary.input_yearly()))) %>% filter(inside == 1) %>%
      dplyr::select(hourtype = all_of(input$hourtype_yearly_2))
    
    round((sum(DATA$hourtype) / 24), digits = 0)
  })
  
  
  
  
  ## Once Filtered, Set Parameters and Manipulate Dataset for Outputs
  
  # Set Reactive Labels for Mapping
  # Main Title Label
  main.title.label_yearly_1 <- reactive({
    if (input$hourtype_yearly_1 == "hours") {
      "Monthly Mean Vessel Hours"
    } else if (input$hourtype_yearly_1 == "fishing_hours") {
      "Monthly Mean Vessel Fishing Hours"
    }
  })
  main.title.label_yearly_2 <- reactive({
    if (input$hourtype_yearly_2 == "hours") {
      "Monthly Mean Vessel Hours"
    } else if (input$hourtype_yearly_2 == "fishing_hours") {
      "Monthly Mean Vessel Fishing Hours"
    }
  })
  
  # Hour-type Label
  time.label_yearly_1 <- reactive({
    if (input$hourtype_yearly_1 == "hours") {
      "Mean Hours (Total)"
    } else if (input$hourtype_yearly_1 == "fishing_hours") {
      "Mean Fishing Hours"
    }
  })
  time.label_yearly_2 <- reactive({
    if (input$hourtype_yearly_2 == "hours") {
      "Mean Hours (Total)"
    } else if (input$hourtype_yearly_2 == "fishing_hours") {
      "Mean Fishing Hours"
    }
  })
  
  # Years Selected Subtitle
  year.subtitle_yearly_1 <- reactive({
    if (length(input$year_check_yearly_1) > 1) {
      years <- paste(sort(input$year_check_yearly_1), collapse = ", ")
      paste("Data Averaged for Years:", years, sep = " ")
    } else if (length(input$year_check_yearly_1) == 1) {
      paste("Data for Year", as.character(input$year_check_yearly_1), sep = " ")
    }
  })
  year.subtitle_yearly_2 <- reactive({
    if (length(input$year_check_yearly_2) > 1) {
      years <- paste(sort(input$year_check_yearly_2), collapse = ", ")
      paste("Data Averaged for Years:", years, sep = " ")
    } else if (length(input$year_check_yearly_2) == 1) {
      paste("Data for Year", as.character(input$year_check_yearly_2), sep = " ")
    }
  })
  
  
  # filter-type Labels 1
  gear.subtitle.label_yearly_1 <- reactive({
    if (!("All" %in% input$mmsi_gear_layer_yearly_1)) {
      subtitle <- paste(input$mmsi_gear_layer_yearly_1, collapse = ", ")
      subtitle <- paste("Vessels with matching gear-type:", subtitle, sep = " ")
    }
  })
  
  flag.subtitle.label_yearly_1 <- reactive({
    if (!("All" %in% input$mmsi_flag_layer_yearly_1)) {
      subtitle <- paste(input$mmsi_flag_layer_yearly_1, collapse = ", ")
      subtitle <- paste("Vessels with matching flag code:", subtitle, sep = " ")
    }
  })
  
  filter.subtitle.label_yearly_1 <- reactive({
    if (!(is.null(flag.subtitle.label_yearly_1())) && !(is.null(gear.subtitle.label_yearly_1()))) {
      paste(flag.subtitle.label_yearly_1(), gear.subtitle.label_yearly_1(), sep = " & ")
    } else {
      paste(flag.subtitle.label_yearly_1(), gear.subtitle.label_yearly_1())
    }
  })
  
  
  # filter-type Labels 2
  gear.subtitle.label_yearly_2 <- reactive({
    if (!("All" %in% input$mmsi_gear_layer_yearly_2)) {
      subtitle <- paste(input$mmsi_gear_layer_yearly_2, collapse = ", ")
      subtitle <- paste("Vessels with matching gear-type:", subtitle, sep = " ")
    }
  })
  
  flag.subtitle.label_yearly_2 <- reactive({
    if (!("All" %in% input$mmsi_flag_layer_yearly_2)) {
      subtitle <- paste(input$mmsi_flag_layer_yearly_2, collapse = ", ")
      subtitle <- paste("Vessels with matching flag code:", subtitle, sep = " ")
    }
  })
  
  filter.subtitle.label_yearly_2 <- reactive({
    if (!(is.null(flag.subtitle.label_yearly_2())) && !(is.null(gear.subtitle.label_yearly_2()))) {
      paste(flag.subtitle.label_yearly_2(), gear.subtitle.label_yearly_2(), sep = " & ")
    } else {
      paste(flag.subtitle.label_yearly_2(), gear.subtitle.label_yearly_2())
    }
  })
  
  
  
  
  
  
  
  # Isolate essential variables to begin transformation to Raster layer
  selected.data_yearly_1 <- reactive({
    filtered.data_yearly_1() %>%
      dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype_yearly_1)) %>%
      group_by("lon" = cell_ll_lon, "lat" = cell_ll_lat) %>%
      summarise("mean" = mean(hourtype), .groups = "drop")
  })
  selected.data_yearly_2 <- reactive({
    filtered.data_yearly_2() %>%
      dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype_yearly_2)) %>%
      group_by("lon" = cell_ll_lon, "lat" = cell_ll_lat) %>%
      summarise("mean" = mean(hourtype), .groups = "drop")
  })
  
  # Get Jenks Breaks
  jenksbreaks_yearly <- reactive({
    getJenksBreaks(c((selected.data_yearly_1()$`mean`), (selected.data_yearly_2()$`mean`)), k = 6)
  })
  # jenksbreaks_yearly_1 <- reactive({
  #   getJenksBreaks(selected.data_yearly_1()$`mean`, k = 6)
  # })
  # jenksbreaks_yearly_2 <- reactive({
  #   getJenksBreaks(selected.data_yearly_2()$`mean`, k = 6)
  # })
  
  # Ungroup, Recode, Mutate
  selected.data.ungrouped_yearly_1 <- reactive({
    selected.data_yearly_1() %>% mutate(jenks = add_jenks(`mean`, jenksbreaks_yearly()))
  })
  selected.data.ungrouped_yearly_2 <- reactive({
    selected.data_yearly_2() %>% mutate(jenks = add_jenks(`mean`, jenksbreaks_yearly()))
  })
  
  
  
  
  #### Tab 3: Map Comparison Outputs ####
  # Summary Text
  # Render summary text--map 1
  
  output$summary_text_map1_mean <- renderUI({
    line1 <- paste("Total Data Extent:", total.map1.mean(), "hours")
    line2 <- paste("Exclusive Economic Zone Boundary:", eez.total.map1.mean(), "hours")
    line3 <- paste("LSMPA Boundary:", lsmpa.total.map1.mean(), "hours")
    HTML(paste(line1, line2, line3, sep = '<br/>'))
  })
  
  output$summary_text_map1_cumul <- renderUI({
    line1 <- paste("Total Data Extent:", total.map1.cumul(), "days")
    line2 <- paste("Exclusive Economic Zone Boundary:", eez.total.map1.cumul(), "days")
    line3 <- paste("LSMPA Boundary:", lsmpa.total.map1.cumul(), "days")
    HTML(paste(line1, line2, line3, sep = '<br/>'))
  })
  
  # Render summary text--map 2
  output$summary_text_map2_mean <- renderUI({
    line1 <- paste("Total Data Extent:", total.map2.mean(), "hours")
    line2 <- paste("Exclusive Economic Zone Boundary:", eez.total.map2.mean(), "hours")
    line3 <- paste("LSMPA Boundary:", lsmpa.total.map2.mean(), "hours")
    HTML(paste(line1, line2, line3, sep = '<br/>'))
  })
  
  output$summary_text_map2_cumul <- renderUI({
    line1 <- paste("Total Data Extent:", total.map2.cumul(), "days")
    line2 <- paste("Exclusive Economic Zone Boundary:", eez.total.map2.cumul(), "days")
    line3 <- paste("LSMPA Boundary:", lsmpa.total.map2.cumul(), "days")
    HTML(paste(line1, line2, line3, sep = '<br/>'))
  })
  
  
  
  # Summary Map 1
  output$yearlymap1 <- renderPlot({
    
    scalebar.coords <- c(max(selected.data.ungrouped_yearly_1()$`lon`) - 0.6, min(selected.data.ungrouped_yearly_1()$`lat`) + 0.5)
    names(scalebar.coords) = c("x","y")
    
    scalebar.data <- selected.data.ungrouped_yearly_1() %>% dplyr::select(long = lon, lat)
    
    minlat <- if (min(selected.data.ungrouped_yearly_1()$`lat`) == 0) {
      0.00001
    } else {
      min(selected.data.ungrouped_yearly_1()$`lat`)
    }
    
    gg <- ggplot()
      if (2020 %in% input$year_check_yearly_1) {
        gg <- gg + geom_sf(data = lsmpa.boundary.input_yearly(), alpha = 0.3, colour = "blue", fill = "green")
      }
    gg <- gg +
      geom_sf(data = eez.boundary.input_yearly(), colour = "black", alpha = 0.5, fill = "dark blue") +
      geom_tile(data = selected.data.ungrouped_yearly_1(), aes(x = lon, y = lat, fill = jenks)) +
      scale_fill_viridis_d(time.label_yearly_1(), option = "C", direction = 1, labels = create_jenks_labels(jenksbreaks_yearly())) +
      geom_sf(data = eez.boundary.input_yearly(), colour = "black", alpha = 0, size = 3)
    
      if (2020 %in% input$year_check_yearly_1) {
        gg <- gg + geom_sf(data = lsmpa.boundary.input_yearly(), alpha = 0, colour = "blue", size = 3)
      }
    gg +
      geom_label(data = location.labels_yearly(), aes(label = id, x = lon, y = lat), label.r = unit(0, "lines"), alpha = 0.9) +
      coord_sf(xlim = c(min(selected.data.ungrouped_yearly_1()$`lon`), max(selected.data.ungrouped_yearly_1()$`lon`)),
               ylim = c(minlat, max(selected.data.ungrouped_yearly_1()$`lat`)), expand = FALSE) +
      labs(title = main.title.label_yearly_1(), subtitle = year.subtitle_yearly_1(), caption = filter.subtitle.label_yearly_1()) +
      theme_classic(base_size = 16) +
      theme(axis.line = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.title = element_text(size=22, hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "bottom") + 
      scalebar(scalebar.data, location = "bottomright", dist = 200, dist_unit = "km",
               transform = TRUE, model = "WGS84", anchor = scalebar.coords, st.color = "white")
    
  })
  # Summary Map 2
  output$yearlymap2 <- renderPlot({
    
    scalebar.coords <- c(max(selected.data.ungrouped_yearly_2()$`lon`) - 0.6, min(selected.data.ungrouped_yearly_2()$`lat`) + 0.5)
    names(scalebar.coords) = c("x","y")
    
    scalebar.data <- selected.data.ungrouped_yearly_2() %>% dplyr::select(long = lon, lat)
    
    minlat <- if (min(selected.data.ungrouped_yearly_2()$`lat`) == 0) {
      0.00001
    } else {
      min(selected.data.ungrouped_yearly_2()$`lat`)
    }
    
    
    gg <- ggplot()
    if (2020 %in% input$year_check_yearly_2) {
      gg <- gg + geom_sf(data = lsmpa.boundary.input_yearly(), alpha = 0.3, colour = "blue", fill = "green")
    }
    gg <- gg +
      geom_sf(data = eez.boundary.input_yearly(), colour = "black", alpha = 0.5, fill = "dark blue") +
      geom_tile(data = selected.data.ungrouped_yearly_2(), aes(x = lon, y = lat, fill = jenks)) +
      scale_fill_viridis_d(time.label_yearly_2(), option = "C", direction = 1, labels = create_jenks_labels(jenksbreaks_yearly())) +
      geom_sf(data = eez.boundary.input_yearly(), colour = "black", alpha = 0, size = 3)
    
    if (2020 %in% input$year_check_yearly_2) {
      gg <- gg + geom_sf(data = lsmpa.boundary.input_yearly(), alpha = 0, colour = "blue", size = 3)
    }
    gg +
      geom_label(data = location.labels_yearly(), aes(label = id, x = lon, y = lat), label.r = unit(0, "lines"), alpha = 0.9) +
      coord_sf(xlim = c(min(selected.data.ungrouped_yearly_2()$`lon`), max(selected.data.ungrouped_yearly_2()$`lon`)),
               ylim = c(minlat, max(selected.data.ungrouped_yearly_2()$`lat`)), expand = FALSE) +
      labs(title = main.title.label_yearly_2(), subtitle = year.subtitle_yearly_2(), caption = filter.subtitle.label_yearly_2()) +
      theme_classic(base_size = 16) +
      theme(axis.line = element_blank(), axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            plot.title = element_text(size=22, hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "bottom") + 
      scalebar(scalebar.data, location = "bottomright", dist = 200, dist_unit = "km",
               transform = TRUE, model = "WGS84", anchor = scalebar.coords, st.color = "white")
    
  })
  
  
  #### Tab 4: Timeseries ####
  
  ## Reactive Inputs for Tab 4: Timeseries
  
  # Render LSMPA Title--Monthly
  output$lsmpa_title_timeseries <- renderText({ 
    if (input$lsmpa_timeseries == c("pnms")) {
      "Palau National Marine Sanctuary"
    }
  })
  
  ## Accessing and Filtering Initial Dataset
  
  #Access appropriate LSMPA data based on LSMPA input
  data.input_timeseries <- reactive({
    lsmpa.selection <- input$lsmpa_timeseries  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      pnms.vessel.tbl.daily.mmsi.data
    }
  })
  
  # Access appropriate LSMPA Geospatial Boundary
  lsmpa.boundary.input_timeseries <- reactive({
    lsmpa.selection <- input$lsmpa_timeseries  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      sf.pnms
    }
  })
  
  # Access appropriate EEZ Geospatial Boundary
  eez.boundary.input_timeseries <- reactive({
    lsmpa.selection <- input$lsmpa_timeseries  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      palau.neighboring.eez
    }
  })
  
  # Access appropriate Location Labels (EEZ Labels)
  location.labels_timeseries <- reactive({
    lsmpa.selection <- input$lsmpa_timeseries  # Character LSMPA Label
    
    # PNMS Data option
    if (lsmpa.selection == c("pnms")) {
      palau.labels.tbl
    }
  })
  
  # # Filter data based on year check selection
  # year.filtered.data.input_timeseries <- reactive({
  #   data.input_timeseries() %>%
  #     filter(year(date) %in% input$year_check_timeseries)
  # })
  
  # Filter data based on year check selection
  year.filtered.data.input_timeseries <- reactive({
    data.input_timeseries() %>%
      filter(date >= as.Date(input$year_range_timeseries[1])) %>%
      filter(date <= as.Date(input$year_range_timeseries[2]))
  })

  
  # Filter data based on further parameters
  # Flag
  flag.filtered.data.input_timeseries <- reactive({
    
    if ("All" %in% input$mmsi_flag_layer_timeseries | length(input$mmsi_flag_layer_timeseries) == 0) {
      year.filtered.data.input_timeseries()
    } else {
      year.filtered.data.input_timeseries() %>% filter(flag_gfw %in% input$mmsi_flag_layer_timeseries)
    }
    
  })
  
  # Gear
  gear.filtered.data.input_timeseries <- reactive({
    
    if ("All" %in% input$mmsi_gear_layer_timeseries | length(input$mmsi_gear_layer_timeseries) == 0) {
      flag.filtered.data.input_timeseries()
    } else {
      flag.filtered.data.input_timeseries() %>% filter(vessel_class_gfw %in% input$mmsi_gear_layer_timeseries)
    }
    
  })
  
  # Filter data to exclude buffer around the LSMPA (only includes points inside the LSMPA)
  boundary.filtered.data.input_timeseries <- reactive({
    DATA <- gear.filtered.data.input_timeseries()
    
    if (input$boundary_limit_timeseries == "eez") {
      DATA.sf <- st_as_sf(DATA, coords = c("cell_ll_lon", "cell_ll_lat"), crs = 4326)
      DATA <- DATA %>% mutate(inside = lengths(st_within(DATA.sf, eez.boundary.input_timeseries())))
      DATA %>% filter(inside == 1)
    } else if (input$boundary_limit_timeseries == "lsmpa") {
      DATA.sf <- st_as_sf(DATA, coords = c("cell_ll_lon", "cell_ll_lat"), crs = 4326)
      DATA <- DATA %>% mutate(inside = lengths(st_within(DATA.sf, lsmpa.boundary.input_timeseries())))
      DATA %>% filter(inside == 1)
    } else {
      DATA
    }
  })
  
  
  
  # No further filtering for now
  filtered.data_timeseries <- reactive({
    boundary.filtered.data.input_timeseries()
  })
  
  # Group by: No Group, Gear Type Selection, Vessel Flag Selection
  
  ## Once Filtered, Set Parameters and Manipulate Dataset for Outputs
  
  # Set Reactive Labels for Mapping
  # Main Title Label
  main.title.label_timeseries <- reactive({
    if (input$hourtype_timeseries == "hours") {
      "Monthly Mean Vessel Hours"
    } else if (input$hourtype_timeseries == "fishing_hours") {
      "Monthly Mean Vessel Fishing Hours"
    }
  })
  
  # Hour-type Label
  time.label_timeseries <- reactive({
    if (input$hourtype_timeseries == "hours") {
      "Mean Hours (Total)"
    } else if (input$hourtype_timeseries == "fishing_hours") {
      "Mean Fishing Hours"
    }
  })
  
  # Years Selected Subtitle
  year.subtitle_timeseries <- reactive({
    if (length(input$year_check_timeseries) > 1) {
      years <- paste(sort(input$year_check_timeseries), collapse = ", ")
      paste("Data Averaged for Years:", years, sep = " ")
    } else if (length(input$year_check_timeseries) == 1) {
      paste("Data for Year", as.character(input$year_check_timeseries), sep = " ")
    }
  })
  
  # filter-type Labels
  gear.subtitle.label_timeseries <- reactive({
    if (!("All" %in% input$mmsi_gear_layer_timeseries)) {
      subtitle <- paste(input$mmsi_gear_layer_timeseries, collapse = ", ")
      subtitle <- paste("Vessels with matching gear-type:", subtitle, sep = " ")
    }
  })
  
  flag.subtitle.label_timeseries <- reactive({
    if (!("All" %in% input$mmsi_flag_layer_timeseries)) {
      subtitle <- paste(input$mmsi_flag_layer_timeseries, collapse = ", ")
      subtitle <- paste("Vessels with matching flag code:", subtitle, sep = " ")
    }
  })
  
  filter.subtitle.label_timeseries <- reactive({
    if (!(is.null(flag.subtitle.label_timeseries())) && !(is.null(gear.subtitle.label_timeseries()))) {
      paste(flag.subtitle.label_timeseries(), gear.subtitle.label_timeseries(), sep = " & ")
    } else {
      paste(flag.subtitle.label_timeseries(), gear.subtitle.label_timeseries())
    }
  })
  
  # Isolate essential variables
  selected.data_timeseries_mean <- reactive({
    if (input$grouping_timeseries == "flag") {
      filtered.data_timeseries() %>%
        dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype_timeseries), flag_gfw) %>%
        group_by("year" = year(`date`), "month" = month(`date`), "flag" = `flag_gfw`) %>%
        summarise("mean" = mean(hourtype), .groups = "drop") %>%
        mutate(dates = ym((paste(`year`, `month`,  sep = "-"))))
    } else if (input$grouping_timeseries == "gear") {
      filtered.data_timeseries() %>%
        dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype_timeseries), vessel_class_gfw) %>%
        group_by("year" = year(`date`), "month" = month(`date`), "gear" = `vessel_class_gfw`) %>%
        summarise("mean" = mean(hourtype), .groups = "drop") %>%
        mutate(dates = ym((paste(`year`, `month`,  sep = "-"))))
    } else {
      filtered.data_timeseries() %>%
        dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype_timeseries)) %>%
        group_by("year" = year(`date`), "month" = month(`date`)) %>%
        summarise("mean" = mean(hourtype), .groups = "drop") %>%
        mutate(dates = ym((paste(`year`, `month`,  sep = "-"))))
    }
  })
  
  selected.data_timeseries_cumul <- reactive({
    if (input$grouping_timeseries == "flag") {
      filtered.data_timeseries() %>%
        dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype_timeseries), flag_gfw) %>%
        group_by("year" = year(`date`), "month" = month(`date`), "flag" = `flag_gfw`) %>%
        summarise("sum" = (sum(hourtype) / 24), .groups = "drop") %>%
        mutate(dates = ym((paste(`year`, `month`,  sep = "-"))))
    } else if (input$grouping_timeseries == "gear") {
      filtered.data_timeseries() %>%
        dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype_timeseries), vessel_class_gfw) %>%
        group_by("year" = year(`date`), "month" = month(`date`), "gear" = `vessel_class_gfw`) %>%
        summarise("sum" = (sum(hourtype) / 24), .groups = "drop") %>%
        mutate(dates = ym((paste(`year`, `month`,  sep = "-"))))
    } else {
      filtered.data_timeseries() %>%
        dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype_timeseries)) %>%
        group_by("year" = year(`date`), "month" = month(`date`)) %>%
        summarise("sum" = (sum(hourtype) / 24), .groups = "drop") %>%
        mutate(dates = ym((paste(`year`, `month`,  sep = "-"))))
    }
  })
  
  # Time Series
  output$timeseries_mean <- renderPlot({
    
    interval <- interval(input$year_range_timeseries[1], input$year_range_timeseries[2])
    seconds <- int_length(interval)
    seconds_intervals <- seconds / 7  # choose number of intervals for graph, e.g., 8
    days_inteval <- seconds_intervals / 86400
    interval_breaks <- seq(as.Date(input$year_range_timeseries[1]), by=days_inteval, len=7)
    
    
    if (input$grouping_timeseries == "flag") {
      data <- selected.data_timeseries_mean() %>% mutate(flag = fct_lump_n(as.factor(replace_na(flag, "UNK")), n = 7))
      
      ggplot(data) + 
        geom_line(aes(x = dates, y = mean, color = flag), size=1.3) + 
        scale_color_brewer("Vessel Flag", palette = "Dark2") +
        labs(title = "Mean Vessel Hours Over Time", y = "Mean Vessel Hours", subtitle = year.subtitle_timeseries(), caption = filter.subtitle.label_timeseries()) +
        theme_bw(base_size = 24) +
        theme(axis.title.x=element_blank()) +
        scale_x_date(breaks = interval_breaks , date_labels = "%b-%y")
    } else if (input$grouping_timeseries == "gear") {
      data <- selected.data_timeseries_mean() %>% mutate(gear = fct_lump_n(as.factor(replace_na(gear, "UNK")), n = 7))
      
      ggplot(data) + 
        geom_line(aes(x = dates, y = mean, color = gear), size=1.3) + 
        scale_color_brewer("Vessel Gear-Type", palette = "Dark2") +
        labs(title = "Mean Vessel Hours Over Time", y = "Mean Vessel Hours", subtitle = year.subtitle_timeseries(), caption = filter.subtitle.label_timeseries()) +
        theme_bw(base_size = 24) +
        theme(axis.title.x=element_blank()) +
        scale_x_date(breaks = interval_breaks , date_labels = "%b-%y")
    } else {
      ggplot(data = selected.data_timeseries_mean()) + 
        geom_line(aes(x = dates, y = mean), size=1.3, color = "dark blue") + 
        labs(title = "Mean Vessel Hours Over Time", y = "Mean Vessel Hours", subtitle = year.subtitle_timeseries(), caption = filter.subtitle.label_timeseries()) +
        theme_bw(base_size = 24) +
        theme(axis.title.x=element_blank()) +
        scale_x_date(breaks = interval_breaks , date_labels = "%b-%y")
    }
  })
  
  output$timeseries_cumul <- renderPlot({

    interval <- interval(input$year_range_timeseries[1], input$year_range_timeseries[2])
    seconds <- int_length(interval)
    seconds_intervals <- seconds / 7  # choose number of intervals for graph, e.g., 8
    days_inteval <- seconds_intervals / 86400
    interval_breaks <- seq(as.Date(input$year_range_timeseries[1]), by=days_inteval, len=7)
  
    
    if (input$grouping_timeseries == "flag") {
      data <- selected.data_timeseries_cumul() %>% mutate(flag = fct_lump_n(as.factor(replace_na(flag, "UNK")), n = 7))
      
      ggplot(data) + 
        geom_line(aes(x = dates, y = sum, color = flag), size=1.3) + 
        scale_color_brewer("Vessel Flag", palette = "Dark2") +
        labs(title = "Total Vessel Days Over Time", y = "Total Vessel Days", subtitle = year.subtitle_timeseries(), caption = filter.subtitle.label_timeseries()) +
        theme_bw(base_size = 24) +
        theme(axis.title.x=element_blank()) +
        scale_x_date(breaks = interval_breaks , date_labels = "%b-%y")
    } else if (input$grouping_timeseries == "gear") {
      data <- selected.data_timeseries_cumul() %>% mutate(gear = fct_lump_n(as.factor(replace_na(gear, "UNK")), n = 7))
      
      ggplot(data) + 
        geom_line(aes(x = dates, y = sum, color = gear), size=1.3) + 
        scale_color_brewer("Vessel Gear-Type", palette = "Dark2") +
        labs(title = "Total Vessel Days Over Time", y = "Total Vessel Days", subtitle = year.subtitle_timeseries(), caption = filter.subtitle.label_timeseries()) +
        theme_bw(base_size = 24) +
        theme(axis.title.x=element_blank()) +
        scale_x_date(breaks = interval_breaks , date_labels = "%b-%y")
    } else {
      ggplot(data = selected.data_timeseries_cumul()) + 
        geom_line(aes(x = dates, y = sum), size=1.3, color = "dark blue") + 
        labs(title = "Total Vessel Days Over Time", y = "Total Vessel Days", subtitle = year.subtitle_timeseries(), caption = filter.subtitle.label_timeseries()) +
        theme_bw(base_size = 24) +
        theme(axis.title.x=element_blank()) +
        scale_x_date(breaks = interval_breaks , date_labels = "%b-%y")
    }
  })
  
  
  
  #### TAB 5: DATA SOURCES ####
  

}

# run app
shinyApp(ui = ui, server = server)










#### DEPRECATED: EXTRA/DRAFTING ####
## The following code is draft code for various sections of the dashboard and
## can be ignored. It is preserved for reference.

## TMAP Supporting functions
# ## Rearrange Data Col-Wise to support Raster mapping
# selected.data.list <- reactive({
#   selected.data.list <- selected.data()  %>% group_by(month) %>%
#     group_split %>%
#     lapply(function(x) {
#       x <- x %>% dplyr::select(-month)
#     })
# 
#   names(selected.data.list) <- c("January", "February", "March",
#                                        "April", "May", "June", "July",
#                                        "August", "September", "October",
#                                        "November", "December")
#   selected.data.list
# })
# 
# 
# # Rename columns to support col-wise bind
# selected.data.list.renamed <- reactive({
#   names(selected.data.list()) %>% lapply(function(month) {
#     tbl <- selected.data.list()[[month]]
#     tbl <- tbl %>% dplyr::select(lon, lat, mean)
#     newname <- paste(month, "mean")
#     tbl <- tbl %>% rename(!!newname := "mean")
#   })
# })
# 
# 
# # Merge
# raster.selected.data <- reactive({
#   rearranged.selectd.data <- Reduce(full_join, selected.data.list.renamed()) %>%
#     mutate_all(~replace(., is.na(.), -1))
#   raster <- rasterFromXYZ(rearranged.selectd.data[,1:2])
#   raster <- rasterize(rearranged.selectd.data[,1:2],
#                       raster, field=rearranged.selectd.data[,3:ncol(rearranged.selectd.data)])
#   crs(raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
#   raster[raster < 0] <- NA
#   names(raster) <- c("January", "February", "March", "April",
#                                    "May", "June", "July", "August",
#                                    "September", "October", "November", "December")
# 
#   raster
# })

## Extra Leaflet
# output$annual2 <- renderLeaflet({
#   jenksbreaksleaflet <- getJenksBreaks(selected.data.annual()$`mean`, k = 6)
#   
#   pal7 <- colorBin("plasma", domain = jenksbreaksleaflet, bins = jenksbreaksleaflet, na.color = "transparent")
#   
#   leaflet() %>%
#     addProviderTiles(providers$Stamen.TonerLite) %>%
#     addPolygons(data = lsmpa.boundary.input.annual(), stroke = TRUE, color = "red", weight = 3, fillOpacity = 0, opacity = 1) %>%
#     addPolygons(data = eez.boundary.input.annual(), stroke = TRUE, color = "black", weight = 3, fillOpacity = 0, opacity = 1) %>%
#     addRasterImage(raster.selected.data.annual(), colors = pal7, opacity = 1) %>%
#     addLegend(pal = pal7, values = values(raster.selected.data.annual()),
#               title = time.label.annual())
# })



# Download test
# # Download Output Plot Button
# output$download_ggplot <- downloadHandler(
#     filename = function() {
#       paste('fishing-plot-dashboard', Sys.Date(), '.png', sep='')
#     },
#     content = function(con) {
#       ggsave(con, output$mainmap, device = "png")
#     }
#   )




# Updating Flag/geartype if needed:
## Dynamically set options for flag and vessel filtering
# observe({
#   # get all character or factor columns
#   flags <- 
#     updateSelectInput(session, "x",
#                       choices = flags, # update choices
#                       selected = NULL) # remove selection
# })

# observe({
#   x <- input$flag_select
# 
#   # Can use character(0) to remove all choices
#   if (is.null(x))
#     x <- character(0)
# 
#   # Can also set the label and select items
#   updateSelectInput(session = getDefaultReactiveDomain(), "inSelect",
#                     label = paste("Select input label", length(x)),
#                     choices = x,
#                     selected = tail(x, 1)
#   )
# })


# Extra shiny panels
# sliderInput("year_range", "Year Range:",
#             min = as.Date("2012-01-01","%Y-%m-%d"),
#             max = as.Date("2020-12-31","%Y-%m-%d"),
#             value=c(as.Date("2019-01-01", "%Y-%m-%d"), as.Date("2020-12-31", "%Y-%m-%d")),
#             timeFormat="%Y-%m-%d"),
# conditionalPanel(
#   condition = "input.datatype.includes('mmsi')",
#   checkboxGroupInput(inputId = "mmsilayer", "Further Filtering; Filter by:", 
#                      choices = c("MMSI Vessel Flag", "MMSI Vessel Class"),
#                      selected = NULL)
# ),
# conditionalPanel(
#   condition = "input.datatype.includes('fleet')",
#   checkboxGroupInput(inputId = "fleetlayer", "Filter by:", 
#                      choices = c("Fleet Flag", "Fleet Geartype"),
#                      selected = NULL)
# ),


## Raster/ old leaflet outputs
# # Create Raster
# raster.selected.data.annual.list <- reactive({
#   selected.data.annual.list() %>% lapply(function(x) {
#     raster <- rasterFromXYZ(x[,1:2])
#     raster <- rasterize(x[,1:2],
#                         raster, field=x[,3])
#     crs(raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#     raster
#   })
# })


# # Isolate essential variables to begin transformation to Raster layer
# selected.data.annual <- reactive({
#   filtered.data.annual() %>%
#     dplyr::select(date, cell_ll_lon, cell_ll_lat, hourtype = all_of(input$hourtype_annual)) %>%
#     group_by("lon" = cell_ll_lon, "lat" = cell_ll_lat) %>%
#     summarise("mean" = mean(hourtype), .groups = "drop")
# })
# # Merge
# raster.selected.data.annual <- reactive({
#   raster <- rasterFromXYZ(selected.data.annual()[,1:2])
#   raster <- rasterize(selected.data.annual()[,1:2],
#                       raster, field=selected.data.annual()[,3])
#   crs(raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#   raster
# })


### OUTPUTS
#Leaflet Map
# output$annual <- renderLeaflet({
#   jenksbreaksleaflet <- getJenksBreaks(values(raster.selected.data.annual()), k = 6)
#   
#   pal7 <- colorBin("plasma", domain = jenksbreaksleaflet, bins = jenksbreaksleaflet, na.color = "transparent")
#   
#   leaflet() %>%
#     addProviderTiles(providers$Stamen.TonerLite) %>%
#     addPolygons(data = lsmpa.boundary.input.annual(), stroke = TRUE, color = "red", weight = 3, fillOpacity = 0, opacity = 1) %>%
#     addPolygons(data = eez.boundary.input.annual(), stroke = TRUE, color = "black", weight = 3, fillOpacity = 0, opacity = 1) %>%
#     addRasterImage(raster.selected.data.annual(), colors = pal7, opacity = 1) %>%
#     addLegend(pal = pal7, values = values(raster.selected.data.annual()),
#               title = time.label.annual()) %>%
#     setMaxBounds(min(selected.data.annual()$`lon`), min(selected.data.annual()$`lat`), max(selected.data.annual()$`lon`), max(selected.data.annual()$`lat`))
#   
# })

# output$leafletoutputmap1 <- renderLeaflet({
#   if (length(input$year_dropdown_annual) >= 1) {
#     jenksbreaksleaflet <- getJenksBreaks(values(raster.selected.data.annual.list()[[1]]), k = 6)
#     
#     pal7 <- colorBin("plasma", domain = jenksbreaksleaflet, bins = jenksbreaksleaflet, na.color = "transparent")
#     
#     leaflet() %>%
#       addProviderTiles(providers$Stamen.TonerLite) %>%
#       addPolygons(data = lsmpa.boundary.input.annual(), stroke = TRUE, color = "red", weight = 3, fillOpacity = 0, opacity = 1) %>%
#       addPolygons(data = eez.boundary.input.annual(), stroke = TRUE, color = "black", weight = 3, fillOpacity = 0, opacity = 1) %>%
#       addRasterImage(raster.selected.data.annual.list()[[1]], colors = pal7, opacity = 1) %>%
#       addLegend(pal = pal7, values = values(raster.selected.data.annual.list()[[1]]),
#                 title = time.label.annual(), opacity = 1) %>%
#       fitBounds(min(selected.data.annual.list()[[1]]$`lon`), min(selected.data.annual.list()[[1]]$`lat`), max(selected.data.annual.list()[[1]]$`lon`), max(selected.data.annual.list()[[1]]$`lat`))
#   }
# })

# jenks.scale.labels.list <- reactive({
#   selected.data.annual.list() %>% lapply(function(x) {
#     jenksbreaks <- getJenksBreaks(x$`mean`, k = 6)
#     
#     breakslabel <- c()
#     
#     for (n in 1:(length(jenksbreaks) - 1)) {
#       breakslabel <- append(breakslabel, paste(as.character(round(jenksbreaks[n]), digits = 2), as.character(round(jenksbreaks[(n + 1)]), digits = 2), sep = "—"))
#     }
#     
#     breakslabel
#   })
# })



### DATA SCALE Options ###
# radioButtons(inputId = "datatype_annual", "Select Data Scale",
#              choiceValues = c("mmsi", "fleet"),
#              choiceNames =  c("MMSI (0.1 degree resolution)", "Fleet (0.01 degree resolution)"),
#              selected = c("mmsi")),
# 
# br(),
# radioButtons(inputId = "datatype", "Select Data Scale",
#              choiceValues = c("mmsi", "fleet"),
#              choiceNames =  c("MMSI (0.1 degree resolution)", "Fleet (0.01 degree resolution)"),
#              selected = c("mmsi")),
# 
# br(),
# ## Vessel Flag Filtering
# conditionalPanel(
#   condition = "input.datatype_annual.includes('mmsi')",
#   selectizeInput(
#     inputId = "mmsi_flag_layer_annual",
#     label = "Filter by MMSI Vessel Flag:",
#     choices = c("All", names(sort(table(pnms.vessel.tbl.daily.mmsi.data$flag_gfw),decreasing=TRUE)[1:4])),
#     selected = c("All"),
#     multiple = T
#   )
#   # dropdownButton(
#   #   inputId = "dropdown",
#   #   circle = FALSE,
#   #   label = "Filter by MMSI Vessel Flag:",
#   #   status = "custom",
#   #   checkboxGroupInput(inputId = "mmsi_flag_layer_annual", label = NULL,
#   #                      choices = c("All", unique(pnms.vessel.tbl.daily.mmsi.data$flag_gfw)),
#   #                      selected = c("All"))
#   # )
# ),
# conditionalPanel(
#   condition = "input.datatype_annual.includes('fleet')",
#   selectizeInput(
#     inputId = "fleet_flag_layer_annual",
#     label = "Filter by Fleet Flag:",
#     choices = c("All", names(sort(table(pnms.tbl.daily.fleet.data$flag),decreasing=TRUE)[1:4])),
#     selected = c("All"),
#     multiple = T
#   )
#   # dropdownButton(
#   #   inputId = "dropdown",
#   #   circle = FALSE,
#   #   label = "Filter by Fleet Flag:",
#   #   status = "custom",
#   #   checkboxGroupInput(inputId = "fleet_flag_layer_annual", label = NULL,
#   #                      choices = c("All", unique(pnms.tbl.daily.fleet.data$flag)),
#   #                      selected = c("All"))
#   # )
# ),
# br(),
# ## Vessel Gear-Type Filtering
# conditionalPanel(
#   condition = "input.datatype_annual.includes('mmsi')",
#   selectizeInput(
#     inputId = "mmsi_gear_layer_annual",
#     label = "Filter by MMSI Vessel Class:",
#     choices =  c("All", unique(pnms.vessel.tbl.daily.mmsi.data$vessel_class_gfw)),
#     selected = c("All"),
#     multiple = T
#   )
#   # dropdownButton(
#   #   inputId = "dropdown",
#   #   circle = FALSE,
#   #   label = "Filter by MMSI Vessel Class:",
#   #   status = "custom",
#   #   checkboxGroupInput(inputId = "mmsi_gear_layer_annual", label = NULL,
#   #                      choices = c("All", unique(pnms.vessel.tbl.daily.mmsi.data$vessel_class_gfw)),
#   #                      selected = c("All"))
#   # )
# ),
# conditionalPanel(
#   condition = "input.datatype_annual.includes('fleet')",
#   selectizeInput(
#     inputId = "fleet_gear_layer_annual",
#     label = "Filter by Fleet Geartype:",
#     choices =  c("All", unique(pnms.tbl.daily.fleet.data$geartype)),
#     selected = c("All"),
#     multiple = T
#   )
#   # dropdownButton(
#   #   inputId = "dropdown",
#   #   circle = FALSE,
#   #   label = "Filter by Fleet Geartype:",
#   #   status = "custom",
#   #   checkboxGroupInput(inputId = "fleet_gear_layer_annual", label = NULL,
#   #                      choices = c("All", unique(pnms.tbl.daily.fleet.data$geartype)),
#   #                      selected = c("All"))
#   # )
# ),

# ## Vessel Flag Filtering
# conditionalPanel(
#   condition = "input.datatype.includes('mmsi')",
#   selectizeInput(
#     inputId = "mmsi_flag_layer",
#     label = "Filter by MMSI Vessel Flag:",
#     choices = c("All", names(sort(table(pnms.vessel.tbl.daily.mmsi.data$flag_gfw),decreasing=TRUE)[1:4])),
#     selected = c("All"),
#     multiple = T
#   )
#   #   dropdownButton(
#   #     inputId = "dropdown",
#   #     circle = FALSE,
#   #     label = "Filter by MMSI Vessel Flag:",
#   #     status = "custom",
#   #     checkboxGroupInput(inputId = "mmsi_flag_layer", label = NULL,
#   #                        choices = c("All", names(sort(table(pnms.vessel.tbl.daily.mmsi.data$flag_gfw),decreasing=TRUE)[1:4])),
#   #                        selected = c("All"))
#   #     
#   #   )
# ),
# conditionalPanel(
#   condition = "input.datatype.includes('fleet')",
#   selectizeInput(
#     inputId = "fleet_flag_layer",
#     label = "Filter by Fleet Flag:",
#     choices = c("All", names(sort(table(pnms.tbl.daily.fleet.data$flag),decreasing=TRUE)[1:4])),
#     selected = c("All"),
#     multiple = T
#   )
#   # dropdownButton(
#   #   inputId = "dropdown",
#   #   circle = FALSE,
#   #   label = "Filter by Fleet Flag:",
#   #   status = "custom",
#   #   checkboxGroupInput(inputId = "fleet_flag_layer", label = NULL,
#   #                      choices = c("All", names(sort(table(pnms.tbl.daily.fleet.data$flag),decreasing=TRUE)[1:4])),
#   #                      selected = c("All"))
#   # )
# ),
# br(),
# ## Vessel Gear-Type Filtering
# conditionalPanel(
#   condition = "input.datatype.includes('mmsi')",
#   selectizeInput(
#     inputId = "mmsi_gear_layer",
#     label = "Filter by MMSI Vessel Class:",
#     choices =  c("All", unique(pnms.vessel.tbl.daily.mmsi.data$vessel_class_gfw)),
#     selected = c("All"),
#     multiple = T
#   )
#   # dropdownButton(
#   #   inputId = "dropdown",
#   #   circle = FALSE,
#   #   label = "Filter by MMSI Vessel Class:",
#   #   status = "custom",
#   #   checkboxGroupInput(inputId = "mmsi_gear_layer", label = NULL,
#   #                      choices = c("All", unique(pnms.vessel.tbl.daily.mmsi.data$vessel_class_gfw)),
#   #                      selected = c("All"))
#   # )
# ),
# conditionalPanel(
#   condition = "input.datatype.includes('fleet')",
#   selectizeInput(
#     inputId = "fleet_gear_layer",
#     label = "Filter by Fleet Geartype:",
#     choices =  c("All", unique(pnms.tbl.daily.fleet.data$geartype)),
#     selected = c("All"),
#     multiple = T
#   )
#   # dropdownButton(
#   #   inputId = "dropdown",
#   #   circle = FALSE,
#   #   label = "Filter by Fleet Geartype:",
#   #   status = "custom",
#   #   checkboxGroupInput(inputId = "fleet_gear_layer", label = NULL,
#   #                      choices = c("All", unique(pnms.tbl.daily.fleet.data$geartype)),
#   #                      selected = c("All"))
#   # )
# ),
# #Access appropriate LSMPA data based on LSMPA input and MMSI/Fleet input
# data.input <- reactive({
#   lsmpa.selection <- input$lsmpa  # Character LSMPA Label
#   data.selection <- input$datatype  # Character data type (MMSI/Fleet)
#   
#   # PNMS Data option
#   if (lsmpa.selection == c("pnms")) {
#     if (data.selection == c("mmsi")) {
#       pnms.vessel.tbl.daily.mmsi.data
#     } else if (data.selection == c("fleet")) {
#       pnms.tbl.daily.fleet.data
#     }
#   }
#   
# })

# # Flag
# flag.filtered.data.input.annual <- reactive({
#   data.selection <- input$datatype_annual  # Character data type (MMSI/Fleet)
#   
#   if (data.selection == c("mmsi")) {
#     if ("All" %in% input$mmsi_flag_layer_annual | length(input$mmsi_flag_layer_annual) == 0) {
#       year.filtered.data.input.annual()
#     } else {
#       year.filtered.data.input.annual() %>% filter(flag_gfw %in% input$mmsi_flag_layer_annual)
#     }
#   } else if (data.selection == c("fleet")) {
#     if ("All" %in% input$fleet_flag_layer_annual | length(input$fleet_flag_layer_annual) == 0) {
#       year.filtered.data.input.annual()
#     } else {
#       year.filtered.data.input.annual() %>% filter(flag %in% input$fleet_flag_layer_annual)
#     }
#   }
# })
# 
# # Gear
# gear.filtered.data.input.annual <- reactive({
#   # If input$mmsi_gear_layer or input$fleet_gear_layer
#   data.selection <- input$datatype_annual  # Character data type (MMSI/Fleet)
#   
#   if (data.selection == c("mmsi")) {
#     if ("All" %in% input$mmsi_gear_layer_annual | length(input$mmsi_gear_layer_annual) == 0) {
#       flag.filtered.data.input.annual()
#     } else {
#       flag.filtered.data.input.annual() %>% filter(vessel_class_gfw %in% input$mmsi_gear_layer_annual)
#     }
#   } else if (data.selection == c("fleet")) {
#     if ("All" %in% input$fleet_gear_layer_annual | length(input$fleet_gear_layer_annual) == 0) {
#       flag.filtered.data.input.annual()
#     } else {
#       flag.filtered.data.input.annual() %>% filter(geartype %in% input$fleet_gear_layer_annual)
#     }
#   }
# })


# Main panel: multiple tabs:
# mainPanel(
#   fluidRow(
#     column (12,
#             h1(textOutput("lsmpa_title"))
#     )
#   ),
#   tabsetPanel(
#     tabPanel("Summary Map",
#              fluidRow(column (12,
#                               plotOutput("summarymap", width = "100%", height = "800px")
#              )
#              )
#     ),
#     tabPanel("Monthly Summary Maps", fluidRow(
#       column (12,
#               plotOutput("monthlymap", width = "100%", height = "800px"))
#     )),
#     tabPanel("Distributions", plotOutput("distribution", width = "100%", height = "800px")),
#     tabPanel("Over Time", plotOutput("timeseries", width = "100%", height = "800px"))
#   )
# )

# year input options
# dropdownButton(
#   inputId = "dropdown",
#   circle = FALSE,
#   label = "Select Year(s):",
#   status = "custom",
#   checkboxGroupInput(inputId = "year_check", label = NULL,
#                      choices = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
#                      selected = c("2020")
#   )
# ),
# sliderInput("year_range", "Select Year(s):",
#             min = 2012, max = 2020,
#             value = c(2019,2020),
#             sep = ""),


# mainPanel(
#   fluidRow(
#     column (12,
#             h1(textOutput("lsmpa_title_timeseries"))
#     )
#   ),
#   fluidRow(
#     column (12,
#             plotOutput("timeseries_mean", width = "100%", height = "800px"))
#   ),
#   fluidRow(
#     column (12,
#             plotOutput("timeseries_cumul", width = "100%", height = "800px"))
#   )
# )