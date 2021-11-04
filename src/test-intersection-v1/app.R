# Test shiny app, v5
# Written by Steve Simon
# Created on 2021-10-20
# Purpose: to test some simple maps in Shiny
# License: Public domain

library(shiny)
library(sf)
library(tidyverse)

path_name <- "../../data"

file_name <- "tl_2020_29_tract.shp"
path_name                              %>%
  paste(file_name, sep="/")            %>%
  st_read(stringsAsFactors=FALSE)      -> mo_tracts

mo_tracts %>%
  filter(COUNTYFP=="095") -> jackson_co_tracts

# Variables:
#   "STATEFP"
#   "COUNTYFP"
#   "TRACTCE"
#   "GEOID"
#   "NAME"
#   "NAMELSAD"
#   "MTFCC"
#   "FUNCSTAT"
#   "ALAND"
#   "AWATER"
#   "INTPTLAT"
#   "INTPTLON"
#   "geometry"

crs_tracts <- st_crs(mo_tracts)

file_name <- "cb_2020_29_bg_500k.shp"
path_name                              %>%
  paste(file_name, sep="/")            %>%
  st_read(stringsAsFactors=FALSE)      -> mo_block_groups

mo_block_groups %>%
  filter(COUNTYFP=="095") -> jackson_co_block_groups

# Variables:
#   "STATEFP"
#   "COUNTYFP"
#   "TRACTCE"
#   "BLKGRPCE" 
#   "AFFGEOID" 
#   "GEOID"
#   "NAME"
#   "NAMELSAD"
#   "LSAD"
#   "ALAND"
#   "AWATER"
#   "geometry"

crs_block_groups <- st_crs(mo_block_groups)

file_name <- "tl_2020_29_tabblock20.shp"
path_name                              %>%
  paste(file_name, sep="/")            %>%
  st_read(stringsAsFactors=FALSE)      -> mo_census_blocks

mo_census_blocks %>%
  filter(COUNTYFP20=="095") -> jackson_co_census_blocks

# Variables:
#   "STATEFP20"
#   "COUNTYFP20"
#   "TRACTCE20"
#   "BLOCKCE20"
#   "GEOID20"
#   "NAME20"
#   "MTFCC20"
#   "UR20"
#   "UACE20"
#   "UATYPE20"
#   "FUNCSTAT20"
#   "ALAND20"
#   "AWATER20"
#   "INTPTLAT20"
#   "INTPTLON20"
#   "geometry" 

crs_blocks <- st_crs(mo_census_blocks)

file_name <- "cb_2017_us_zcta510_500k.shp"
path_name                              %>%
  paste(file_name, sep="/")            %>%
  st_read(stringsAsFactors=FALSE)      -> mo_zcta

mo_zcta %>%
  filter(str_detect(GEOID10, "^641")) -> z641 

# Variables:
#    "ZCTA5CE10"
#    "AFFGEOID10"
#    "GEOID10"
#    "ALAND10"
#    "AWATER10"
#    "geometry"

crs_zcta <- st_crs(mo_zcta)

file_name <- "Community_District.shp"
path_name                              %>%
  paste(file_name, sep="/")            %>%
  st_read(stringsAsFactors=FALSE)      -> community_districts

# Variables:
#    "State"
#    "STATE_FIPS"
#    "CD_ID"
#    "shid"
#    "CD_NAME"
#    "NAME"
#    "Shape_Area"
#    "Shape_Leng"
#    "geometry" 

crs_community_districts <- st_crs(community_districts)

file_name <- "Neighborhoods.shp"
path_name                              %>%
  paste(file_name, sep="/")            %>%
  st_read(stringsAsFactors=FALSE)      -> neighborhoods

# Variables:
#    "NID"
#    "AreaName"
#    "nbr_id"
#    "CommunityD"
#    "id"
#    "label_long"
#    "shid"
#    "Shape_Leng"
#    "Shape_Le_1"
#    "Shape_Area"
#    "geometry" 

crs_neighborhoods <- st_crs(neighborhoods)

map_data <- neighborhoods[i, ]
map_names <- neighborhood$AreaName
map_number <- length(map_names)
map_list <- 1:map_number
names(map_list) <- map_names

save(list=ls(), file="../../data/test-intersection.RData")

ui <- fluidPage(

  titlePanel("Test intersection, v1"),

  sidebarLayout(
    mainPanel(plotOutput("distPlot")), 
    sidebarPanel(
      radioButtons(
        "radio", 
         h3("Radio buttons"), 
         choices = list(
           "Neighborhoods" = 1, 
           "Community districts" = 2)),
      selectInput(
        "select",
        h3("Select box"),
        choices=map_list),
      helpText("Insert diagnostic message here.")
      )
    )
  )


server <- function(input, output) {
  output$distPlot <- 
  renderPlot(
    {

      i <- input$select
      
      if (input$radio==1) {
        map_data <- neighborhoods[i, ]
        map_names <- neighborhood$AreaName
        map_number <- length(map_names)
        map_list <- 1:map_number
        names(map_list) <- map_names
      }
      
      if (input$radio==2) {
        map_data <- community_districts[i, ]
        map_names <- community_districts$CD_NAME
        map_number <- length(map_names)
        map_list <- 1:map_number
        names(map_list) <- map_names
      }
      
      if (input$radio==1) lb <- paste(neighborhoods[i, "AreaName"], "Neighborhood")
      if (input$radio==2) lb <- paste(community_districts[i, "CD_NAME"], "Community district")

      # contained_blocks <- st_contains(map_data, jackson_co_census_blocks, sparse=FALSE)
      # intersect_blocks <- st_intersects(map_data, jackson_co_census_blocks, sparse=FALSE)
      # touching_blocks <- st_touches(map_data, jackson_co_census_blocks, sparse=FALSE)
      # partial_blocks <- intersect_blocks & (!contained_blocks) & (!touching_blocks)

      map_data %>%
        # bind_rows(jackson_co_census_blocks[contained_blocks, ]) %>%
          ggplot()                              +
          geom_sf(aes())                 +
          ggtitle(paste("Map of", lb))
      
    }
  )
}

shinyApp(ui = ui, server = server)
