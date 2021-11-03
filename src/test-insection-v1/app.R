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

file_name <- "Neighborhoods.shp"
path_name                              %>%
  paste(file_name, sep="/")            %>%
  st_read(stringsAsFactors=FALSE)      -> neighborhoods

union_neighborhoods <- st_union(neighborhoods)

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

ui <- fluidPage(

  titlePanel("Test maps, v5"),

  sidebarLayout(
    mainPanel(plotOutput("distPlot")), 
    sidebarPanel(
      radioButtons(
        "radio", 
         h3("Radio buttons"), 
         choices = list(
           "first jackson_co_tract with census blocks" = 1, 
           "first jackson_co_tract" = 2,
           "first jackson_co_tract with partial intersections" = 3,
         selected = 2))
      )
    )
  )

server <- function(input, output) {
  output$distPlot <- 
  renderPlot(
    {
      contained_blocks <- st_contains(jackson_co_tracts[1, ], mo_zcta, sparse=FALSE)
      intersect_blocks <- st_intersects(jackson_co_tracts[1, ], mo_zcta, sparse=FALSE)
      touching_blocks <- st_touches(jackson_co_tracts[1, ], mo_zcta, sparse=FALSE)
      partial_blocks <- intersect_blocks & (!contained_blocks) & (!touching_blocks)

      if (input$radio==1) map_data <- mo_zcta[contained_blocks, ]
      if (input$radio==2) map_data <- jackson_co_tracts[1 , ]
      if (input$radio==3) map_data <- mo_zcta[partial_blocks, ]
      
      if (input$radio==1) lb <- "first jackson_co_tract with zcta"
      if (input$radio==2) lb <- "firstjackson_co_tract"
      if (input$radio==3) lb <- "first jackson_co_tract with partial zcta"

      map_data %>%
        ggplot()                              +
        geom_sf(aes())                 +
        ggtitle(paste("Map of", lb))
      
    }
  )
}

shinyApp(ui = ui, server = server)
