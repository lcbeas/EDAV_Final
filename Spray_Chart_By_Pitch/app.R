
library(GeomMLBStadiums)
library(shiny)
library(tidyverse)

load("pbp2019.rda")

pbp2019 <- transform(pbp2019,
                     plate_x = as.numeric(plate_x),
                     plate_z = as.numeric(plate_z),
                     release_speed = as.numeric(release_speed),
                     hc_x = as.numeric(hc_x),
                     hc_y = as.numeric(hc_y),
                     launch_speed = as.numeric(launch_speed)) # correcting column formats

ui <- fluidPage(

   titlePanel("Spray Chart by Pitch Type and Location"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput("p_type",
                    label = "Pitch Type: FF= Four-seam FB, SL=Slider, SI= Sinker, etc.",
                    choices = c(c("FF"),c("FS"),c("SL"),c("SI"),c("CH"),
                                c("FC"),c("CU"),c("FT"),c("KC")),
                    multiple=TRUE,
                    selected = c("FF")),

        sliderInput("x_axis",
                    label = "Horizontal pitch location (0 is middle of the plate, strike zone ~ [-1, 1]):",
                    min = -2 , max = 2, value = c(-2,2), step = 0.05),

        sliderInput("y_axis", label = "Vertical pitch location (strike zone ~ [2,4] :",
                    min = 0 , max = 5, value = c(0,5), step = 0.05),

        textInput("batter",
                  label = "Batter ID # (found on MLB.com)- ex. Mike Trout = 545361",
                  value = "545361")
      ),
   
     mainPanel(
       plotOutput("plot")
        )
   )
)


server <- function(input, output) {
   
   output$plot <- renderPlot({
     pbp2019 %>%
       filter(pitch_type %in% input$p_type,
              batter == as.numeric(as.character(input$batter)),
              plate_x > input$x_axis[1], plate_x < input$x_axis[2],
              plate_z > input$y_axis[1], plate_z <input$y_axis[2]) %>%
       mlbam_xy_transformation() %>%  
       ggplot(aes(x=hc_x_, y=hc_y_, color = bb_type )) + 
       geom_spraychart(stadium_ids = "generic",
                       stadium_transform_coords = TRUE, 
                       stadium_segments = "all") + 
       theme_void() + 
       coord_fixed()
     })
}

 
shinyApp(ui = ui, server = server)

