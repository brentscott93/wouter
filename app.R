library(shiny)
library(tidyverse)
library(cowplot)
library(rhandsontable)
library(RColorBrewer)
library(ggpubr)
library(DT)
library(dbalance)

ui <- navbarPage("WOUTER",
       tabPanel("Home",
        column(3),
        column(6, 
         h1("WOUTER", style = "text-align: center; margin-top: 150px"),
         br(),
         h5("Wouter Online Utility for Training Endurance Running",style = "text-align: center")),
         ),
       
       tabPanel("About",
                column(3),
                column(6, 
                HTML("
                    <h2>About</h2>

                   <p> Predicting endurance running performance is challening as there is a complex interaction between many underlying
                   physiological processes that ultimately govern a given performance (VO2max, lactate threshold, anaerobic power, etc).
                   Unfortunately, obtaining accurate values for these pertinent physiological markers is not reasonable for the average runner
                   or coach as the tests require expensive equipment. Recently, the Critical Speed Model has been proposed as a reliable method
                   to predict distance running performance (Jones 2017). A main advantage of the Critical Speed Model is that the predictions are laboratory and
                   equipment free, thus reducing the cost barrier to the average runner. </p>
                   
                    <p>
                   The Critical Speed Model uses a runner's personal bests at given distances to estimate both their Critical Speed and D' (pronounced as 'd-prime'). 
                   The estimations of these values are obtained by optimizing the line of best fit through the runner's personal bests times versus the respective distances. 
                   Consequently, the equation for this line is of familiar form, y = mx+b. In the Critical Speed Model, the slope of the line (m) represents the athlete's
                   critical speed in units of meters/second and the y-intercept (b) is the D' value with units of meters. The D' value is a distance (in meters) that the
                   runner can cover at a given speed above their Critical Speed. Theoretically, 1 meter of D' is utilized for every second that the athlete runs
                   for 1m/s above their Critical Speed. Once D’ has been utilized, the maximum speed the runner can obtain is their Critical Speed,
                   at least until D’ Prime can become “reconstituted” or restored. </p>
                   
                    <p>
                   Moreover, the so-called D’ Balance Model has been shown to be highly correlated with performance in track distance races (Kirby 2021). The D' Balance Model
                   can use the information estimated by the Critical Speed Model (the Critical Speed and D' values) to simulate race outcomes. As such, the D’ Balance Model was 
                   suggested to become a useful tool for athletes and coaches to modify training and race preparations, but there is a high technological barrier to perform the
                   advanced modelling.
                    </p>
                    <p>
                   To adress these concerns, first we developed this online tool, WOUTER, which is a recursive acronym that stands for 'Wouter Online Utility for Training Endurance Running'. This is a web-based application that can
                   take user inputted information of a runners personal best times for known race distances and perform the mathmetical optimzation of the line of best to generate
                   the estimations for an athlete's Critical Speed and D'. Additionally, WOUTER can simulate race outcomes using the D' Balance Model with a custom field of runners with 
                   unique Critical Speed and D' values to predict race outcomes. A very practical workflow then becomes 1) Estimate runner's Critical Speed and D', and then 2) Simulate race
                   prediction outcomes amongst the runners using the D' Balance Model,  all within this online tool. 
                    </p>
                    <p>
                   However, wanting to go beyond online simulations and theory, we developed the D' Balance Model into a Garmin application which turns D' Balance into a real time training metric.
                   Now, any runner with a garmin watch can have access and incorporate the D' Balance Model into their training completely translating the theoretical model from 'bench-to-bedside' as they say in the
                   medical research field. See more in the 'Garmin App' tab.
                   </p>
                    <p>
                   Ultimately, WOUTER and the Garmin D' Balance app reduces the technological barrier for athlete's and coaches providing free and easily accesible tools to obtain high-performance modelling of running
                   performance. This results in the Critical Speed and D' Balance Model as being a less expensive, user-friendly, and realistic method for the average runner and coach
                   to obtain high quality performance predictions and real time training metrics independent of expensive laboratory based tests.
           </p>"
         )
        )
       ),
       
       
       tabPanel("Critical Speed",
        sidebarLayout(
         sidebarPanel(width = 4,
          h4("Critical Speed Calculator:"),
          h6("Fully editable table"),
          h6("Right click to add more rows"),
          h6("Multiple athletes accepted in long format"),
          checkboxInput("use_demo_cs", label = "Use demo data", value = FALSE),
          rHandsontableOutput("runner_pr_input"),
          br(),
          actionButton("cs_enter", "Calculate")
           ),
          mainPanel(width = 8, 
            tabsetPanel(
             tabPanel("Plot",
              plotOutput("cs_plot")
                     ),
             tabPanel("Model Summary",
               verbatimTextOutput("lm_model")
             ),
              tabPanel("Race Predictions",
                tableOutput("race_prediction_table")
               )
              )
             )
            )
           ),
          tabPanel("D' Balance",
            sidebarLayout(
             sidebarPanel(width = 5,
              h4("D' Balance Race Simulator:"),
              h6("Fill out BOTH runner & race data"),
              checkboxInput("use_demo_data", label = "Use demo data", value = FALSE),
              tabsetPanel(
                tabPanel("Runner Data",
                 h6("One athlete per row"),
                 h6("Right click to add more rows"),
                 rHandsontableOutput("d_balance_runner_data_input")),
                tabPanel("Race Data",
                 h6("Each row is a split"),
                 h6("Enter cumulative distance (meters) at each split"),
                 h6("Enter total elapsed time at split"),
                 h6("Right click to add more rows"),
                 rHandsontableOutput("d_balance_race_data_input"))
              ),
              br(),
              actionButton("calculate_d_balance", "Calculate")
            ),
            mainPanel(width = 7, 
             tabsetPanel(
              tabPanel("Plot",
               plotOutput("d_balance_plot")
                ),
              tabPanel("Data",
               DTOutput("d_balance_data")
                )
               )
              )
             )
            ),

       tabPanel("Garmin App",
                column(3),
                column(6,
                HTML("
                    <h2>Garmin app available in the Connect IQ Store</h2>
                   <p>After validating and testing the D' Balance Model we developed a Garmin application which provides
                       D' Balance updates to any Garmin watch owner. The Garmin app uses the same formulas as this
                       online tool to calculate the D' Balance values and is inputted with real time updates
                       of speed and distance from the Garmin device! We even performed a wear test to show that the Garmin app
                       produces similiar results to the modeling we performed in our peer-reviewed paper.</p>

                     <p>The Garmin app provides a data field that can be added to the built-in Garmin 'Run' activity
                        you most likely are already using on your watch. When enabled, the app provides live D' Balance updates
                        which includes total D' remaining (in meters), % D' remaining shown in an easy to view loading bar, and
                        a 'TTI' value. TTI, or 'Time To Implosion', is a fun take on the theoretical calculation of how much longer
                         you, in theory, can sustain your current pace until your D' is completed gone... Value is HH::MM::SS when running at a D' draining pace and shows --:--:-- when
                        below your critical speed.</p>

                    <h2>How to use the Garmin App</h2>
                     <ol>
                       <li>Download the <a href = 'https://apps.garmin.com/en-US/apps/35a022c2-2a65-4e36-b759-5677bfa6a496?tab=reviews&displayCurrentVersion=true'>
                                        D' Balance Data Field</a> from the Connect IQ Store</li>
                       <li>Add the D' Balance data field to your 'Run' activity on your watch (Run > Settings (hold up arrow) > Data Screens).
                            <a href='https://support.garmin.com/en-US/?faq=gIGxb776tt1ZL8bG0iO3W7'>
                             Consult the Garmin website for model specifics
                             </a></li>
                      <li>Estimate your CS and D' using WOUTER ('Critical Speed' tab)</li>
                      <li>Input your CS and D'values into your settings for the Garmin app in the Connect IQ store</li>
                      <li>Set a new PR</li>
                     </ol>
                    <p>
                    A note on D' Balance Model - currently, there is only support for level running. Results will be best/most useful
                    on a track or other flat surfaces.
           </p>"
         )
        )
       ),
       tabPanel("Disclaimers",
         column(3),
         column(6,
         HTML("<h6>This online tool is an open source project written in R developed by the <a href='https://sites.google.com/umass.edu/umill?pli=1'>
               University of Massachusetts Integrated Locomotion Lab</a>.
            <a href='https://github.com/brentscott93/WOUTER'>You can find the source code here.
            </a> Contributions welcome.</h6>
              <h6>Remember, 'all models are wrong, but some are useful'. There is no guaruntee that any predictions made by any
                  tools, inlcuding these one, will be 100% correct 100% of the time.</h6>
               <h6>Lastly, thanks to Dr. Wouter Hoogkamer's Energetics of Human Locomotion (KIN 697u) class at UMass Amherst
                  for spawning this fun project. Side note, Dr. H had no involvement in the naming of these tools 🙃")
                )
       ),
      )
           

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$runner_pr_input <- renderRHandsontable({
    if(input$use_demo_cs){
     pr_input <- 
      tribble(
        ~Athlete, ~"Distance (m)", ~Hours, ~Minutes, ~Seconds, 
        "Hassan", 800, 0, 1, 56,
        "Hassan", 1000, 0, 2, 34,
        "Hassan", 1500, 0, 3, 51,
        "Hassan", 1609, 0, 4, 12,
        "Hassan", 3000, 0, 8, 18,
        "Hassan", 5000, 0, 14,22,
        "Hassan", 10000, 0, 29, 06,
        
        "Obiri", 800, 0, 2, 0,
        "Obiri", 1500, 0, 3, 57,
        "Obiri", 1609, 0, 4, 16,
        "Obiri", 3000, 0, 8, 20,
        "Obiri", 3218, 0, 9, 14,
        "Obiri", 5000, 0 , 14, 18,
        "Obiri", 10000, 0, 29, 59)
    } else {
     pr_input <- 
      tibble("Athlete" = rep("Athlete 1", 4),
           "Distance (m)" = as.integer(c(1500, 3000, 5000, 10000)),
           "Hours" = rep(0, 4),
           "Minutes" = rep(0, 4),
           "Seconds" = rep(0, 4))
    }
    pr_input %>% 
      rhandsontable() %>% 
      hot_col(col = "Hours", type = "dropdown", source = 0:9) %>% 
      hot_col(col = "Minutes", type = "dropdown", source = 0:59) %>% 
      hot_col(col = "Seconds", type = "dropdown", source = 0:59)
  })
  
  output$runner_data_input <- renderRHandsontable({
     tibble("Athlete" = rep("Athlete 1", 4),
                "Distance (m)" = as.integer(c(1500, 3000, 5000, 10000)) ,
                "Hours" = rep(0, 4),
                "Minutes" = rep(0, 4),
                "Seconds" = rep(0, 4)) %>% 
     rhandsontable() %>% 
     hot_col(col = "Hours", type = "dropdown", source = 0:9) %>% 
     hot_col(col = "Minutes", type = "dropdown", source = 0:59) %>% 
     hot_col(col = "Seconds", type = "dropdown", source = 0:59)
  })
  
  
 output$race_data_input <- renderRHandsontable({
    tibble("Meter Marker" =  as.integer(c(200, 600, 1000)),
           "Lap Speed (m/s)" = rep(NA, 3)) %>% 
      rhandsontable() %>% 
      hot_col(col = "Lap Speed (m/s)", type = "numeric") 
    })
  
  
    cs <- reactiveValues()
    observeEvent(input$cs_enter, {

      pr_data <- 
        hot_to_r(input$runner_pr_input) %>% 
        rename(athlete = "Athlete",
               meters = "Distance (m)") %>% 
        mutate(seconds = ((Hours*3600) + (Minutes*60) + Seconds))
    
       if(anyNA(pr_data)){
          showNotification("Whoops. Missing values detected. All cells must contain a value.")
          req(!anyNA(pr_data))
       }
      
      if(any(pr_data == "")){
        showNotification("Whoops. Missing values detected. All cells must contain a value.")
        req(!any(pr_data == ""))
      }
      
      cs_data <-
        pr_data %>% 
        dplyr::select(athlete, meters, seconds) %>% 
        group_by(athlete) %>% 
        nest(pr_data = !athlete) %>% 
        mutate(lm_mod = map(pr_data, ~lm(meters ~ seconds, data = .x)),
               d_prime_meters = map_dbl(lm_mod, ~coef(.x)[[1]]),
               critical_speed_meters_second =  map_dbl(lm_mod, ~coef(.x)[[2]]),
               lm_predict = map(lm_mod, 
                                ~as.data.frame(
                                 predict(.x, 
                                         newdata = data.frame(seconds = 0:21600),
                                         interval = "conf")) %>% 
                                  mutate(x = 0:21600)))
        
      lm_predict <- 
        cs_data %>% 
        group_by(athlete) %>% 
        dplyr::select(athlete, lm_predict) %>% 
        unnest(cols = lm_predict)

      plot_colors <- RColorBrewer::brewer.pal(8, "Dark2")[1:nrow(cs_data)]
      cs_table <-
        cs_data %>% 
        dplyr::select("Athlete" = athlete, 
                      "CS (m/s)" = critical_speed_meters_second, 
                      "D' (m)" = d_prime_meters) %>% 
        mutate(across(where(is.numeric), ~round(., digits = 2))) %>% 
        ggpubr::ggtexttable(theme = ggpubr::ttheme(base_style = 'light', 
                                                   base_size = 20,
                                                   colnames.style = ggpubr::colnames_style(fill = "black", color = "white", size = 16),
                                                   tbody.style = ggpubr::tbody_style(fill = alpha(plot_colors, 0.4), size = 16)),
                            
                            rows = NULL) 
      
      cs$pr_data <- pr_data
      cs$cs_data <- cs_data
      cs$cs_table <- cs_table
      cs$lm_predict <- lm_predict
      cs$plot_colors <- plot_colors
         
 })
    
 output$cs_plot <- renderPlot({
  validate(need(cs$pr_data, "Enter PR data to begin. Press 'Go' to calculate."))
   main_plot <-
    ggplot()+
     geom_point(data = cs$pr_data, aes(seconds, meters, color = athlete), size = 2)+
     geom_ribbon(data = cs$lm_predict, aes(x = x, ymin = lwr, ymax = upr, fill  = athlete), alpha = 0.3)+
     geom_line(data = cs$lm_predict, aes(x = x, y = fit, color = athlete), linetype = "dashed")+
     coord_cartesian(xlim = c(0, max(cs$pr_data$seconds)+10),
                    ylim = c(0, max(cs$pr_data$meters)+10))+
     scale_color_manual(values = cs$plot_colors)+
     scale_fill_manual(values = cs$plot_colors)+
     xlab("Seconds")+
     ylab("Meters")+
     theme_minimal_grid(font_size = 16)+
     theme(
      legend.position = "none"
       )
    plot_grid(main_plot, cs$cs_table, rel_widths = c(0.7, 0.3))
 })
     
 output$lm_model <- renderPrint({
  mod_summary <- map(cs$cs_data$lm_mod, summary)
  names(mod_summary) <- cs$cs_data$athlete
  mod_summary
 })
     
output$race_prediction_table <- renderTable({
 validate(need(cs$cs_data, "Enter PR data to begin. Press 'Go' to calculate."))
 req(cs$cs_data)
 cs$cs_data %>% 
  dplyr::select(athlete, critical_speed_meters_second, d_prime_meters) %>% 
  group_by(athlete) %>% 
  mutate(predict_table = map2(critical_speed_meters_second ,
                              d_prime_meters,
                              ~tibble(Meters = c(800, 1000, 1500, 1609, 3000, 3200, 5000, 10000, 21097, 42195),
                                      Time =  as.character(lubridate::seconds_to_period(round((Meters - .y)/.x, 0)))
                                      )
                                     )
                                    ) %>% 
  dplyr::select("Athlete" = athlete, predict_table) %>% 
  unnest(cols = c(predict_table))
 })

#### D' Balance ####
 output$d_balance_runner_data_input <- renderRHandsontable({
   
   if(input$use_demo_data){
     runner_data_input <- 
       tribble(
               ~Athlete, ~"Critical Speed (m/s)", ~"D' Prime (m)",
               "Hassan", 5.63, 169,
               "Obiri", 5.46, 212,
               "Tsegay", 5.53, 196,
               "Tirop", 5.37, 256
     )
   } else {
     runner_data_input <- 
       tibble(Athlete = "Athlete 1",
             "Critical Speed (m/s)" = NA,
              "D' Prime (m)" = NA) 
   }
   
  runner_data_input %>% 
     rhandsontable() %>% 
     hot_col(col = "Critical Speed (m/s)", type = "numeric") %>% 
     hot_col(col =  "D' Prime (m)", type = "numeric")
     
 })

 output$d_balance_race_data_input <- renderRHandsontable({
   
   if(input$use_demo_data){
     race_data_input <- 
       tribble(
         ~"Cumulative Meters", ~"Hours", ~"Minutes", ~"Seconds",
         200,  "0", "0",   "38",
         1000, "0", "3",   "0",
         1400, "0", "4",   "12",
         1800, "0", "5",   "25",
         2000, "0", "6",   "0",
         2200, "0", "6",   "36",
         2600, "0", "7",   "46",
         3000, "0", "8" ,  "59",
         3400, "0", "10",  "11",
         3800, "0", "11",  "21",
         4000, "0", "11" , "57",
         4200, "0", "12" , "31",
         4600, "0", "13",  "39", 
         4800, "0", "14" , "9",
         5000, "0", "14" , "36"
       ) 
     
   } else {
    race_data_input <- 
     tibble("Cumulative Meters" = rep(NA, 13),
            "Hours" = rep(NA, 13),
            "Minutes" = rep(NA, 13),
            "Seconds" = rep(NA, 13)) 
   }
   
   race_data_input %>% 
    mutate("Cumulative Meters" = as.integer(`Cumulative Meters`)) %>% 
    rhandsontable() %>% 
    hot_col(col = "Cumulative Meters", type = "numeric") %>% 
    hot_col(col = "Hours", type = "dropdown", source = 0:9) %>% 
    hot_col(col = "Minutes", type = "dropdown", source = 0:59) %>% 
    hot_col(col = "Seconds", type = "dropdown", source = 0:59)
 })
 
 d_bal <- reactiveValues()
 observeEvent(input$calculate_d_balance, {
   if(is.null(input$d_balance_race_data_input)){
     showNotification("Need to input Race data, or switch to tab to activate", type = "error")
   } else if(anyNA(hot_to_r(input$d_balance_race_data_input))){
     showNotification("Need to input Race data, or switch to tab to activate", type = "error")
   }
   req(!is.null(input$d_balance_race_data_input))
   req(!anyNA(hot_to_r(input$d_balance_race_data_input)))
   
   if(is.null(input$d_balance_runner_data_input)){
     showNotification("Need to input Runner data, or switch to tab to activate", type = "error")
   } else if(anyNA(hot_to_r(input$d_balance_runner_data_input))){
     showNotification("Need to input Runner data, or switch to tab to activate", type = "error")
   }
   req(!is.null(input$d_balance_runner_data_input))
   req(!anyNA(hot_to_r(input$d_balance_runner_data_input)))
   
   runner_data <- 
     hot_to_r(input$d_balance_runner_data_input) %>% 
     rename("athlete" = Athlete,
            critical_speed_meters_second = "Critical Speed (m/s)",
            d_prime_meters = "D' Prime (m)")
   
   race_data <-
     hot_to_r(input$d_balance_race_data_input) %>% 
     mutate(
       seconds = ((as.numeric(Hours)*3600) + (as.numeric(Minutes)*60) + as.numeric(Seconds)),
       relative_split_time_seconds = seconds-lag(seconds, 1, default = 0),
       lap_distance_meters = `Cumulative Meters` - lag(`Cumulative Meters`, default = 0),
       lap_leader_speed_meters_second = lap_distance_meters/relative_split_time_seconds
     )
   
   
   d_balance_race <- dbalance::calc_d_balance(runner_data = runner_data,
                                              race_data = race_data)
   
   finish <- 
     d_balance_race %>%
     dplyr::filter(total_race_distance_meters == max(total_race_distance_meters)) %>%
     arrange(desc(d_balance)) %>% 
     mutate(finish_position = 1:nrow(.)) 
   
   finish_order <- finish %>% pull(athlete)
   
   d_balance_race$athlete <- factor(d_balance_race$athlete, levels = finish_order)
   
   d_bal$data <- d_balance_race
   d_bal$finish <- finish
 })
 
 output$d_balance_data <- renderDT({
   req(d_bal$data)
   d_bal$data %>% 
     mutate(across(c(meters_second, d_balance, total_race_distance_meters), ~round(., 2))) %>% 
     select("Athlete" = athlete,
            "Lap" = lap,
            "Lap Timer" = "lap_time",
            "Lap Speed (m/s)" = meters_second,
            "D' Balance" = d_balance,
            "Status" = balance_status,
            "Total Elapsed Time (s)" = total_race_time, 
            "Total Distance (m)" = total_race_distance_meters)
     
 })
 
 output$d_balance_plot <- renderPlot({
   req(d_bal$data)
   plot_colors <-  c("gold", "grey50", "darkorange", "black", RColorBrewer::brewer.pal(8, "Dark2"))
   number_finishers <- 1:nrow(d_bal$finish)
   d_bal_plot <- 
     plot_d_balance(d_bal$data)+
      scale_color_manual(values = plot_colors[number_finishers])+
      labs(title = "Race Simulation using D' Balance Model",
           subtitle = "Athlete with highest remaining D' Balance at finish is most likely winner*",
           caption = "*Simulations are not always 100% accurate...")+
     theme(
       legend.position = "none"
     )
   
   finish_table <- 
     d_bal$finish %>% 
     mutate(d_balance =  round(d_balance, 0)) %>% 
     dplyr::select(
       "Athlete" = athlete,
       "D' Finish (m)" = d_balance,
       "Position" = finish_position
     ) %>% 
     ggpubr::ggtexttable(theme = ggpubr::ttheme(base_style = 'light', 
                                                base_size = 14,
                                                colnames.style = ggpubr::colnames_style(fill = "black", color = "white", size = 12),
                                                tbody.style = ggpubr::tbody_style(fill = alpha(plot_colors[number_finishers], 0.4), size = 16)),
                         
                         rows = NULL) 
   
   plot_grid(d_bal_plot, finish_table, rel_widths = c(0.7, 0.3))
 })
}

# Run the application 
shinyApp(ui = ui, server = server)
