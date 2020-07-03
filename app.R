library(shiny) 
library(bootstraplib) 
library(tidyverse) 
library(fingertipsR)
library(zoo) 
library(scales) 
library(shinyWidgets)
library(ggiraph)
library(reactable)
library(lubridate)

source("global.R")

# set Sass variables
bs_theme_new(version = "4+3", bootswatch = NULL)
bs_theme_add_variables(
  "primary" = "#969696",
  "secondary" = "#bdbdbd",
  "body-color" = "#212121",
  "input-border-color" = "#202528",
  "font-family-sans-serif" = "Open Sans, sans-serif"
)

# downloadButton() without icon
download_button <- function(outputId, label = "Download"){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, NULL, label)
}

ui <- bootstrapPage(
  bootstrap(),
  includeCSS("styles.css"),
  titlePanel(
    div(
      class = "headerContainer",
      a(
        img(
          src = "https://www.trafforddatalab.io/assets/logo/trafforddatalab_logo.svg",
          style = "position: relative; top: -3px; margin-left:10px; padding-top:10px; padding-right:10px;",
          height = 40
        ),
        href = "https://www.trafforddatalab.io",
        target = "_blank"
      )),
    windowTitle = "Local COVID-19"
  ),
  h1(class = "text-center mt-2", style = "display: flex; justify-content: center;", "Local COVID-19"),
  div(class = "container-fluid",
      fluidRow(
        selectInput(inputId = "ltla",
                           label = NULL,
                           choices = c("Select a local authority" = '', sort(unique(ltla$area_name)))))),
  br(),
  tabsetPanel(type = "pills",
              tabPanel("Summary",
                       fluidRow(
                         div(class = "col-sm-6",
                             br(),
                             htmlOutput("summary_text")))),
              tabPanel("New cases",
                       fluidRow(
                         div(class = "col-sm-8",
                             br(),
                             uiOutput("new_cases_ui"),
                             br(),
                             girafeOutput("new_cases_plot", width = "100%"),
                         ))),
              tabPanel("Total cases",
                       fluidRow(
                         div(class = "col-sm-8",
                             br(),
                             uiOutput("total_cases_ui"),
                             br(),
                             uiOutput("total_cases_table_title"),
                             br(),
                             reactableOutput("total_cases_table", height = "100%"),
                             br()
                         ))),
              tabPanel("Hospital deaths",
                       fluidRow(
                         div(class = "col-sm-8",
                             br(),
                             uiOutput("hospital_deaths_ui"),
                             br(),
                             girafeOutput("hospital_deaths_plot", width = "100%"),
                                    ))),
              tabPanel("Care home deaths",
                       fluidRow(
                         div(class = "col-sm-8",
                         br(),
                         uiOutput("care_home_deaths_ui"),
                         br(),
                         girafeOutput("care_home_deaths_plot", width = "100%"),
                       )))
              ))

shinyApp(ui, function(input,output){
  
  
  # -------------------------------------------
  # Summary
  # -------------------------------------------
  
  output$summary_text <- renderUI({ 
    req(input$ltla)
    latest_date_cases <- max(cases$date)
    cumulative_cases <- pull(filter(cases, area_name == input$ltla, date == max(date)), cum_cases)
    resident_population <- pull(filter(ltla, area_name == input$ltla),population)
    infection_rate <- cumulative_cases/resident_population*100000
    
    england_cumulative_cases <- pull(filter(phe, `Area name` == "England", `Specimen date` == max(`Specimen date`)),`Cumulative lab-confirmed cases`)
    england_resident_population <- sum(ltla$population, na.rm = TRUE)
    england_infection_rate <- england_cumulative_cases/england_resident_population*100000
    
    latest_date_deaths <- max(deaths$date)
    covid19_deaths <- filter(deaths, area_name == input$ltla, cause_of_death == "COVID-19") %>% 
      summarise(number_of_deaths = sum(number_of_deaths, na.rm = TRUE)) %>% pull()
    
    
    HTML(paste0("<br/><p>As of <strong>", format(latest_date_cases, '%A %d %B %Y'), "</strong>, the total number of confirmed cases of COVID-19 in <strong>", input$ltla, "</strong> was <strong>", 
         comma(cumulative_cases), "</strong>, a rate of <strong> ", round(infection_rate,1), "</strong> cases per 100,000 people. The infection rate in England is <strong>", round(england_infection_rate,1), "</strong> for every 100,000.</p>
         <p>There have been a total of <strong>", covid19_deaths, "</strong> coronavirus-related deaths registered in <strong>", input$ltla, "</strong> up to the week ending <strong>", format(latest_date_deaths, '%d %B %Y'), "</strong>. </p>")) 
  })
  
  # -------------------------------------------
  # New cases
  # -------------------------------------------
  
  new_cases_selection <- reactive(
    filter(cases, area_name == input$ltla) %>% 
      mutate(ma_cases = rollmean(new_cases, 7, align = "center", fill = NA),
             tooltip =  paste0("<strong>", new_cases, "</strong> cases<br/>", date))
  )
  
  new_cases_plot <- reactive(
    ggplot() +
      geom_hline(yintercept = 0, size = 0.3, colour = "#333333") +
      geom_col_interactive(data = new_cases_selection(), aes(x = date, y = new_cases, tooltip = tooltip), 
               fill = ifelse(new_cases_selection()$date >= max(new_cases_selection()$date)-4, "#bdbdbd", "#39809E"), alpha = 0.6) +
      geom_line_interactive(data = new_cases_selection(), aes(x = date, y = ma_cases, tooltip = "7 day rolling average", colour = "ma_cases"), size = 1) +
      scale_colour_manual(values = c("ma_cases" = "#39809E"), name = NULL, labels = "7-day rolling average") +
      scale_x_date(expand = c(0.005, 0.005), date_labels = "%d-%b") +
      scale_y_continuous(expand = c(0.005, 0.005), breaks = function(x) unique(
        floor(pretty(seq(0, (max(x) + 1) * 1.1)))), position = "right") +
      labs(x = NULL, y = NULL, 
           title = "Daily confirmed new cases",
           subtitle = paste0(input$ltla, ", as of ", format(max(new_cases_selection()$date), '%A %d %B %Y')), 
           caption = "Source: Public Health England") +
      theme_minimal() +
      theme(plot.margin = unit(rep(0.5, 4), "cm"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title.position = "plot",
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
            plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
            legend.position = "top")
  )
  
 output$new_cases_plot <- renderGirafe({
   req(input$ltla)
   gg <- new_cases_plot()
   girafe(ggobj = gg, width_svg = 7,
          options = list(
            opts_sizing(rescale = FALSE),
            opts_tooltip(use_fill = TRUE),
            opts_toolbar(saveaspng = FALSE)))
   })
 
 output$new_cases_ui <- renderUI({
   req(input$ltla)
   div(
     div(style = "position: absolute; right: 8.5em; top: 0em;",
         dropdown(includeMarkdown("data/metadata/new_cases.md"), icon = icon("info-circle"), size = "s", style = "jelly", width = "400px", right = TRUE, up = FALSE)),
     div(style = "position: absolute; right: 5em; top: 0em;",
         dropdown(download_button("download_new_cases_plot", label = "Download plot"), icon = icon("image"), size = "s", style = "jelly", width = "180px", right = FALSE, up = FALSE)),
     div(style = "position: absolute; right: 1.5em; top: 0em;",
         dropdown(download_button("download_new_cases_data", label = "Get the data"), icon = icon("table"), size = "s", style = "jelly", width = "180px", right = FALSE, up = FALSE),
         tags$style(HTML('.fa {color: #525252;}.bttn-jelly.bttn-default{color:#f0f0f0;}.bttn-jelly:hover:before{opacity:1};')))
     )
 })
  
 output$download_new_cases_plot <- downloadHandler(
    filename = function() {paste0("new_cases_", input$ltla, ".png")},
    content = function(file) {ggsave(file, plot = new_cases_plot(), width = 8, height = 6, dpi = 300, device = "png")}
  )
 
 output$download_new_cases_data <- downloadHandler(
   filename = function() {paste0("new_cases_", input$ltla, ".csv")},
   content = function(file) {write.csv(
     new_cases_selection() %>% 
       select(-c(ma_cases, tooltip)) %>% 
       arrange(desc(date)),
            file, row.names = FALSE)}
   )
 
 # -------------------------------------------
 # Total cases
 # -------------------------------------------
 
 cipfa <- reactive({
   req(input$ltla)
   tryCatch(c(nearest_neighbours(AreaCode = pull(filter(ltla, area_name == input$ltla), area_code), 
                                        AreaTypeID = 101, 
                                        measure = "CIPFA"), 
                     pull(filter(ltla, area_name == input$ltla), area_code)),
            error = function(cond) { return(NULL) },
            warning = function(cond) { return(NULL) },
            finally = NA)
 })
 
 total_cases_selection <- reactive(
   filter(cases, area_code %in% cipfa(), date >= max(date) - days(7)) %>% 
     group_by(area_name) %>% 
     summarise(cum_cases = max(cum_cases),
               cum_rate = max(cum_rate),
               recent_cases = sum(new_cases),
               recent_rate = round(recent_cases/population*100000,1)) %>% 
     distinct(area_name, .keep_all = TRUE)
 )
 
 output$total_cases_table <- renderReactable({
   req(input$ltla)
   
   validate(
     need(try(!is.null(cipfa())), "Please refer to the summary page for the total number of cases")
   )
   
   with_tooltip <- function(value, tooltip) {
     span(style = "text-decoration: underline; text-decoration-style: dotted;", title = tooltip, value)
   }
   
   sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                        borderRight = "1px solid #eee")
   
   reactable(class = "table",
             total_cases_selection(),
             height = 500,
             pagination = FALSE,
             wrap = FALSE,
             defaultColGroup = colGroup(headerClass = "group-header", align = "left"),
             defaultColDef = colDef(headerClass = "header", align = "left"),
             defaultSorted = "recent_rate",
             defaultSortOrder = "desc",
             rowClass = function(index) {if (total_cases_selection()[index, "area_name"] == input$ltla) {"value-highlight"}},
             columns = list(
               area_name = colDef(name = "Local Authority", 
                                  minWidth = 210,
                                  style = sticky_style,
                                  headerStyle = sticky_style),
               cum_cases = colDef(name = "Cases",
                                  format = colFormat(separators = TRUE),
                                  align = "left"),
               cum_rate = colDef(name = "Per 100,000",
                                 align = "left"),
               recent_cases = colDef(name = "Cases",
                                     format = colFormat(separators = TRUE),
                                     align = "left"),
               recent_rate = colDef(name = "Per 100,000",
                                    align = "left")
             ),
             columnGroups = list(
               colGroup(name = "Total cases", columns = c("cum_cases", "cum_rate")),
               colGroup(name = "Last 7 days", columns = c("recent_cases", "recent_rate"))
             )
      )
 })
 
 output$total_cases_ui <- renderUI({
   req(input$ltla, !is.null(cipfa()))
   
   div(
     div(style = "position: absolute; right: 8.5em; top: 0em;",
         dropdown(includeMarkdown("data/metadata/total_cases.md"), icon = icon("info-circle"), size = "s", style = "jelly", width = "400px", right = TRUE, up = FALSE)),
     div(style = "position: absolute; right: 5em; top: 0em;",
         dropdown(download_button("download_total_cases_data", label = "Get the data"), icon = icon("table"), size = "s", style = "jelly", width = "180px", right = FALSE, up = FALSE),
         tags$style(HTML('.fa {color: #525252;}.bttn-jelly.bttn-default{color:#f0f0f0;}.bttn-jelly:hover:before{opacity:1};')))
   )
   
 })
 
 output$total_cases_table_title <- renderUI({
   req(input$ltla, !is.null(cipfa()))
   
  div(style = "text-align: center; margin: 8px 0; font-size: 16px;",
       div(style = "font-size: 16px; font-weight: 600;", 
           paste("Cumulative confirmed cases,", format(max(cases$date), '%d %B %Y'))),
       paste(input$ltla , "compared with similar local authorities")
   )
  
 })
 
 output$download_total_cases_data <- downloadHandler(
   filename = function() {paste0("total_cases_", input$ltla, ".csv")},
   content = function(file) {write.csv(
     total_cases_selection() %>% 
       rename(`Local authority` = area_name,
              `Total cases` = cum_cases,
              `Rate` = cum_rate,
              `Total cases - last 7 days` = recent_cases,
              `Rate - last 7 days` = recent_rate),
     file, row.names = FALSE)}
 )
 
 # -------------------------------------------
 # Hospital deaths
 # -------------------------------------------
 
 hospital_deaths_selection <- reactive(
   filter(deaths, area_name == input$ltla, place_of_death == "Hospital") %>% 
     mutate(tooltip =  paste0("<strong>", number_of_deaths, "</strong> deaths<br/>", "<em>", cause_of_death, "</em><br/>", date))
 )

 hospital_deaths_plot <- reactive(
   ggplot(hospital_deaths_selection(), aes(x = factor(date), y = number_of_deaths)) +
     geom_col_interactive(aes(fill = fct_rev(cause_of_death), tooltip = tooltip)) +
     geom_hline(yintercept = 0, size = 0.3, colour = "#333333") +
     scale_fill_manual(values = c("COVID-19" = "#8D2313", "Other causes" = "#d4ada7"), guide = guide_legend(reverse = TRUE)) +
     scale_y_continuous(expand = c(0.005, 0.005), position = "right") +
     labs(x = NULL, y = NULL, 
          title = "Weekly registered deaths in hospital",
          subtitle = paste0(input$ltla, ", to week ending ", format(max(hospital_deaths_selection()$date), '%d %B %Y')), 
          caption = "Source: Office for National Statistics",
          fill = NULL) +
     theme_minimal() +
     theme(plot.margin = unit(rep(0.5, 4), "cm"),
           panel.spacing = unit(1, "lines"),
           panel.grid.major.x = element_blank(),
           panel.grid.minor = element_blank(),
           plot.title.position = "plot",
           plot.title = element_text(size = 14, face = "bold"),
           plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
           plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
           axis.text.x = element_text(angle = 90),
           legend.position = "top")
 )
 
 output$hospital_deaths_plot <- renderGirafe({
   req(input$ltla)
   gg <- hospital_deaths_plot()
   girafe(ggobj = gg, width_svg = 7,
          options = list(
            opts_sizing(rescale = FALSE),
            opts_tooltip(use_fill = TRUE),
            opts_toolbar(saveaspng = FALSE)))
 })
 
 output$hospital_deaths_ui <- renderUI({
   req(input$ltla)
   div(
     div(style = "position: absolute; right: 8.5em; top: 0em;",
         dropdown(includeMarkdown("data/metadata/hospital_deaths.md"), icon = icon("info-circle"), size = "s", style = "jelly", width = "400px", right = TRUE, up = FALSE)),
     div(style = "position: absolute; right: 5em; top: 0em;",
         dropdown(download_button("download_hospital_deaths_plot", label = "Download plot"), icon = icon("image"), size = "s", style = "jelly", width = "180px", right = FALSE, up = FALSE)),
     div(style = "position: absolute; right: 1.5em; top: 0em;",
         dropdown(download_button("download_hospital_deaths_data", label = "Get the data"), icon = icon("table"), size = "s", style = "jelly", width = "180px", right = FALSE, up = FALSE),
         tags$style(HTML('.fa {color: #525252;}.bttn-jelly.bttn-default{color:#f0f0f0;}.bttn-jelly:hover:before{opacity:1};')))
     )
 })
 
 output$download_hospital_deaths_plot <- downloadHandler(
   filename = function() {paste0("hospital_deaths_", input$ltla, ".png")},
   content = function(file) {ggsave(file, plot = hospital_deaths_plot(), width = 8, height = 6, dpi = 300, device = "png")}
 )
 
 output$download_hospital_deaths_data <- downloadHandler(
   filename = function() {paste0("hospital_deaths_", input$ltla, ".csv")},
   content = function(file) {write.csv(
     hospital_deaths_selection() %>% select(-tooltip),
     file, row.names = FALSE)}
 )
 
 # -------------------------------------------
 # Care homes deaths
 # -------------------------------------------
 
 care_home_deaths_selection <- reactive(
   filter(deaths, area_name == input$ltla, place_of_death == "Care home") %>% 
     mutate(tooltip =  paste0("<strong>", number_of_deaths, "</strong> deaths<br/>", "<em>", cause_of_death, "</em><br/>", date))
 )
 
 care_home_deaths_plot <- reactive(
   ggplot(care_home_deaths_selection(), aes(x = factor(date), y = number_of_deaths)) +
     geom_col_interactive(aes(fill = fct_rev(cause_of_death), tooltip = tooltip)) +
     geom_hline(yintercept = 0, size = 0.3, colour = "#333333") +
     scale_fill_manual(values = c("COVID-19" = "#8D2313", "Other causes" = "#d4ada7"), guide = guide_legend(reverse = TRUE)) +
     scale_y_continuous(expand = c(0.005, 0.005), position = "right") +
     labs(x = NULL, y = NULL, 
          title = "Weekly registered deaths in care homes",
          subtitle = paste0(input$ltla, ", to week ending ", format(max(care_home_deaths_selection()$date), '%d %B %Y')), 
          caption = "Source: Office for National Statistics",
          fill = NULL) +
     theme_minimal() +
     theme(plot.margin = unit(rep(0.5, 4), "cm"),
           panel.spacing = unit(1, "lines"),
           panel.grid.major.x = element_blank(),
           panel.grid.minor = element_blank(),
           plot.title.position = "plot",
           plot.title = element_text(size = 14, face = "bold"),
           plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
           plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
           axis.text.x = element_text(angle = 90),
           legend.position = "top")
 )
 
 output$care_home_deaths_plot <- renderGirafe({
   req(input$ltla)
   gg <- care_home_deaths_plot()
   girafe(ggobj = gg, width_svg = 7,  
          options = list(
            opts_sizing(rescale = FALSE),
            opts_tooltip(use_fill = TRUE),
            opts_toolbar(saveaspng = FALSE)))
 })
 
 output$care_home_deaths_ui <- renderUI({
   req(input$ltla)
   div(
     div(style = "position: absolute; right: 8.5em; top: 0em;",
         dropdown(includeMarkdown("data/metadata/care_home_deaths.md"), icon = icon("info-circle"), size = "s", style = "jelly", width = "400px", right = TRUE, up = FALSE)),
     div(style = "position: absolute; right: 5em; top: 0em;",
         dropdown(download_button("download_care_home_deaths_plot", label = "Download plot"), icon = icon("image"), size = "s", style = "jelly", width = "180px", right = FALSE, up = FALSE)),
     div(style = "position: absolute; right: 1.5em; top: 0em;",
         dropdown(download_button("download_care_home_deaths_data", label = "Get the data"), icon = icon("table"), size = "s", style = "jelly", width = "180px", right = FALSE, up = FALSE),
         tags$style(HTML('.fa {color: #525252;}.bttn-jelly.bttn-default{color:#f0f0f0;}.bttn-jelly:hover:before{opacity:1};')))
     )
 })
 
 output$download_care_home_deaths_plot <- downloadHandler(
   filename = function() {paste0("care_home_deaths_", input$ltla, ".png")},
   content = function(file) {ggsave(file, plot = care_home_deaths_plot(), width = 8, height = 6, dpi = 300, device = "png")}
 )
 
 output$download_care_home_deaths_data <- downloadHandler(
   filename = function() {paste0("care_home_deaths_", input$ltla, ".csv")},
   content = function(file) {write.csv(
     care_home_deaths_selection() %>% select(-tooltip),
     file, row.names = FALSE)}
 )
 
})
