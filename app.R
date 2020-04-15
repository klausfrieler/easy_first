#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
source("analysis.R")
source("easy_util.R")


#printf <- function(...) print(sprintf(...))
#messagef <- function(...) message(sprintf(...))


source("ui.R")
version <- "0.1"


default_color <- "lightblue4"

get_fd_intro_text<-function(){t
  p1 <- p("For more information see",
          a("our online documentation", href="/commandline_tools/melfeature/melfeature_features.html", target="_blank"), "or ",
          a("the third chapter", href="http://www.mu-on.org/frieler/docs/frieler_computational_melody_analysis_2017.pdf", target="_blank"),
          "in our ",
          a("book.", href="http://schott-campus.com/jazzomat/", target="_blank"),
          style = "font-size: 11pt; display: block; border-style: solid; border-width: 0pt;"
  )
  #print(class(p1))
  paste(p1)
}
get_info_text<-function(){
  p1 <- p(
    "This is an interactive visualization of the 'easy first' hypothesis for jazz solo improvisation. It conjectures,
    that, as can be observed in language, solo phrases start with easy material first. We operationalize easiness here
    in two ways: On one hand using  n-gram (pattern) probability, and on the other hand using several simple features
    of the n-grams themselves.  
    The interval n-grams and the other information were extracted from the",
    a("Weimar Jazz Database", href="/dbformat/dboverview.html", target = "_blank"), 
    "with the help of the", tags$b("melpat"), "application from the", 
    a("MeloSpySuite", href="/download/download.html", target="_blank"), 
    "developed by the ", 
    a("Jazzomat Research Project.", href="http://jazzomat.hfm-weimar.de", target="_blank"),
    style = "font-size: 11pt; display: block"
  )  
  p2 <- p(
    tags$h4("Panel: Phrase positions"),
    p("Here plots of a selected easiness measures against phrase positions and separated by n-gram length are displayed, along with regression tables."),
    tags$b("Easiness measure:"), "The easiness measure to use.", tags$br(),
    tags$b("Smooting:"), ": Select a method of fitting a curve to the data. Polynomials of degress 1 to 4 as well as LOESS",tags$br(),
    tags$b("Minimal N:"), "Fix the minimal N for plotting, maximal N is 10.",tags$br(),
    tags$b("Min. Frequency:"), "Use  only n-grams that occur a least this often.",tags$br(),
    tags$b("Maximum Position:"), "Us this maximal value phrase position.",tags$br(),
    tags$b("Fix y-axis:"), "USe the same scale on the y-axis for all subplots (facets).",tags$br(),
    tags$b("Standardize:"), "Standardize/scale the easiness measure for separate Ns. This makes the value scales for different N's comparable and poolable.",tags$br(),
    tags$b("Use facets:"), "Make a separate subplot (facet) for each N.",tags$br(),
    tags$b("Use patterns:"),"Use only n-grams  of these types.",tags$br(),
    style = "font-size: 11pt; display: block; border-style: solid; border-width: 0pt;"
  )

  p3 <- p(
    tags$h4("Panel: Groups "),
    p("Here plots of a selected easiness measures against phrase positions are displayed grouped either with respect to style or performers."),
    tags$b("Easiness measure:"), "The easiness measure to use.", tags$br(),
    tags$b("Smooting:"), ": Select a method of fitting a curve to the data. Polynomials of degress 1 to 4 as well as LOESS.",tags$br(),
    tags$b("Minimal N:"), "Fix the minimal N for plotting, maximal N is 10.",tags$br(),
    tags$b("Min. Frequency:"), "Use  only n-grams that occur a least this often.",tags$br(),
    tags$b("Maximum Position:"), "Us this maximal value phrase position.",tags$br(),
    tags$b("Group by:"), "Group plots by this variable.",tags$br(),
    tags$b("Fix y-axis:"), "Use the same scale on the y-axis for all subplots.",tags$br(),
    tags$b("Use facets:"), "Make a separate plot (facet) for each N.",tags$br(),
    tags$b("Select styles:"),"Filter data for these styles.",tags$br(),
    tags$b("Invert  styles:"),"Inversion of selected styles.",tags$br(),
    tags$b("Select performer:"),"Filter data for these performers.",tags$br(),
    tags$b("Invert  performer:"),"Inversion of the selected performers.",tags$br(),
    style = "font-size: 11pt; display: block; border-style: solid; border-width: 0pt;"
  )

  p4 <- p(
    tags$h4("Panel: Correlations "),
    p("Here correlation plots and  tests for two selected easiness measures are displayed"),
    tags$b("Easiness measure (x):"), "The first easiness measure", tags$br(),
    tags$b("Easiness measure (y):"), "The second easiness measure", tags$br(),
    tags$b("Use patterns:"),"Use only ng-rams  these types.",tags$br(),
    style = "font-size: 11pt; display: block; border-style: solid; border-width: 0pt;"
  )
  
  return(paste(p1, p2, p3, p4))
}


apply_filters <- function(data, pattern_type){
  orig_rows <- nrow(data)
  #data <- apply_pattern_filter(data, pattern_type)
  messagef("Filtered %d rows by pattern type, now: %d", orig_rows - nrow(data), nrow(data))
  data
}

apply_filters_group <- function(data, style, performer){
  orig_rows <- nrow(data)
  data <- apply_style_filter(data, style)
  messagef("Filtered %d rows by style, now: %d", orig_rows - nrow(data), nrow(data))
  orig_rows <- nrow(data)
  data <- apply_performer_filter(data, performer)
  messagef("Filtered %d rows by performer, now: %d", orig_rows - nrow(data), nrow(data))
  data
}
apply_pattern_filter <- function(data, pattern_type){
  if(!is.na(pattern_type) && !is.null(pattern_type) && nchar(pattern_type) > 0  && pattern_type != "all"){
    data <- data %>% filter(sub_type %in% pattern_type)
  }
  data
}

apply_performer_filter <- function(data, performer_set){
  messagef("Filtering by %d performers", length(performer_set))
  if(!is.na(performer_set) && !is.null(performer_set)  && length(performer_set) > 0){
    data <- data %>% filter(performer %in% performer_set)
  }
  data
}

apply_style_filter <- function(data, style_set){
  messagef("Filtering by %d styles", length(style_set))
  
  if(!is.na(style_set) && !is.null(style_set) && length(style_set) > 0 ){
    data <- data %>% filter(style %in% style_set)
  }
  data
}


# Define server logic required to draw a plot
server <- function(input, output, session) {
   message("*** STARTING APP***")
   observe({
   })
   shiny::observeEvent(input$style, {
     message("Updating performer")
     #print(input$select_country_for_download)
     performers <- apply_style_filter(wjd_all, input$style) %>%  pull(performer) %>% unique()
     updateSelectizeInput(session,
                          "performer",
                          selected = performers)

     
   })
   shiny::observeEvent(input$invert_style, {
     message("Inverting styles")
     #print(input$select_country_for_download)
     inv_styles <- setdiff(all_styles, input$style)
     print(inv_styles)
     performers <- apply_style_filter(wjd_all, inv_styles) %>%  pull(performer) %>% unique()
     updateSelectizeInput(session,
                          "style",
                          selected = inv_styles)
     
     updateSelectizeInput(session,
                          "performer",
                          selected = "")
     
   })
   shiny::observeEvent(input$invert_performer, {
     message("Inverting performer")
     #print(input$select_country_for_download)
     inv_performer <- setdiff(all_performers, input$performer)
     print(inv_performer)
     styles <- apply_performer_filter(wjd_all, inv_performer) %>%  pull(style) %>% unique()
     updateSelectizeInput(session,
                          "style",
                          selected = "")
     
     updateSelectizeInput(session,
                          "performer",
                          selected = inv_performer)
     
   })

   shiny::observeEvent(input$var_stats, {
     #print(input$select_country_for_download)
     if(input$var_stats %in% easiness_measures){
       group_var <- "N"
       choices <- c("N" = "N")
     }
     else{
       group_var <- "style"
       choices <- c("Style" = "style")
     }
     updateSelectizeInput(session,
                          "group_var_stats",
                          choices = choices,
                          selected = group_var)
     

   })


   output$easiness_plot_phrase_pos <- renderPlot({
     data <- apply_filters(wjd_all, input$pattern_type)
     sp <- get_smoothing_params(input$smoothing)
     messagef("Smoothing: %s, poly degree %d, max pos %d", sp$method, as.integer(sp$degree), as.integer(input$max_pos))
     plot_easiness_by_phrase_pos(data = data, 
                                 min_N = as.integer(input$min_N),
                                 min_freq = as.integer(input$min_freq),
                                 max_pos  = as.integer(input$max_pos),
                                 fix_scale = input$fix_scale,
                                 standardize = input$standardize,
                                 facetting = input$facetting,
                                 easiness = input$easiness,
                                 easiness_label = get_easiness_label(input$easiness, input$standardize),
                                 smooth = sp$method,
                                 poly_degree = sp$degree) + get_default_theme(keep_legend = !input$facetting)

   })
   output$easiness_plot_group <- renderPlot({
     data <- apply_filters_group(wjd_all, input$style, input$performer)
     if(nrow(data) == 0){
       return()
     }
     sp <- get_smoothing_params(input$smoothing_group)
     messagef("plot group: Smoothing: %s, poly degree %d, max pos %d", sp$method, as.integer(sp$degree), as.integer(input$max_pos))
     plot_easiness_by_group(data = data, 
                            group_var = input$group_var,
                                 min_N = as.integer(input$min_N_group),
                                 min_freq = as.integer(input$min_freq_group),
                                 max_pos  = as.integer(input$max_pos_group),
                                 fix_scale = input$fix_scale_group,
                                 standardize = TRUE,
                                 facetting = input$facetting_group,
                                 easiness = input$easiness_group,
                                 easiness_label = get_easiness_label(input$easiness_group, TRUE),
                                 smooth = sp$method,
                                 poly_degree = sp$degree) + get_default_theme(keep_legend = !input$facetting_group)
     
   })
   output$easiness_cor_plot <- renderPlot({
     x <- input$easiness_x
     y <- input$easiness_y
     xlab <- get_easiness_label(x)
     ylab <- get_easiness_label(y)
     #data <- wjd_all %>% apply_pattern_filter(input$pattern_type_cor)
     data <- wjd_all
     plot_easiness_cor(data = data, 
                       x = x, xlab = xlab,
                       y = y, ylab = ylab,
                       sample_f = .01
                       ) + get_default_theme()  
   })
   output$hist_plot_stats <- renderPlot({
     var <- input$var_stats
     group_var <- input$group_var_stats
     plot_facet_histogram(var = var, 
                          group_var = group_var,
                          var_label = get_stats_var_label(var),
                          facetting = input$facetting_stats, 
                          fix_scale = input$fix_scale_stats)
   })

   output$single_phrase_plot <- renderPlot({
     phrase_N <- input$phrase_N
     phrase_max_pos <- input$phrase_max_pos
     phrase_id <- input$phrase_id
     single_phrase_plot2(phrase_id = phrase_id, 
                         phrase_N = as.integer(phrase_N), 
                         easiness = easiness_measures, 
                         max_pos = as.integer(phrase_max_pos))
   })
   
   output$info_phrase <- renderText({
     sprintf("<b>%s:</b> %s.", get_easiness_label(input$easiness, input$standardize), get_easiness_description(input$easiness))
   })
   output$info_group <- renderText({
     sprintf("<b>%s:</b> %s.", get_easiness_label(input$easiness_group, TRUE), get_easiness_description(input$easiness_group))
   })
   output$info_cor <- renderText({
     sprintf("<b>%s (x):</b> %s.<br/><b>%s (y):</b> %s.<br/>", get_easiness_label(input$easiness_x), get_easiness_description(input$easiness_x),  get_easiness_label(input$easiness_y), get_easiness_description(input$easiness_y))
   })
   output$help <- renderText({
     get_info_text()
   })
   
   output$fd_intro <- renderText({
    get_fd_intro_text()
   })
   output$easiness_table <- renderTable({
     easiness_descriptions %>% select("Measure" = label, "Explanation" = explanation)
   }, width = "100%"
   )
   output$regression_table_raw <- renderTable({
     pattern_type = input$pattern_type
     #data <- apply_pattern_filter(wjd_all, pattern_type)
     data <- wjd_all
     degree <- get_smoothing_params(input$smoothing)$degree     
     if(is.na(degree)) return()
     tab <- get_regressions(data = data,
                            min_N = as.integer(input$min_N),
                            max_pos = as.integer(input$max_pos),
                            standardize = input$standardize, 
                            aggregate = F,
                            easiness = input$easiness,
                            degree = degree)
     
     tab}, caption = "Regressions (raw)", caption.placement = "top")   
   
   output$regression_table_aggregated <- renderTable({
     pattern_type = input$pattern_type
     #data <- apply_pattern_filter(wjd_all, pattern_type)
     data <- wjd_all
     degree <- get_smoothing_params(input$smoothing)$degree     
     #if(is.na(degree)) degree = 1
     if(is.na(degree)) return()
     tab <- get_regressions(data = data,
                            min_N = as.integer(input$min_N),
                            max_pos = as.integer(input$max_pos),
                            standardize = input$standardize, 
                            easiness = input$easiness,
                            degree = degree)
     
     tab}, caption = "Regressions (aggregated)", caption.placement = "top")   

   output$regression_table_raw_group <- renderTable({
     data <- apply_filters_group(wjd_all, input$style, input$performer)
     if(nrow(data) == 0){
       return()
     }
     degree <- get_smoothing_params(input$smoothing)$degree     
     if(is.na(degree)) return()
     tab <- get_regressions_with_group(data = data,
                            min_N = as.integer(input$min_N),
                            max_pos = as.integer(input$max_pos),
                            aggregate = FALSE,
                            easiness = input$easiness,
                            group_var = input$group_var,
                            degree = degree)
     
     tab}, caption = "Regressions (raw)", caption.placement = "top")   
   
   output$regression_table_aggregated_group <- renderTable({
     data <- apply_filters_group(wjd_all, input$style, input$performer)
     if(nrow(data) == 0){
       return()
     }
     degree <- get_smoothing_params(input$smoothing)$degree     
     if(is.na(degree)) return()
     tab <- get_regressions_with_group(data = data,
                            min_N = as.integer(input$min_N),
                            max_pos = as.integer(input$max_pos),
                            easiness = input$easiness,
                            group_var = input$group_var,
                            degree = degree)
     
     tab}, caption = "Regressions (aggregated)", caption.placement = "top")   
   
   output$cor_table <- renderTable({
     x <- input$easiness_x
     y <- input$easiness_y
     #pattern_type = input$pattern_type
     
     min_N <- input$min_N
     tab <- map_dfr(min_N:10, 
                    function(k){ 
                      wjd_all %>% 
                        filter(N == k) %>% 
                        my_cor_test(x, y, method = input$cor_method, remove_outliers = input$cor_outlier) %>% 
                        mutate(N = k)})
     
     tab %>% select(N, 1:(length(.)-1))}, caption = "Correlations", caption.placement = "top")   
   
   output$phrase_info <- renderTable({
     wjd_all %>% 
       filter(g_phrase_id == input$phrase_id, 
              N == as.integer(input$phrase_N),
              phrase_pos <= as.integer(input$phrase_max_pos)) %>% 
       arrange(phrase_pos) %>% 
      select(`Position`= phrase_pos, `N-Gram`= value, easiness_measures) 
   }, width = "100%"
   )

   output$phrase_link <- renderText({
     ngrams <- 
       wjd_all %>% 
       filter(g_phrase_id == input$phrase_id, 
              N == as.integer(input$phrase_N),
              phrase_pos <= as.integer(input$phrase_max_pos) + as.integer(input$phrase_N) -1) %>% 
       arrange(phrase_pos) %>% 
       pull(value)
     ngram <- 
       map_int(1:length(ngrams), function(i) {
         pattern_to_int(ngrams[i])[1]
         }) %>% paste(collapse = ",")
     url <- sprintf("http://dig-that-lick.hfm-weimar.de/pattern_search/search?database_name=wjazzd&primary_pattern=%s&primary_transformation=interval&within_single_phrase=&generate_audio=True&generate_score=True", ngram)
     sprintf("<a href='%s' target = '_blank'>Search in the WJD</a>", url)
   })
   
   # Downloadable csv of selected dataset ----
   output$download_data <- downloadHandler(
     filename = "wjd_all.RDS",
     content = function(file){
       saveRDS(wjd_all, file)
       #write.table(wjd_all[1:10,], file, row.names = F, sep = ";")
     }
   )   
}
# Run the application 
shinyApp(ui = ui, server = server)

