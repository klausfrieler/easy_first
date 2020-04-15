messagef("Loading workspace ...")
setup_workspace()
messagef("...done.")
messagef("Read %d lines of data.", nrow(wjd_all))


smoothing_choices <- c("Linear" = 1, "Polynomial (2nd)" = 2, "Polynomial (3rd)" = 3, "Polynomial (4th)" = 4, "Polynomial (5th)" = 5, "LOESS" = "loess")
get_smoothing_params <- function(selected){
  if(selected == "loess"){
    method <- "loess"
    degree <- NA
  }
  else{
    method <- "poly"
    degree <- as.integer(selected)
  }
  return(list(method = method, degree = degree))
}
sort_phrase_ids <- function(ids){
  ids <- strsplit(unique(ids), "_")
  #browser()
  tmp <-
    map_dfr(ids, function(x){
      tibble(melid =  as.integer(x[[1]]), 
             phrase = as.integer(x[[2]]))
    }) %>% arrange(melid, phrase)
  tmp %>% mutate(phrase_id = sprintf("%d_%d", melid, phrase)) %>% pull(phrase_id)
}

load_phrase_selection <- function(min_len = 15){
  phrase_select <- readRDS("data/phrase_selection.RDS") %>% filter(phrase_len >= min_len)
  phrase_ids <- phrase_select %>% pull(phrase_id)
  names(phrase_ids) <- sprintf("%s (%s)", phrase_select$phrase_id, gsub("_", " ", phrase_select$type, fixed = T))
  phrase_ids
}

get_phrase_selection <- function(min_len = 15, count = 100, seed = 666){
  set.seed(seed)
  all_phrases <- wjd_all %>% filter(phrase_len >= min_len) %>% pull(g_phrase_id)
  phrase_selection <- sample(x = all_phrases, count)
  wjd_all %>% 
    filter(g_phrase_id %in% phrase_selection) %>% 
    pull(g_phrase_id) %>% 
    sort_phrase_ids()
}
#print(nrow(wjd_all))
phrase_plot_sidebar <- 
  sidebarPanel(
    selectInput("easiness", "Easiness measure:",
                choices = easiness_measures,
                selected = "surprise"),
    selectInput("smoothing", "Smoothing:",
                choices = smoothing_choices,
                selected = 3
    ),
    selectInput("min_N", "Minimal N:",
                choices = 3:10,
                selected = 3
    ),
    selectInput("min_freq", "Min. Frequency:",
                choices = c(1, 2, 5, 10, 20),
                selected = 1
    ),
    selectInput("max_pos", "Maximum position:",
                choices = 10:50,
                selected = 30
    ),
    checkboxInput("fix_scale", "Fix y-axis", FALSE),
    checkboxInput("standardize", "Standardize", TRUE),
    checkboxInput("facetting", "Use facets", TRUE),
    selectizeInput("pattern_type", "Use pattern types:",
                   choices = all_pattern_types,
                   selected = all_pattern_types,
                   multiple = TRUE,
                   options = list(placeholder = "Enter pattern types here")
    ),
    width = 3
  )

phrase_plot_main_panel <- 
  mainPanel(
    tabsetPanel(type ="tabs",
                tabPanel("Plot",
                         plotOutput("easiness_plot_phrase_pos", width = 800, height = 600),
                         p(htmlOutput("info_phrase")),
                         tableOutput("regression_table_aggregated"),
                         tableOutput("regression_table_raw")
                ),
                tabPanel("Easiness Descriptions", 
                         #p(htmlOutput("fd_intro"))
                         tableOutput("easiness_table")
                )
    )
  )

phrase_plot_panel <-   tabPanel(
  "Phrase position plots",
  sidebarLayout(	
    sidebarPanel = phrase_plot_sidebar,
    mainPanel = phrase_plot_main_panel
  )
)

group_plot_sidebar <- 
  sidebarPanel(
    selectInput("easiness_group", "Easiness measure:",
                choices = easiness_measures,
                selected = "surprise"),
    selectInput("smoothing_group", "Smoothing:",
                choices = smoothing_choices,
                selected = 3
    ),
    selectInput("min_N_group", "Minimal N:",
                choices = 3:10,
                selected = 3
    ),
    selectInput("min_freq_group", "Min. Frequency:",
                choices = c(1, 2, 5, 10, 20),
                selected = 1
    ),
    selectInput("max_pos_group", "Maximum position:",
                choices = 10:50,
                selected = 50
    ),
    selectInput("group_var", "Group by:",
                choices = c("Style" = "style", "Performer" = "performer"),
                selected = "style"
    ),
    checkboxInput("fix_scale_group", "Fix y-axis", FALSE),
    checkboxInput("facetting_group", "Use facets", TRUE),
    selectizeInput("style", "Select styles:",
                   choices = all_styles,
                   selected = all_styles,
                   multiple = TRUE,
                   options = list(placeholder = "Enter styles here:")
    ),
    actionButton("invert_style", "Invert styles", style="margin-bottom: 10px;"),
    selectizeInput("performer", "Select performer:",
                   choices = all_performers,
                   selected = all_performers,
                   multiple = TRUE,
                   options = list(placeholder = "Enter performer here:")
    ),
    actionButton("invert_performer", "Invert performer", style="margin-bottom: 10px;"),
    
    width = 3
  )

group_plot_main_panel <- 
  mainPanel(
    plotOutput("easiness_plot_group", width = 800, height = 600),
    p(htmlOutput("info_group")),
    tableOutput("regression_table_aggregated_group"),
    tableOutput("regression_table_raw_group")
  )

group_plot_panel <-   tabPanel(
  "Group plots",
  sidebarLayout(	
    sidebarPanel = group_plot_sidebar,
    mainPanel = group_plot_main_panel
  )
)
cor_choices <- c("Pearson" = "pearson",  "Spearman's rho" = "spearman")
cor_plot_sidebar <- 
  sidebarPanel(
    selectInput("easiness_x", "Easiness measure (x):",
                choices = easiness_measures,
                selected = "surprise"),
    selectInput("easiness_y", "Easiness measure (y):",
                choices = easiness_measures,
                selected = "int_variety"),
    selectInput("cor_method", "Correlation method:",
                choices = cor_choices,
                selected = "pearson"),
    checkboxInput("cor_outlier", "Remove Outlier", FALSE),
    #selectizeInput("pattern_type_cor", "Use pattern types:",
    #               choices = all_pattern_types,
    #               selected = all_pattern_types,
    #               multiple = TRUE,
    #               options = list(placeholder = "Enter pattern types here")),
    
    width = 3
  )

cor_plot_main_panel <- 
  mainPanel(
    plotOutput("easiness_cor_plot", width = 800, height = 600),
    p(htmlOutput("info_cor")),
    p(tableOutput("cor_table"))
  )


cor_plot_panel <-   tabPanel(
  "Correlations",
  sidebarLayout(	
    sidebarPanel = cor_plot_sidebar,
    mainPanel = cor_plot_main_panel
  )
)
stats_plot_sidebar <- 
  sidebarPanel(
    selectInput("var_stats", "Variable:",
                choices = stats_vars,
                selected = "surprise"),
    selectInput("group_var_stats", "Group by:",
                choices = c("N" = "N", "Style" = "style"),
                selected = "N"
    ),
    checkboxInput("fix_scale_stats", "Fix axis", FALSE),
    checkboxInput("facetting_stats", "Use facets", TRUE),
    
    width = 3
  )

stats_plot_main_panel <- 
  mainPanel(
    plotOutput("hist_plot_stats", width = 800, height = 600)
    #p(htmlOutput("info_stats"))
  )

stats_plot_panel <-   tabPanel(
  "Histograms",
  sidebarLayout(	
    sidebarPanel = stats_plot_sidebar,
    mainPanel = stats_plot_main_panel
  )
)

all_phrases <- load_phrase_selection(min_len = 30)

singles_plot_sidebar <- 
  sidebarPanel(
    selectizeInput("phrase_id", "Phrase ID",
                choices = all_phrases,
                selected = "1_1"
    ),
    selectInput("phrase_N", "N:",
                choices = 4:6,
                selected = 5
    ),
    selectInput("phrase_max_pos", "Maximum position:",
                choices = 15:30,
                selected = 15
    ),
    
    width = 3
  )

singles_plot_main_panel <- 
  mainPanel(
    plotOutput("single_phrase_plot", width = 800, height = 600),
    p(htmlOutput("phrase_link")),
    p(tableOutput("phrase_info"))
  )
singles_plot_panel <- tabPanel(
  "Single Phrases",
  sidebarLayout(	
    sidebarPanel = singles_plot_sidebar,
    mainPanel = singles_plot_main_panel
  )
)
ui <- navbarPage(
  
  # Application title
  title = "Easy First",
  theme = shinytheme("spacelab"),
  id = "tabs",
  tabPanel(
    "Phrase positions",
    phrase_plot_panel
  ),
  tabPanel(
    "Groups",
    group_plot_panel
  ),
  tabPanel(
    "Correlations",
    cor_plot_panel
  ),
  tabPanel(
    "Histograms",
    stats_plot_panel
  ),
  tabPanel(
    "Single Phrases",
    singles_plot_panel
  ),
  tabPanel(
    "Info",
    fluidRow(
      column(2),
      column(8, 
             p(htmlOutput("help"))),
      column(2)
    )
  )
  
  
)#navbar

