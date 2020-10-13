library(tidyverse)
library(ggrepel)
library(osfr)

messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

easiness_descriptions <- tribble(
  ~label, ~name, ~explanation,
  #"N-gram Surprise", "surprise", "Surprise of an n-gram is the negative logarithm of its occurrence probability. The less frequent the n-gram, the higher the surprise.",
  "Extrinsic Difficulty", "surprise", "Defined as the surprise of an n-gram, which is the negative logarithm of its occurrence probability. The less frequent the n-gram, the higher the surprise.",
  "Extrinsic Difficulty (P)", "surprise_p", "Defined as the surprise of an n-gram, which is the negative logarithm of its occurrence probability with a specific performer",
  #  "N-gram Surprise (S)", "surprise_s", "Surprise of an n-gram is the negative logarithm of its occurrence probability within a specific solo",
  "Intrinsic Difficulty", "combined_easiness", "(N-scale) sum of N-scaled interval and pitch variety, direction changes and zig-zaggity", 
  "Mean Interval Size", "mean_int_size", "Mean of the absolute values of the intervals in an n-gram", 
  "Max. Interval Size" , "int_range", "Maximal absolute interval size in an n-gram",
  "Interval Variety", "int_variety", "Number of different intervals in the n-gram divided by n-gram length. If all intervals are different then this value is 1, if  all intervals are equal then this value is 1/N, where N is the n-gram length",
  "Pitch Variety", "pitch_variety", "Number of different pitches in the n-gram divided by n-gram length. If all pitches are different then this value is 1, if all pitches are equal then this value is 1/N, where N is the n-gram length",
  "Direction Changes", "dir_change", "Number of direction changes in the n-gram",
  "Zig-zaggity" , "mean_run_length", "The mean lenght of stretches of intervals in the same direction (ascending, descending, repeating).")


easiness_measures <- easiness_descriptions %>% pull(name)
names(easiness_measures) <- easiness_descriptions %>% pull(label)
stats_vars <- c(easiness_measures, "Number of notes" = "number_notes", "Phrase lengths" = "phrase_len")

get_stats_var_label <- function(var){
  names(stats_vars)[which(stats_vars == var)]
}

get_easiness_label <- function(easiness_var, standardize = FALSE){
  if(substr(easiness_var, 1, 2) == "z_"){
    easiness_var <- substr(easiness_var, 3, nchar(easiness_var))
    standardize <- F
  }
  label <- names(easiness_measures)[which(easiness_measures == easiness_var)]
  if(length(label) == 0){
    return(easiness_var)
  }
  if(standardize){
    label <- sprintf("%s (standardized)", label)
  }
  label
}
get_easiness_description <- function(easiness_var){
  easiness_descriptions %>% filter(name == easiness_var) %>% pull(explanation)
}

int_class <- Vectorize(
  function(x){
  class_vec <- list("0" = 0, "1" = 1, "2" = 1, "3" = 2, "4" = 2, "5" = 3, "6" = 3, "7" = 3)
  s <- sign(x)
  a <- abs(x)
  if(a > 7){
    return(s * 4)
  }
  return(s * class_vec[[as.character(a)]])

})
n_gram_easiness <- function(pattern){
  if(length(pattern) > 1){
    return(map_dfr(pattern, n_gram_easiness))
  }
  #browser()
  v <- pattern_to_int(pattern)
  mean_abs_int <- mean(abs(v))
  #int_path <- sum(abs(v))
  int_range <- max(abs(v))
  l <- length(v)
  r <- rle(sign(v))
  #browser()
  dir_change <- length(r$values) - 1
  mean_dir_change <- (length(r$values) - 1)/(l-1)
  mean_run_length <- 1 - mean(r$lengths)/l
  int_variety <- n_distinct(v)/l
  pitch_variety <- n_distinct(c(0, cumsum(v)))/(l+1)
  tibble(value = pattern,
         mean_int_size = mean_abs_int, 
         #int_path = int_path, 
         int_range = int_range, 
         dir_change = dir_change, 
         mean_dir_change = mean_dir_change, 
         int_variety = int_variety,
         pitch_variety = pitch_variety,
         mean_run_length = mean_run_length)
}
pattern_to_int <- function(x){
  if(length(x) > 1){
    return(lapply(x, pattern_to_int))
  }
  strsplit(gsub("\\]", "", gsub("\\[", "", x)), ",") %>% unlist() %>% as.integer()
}

freq_table <- function(x, prop_var) {
  prop_var  <- enquo(prop_var)
  tmp <- x %>% 
    group_by( !!prop_var) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n /sum(n)) %>% 
    ungroup
  tmp
  #tmp %>% ggplot(aes_string(x = rlang::quo_text(prop_var), y= "freq")) + geom_col()
}

freq2_table <- function(x, group_var, prop_var) {
  group_var <- enquo(group_var)
  prop_var  <- enquo(prop_var)
  tmp <- x %>% 
    group_by(!!group_var, !!prop_var) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n /sum(n)) %>% 
    ungroup
  tmp
  #tmp %>% ggplot(aes_string(x = rlang::quo_text(prop_var), y= "freq")) + geom_col()
}
classify_int_pattern <- function(pattern){
  if(length(pattern) > 1){
    return(bind_rows(map(pattern, classify_int_pattern)))
  }
  #printf("Classifying pattern %s", pattern)
  x <- pattern_to_int(pattern)
  main_type <- "cool"
  main_type <- "other"
  #get rid of repetitions
  x_sz <- x[x != 0]
  #1. only repetition: uncool
  #print(x_sz)
  if(length(x_sz) <= 1){
    return(tibble(main_type =  "uncool", sub_type = "repetition"))
  }
  if(length(x_sz) < 4){
    return(tibble(main_type =  "cool", sub_type = "other"))
  }
  #get cumsum
  cs <- c(0, cumsum( x_sz))
  
  #print(cs)
  #get period of potential trill
  zero_pos <- which(cs == 0)
  #print(zero_pos)
  if(length(zero_pos) > 2){
    if(length(cs) %in% zero_pos || length(zero_pos) >= 3){
      
      periods <- unique(diff(zero_pos))
      #print(periods)
      if(length(periods) <= 1){
        if(length(cs)/length(zero_pos) <= periods[1]){
          #printf("l(x_sz) = %d, l(zp) = %d, r = %f, periods = %d", length(cs), length(zero_pos), length(cs)/length(zero_pos), periods[1])
          #print(cs[zero_pos[1]:(zero_pos[2]-1)])
          #print(cs[zero_pos[2]:(zero_pos[3]-1)])
          if(identical(cs[zero_pos[1]:(zero_pos[2]-1)], cs[zero_pos[2]:(zero_pos[3]-1)])){
            return(tibble(main_type =  "uncool", sub_type = "trill"))
          }
          
        }
      }
    }
  }
  #check for scales
  steps <- union(unique(abs(x)), c(1,2))
  #print(steps)
  is_scale <- length(steps) == 2
  if (is_scale){
    return(tibble(main_type =  "uncool", sub_type = "scale"))
    
  }
  tibble(main_type =  "cool", sub_type = "other")
}

add_pattern_classes <- function(data){
  vals <- unique(data$value)
  data$main_type <- NULL
  data$sub_type <- NULL
  pattern_types <- cbind(value = vals, classify_int_pattern(vals))
  data %>% left_join(pattern_types, by ="value")
}

prepare_simul_data <-function(){
  wjd_simul <- read.csv("data/wjd_simul_1_db.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()
  wjd_simul$melid <- gsub("SIMUL_", "", wjd_simul$id, fixed = T) %>% as.integer()
  wjd_simul$melid <- wjd_simul$melid  + 1 
  #
  
  #browser()
  #not_fit_ids <- which(table(wjd_simul$melid) != table(wjd_all$melid))
  wjd_simul <- 
    wjd_simul %>% 
    mutate(melid = case_when(melid %in% 446 ~ 447,
                             melid %in% 447 ~ 446,
                             TRUE ~ as.numeric(melid)))

  wjd_simul <- 
    wjd_simul %>% 
    left_join(wjd_all %>% 
                select(melid, N, start, g_phrase_id, phrase_pos, performer, style, recordingyear, performer_style), by = c("melid", "N", "start"))
  #wjd_simul <- add_pattern_classes(wjd_simul)
  #simul_pattern_features <- n_gram_easiness(unique(wjd_simul$value))
  #saveRDS(simul_pattern_features, "data/simul_pattern_features.RDS")
  simul_pattern_features <- readRDS("data/simul_pattern_features.RDS")
  wjd_simul <- wjd_simul %>% left_join(simul_pattern_features, by = "value")
  tmp_prob <- 
    wjd_simul %>% 
    distinct(value, .keep_all = T) %>% 
    group_by(N) %>% 
    mutate(total = sum(freq), prob = freq/total, surprise = -log2(prob)) %>% 
    ungroup() 
  wjd_simul <- wjd_simul %>% left_join(tmp_prob %>% select(value, prob, surprise), by = "value")
  
  wjd_simul <- 
    wjd_simul %>% 
    group_by(N, performer, value) %>%  
    mutate(freq_p = n()) %>% 
    ungroup() %>% 
    group_by(N, performer) %>% 
    mutate(total = sum(freq_p), 
           prob_p = freq_p/total, 
           surprise_p = -log2(prob_p))  %>% 
    ungroup()
  wjd_simul <- calc_overall_complexity(wjd_simul, 
                                       measures = c("int_variety", "pitch_variety", "dir_change", "mean_run_length"))
  wjd_simul <- 
    wjd_simul %>% 
    mutate(start_id = sprintf("%d_%d", as.integer(factor(melid)), start)) %>% 
    left_join(phrase_pos %>% 
                select(rev_phrase_pos, rel_phrase_pos, start_id, phrase_len), by = "start_id") %>% 
    mutate(in_phrase = (rev_phrase_pos - N) > 0 ) 
  
  saveRDS(wjd_simul, "data/wjd_simul.RDS")
  wjd_simul  
}

prepare_data <- function(full = F){
  wjd_transforms <-  readRDS("data/wjd_transforms.RDS") %>% 
    mutate(id = as.character(id))
  assign("wjd_transforms", wjd_transforms, globalenv())
  phrase_pos <- calc_phrase_positions(wjd_transforms %>% 
                                        select(id, phrase_id_raw))
  saveRDS(phrase_pos, "data/phrase_pos.RDS") 
  
  wjd_all <- readRDS("data/wjd_all_int.RDS")
  if(full) wjd_all <- add_pattern_classes(wjd_all)
  wjd_all <- 
    wjd_all %>% 
    mutate(start_id = sprintf("%d_%d", as.integer(factor(melid)), start)) %>% 
    mutate(surprise = -log2(prob100/100)) %>% 
    group_by(N) %>% 
    mutate(z_surprise = scale(surprise)) %>% 
    ungroup() %>% 
    left_join(phrase_pos %>% 
                select(phrase_pos, rev_phrase_pos, rel_phrase_pos, start_id, phrase_len), by = "start_id") %>% 
    mutate(in_phrase = (rev_phrase_pos - N) > 0 ) %>% 
    left_join(phrase_pos %>% freq_table(phrase_pos) %>% select(phrase_pos, pos_weight = freq), by = "phrase_pos") 
  #wjd_all <- wjd_all %>% group_by(N, value) %>%   mutate(freq = n()) %>% ungroup() %>% group_by(N) %>% mutate(total = sum(freq), prob = freq/total, surprise = -log2(prob))  %>% ungroup() %>% select(-total)
  #wjd_all <- wjd_all %>% group_by(N, id, value) %>%   mutate(freq_s = n()) %>% ungroup() %>% group_by(N, id) %>% mutate(total = sum(freq_s), prob_s = freq_s/total, surprise_s = -log2(prob_s))  %>% ungroup() %>% select(-total)
  #wjd_all <- wjd_all %>% group_by(N, performer, value) %>%   mutate(freq_p = n()) %>% ungroup() %>% group_by(N, performer) %>% mutate(total = sum(freq_p), prob_p = freq_p/total, surprise_p = -log2(prob_p))  %>% ungroup() %>% select(-total)
  saveRDS(wjd_all, "data/wjd_all.RDS")  
}

download_data_osf <- function(force = F){
  data_files<- c("wjd_all.RDS", 
                 "wjd_all_int.RDS",
                 "wjd_simul.RDS",
                 "wjd_transforms.RDS",
                 "phrase_pos.RDS",
                 "phrase_selection.RDS",
                 "simul_pattern_features.RDS",
                 "wjd_metadata.csv")
  data_dir <- "data"
  ef_project <- osf_retrieve_node("svm2z") %>% osf_ls_files()
  map_dfr(data_files, function(x){
    if(force || !file.exists(file.path(data_dir, x))){
      messagef("Downloading %s", x)
      ef_project %>% osf_ls_files(pattern = x) %>% osf_download(path = data_dir)
    }
  })
}
setup_workspace <-function(recalc = F, full = F, simul = F){
  download_data_osf()
  if(recalc){
    prepare_data(full = full)
  }
  if(simul){
    assign("wjd_all", readRDS("data/wjd_simul.RDS"), globalenv())
  }
  else{
    assign("wjd_all", readRDS("data/wjd_all.RDS"), globalenv())
    
  }
  assign("phrase_pos", readRDS("data/phrase_pos.RDS"), globalenv())
  assign("wjd_transforms", readRDS("data/wjd_transforms.RDS"), globalenv())
  assign("all_performers",  unique(wjd_all$performer), globalenv())
  assign("all_styles", unique(wjd_all$style), globalenv())
  assign("all_pattern_types", unique(wjd_all$sub_type), globalenv())
  
  good_performer <- 
    wjd_all %>% 
    distinct(performer, id) %>% 
    count(performer) %>% 
    filter(n > 7) %>% 
    pull(performer)
  assign("good_performer", good_performer, globalenv())
  assign("good_style", setdiff(unique(wjd_all$style), c("FREE", "FUSION")), globalenv())  
}

calc_phrase_positions <- function(pp){
  pp <- 
    pp %>% 
    mutate(g_phrase_id = sprintf("%d_%d", as.integer(factor(id)), phrase_id_raw)) %>% 
    group_by(g_phrase_id) %>% 
    mutate(phrase_len = n(),
           phrase_pos = 1:n(), 
           rev_phrase_pos = n():1,
           rel_phrase_pos = (phrase_pos-1)/(n()-1)) %>% 
    ungroup() %>% 
    group_by(id) %>% 
    mutate(note_pos = 0:(n()-1 )) %>% 
    ungroup() %>% 
    mutate(start_id = sprintf("%d_%d", as.integer(factor(id)), note_pos)) 
    
    pp 
}
calc_overall_complexity <- function(data = wjd_all, measures = easiness_measures){
  #browser()
  measures <- setdiff(measures, "combined_easiness")
  for(m in measures){
    data$easiness <- data[, m]
    data <- data %>% group_by(N) %>% mutate(easiness = scale(easiness)) %>% ungroup()
    if(any(is.na(data$easiness))){
      #printf("%s %d", m, sum(is.na(data$easiness)))
      #print(table(data[is.na(data$easiness),]$N ))
      #browser()
      data[as.vector(is.na(data$easiness)),]$easiness <- 0
    }
    if(any(!is.finite(data$easiness))){
      #printf("%s: %d", m, sum(!is.finite(data$easiness)))
      #print(table(data[!is.finite(data$easiness),]$N ))
      #browser()
      data[as.vector(!is.finite(data$easiness)),]$easiness <- 0
    }
    data[, sprintf("z_%s", m)] <- data$easiness
  }
  data$easiness <- NULL
  data$combined_easiness <- data[, sprintf("z_%s", measures)] %>%  as.matrix %>% rowSums()
  #data$combined_easiness <- tot/sqrt(length(measures))
  #browser()
  data <- data %>% group_by(N) %>% mutate(combined_easiness = scale(combined_easiness)) %>% ungroup()
  #tmp <- data %>% group_by(N) %>% summarise(cor(ce, combined_easiness))
  data
}

filter_in_phrase_ngrams <- function(data){
  data <- data %>% mutate(in_phrase = (rev_phrase_pos - N) > 0)
  data <- data %>% filter(in_phrase, !(sub_type %in% c("trill", "repetition"))) %>% group_by(N) %>% mutate(total_freq = n()) %>% ungroup() 
  
  data <- data %>% group_by(value) %>% mutate(freq = n(), surprise = -log2(freq/total_freq)) %>% ungroup()
  data <- data %>% group_by(N) %>% mutate(z_surprise = scale(surprise)) %>% ungroup()
  data <- calc_overall_complexity(data, measures = c("int_variety", "pitch_variety", "dir_change", "mean_run_length"))
  data  
}

plot_easiness_by_phrase_pos <- function(data = wjd_all, 
                                        min_N = 3, 
                                        min_freq = 1, 
                                        max_pos = 50, 
                                        fix_scale = FALSE,
                                        standardize = FALSE,
                                        facetting = TRUE,
                                        easiness = "surprise",
                                        easiness_label = "Mean Surprise",
                                        smooth = "poly", poly_degree = 3){
  #data$easiness <- data %>% pull(!!rlang::enquo(easiness))
  data$easiness <- as.data.frame(data)[, easiness]
  if(standardize){
    data <- data %>% group_by(N) %>% mutate(easiness = scale(easiness)) %>% ungroup()
  }  
  #browser()
  data <- 
    data %>% 
    filter(N >= min_N, in_phrase, freq >= min_freq, phrase_pos <= max_pos, phrase_len >= N) %>%  
    group_by(N, phrase_pos) %>% 
    summarise(easiness_mean = mean(easiness), 
              easiness_sd = sd(easiness), 
              n = n(), 
              easiness_se = easiness_sd/sqrt(n())) %>% 
    ungroup() 
  mean_eff_size <- 
    data %>% 
    group_by(N) %>% 
    summarise(r = diff(range(easiness_mean)), mem = mean(easiness_mean)) %>% 
    print() %>% 
    summarise(d = mean(r), max_d = max(r), min_d = min(r), med_d = median(r), mem = mean(mem)) %>% 
    print() %>% 
    pull(med_d)
  #browser()
  cor <- my_cor_test(data %>% filter(N == 4), "phrase_pos", "easiness_mean") 
  mean_cor <- 
    map_dfr(3:10, function(x) {
      my_cor_test(data %>% filter(N == x), "phrase_pos", "easiness_mean")}) %>% 
    summarise(mean_cor = mean(estimate)) %>% 
    print()
  
  printf("Data points: %d, mean eff. size: %f", sum(data$n), mean_eff_size)
  
  q <- 
    data %>% 
    ggplot(aes(x = phrase_pos, y = easiness_mean, color = factor(N))) 
  q <- q + geom_errorbar(aes(ymin = easiness_mean - easiness_se, ymax = easiness_mean + easiness_se)) 
  if(facetting){
    if(fix_scale){
      q <- q + facet_wrap(~N) 
    }
    else{
      q <- q + facet_wrap(~N, scales = "free_y") 
    }
    q <- q + theme(legend.position = "none") 
  }
  if(smooth == "poly"){
    f <- sprintf("y ~ poly(x, %d)", poly_degree)
    q <- q + geom_smooth(method = "lm", formula = f) 
  }
  else{
    q <- q + geom_smooth() 
    
  }
  q <- q + theme_bw() 
  q <- q + labs(x = "Phrase position", y  = easiness_label, caption  = sprintf("N = %d", sum(data$n))) 
  q <- q + geom_point()
  q
}

plot_easiness_by_rev_phrase_pos <- function(data = wjd_all, 
                                        min_N = 3, 
                                        min_freq = 1, 
                                        max_pos = 50, 
                                        fix_scale = FALSE,
                                        standardize = FALSE,
                                        facetting = TRUE,
                                        easiness = "surprise",
                                        easiness_label = "Mean Surprise",
                                        smooth = "poly", poly_degree = 3){
  #data$easiness <- data %>% pull(!!rlang::enquo(easiness))
  data$easiness <- as.data.frame(data)[, easiness]
  data$phrase_pos <- data$rev_phrase_pos
  if(standardize){
    data <- data %>% group_by(N) %>% mutate(easiness = scale(easiness)) %>% ungroup()
  }  
  #browser()
  data <- 
    data %>% 
    filter(N >= min_N, in_phrase, freq >= min_freq, phrase_pos <= max_pos, phrase_pos >= N) %>%  
    group_by(N, phrase_pos) %>% 
    summarise(easiness_mean = mean(easiness), 
              easiness_sd = sd(easiness), 
              n = n(), 
              easiness_se = easiness_sd/sqrt(n())) %>% 
    ungroup() %>% 
    mutate(phrase_pos =  - phrase_pos)
  
  mean_eff_size <- 
    data %>% 
    group_by(N) %>% 
    summarise(r = diff(range(easiness_mean)), mem = mean(easiness_mean)) %>% 
    print() %>% 
    summarise(d = mean(r), max_d = max(r), min_d = min(r), med_d = median(r), mem = mean(mem)) %>% 
    print() %>% 
    pull(med_d)
  #browser()
  cor <- my_cor_test(data %>% filter(N == 4), "phrase_pos", "easiness_mean") 
  mean_cor <- 
    map_dfr(3:10, function(x) {
      my_cor_test(data %>% filter(N == x), "phrase_pos", "easiness_mean")}) %>% 
    summarise(mean_cor = mean(estimate)) %>% 
    print()
  
  printf("Data points: %d, mean eff. size: %f", sum(data$n), mean_eff_size)
  
  q <- 
    data %>% 
    ggplot(aes(x = phrase_pos, y = easiness_mean, color = factor(N))) 
  q <- q + geom_errorbar(aes(ymin = easiness_mean - easiness_se, ymax = easiness_mean + easiness_se)) 
  if(facetting){
    if(fix_scale){
      q <- q + facet_wrap(~N) 
    }
    else{
      q <- q + facet_wrap(~N, scales = "free_y") 
    }
    q <- q + theme(legend.position = "none") 
  }
  if(smooth == "poly"){
    f <- sprintf("y ~ poly(x, %d)", poly_degree)
    q <- q + geom_smooth(method = "lm", formula = f) 
  }
  else{
    q <- q + geom_smooth() 
    
  }
  q <- q + theme_bw() 
  q <- q + labs(x = "Phrase position", y  = easiness_label, caption  = sprintf("N = %d", sum(data$n))) 
  q <- q + geom_point()
  q
}

plot_easiness_by_group <- function(data = wjd_all, 
                                   group_var = "style", 
                                   min_N = 3, 
                                   min_freq = 1, 
                                   max_pos = 50, 
                                   fix_scale = FALSE,
                                   standardize = TRUE,
                                   facetting = TRUE,
                                   easiness = "surprise",
                                   easiness_label = "Mean Surprise",
                                   smooth = "poly", 
                                   poly_degree = 3){
  #data$group_var <- data %>% pull(!!rlang::enquo(group_var))
  #data$easiness <- data %>% pull(!!rlang::enquo(easiness))
  data$easiness <- as.data.frame(data)[, easiness]
  data$group_var <- as.data.frame(data)[, group_var]
  #browser()
  if(standardize){
    data <- data %>% group_by(N) %>% mutate(easiness = scale(easiness)) %>% ungroup()
    
  }
  data <- 
    data %>% 
    filter(N == min_N, in_phrase, freq >= min_freq, phrase_pos <= max_pos) %>%  
    group_by(group_var, phrase_pos) %>% 
    summarise(sp = mean(easiness), 
              sp_sd = sd(easiness), 
              n = n(), 
              se = sp_sd/sqrt(n())) 
  printf("Data points: %d", sum(data$n))
  q <- 
    data %>% 
    ggplot(aes(x = phrase_pos, y = sp, color = factor(group_var))) 
  q <- q + geom_errorbar(aes(ymin = sp - se, ymax = sp + se)) 
  if(facetting){
    if(fix_scale){
      q <- q + facet_wrap(~group_var) 
    }
    else{
      q <- q + facet_wrap(~group_var, scales = "free_y") 
    }
    q <- q + theme(legend.position = "none") 
  }
  if(smooth == "poly"){
    f <- sprintf("y ~ poly(x, %d)", poly_degree)
    q <- q + geom_smooth(method = "lm", formula = f) 
  }
  else{
    q <- q + geom_smooth() 
    
  }
  q <- q + theme_bw() 
  
  q <- q + labs(x = "Phrase position", y  = easiness_label, caption  = sprintf("N = %d", sum(data$n))) 
  q <- q + geom_point()
  q
}

plot_easiness_by_type <- function(data = wjd_comb, 
                                   min_N = 3, 
                                   min_freq = 1, 
                                   max_pos = 30, 
                                   standardize = FALSE,
                                   easiness = "combined_easiness",
                                   smooth = "poly", 
                                   poly_degree = 3,
                                   plot_type = "errorbars"){
  #data$group_var <- data %>% pull(!!rlang::enquo(group_var))
  #data$easiness <- data %>% pull(!!rlang::enquo(easiness))
  data$easiness <- as.data.frame(data)[, easiness]
  easiness_label <- get_easiness_label(easiness)
  
  if(standardize){
    data <- data %>% group_by(N) %>% mutate(easiness = scale(easiness)) %>% ungroup()
    
  }
  data <- 
    data %>% 
    filter(N >= min_N, freq >= min_freq, phrase_pos <= max_pos) %>%  
    group_by(type, phrase_pos, N) %>% 
    summarise(sp = mean(easiness), 
              sp_sd = sd(easiness), 
              n = n(), 
              se = sp_sd/sqrt(n())) %>% 
    ungroup()
  data$type <- (factor(data$type, labels = c("Simulated", "WJD")) )
  if(plot_type == "ribbon"){
    data$data_type <- data$type
    data <- data %>% mutate(type = sprintf("%s", type))
  }
  q <- 
    data %>% 
    ggplot(aes(x = phrase_pos, y = sp)) 
  if(plot_type == "errorbars"){
    q <- q + geom_errorbar(aes(ymin = sp - se, ymax = sp + se, color = type, shape = type)) 
    q <- q + geom_point()
    
  } 
  else{
    q <- q + geom_ribbon(aes(ymin = sp - se, ymax = sp + se, fill = type, group = type), alpha = .5)
    q <- q + geom_line(aes(group = type), size = .5, color = "black")
  }
  q <- q + facet_wrap(~N) 
  
  if(smooth == "poly"){
    f <- sprintf("y ~ poly(x, %d)", poly_degree)
    q <- q + geom_smooth(aes(group = type), method = "lm", formula = f, colour = "black", se = F) 
  }
  else if (smooth == "standard"){
    q <- q + geom_smooth(aes(group = type)) 
    
  }

  q <- q + get_default_theme()
  q <- q + theme(legend.position = c(.85, .2), legend.background = element_rect(colour = "black")) 
  
  #q <- q + labs(x = "Phrase position", y  = easiness_label, caption  = sprintf("N = %d", sum(data$n))) 
  q <- q + labs(x = "Phrase position", y  = easiness_label) 
  q
}

plot_easiness_by_type_reversed <- function(data = wjd_comb, 
                                  min_N = 3, 
                                  min_freq = 1, 
                                  max_pos = 30, 
                                  standardize = FALSE,
                                  easiness = "surprise",
                                  smooth = "poly", 
                                  poly_degree = 3){
  #data$group_var <- data %>% pull(!!rlang::enquo(group_var))
  #data$easiness <- data %>% pull(!!rlang::enquo(easiness))
  data$easiness <- as.data.frame(data)[, easiness]
  easiness_label <- get_easiness_label(easiness)
  data$phrase_pos <- data$rev_phrase_pos
  #browser()
  if(standardize){
    data <- data %>% group_by(N) %>% mutate(easiness = scale(easiness)) %>% ungroup()
    
  }
  data <- 
    data %>% 
    filter(N >= min_N, in_phrase, freq >= min_freq, phrase_pos <= max_pos, phrase_pos >= N) %>%  
    group_by(type, phrase_pos, N) %>% 
    summarise(sp = mean(easiness), 
              sp_sd = sd(easiness), 
              n = n(), 
              se = sp_sd/sqrt(n())) %>% 
    ungroup() %>% 
    mutate(phrase_pos = -phrase_pos)
  
  
  data$type <- (factor(data$type, labels = c("Simulated", "WJD")))
  q <- 
    data %>% 
    ggplot(aes(x = phrase_pos, y = sp, color = type, shape = type)) 
  q <- q + geom_errorbar(aes(ymin = sp - se, ymax = sp + se)) 
  q <- q + facet_wrap(~N) 
  if(smooth == "poly"){
    f <- sprintf("y ~ poly(x, %d)", poly_degree)
    q <- q + geom_smooth(method = "lm", formula = f) 
  }
  else{
    q <- q + geom_smooth() 
    
  }
  q <- q + get_default_theme()
  q <- q + theme(legend.position = c(.85, .2), legend.background = element_rect(colour = "black")) 
  
  #q <- q + labs(x = "Phrase position", y  = easiness_label, caption  = sprintf("N = %d", sum(data$n))) 
  q <- q + labs(x = "Reversed phrase position ", y  = easiness_label) 
  q <- q + geom_point()
  q
}

plot_easiness_by_type_MLA <- function(data = wjd_inphrase_comb, 
                                      min_N = 3, 
                                      min_freq = 1, 
                                      max_pos = 30, 
                                      standardize = FALSE,
                                      easiness = "z_surprise",
                                      MLA_main_types = c("line", "lick"),
                                      smooth = "poly", 
                                      poly_degree = 1){
  #data$group_var <- data %>% pull(!!rlang::enquo(group_var))
  #data$easiness <- data %>% pull(!!rlang::enquo(easiness))
  data$easiness <- as.data.frame(data)[, easiness]
  easiness_label <- get_easiness_label(easiness)
  #browser()
  if(standardize){
    data <- data %>% filter(in_phrase) %>% group_by(N) %>% mutate(easiness = scale(easiness)) %>% ungroup()
    
  }
  data <- 
    data %>% 
    filter(N >= min_N, freq >= min_freq, phrase_pos <= max_pos, MLA_main_type %in% MLA_main_types) %>%  
    group_by(type, phrase_pos, N, MLA_main_type) %>% 
    summarise(sp = mean(easiness), 
              sp_sd = sd(easiness), 
              n = n(), 
              se = sp_sd/sqrt(n())) 
  data$type <- as.character(factor(data$type, labels = c("Simulated", "WJD")) )
  data$data_type <- data$type
  data <- data %>% mutate(type = sprintf("%s-%s", type, MLA_main_type))
  q <- 
    data %>% 
    ggplot(aes(x = phrase_pos, y = sp)) 
  #q <- q + geom_errorbar(aes(ymin = sp - se, ymax = sp + se)) 
  q <- q + geom_ribbon(aes(ymin = sp - se, ymax = sp + se, fill = data_type, group = type), alpha = .5)
  q <- q + geom_line(aes(linetype = MLA_main_type, group = type), size = 1, color = "black")
  q <- q + facet_wrap(~N) 
  #if(smooth == "poly"){
  #  f <- sprintf("y ~ poly(x, %d)", poly_degree)
  #  q <- q + geom_smooth(method = "lm", formula = f, aes(group = type), colour = "black", se = F) 
  #}
  #else{
  #  q <- q + geom_smooth(se = F, aes(group = type)) 
  #  
  #}
  
  q <- q + get_default_theme(keep_legend = T)
  q <- q + theme(legend.key.size = unit(0.5, "cm"))
  q <- q + theme(legend.key.width = unit(1.0, "cm"))
  q <- q + theme(legend.position = c(.85, .1), legend.background = element_rect(colour = "black")) 
  
  #q <- q + labs(x = "Phrase position", y  = easiness_label, caption  = sprintf("N = %d", sum(data$n))) 
  q <- q + labs(x = "Phrase position", y  = easiness_label) 
  #q <- q + geom_point()
  q
}
plot_easiness_by_type_MLA_rev <- function(data = wjd_inphrase_comb, 
                                      min_N = 3, 
                                      min_freq = 1, 
                                      max_pos = 30, 
                                      standardize = FALSE,
                                      easiness = "z_surprise",
                                      MLA_main_types = c("line", "lick"),
                                      smooth = "poly", 
                                      poly_degree = 3){
  #data$group_var <- data %>% pull(!!rlang::enquo(group_var))
  #data$easiness <- data %>% pull(!!rlang::enquo(easiness))
  data$easiness <- as.data.frame(data)[, easiness]
  easiness_label <- get_easiness_label(easiness)
  data$phrase_pos <- data$rev_phrase_pos
  #browser()
  if(standardize){
    data <- data %>% filter(in_phrase) %>% group_by(N) %>% mutate(easiness = scale(easiness)) %>% ungroup()
    
  }
  data <- 
    data %>% 
    filter(N >= min_N, freq >= min_freq, phrase_pos <= max_pos, phrase_pos >= N, MLA_main_type %in% MLA_main_types) %>%  
    group_by(type, phrase_pos, N, MLA_main_type) %>% 
    summarise(sp = mean(easiness), 
              sp_sd = sd(easiness), 
              n = n(), 
              se = sp_sd/sqrt(n())) %>% 
    ungroup() %>%
    mutate(phrase_pos = -phrase_pos)
    
  data$type <- as.character(factor(data$type, labels = c("Simulated", "WJD")) )
  data$data_type <- data$type
  data <- data %>% mutate(type = sprintf("%s-%s", type, MLA_main_type))
  q <- 
    data %>% 
    ggplot(aes(x = phrase_pos, y = sp)) 
  #q <- q + geom_errorbar(aes(ymin = sp - se, ymax = sp + se)) 
  q <- q + geom_ribbon(aes(ymin = sp - se, ymax = sp + se, fill = data_type, group = type))
  q <- q + geom_line(aes(linetype = MLA_main_type, group = type), size = 1, color = "black")
  q <- q + facet_wrap(~N) 
  if(smooth == "poly"){
    f <- sprintf("y ~ poly(x, %d)", poly_degree)
    q <- q + geom_smooth(method = "lm", formula = f, aes(group = type), colour = "black", se = F) 
  }
  else{
    q <- q + geom_smooth(se = F, aes(group = type)) 
    
  }
  q <- q + get_default_theme(keep_legend = T)
  q <- q + theme(legend.key.size = unit(0.5, "cm"))
  q <- q + theme(legend.key.width = unit(1.0, "cm"))
  
  q <- q + theme(legend.position = c(.85, .1), legend.background = element_rect(colour = "black")) 
  
  #q <- q + labs(x = "Phrase position", y  = easiness_label, caption  = sprintf("N = %d", sum(data$n))) 
  q <- q + labs(x = "Phrase position", y  = easiness_label) 
  #q <- q + geom_point()
  q
}
my_cor_test <- function(data, x, y, method = "pearson", remove_outliers = F){
  #browser()
  vec_x <- as.data.frame(data)[, x]
  vec_y <- as.data.frame(data)[, y]
  if(remove_outliers){
    out_x <- boxplot(vec_x, plot = FALSE)$out
    out_y <- boxplot(vec_y, plot = FALSE)$out
    out_x <- which(vec_x %in% out_x)
    out_y <- which(vec_y %in% out_y)
    out <- union(out_x, out_y)
    #if(n_distinct(vec_x[-out]) != 1 && n_distinct(vec_y[-out]) != 1){
    vec_x <- vec_x[-out]
    vec_y <- vec_y[-out]
    #}
    #printf("Len after outlier removing: %d", length(vec_x))
  }
  
  if(length(vec_x) < 2 || length(vec_y) < 2 ||  n_distinct(vec_x) == 1 || n_distinct(vec_y) == 1){
    return(tibble(
      estimate = NA,
      statistic  = NA,
      p.value  =NA, 
      parameter = NA, 
      conf.low = NA, 
      conf.high =NA, 
      method = NA,
      alternative  =NA,
      x = x,
      y = y
    ))
  }
  #browser()
  ct <- suppressWarnings(cor.test(vec_x, vec_y, method = method) %>% broom::tidy())
  if(length(names(x)) > 0) {
    ct$x <- names(x)
  }
  else{
    ct$x <- x
    
  }
  if(length(names(y)) > 0) {
    ct$y <- names(y)
  }
  else{
    ct$y <- y
    
  }
  ct
}

plot_easiness_cor <- function(data = wjd_all, 
                              min_N = 3, 
                              x = "surprise", xlab = "Surprise",
                              y = "int_variety", ylab = "Interval variety",
                              sample_f = .25, 
                              facetting = TRUE,
                              fix_scale = TRUE,
                              alpha = .2){
  data <- data %>% filter(N >= min_N)
  if(!is.null(sample_f) || sample_f < 1.){
    orig <- nrow(data)
    data <- data %>% sample_frac(sample_f)
    messagef("Sampled %.2f from %d to %d", sample_f, orig, nrow(data))
  }
  
  data$x <- as.data.frame(data)[, x]
  data$y <- as.data.frame(data)[, y]
  #q <- data %>% 
  #  count(N, x, y) %>% 
  #  ggplot(aes(x = x, y = y)) 
  #q <- 
  #  data  %>%  
  #  count(N, x, y) %>% 
  #  group_by(N) %>% 
  #  mutate(freq = n/sum(n)) %>% 
  #  ungroup() %>% 
  #  ggplot(aes(x = x, y = y)) 
  #q <- q + geom_point(aes(size = freq + N, alpha = freq), color = "lightblue4") 
  q <- data %>% ggplot(aes(x = x, y = y)) 
  q <- q + geom_point(alpha = .2, color = "lightblue4")
  if(facetting){
    if(fix_scale){
      q <- q + facet_wrap(~N) 
    }
    else{
      q <- q + facet_wrap(~N, scales = "free_y") 
    }
    q <- q + theme(legend.position = "none") 
  }
  q <- q + theme_bw() 
  q <- q + geom_smooth(method = "lm", colour ="indianred") 
   
  q <- q + labs(x = xlab, y  = ylab, caption  = sprintf("N = %d", nrow(data))) 
  q
}

get_regressions <- function(data = wjd_all, 
                            min_N = 3, 
                            max_pos = 30, 
                            easiness = "surprise", 
                            standardize = FALSE, 
                            aggregate = TRUE, 
                            degree = 1){
  data$easiness <- as.data.frame(data)[, easiness]
  if(standardize){
    data <- data %>% group_by(N) %>% mutate(easiness = scale(easiness)) %>% ungroup()
  }
  #browser()
  if(aggregate){
    if(any(is.infinite(data$easiness))) {
      data[is.infinite(data$easiness),]$easiness <- NA
    }
    data <- data %>% group_by(N, phrase_pos) %>% summarise(easiness = mean(easiness, na.rm = T)) %>% ungroup()
    
  }
  messagef("Running regressions for %s with degree %d (min N = %d, max_pos = %d, aggregated = %s)", easiness, degree, min_N, max_pos, aggregate)
  f <- as.formula(sprintf("easiness ~ poly(phrase_pos, %d)", degree))
  #browser()
  max_N <- max(data$N, na.rm = T)
  map_dfr(min_N:max_N, function(k){
    model <- lm(formula = f, data = data %>% filter(N == k, phrase_pos <= max_pos)) %>% broom::glance() %>% mutate(N = k)  
    #model <- lm(formula = f, data = data %>% filter(N == k, phrase_pos <= max_pos)) %>% broom::tidy() %>% mutate(N = k)  
  }) %>% select(N, 2:length(.))
}

get_linear_betas <- function(data = wjd_inphrase, 
                            MLA_main_type = c("lick", "line"),
                            min_N = 3, 
                            max_pos_range = 15:30, 
                            easiness = "combined_easiness", 
                            aggregate = TRUE){
  #browser()
  data$easiness <- as.data.frame(data)[, easiness]
  data <- data %>% filter(N >= min_N, phrase_pos <= max(max_pos_range))
  if(aggregate){
    if(any(is.infinite(data$easiness))) {
      data[is.infinite(data$easiness),]$easiness <- NA
    }
    data <- data %>% group_by(N, phrase_pos, MLA_main_type) %>% summarise(easiness = mean(easiness, na.rm = T)) %>% ungroup()
    
  }
  messagef("Running linear regression for %s (min N = %d, min_ax_pos = %d, max_max_pos = %d, aggregated = %s)", easiness, min_N, min(max_pos_range), max(max_pos_range), aggregate)
  f <- as.formula(sprintf("easiness ~ phrase_pos"))
  #browser()
  max_N <- max(data$N, na.rm = T)
  map_dfr(MLA_main_type, function(mla){
    map_dfr(max_pos_range, function(mp){
      map_dfr(min_N:max_N, function(k){
        #browser()
        model <- lm(formula = f, data = data %>% filter(N == k, phrase_pos <= mp, MLA_main_type == mla)) %>% 
          broom::tidy() %>% 
          mutate(N = k, max_pos = mp, MLA_main_type = mla) %>% 
          filter(term != "(Intercept)") %>% 
          select(-term)
        model
      })
    })
  })  %>% select(MLA_main_type, max_pos, N, everything()) %>% arrange(MLA_main_type, max_pos, N)
}

get_regressions_with_group <- function(data = wjd_all, min_N = 3, max_pos = 30, easiness = "surprise", group_var = "style", aggregate = TRUE, degree = 1){
  data$easiness <- as.data.frame(data)[, easiness]
  if(any(is.infinite(data$easiness))) {
    data[is.infinite(data$easiness),]$easiness <- NA
  }
  data$group_var <- as.data.frame(data)[, group_var]
  if(n_distinct(data$group_var) == 1){
    return(NULL)
  }
  data <- data %>% group_by(N) %>% mutate(easiness = scale(easiness)) %>% ungroup()
  #browser()
  if(aggregate){
    data <- data %>% group_by(group_var, phrase_pos) %>% summarise(easiness = mean(easiness, na.rm = T)) %>% ungroup()
  }
  messagef("Running regressions for %s with degree %d (min N = %d, max_pos = %d, aggregated = %s)", easiness, degree, min_N, max_pos, aggregate)
  f <- as.formula(sprintf("easiness ~ poly(phrase_pos, %d) + group_var", degree, group_var))
  f2 <- as.formula(sprintf("easiness ~ poly(phrase_pos, %d)", degree, group_var))
  model <- lm(formula = f, data = data %>% filter(phrase_pos <= max_pos)) %>% broom::glance() %>% mutate(group = "overall")
  model2 <-
    map_dfr(unique(data$group_var), function(k){
    model <- lm(formula = f2, data = data %>% filter(phrase_pos <= max_pos, group_var == k)) %>% broom::glance() %>% mutate(group = k)  
  })
  bind_rows(model, model2) %>% select(group, 2:length(.))
}

plot_facet_histogram <- function(data = wjd_all, 
                                 group_var = "N", 
                                 var = "surprise", 
                                 var_label = "Surprise", 
                                 min_N = 3,
                                 facetting = TRUE,
                                 fix_scale = TRUE){
  min_N <- max(min(min_N, 10), 3)
  data <- data  %>% filter(N >= min_N) 
  data$var <- as.data.frame(data)[, var]
  data$group_var <- as.data.frame(data)[, group_var]
  #browser()
  if(group_var != "N"){
    data <- data %>% filter(N == max(N))
  }
  if(facetting) {
    max_val <- 
      data %>% 
      group_by(group_var) %>% 
      summarise(l = max(var)) %>% 
      summarise(m = max(l) < 150) %>% 
      pull(m)
    max_cat <- 
      data %>% 
      group_by(group_var) %>% 
      summarise(l = length(unique(var))) %>% 
      summarise(m = max(l) < 150) %>% 
      pull(m)
  }
  else{
    max_val <- 
      data %>% 
      summarise(m = max(var) < 150) %>% 
      pull(m)
    max_cat <- 
      data %>% 
      summarise(l = length(unique(var))) %>% 
      summarise(m = max(l) < 150) %>% 
      pull(m)
  }
  use_bar_plot <- max_val && max_cat
  messagef("Plotting histograms for %s with %s, Bar plot: %s", var, group_var, use_bar_plot)
  q <- data %>% ggplot(aes(x = var, y = ..count../tapply(..count..,..PANEL..,sum)[..PANEL..])) 
  if(use_bar_plot){
    q <- q + geom_bar(fill = "lightblue4", color = "black") 
  }
  else{
    q <- q + geom_histogram(fill = "lightblue4", color = "black")
  }
  if(facetting){
    if(fix_scale){
      q <- q + facet_wrap(~group_var) 
    }
    else{
      q <- q + facet_wrap(~group_var, scales = "free") 
    }
  }
  q <- q + scale_y_continuous(labels = scales::percent, name="Percentage (%)") 
  q <- q + labs(x = var_label)
  q <- q + get_default_theme()
  q
}

single_phrase_plot <- function(data = wjd_all, phrase_id, easiness = "surprise", phrase_N = 5, max_pos = 15){
  data <- data %>% filter(g_phrase_id == phrase_id, N == phrase_N, phrase_pos <= max_pos, phrase_len >= max_pos) 
  if(nrow(data) == 0){
    return(NULL)
  }
  
  data$easiness <- as.data.frame(data)[, easiness]  
  browser()
  q <- data %>%  
    ggplot(aes(x = phrase_pos, y = easiness)) 
  q <- q + geom_point() 
  q <- q + geom_smooth(method ="lm", color = "black") 
  q <- q + geom_label_repel(aes(label = value)) 
  q <- q + get_default_theme()
  q <- q + labs(x = "Phrase position", y = get_easiness_label(easiness), title = sprintf("Phrase: %s", phrase_id))
  q
}

multi_rename <- function(data, named_vector){
  for(i in 1:length(named_vector)){
    x <- named_vector[i]
    #browser()
    if(x %in% names(data)) {
      names(data)[which(names(data) == x)] <- names(named_vector)[i]
    }
    
  }  
  data
}
single_phrase_plot2 <- function(data = wjd_all, phrase_id, easiness = "surprise", phrase_N = 5, max_pos = 15, with_labels = F){
  data <- data %>% filter(g_phrase_id == phrase_id, N == phrase_N, phrase_pos <= max_pos, phrase_len >= max_pos) 
  if(nrow(data) == 0){
    return(NULL)
  }
  cor_df <- 
    map_dfr(easiness, function(x) {
      my_cor_test(data, "phrase_pos", x)
      })
  #browser()  
  cor_df$y <- get_easiness_label(cor_df$y)
  cor_df <- 
    cor_df %>% 
    mutate( sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < .05 ~ "*", TRUE ~ "")) %>% 
    rename(key = y)
  mel <- data %>% distinct(id)  %>% pull(id)
  #browser()
  data <- 
    data %>% 
    select(phrase_pos, ngram = value, easiness) %>% 
    gather(key = "key", value = "value", -phrase_pos, -ngram)
  
  if(length(names(easiness)) == 0) {
    data$key <- map_chr(data$key, get_easiness_label)
  }
  data <- data %>% left_join(cor_df %>% select(sig, key), by = "key")
  data$sig <- factor(data$sig, levels = c("", "*", "**", "***"), labels = c("n.s.", "*", "**", "***"))
  q <- data %>%  
    ggplot(aes(x = phrase_pos, y = value, colour = sig)) 
  q <- q + geom_point() 
  q <- q + geom_line() 
  q <- q + geom_smooth(method ="lm", color = "black") 
  q <- q + scale_colour_discrete(drop = FALSE)
  q <- q + facet_wrap(~key, scales = "free") 
  if(with_labels){
    q <- q + geom_text_repel(aes(label = ngram), size = 3) 
  }
  q <- q + get_default_theme(keep_legend = T)
  q <- q + labs(x = "Phrase position", y = "Value", title = sprintf("Phrase: %s (%s)", phrase_id, mel))
  q
}
get_all_correlations <- function(data = wjd_all, 
                                 phrase_N = 5, 
                                 easiness_measures = c("combined_easiness", "z_surprise"),
                                 method = "pearson", 
                                 max_pos = 15, 
                                 size = 1000,
                                 seed = 666,
                                 phrase_ids = NULL){
  #browser()
  set.seed(seed)
  data <- data %>% filter(phrase_len >= max_pos + phrase_N - 1,
                          N == phrase_N) 
  if(is.null(phrase_ids)){                          
    phrase_ids <- tibble(phrase_ids = unique(data$g_phrase_id)) %>% sample_n(size) %>% pull(phrase_ids)
  }
  printf("N = %d, size = %d", phrase_N, length(phrase_ids))
  phrase_cors <- map_dfr(phrase_ids, function(p_id) {
    #browser()
    d <- data %>% filter(g_phrase_id == p_id, 
                         phrase_pos <= max_pos)
    if(nrow(d)== 0){
      return(NULL)
    }
    cors <- 
      map_dfr(easiness_measures, function(y){
        #browser()
        fit <- my_cor_test(data = d, x = "phrase_pos", y = y, method = method, remove_outliers = F) %>% 
          select(estimate, p.value, method, easiness = y)
        #f <- as.formula(sprintf("%s ~ phrase_pos", y))
        #fit <- lm(formula = f, data = d) %>% 
        #  broom::tidy() %>% 
        #  mutate(N = phrase_N, max_pos = max_pos, easiness = y) %>% 
        #  filter(term != "(Intercept)") %>% 
        #  select(-term)
        fit
      })

    cors$phrase_id <- p_id 
    cors
    })  
  
  phrase_cors$N <- phrase_N
  phrase_cors$max_pos <- max_pos
  phrase_cors
}

get_next_val <- function(data, val){
  candidates <- data %>% filter(value == val)
  if(nrow(candidates) == 0){
    return(NA)
  } 
  if(all(is.na(candidates$next_val))){
    return(NA)
  }
  val <- candidates %>%  sample_n(1) %>% pull(next_val) 
  i <- 0
  while(is.na(val) && i < 10){
    i <- i +1
    val <- candidates %>%  sample_n(1) %>% pull(next_val) 
  }
  val
}

generate_phrase <-function(data, max_pos = 15){
  ret <- character(max_pos)
  ret[1] <- sample(unique(data$value), 1)
  for(i in 2:max_pos){
    #browser()
    next_val <- get_next_val(data, ret[i-1])
    if(is.na(next_val)){
      return(NULL)
    }   
    ret[i] <- next_val
  }
  print("Generated")
  print(ret)
  ret
}
simulate_phrase <-function(data = wjd_all, phrase_N = 5, max_pos = 15, size = 10, seed = 666, as_correlation = F, easiness = "combined_easiness"){
  set.seed(seed)
  ngram_features <- data %>% distinct(value, .keep_all = T)
  data <- 
    data %>% 
    filter(N == phrase_N, phrase_len >= max_pos  + phrase_N) %>% 
    group_by(g_phrase_id) %>%
    arrange(phrase_pos) %>% 
    mutate(next_val = lead(value)) %>% 
    ungroup() %>% 
    filter(in_phrase, !is.na(next_val)) 
  browser()
  sim <- 
    map_dfr(1:size, function(s){
      phrase <- generate_phrase(data, max_pos)
      if(is.null(phrase)){
        return(NULL)
      } else {
        tibble(g_phrase_id = s, value = phrase, phrase_pos = 1:max_pos)  
      }
  })
  easiness_measures <- as.vector(easiness_measures)
  sim <- sim %>% left_join(ngram_features %>% select(value, easiness_measures, N, freq), by = "value")
  sim$in_phrase <- TRUE
  if(as_correlation){
    sim <- 
      map_dfr(unique(sim$g_phrase_id), function(s){
      my_cor_test(sim %>% filter(g_phrase_id == s), x = "phrase_pos", y = easiness)
    })  
  }
  sim
}
get_phrase_correlations <- function(data = wjd_all, 
                                    easiness = "combined_easiness", 
                                    max_pos = 15, 
                                    phrase_N = 5, 
                                    size = 1000, phrase_ids = NULL){
  set.seed(666)
  
  d <- data %>% filter(N == phrase_N, phrase_pos <= max_pos, phrase_len >= max_pos + phrase_N) 
  if(is.null(phrase_ids)){
    phrase_ids <- sample(unique(d$g_phrase_id), size)  
  }
  phrase_cor <- 
    map_dfr(phrase_ids, function(x) {
      tmp <- d %>% filter(g_phrase_id == x)
      if(nrow(tmp)== 0) return(NULL)
      my_cor_test(tmp, x = "phrase_pos", y = easiness)
    })
  return(list(phrase_cor = phrase_cor, phrase_ids = phrase_ids))
}
get_phrase_correlations_diff <- function(data = wjd_all, 
                                    easiness = "combined_easiness", 
                                    max_pos = 15, 
                                    phrase_N = 5, 
                                    size = 1000, phrase_ids = NULL){
  set.seed(666)
  d <- data %>% filter(N == phrase_N, phrase_pos <= max_pos, phrase_len >= max_pos + phrase_N) 
  if(is.null(phrase_ids)){
    phrase_ids <- sample(unique(d$g_phrase_id), size)  
  }
  d$easiness <- as.data.frame(d)[, easiness]
  phrase_cor <- 
    map_dfr(phrase_ids, function(x) {
      #browser()
      
      tmp <- 
        d %>% 
        filter(g_phrase_id == x) %>% 
        arrange(phrase_pos) %>% 
        mutate(easiness = c(diff(easiness), NA)) %>%
        filter(!is.na(easiness))
      if(nrow(tmp)== 0) return(NULL)
      #print(x)
      #print(my_cor_test(tmp, x = "phrase_pos", y = "easiness"))
      #print(my_cor_test(tmp, x ="phrase_pos", y = "combined_easiness"))
      #print(t.test(tmp$easiness))
      ret <- t.test(tmp$easiness) %>% broom::tidy() %>% mutate(d = statistic /sqrt(nrow(tmp) - 1) )
      #my_cor_test(tmp, x = "phrase_pos", y = "easiness")
    })
  return(list(phrase_cor = phrase_cor, phrase_ids = phrase_ids))
}

compare_single_phrase_correlations <- function(data = wjd_all,
                                               simul_data = wjd_simul,
                                               easiness = "combined_easiness", 
                                               max_pos = 15, 
                                               phrase_N = 5, 
                                               size = 1000, 
                                               seed = 666, 
                                               with_plot = F){
  set.seed(seed)
  #sim_cor <- simulate_phrase(data = data, 
  #                           size = 2 * size, 
  #                           as_correlation = T, 
  #                           seed = seed, 
  #                           easiness = easiness) %>% sample_frac(.5)
  #browser()
  tmp <- 
    get_phrase_correlations(data = data, easiness = easiness, max_pos = max_pos, phrase_N = phrase_N, size = size)
  phrase_cor <- tmp$phrase_cor
  sim_cor <-  get_phrase_correlations(data = simul_data, 
                                      easiness = easiness, 
                                      max_pos = max_pos, 
                                      phrase_N = phrase_N, 
                                      size = size, 
                                      phrase_ids = tmp$phrase_ids)$phrase_cor

  comb_cor <- 
    bind_rows(sim_cor %>% mutate(type = "Simulated WJD"), phrase_cor %>% mutate(type = "WJD")) %>% 
    mutate(type = fct_rev(factor(type))) %>% 
    filter(!is.na(p.value)) %>% 
    mutate(sig_05 = p.value < .05, 
           sig_01 = p.value < .01, 
           sig_001 = p.value < .001, 
           easy_type = factor(estimate <= 0, labels = c("Easy First", "Easy Last"))) 
  if(with_plot){
    q <-  comb_cor %>% ggplot(aes(x = type, y = estimate, fill = type)) 
    q <- q + geom_boxplot(width = .5) 
    q <- q + geom_jitter(colour = "black", alpha = .2, width = .1) 
    q <- q + geom_violin(width = .5, alpha = .1)
    #q <- q + scale_color_manual(values = c("black", "orange"), name = "Significance", labels = c("n.s.", "p<.05"))
    q <- q + scale_fill_discrete(guide = F)
    #q <- q + get_default_theme(keep_legend = T)
    q <- q + theme_minimal(base_size = 12)
    q <- q + theme(strip.text = element_text(size = round(default_text_size*.75), hjust=0))
    q <- q + theme(panel.border=element_blank())
    q <- q + theme(panel.grid.major	= element_line(colour=default_grid_color, size=.3))
    q <- q + theme(panel.grid.minor	= element_blank())
    q <- q + theme(text = element_text(size=default_text_size))
    
    easiness_label <- get_easiness_label(easiness)
    q <- q + labs(x = sprintf("%s (N = %d, size = %d)", easiness_label, phrase_N, size), y = sprintf("Mean difference", easiness))

    print(q)
  }
  ks_df <- ks.test(sim_cor$estimate, phrase_cor$estimate) %>% broom::tidy()
  lm_df <- lm(estimate~type + easy_type, data = comb_cor) %>% broom::glance()
  tt_df <- t.test(estimate ~ type, data = comb_cor) %>% broom::glance()
  #browser()
  stat_05 <- comb_cor %>% group_by(type, easy_type) %>% summarise(mean_estimate = mean(estimate), perc_sig = mean(sig_05), BF = perc_sig/.025) %>% mutate(alpha = ".05", N = phrase_N, size = size) %>% ungroup()
  stat_01 <- comb_cor %>% group_by(type, easy_type) %>% summarise(mean_estimate = mean(estimate), perc_sig = mean(sig_01), BF = perc_sig/.005) %>% mutate(alpha = ".01", N = phrase_N, size = size) %>% ungroup() 
  stat_001 <- comb_cor %>%  group_by(type, easy_type) %>% summarise(mean_estimate = mean(estimate), perc_sig = mean(sig_001), BF = perc_sig/.0005) %>% mutate(alpha = ".001", N = phrase_N, size = size) %>% ungroup()
  stats <- bind_rows(stat_05, stat_01, stat_001) %>% mutate(easy_type = as.character(easy_type)) %>% arrange(desc(alpha), easy_type, type)
  list(data = comb_cor, ks = ks_df, lm = lm_df, tt = tt_df, stats = stats)
}


mif <- function(x, d){
  require(infotheo)
  l <- length(x)
  if(d >= l-1 || d < 1){
    return(NA)
  }
  base_idx <- seq(1, l, d)
  shift_idx <- seq(1 + d, l, d)
  base_idx <- base_idx[base_idx <= l-d]
  shift_idx <- shift_idx[shift_idx  <=l]
  #print(base_idx)
  #print(shift_idx) 
  #browser()
  parts <- 
    map_dfr(seq(0, d-1), function(i){
      tibble(x1 = x[base_idx + i], x2 = x[shift_idx + i], i = i) 
  }) %>% filter(!is.na(x1), !is.na(x2))
  ret <- NA
  if(nrow(parts) >0 ){
    ret <- mutinformation(parts$x1, parts$x2, method = "sg")
  }
  ret
}

get_ngrams_from_vector <- function(x, idx, N){
  l <- length(x)
  idx <- idx[idx > 0]
  idx <- idx[idx + N < l]
  if(length(idx) == 0){
    return(character(0))
  }
  #printf("Getting %d-grams for %s", N, paste0(idx, collapse = ","))
  tmp <- 
    map_chr(idx, function(i){
    if(i + N -1 > l){
      return("")
    }
    paste0(x[i:(i + N - 1)],  collapse = "," )
  })
  #browser()
  tmp[nchar(tmp) > 0]
}

mif_ngram<- function(x, N, d){
  require(infotheo)
  l <- length(x)
  if(d >= l-1 || d < 1){
    return(NA)
  }
  base_idx <- seq(1, l, d)
  shift_idx <- seq(1 + d, l, d)
  base_idx <- base_idx[base_idx <= l-d]
  shift_idx <- shift_idx[shift_idx  <=l]
  #print(base_idx)
  #print(shift_idx) 
  #browser()
  parts <- 
    map_dfr(seq(0, d-1), function(i){
      ngrams1 <- get_ngrams_from_vector(x, base_idx + i, N)
      ngrams2 <- get_ngrams_from_vector(x, shift_idx + i, N )
      l1 <- length(ngrams1)
      l2 <- length(ngrams2)
      #printf("Before l1 = %d, l2 = %d", l1, l2)      
      ngrams1 <- ngrams1[1:min(l1, l2)]
      ngrams2 <- ngrams2[1:min(l1, l2)]
      #l1 <- length(ngrams1)
      #l2 <- length(ngrams2)
      #printf("After l1 = %d, l2 = %d", l1, l2)      
      #print(ngrams1)
      if(length(ngrams1) != length(ngrams2)){
        return(tibble())
      }
      tibble(x1 = ngrams1, 
             x2 = ngrams2
      )
    }) %>% filter(!is.na(x1), !is.na(x2))
  ret <- NA
  if(nrow(parts) >0 ){
    ret <- mutinformation(parts$x1, parts$x2, method = "sg")
  }
  ret
}


get_all_mifs <- function(data, group = "id", vars = "int_raw", d_range = 1:10){
  
  map_dfr(vars, function(x) {
    var <- sym(x)
    mifs <- 
      map_dfr(d_range, function(d){
        messagef("Testing %s (%d)", x, d)
        if(nchar(group) > 0){
          group <- sym(group)
          data %>% group_by(!!group) %>% summarise(mif = mif(!!var, d)) %>% mutate(d = d)
        }
        else{
          data %>% summarise(mif = mif(!!var, d)) %>% mutate(d = d)
        }
    })
    mifs$type <- x
    mifs
  })
}

get_all_ngram_mifs <- function(data,  vars = "int_raw", d_range = 1:10, N_range = 2:5){
    
  map_dfr(vars, function(x) {
    var <- sym(x)
    mifs <- 
      map_dfr(d_range, function(d){
        map_dfr(N_range, function(n){
          messagef("Testing %s (d = %d, N = %d) ", x, d, n)
          data %>% summarise(mif = mif_ngram(!!var, n, d)) %>% mutate(d = d, N = n)
        })
      })
    mifs$type <- x
    mifs
  })
}

ngram_mif_by_position <- function(data, ids, phrase_pos1 = 1, phrase_pos2 = 2, ngram_N = 2){
  #print(data %>% filter(id %in% id1, N == ngram_N ) %>% pull(value))
  max_len <- max(phrase_pos1, phrase_pos2) + ngram_N
  #print(max_len)
  data <- data %>% filter(id %in% ids, phrase_len >= max_len, N == ngram_N )
  val1 <- data %>% filter(phrase_pos == phrase_pos1) %>% pull(value)
  val2 <- data %>% filter(phrase_pos == phrase_pos2) %>% pull(value)
  mif0 <- mutinformation(val1, sample(val1, replace = T))
  mif <- mutinformation(val1, val2) 
  tibble(id = length(ids), mif = mif, mif0 = mif0, z_mif = mif - mif0, phrase_pos1 = phrase_pos1, phrase_pos2 = phrase_pos2, N = ngram_N)
}

entire_phrase_easiness <- function(data){
  phrase_ids <- unique(data$g_phrase_id)
  ret <-
    map_dfr(phrase_ids, function(x) {
      #browser()
      melid <- as.integer(strsplit(x, "_")[[1]][1])
      phrase_no <- as.integer(strsplit(x, "_")[[1]][2])
      int_vec <- data %>% filter(g_phrase_id == x) %>% filter(!is.na(int_raw)) %>% pull(int_raw) 
      tmp <- int_vec %>% paste(collapse = ",") %>% n_gram_easiness()
      tmp$phrase_id <- x
      tmp$phrase_no <- phrase_no
      tmp$melid <- melid
      tmp$phrase_len <- int_vec %>% length()
      tmp
      })
  
  ret$combined_easiness <- as.vector(scale(ret$int_variety) + scale(ret$pitch_variety) + scale(ret$dir_change) + scale(ret$mean_run_length))
  ret$z_combined_easiness <- as.vector(scale(ret$combined_easiness))
  ret <- ret %>%  group_by(melid) %>% 
    mutate(phrase_count = max(phrase_no), 
           d_easiness = c(diff(z_combined_easiness), NA), 
           sgn_d_easiness = sign(d_easiness)) %>% 
    ungroup() 
  ret <- ret %>% mutate(s_e_1 = lead(sgn_d_easiness,1), 
                 s_e_2 = lead(sgn_d_easiness,2)) %>% 
    #filter(!is.na(s_e_1), !is.na(s_e_2), !is.na(sgn_d_easiness)) %>% 
    mutate(s_trigrams = sprintf("%s,%s,%s", sgn_d_easiness, s_e_1, s_e_2), 
           s_bigrams = sprintf("%s,%s", sgn_d_easiness, s_e_1))
  #browser()
  ret$s_bigrams[grep("NA", ret$s_bigrams)] <- NA
  ret$s_trigrams[grep("NA", ret$s_trigrams)] <- NA
  ret
}

test_phrase_easiness_sequence <- function(easiness_dist, n = 1000, m = 10){
  values <- sample(easiness_dist, size = n, replace = T)  
  d_values <- diff(values)
  s_d_values <- sign(d_values) 
  s_d_values_lag1 <- lead(s_d_values)
  bigrams <- sprintf("%s,%s", s_d_values[!is.na(s_d_values_lag1)], s_d_values_lag1[!is.na(s_d_values_lag1)])
  tibble(values = values[1:(length(values)-2)], 
         d_values = d_values[!is.na(s_d_values_lag1)], 
         s_d_values = s_d_values[!is.na(s_d_values_lag1)],
         s_d_values_lag1 = s_d_values_lag1[!is.na(s_d_values_lag1)],
         bigrams = bigrams)
}

global_correlation_analysis <- function(data, 
                                       N_range = 3:10, 
                                       easiness_measures = easiness_measures, 
                                       remove_outlier = F){
  l <- length(easiness_measures)
  map_dfr(1:l, function(i){
    map_dfr(1:l, function(j){
      if(j == i ){
        return(NULL)
      }
      em1 <- as.vector(easiness_measures[i])
      em2 <- as.vector(easiness_measures[j])
      map_dfr(N_range, function(k){
        data %>% 
          filter(N == k) %>% 
          my_cor_test(em1, em2, remove_outliers = F) %>% 
          mutate(N = k)})
      })
  })
}

make_combined_df <- function(){
  tmp1 <- wjd_tmp %>% 
    #select(id, start, N, value, phrase_pos, rev_phrase_pos, combined_easiness, surprise, z_surprise, in_phrase, freq, g_phrase_id) %>% 
    mutate(type = "wjd") 
    
  tmp2 <- wjd_simul2 %>% 
    #select(id, start, N, value, phrase_pos, rev_phrase_pos, combined_easiness, surprise, z_surprise, in_phrase, freq, g_phrase_id) %>% 
    mutate(type = "simul") 
  bind_rows(tmp1, tmp2)
}

get_event_densities <-function(solo_data, window_size = 5, hop_ratio = .5){
  solo_data <-  solo_data %>% mutate(onset = onset - min(onset))
  max_t <- max(solo_data$onset)
  if(hop_ratio <= 0 || hop_ratio > 1){
    stop(sprintf("Invalid hop ratio: %f", hop_ratio))
  }
  hop_size = window_size * hop_ratio 
  num_windows <- max_t/hop_size
  window_starts <- (1:num_windows ) * hop_ratio 
  t <- 0
  ret <- list()
  while ( (t  + window_size) < max_t){
    tmp <- solo_data %>% 
      filter(onset >= t, onset < (t + window_size)) %>%
      summarise(dur = diff(range(onset)), 
                n = n(), 
                event_density = n/dur) %>% 
      mutate(win_t = t)
    tmp[!is.finite(tmp$event_density),] <- NA
    ret[[as.character(t)]] <- tmp
    t <- t + hop_size
  }
  bind_rows(ret)
}

get_event_densities_range <- function(solo_data, window_range = c(1, 2, 3, 4, 5), hop_range = c(.25, .5, .75, 1)){
  map_dfr(window_range, function(ws){
    map_dfr(hop_range, function(hr){
      get_event_densities(solo_data, ws, hr) %>% mutate(window_size = ws, hop_ratio = hr)
    })
  })
}
get_event_densities_range_for_solos <- function(data, window_range = c(1, 3, 5,  7, 9, 15, 20), hop_range = c(.5)){
 ids <- data %>% pull(id) %>% unique()
 map_dfr(ids, function(i){
   get_event_densities_range(data %>% filter(id == i), window_range, hop_range) %>% 
     group_by(window_size, hop_ratio) %>% 
     summarise(m = mean(event_density, na.rm = T), s = sd(event_density, na.rm = T)) %>% 
     mutate(id = i) %>% 
     select(id, everything())
 })
}
get_mean_phrase_pos <- function(data, group  = "type", min_N = 3, min_freq = 100, max_pos = 30){
  data <- data %>% filter(N >= min_N, freq >= min_freq, phrase_pos <= max_pos, in_phrase)
  values <- unique(data$value)
  data %>% 
    group_by(value, !!sym(group), N) %>% 
    summarise(m_pos = mean(phrase_pos), med_pos = median(phrase_pos), difficulty = mean(z_surprise)) %>% 
    ungroup() %>% 
    arrange(N, value, difficulty, type)
  
}

get_distribution_stats <- function(data, min_N = 3, max_N = 10, max_pos = 10, easiness = "combined_easiness"){
  stats <- data %>% 
    group_by(N, phrase_pos) %>% 
    summarise(m = mean(!!sym(easiness)), 
              med = median(!!sym(easiness)), 
              skew = moments::skewness(!!sym(easiness)),
              n = n()) %>% 
    ungroup() 
  
  stats_all <- data %>% 
    group_by(N) %>% 
    summarise(m = mean(!!sym(easiness)), 
              med = median(!!sym(easiness)), 
              skew = moments::skewness(!!sym(easiness)),
              n = n()) %>% 
    mutate(phrase_pos = 0) %>% 
    ungroup() 
  bind_rows(stats, stats_all)
}

plot_distribution <- function(data, min_N = 3, max_N = 10, max_pos = 1000, easiness = "combined_easiness"){
  data <- data %>% filter(N >= min_N, N <= max_N, phrase_pos <= max_pos)
  stats <- get_distribution_stats(data)
  q <- data %>% ggplot(aes(!!sym(easiness), ..density..)) 
  q <- q + geom_histogram(fill = "white", color = "black") 
  q <- q + geom_density(fill = "lightblue3", alpha = .5, adjust = 2)
  #q <- q + facet_grid(phrase_pos ~ N, scales = "free_y" )
  q <- q + facet_wrap( ~ N, scales = "free" )
  q <- q + get_default_theme()
  q <- q + theme(panel.grid.major.x = element_blank())
  #q <- q + ggthemes::theme_tufte()
  q <- q + geom_vline(data = stats %>% filter(phrase_pos == 0), aes(xintercept = m), size = 1, color = "black")
  q <- q + geom_vline(data = stats %>% filter(phrase_pos == 0 ), aes(xintercept = med), size = 1, color = "black", linetype = "dotted")
  q <- q + labs(x = get_easiness_label(easiness), y = "Density")
  
  #s <- stats %>% ggplot(aes(x = factor(phrase_pos), y = med))
  #s <- s + geom_point()
  #s <- s + facet_wrap(~N)
  #s <- s + get_default_theme()
  q
}

get_longest_pattern <- function(data, min_N = 3, max_pos = 30){
  data <- data %>% 
    filter(in_phrase, N >= 3, phrase_pos <= 30, freq > 1) %>% 
    mutate(pos_id = sprintf("%s_%s", g_phrase_id, phrase_pos))
  #browser()
  tmp <- data %>% group_by(pos_id) %>% mutate(best  = (N == max(N)))  %>% ungroup()
  tmp <- tmp %>% filter(best) %>% select(-best)
  return(tmp)
  pos_ids <- unique(data$pos_id)
  map_dfr(pos_ids, function(pi){
    
    tmp <- data %>% filter(pos_id == pi, freq > 1) 
    ret <- NULL
    browser()
    if(nrow(tmp)){
      ret <- tmp %>% filter(N == max(N)) %>% select(g_phrase_id, phrase_pos, N, freq, z_surprise, combined_easiness)
    }
    ret
  })
}
add_mla_annotations <- function(data, mla_filename = "c:/MeloSpyGUI/analysis/feature+viz/wjd_mla.csv"){

  wjd_mla <- read.csv(mla_filename, sep = ";", stringsAsFactors = F) %>% 
    as_tibble() %>% 
    mutate(melid = as.integer(factor(id)),
           g_phrase_id = sprintf("%s_%s", melid, phrase_id_raw)) %>% 
    group_by(g_phrase_id) %>% 
    mutate(phrase_pos = 1:n()) %>% 
    ungroup() %>% 
    filter(melid != 194) 
  
  mlas <- wjd_mla %>% 
    filter(!is.na(MLA_main_type))
  
  expanded_MLAs <- 
    map2_dfr(mlas$MLA_main_type, mlas$MLA_length, function(x, y){
      if(y > 0)
        tibble(MLA_main_type = rep(x, y), MLA_length = y)
    })
  #browser()
  ret <- wjd_mla %>% 
    select(melid, g_phrase_id, phrase_pos) %>% 
    bind_cols(expanded_MLAs) %>% 
    group_by(melid, g_phrase_id, MLA_main_type) %>% 
    mutate(MLA_pos = 1:n()) %>% 
    ungroup()
  data %>% left_join(ret, by = c("melid", "g_phrase_id", "phrase_pos"))
}