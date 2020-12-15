library(cowplot)

source("analysis.R")
source("easy_util.R")
#source("ui.R")

flatten_corr_matrix <- function(hmisc_cor) {
  cor_mat <- hmisc_cor$r
  p_mat <- hmisc_cor$P
  n_mat <- hmisc_cor$n
  ut <- upper.tri(cor_mat)
  data.frame(
    x = rownames(cor_mat)[row(cor_mat)[ut]],
    y = rownames(cor_mat)[col(cor_mat)[ut]],
    r  =(cor_mat)[ut],
    p = p_mat[ut],
    n_obs = n_mat[ut]
  )
}

get_min_degree <- function(data, criterion = "AIC", threshold = -5){
  Ns <- unique(data$N)
  max_pos_range <- unique(data$max_pos)
  criterion <- sym(criterion)
  map_dfr(Ns, function(x) 
    map_dfr(max_pos_range, function(y){
      data %>% 
        filter(N == x, max_pos == y) %>% 
        arrange(degree) %>% 
        mutate(d_IC = c(NA, diff(!!criterion))) %>% 
        #filter(d_IC == min(d_IC, na.rm = T)) %>% 
        filter(d_IC < threshold | d_IC == min(d_IC, na.rm = T))
    }))
}

compare_regressions <- function(data, min_N = 3, 
                                max_pos_range = seq(10, 50, 10), standardize = T, 
                                aggregate = TRUE, 
                                easiness = "combined_easiness", 
                                degrees = 1:5){
  map_dfr(max_pos_range, function(max_pos){
    map_dfr(degrees, function(degree){
      tmp <- get_regressions(data, 
                             min_N = min_N, 
                             max_pos = max_pos, 
                             standardize = standardize, 
                             aggregate = aggregate, 
                             easiness = easiness, 
                             degree = degree)
      tmp$max_pos <- max_pos
      tmp$degree <- degree
      tmp
    })
  })  
}

make_poly_degree_table <- function(max_pos = 30, 
                                   p_thresh = .01, 
                                   d_AIC = -10, 
                                   easiness_measures = c("combined_easiness", "z_surprise"), 
                                   MLA_types = c("lick", "line")){
  map_dfr(easiness_measures, function(em){
    map_dfr(MLA_types, function(mt){
      compare_regressions(wjd_inphrase %>% filter(MLA_main_type == mt),
                          max_pos_range = max_pos, 
                          easiness = em) %>% 
        get_min_degree("AIC", d_AIC) %>%
        group_by(N, max_pos) %>% 
        filter(adj.r.squared == max(adj.r.squared), 
               max_pos == max_pos, 
               p.value < p_thresh) %>% 
        ungroup() %>% 
        mutate(type = mt, easiness = em)  
      
    })
  })%>% 
    select(easiness, type, N, degree, p.value, adj.r.squared, AIC,  -max_pos) %>% 
    mutate_if(is.numeric, round, 3)
}

make_single_phrase_correlations <-function(data = wjd_all, simul_data = wjd_simul, 
                                           max_pos = 15, 
                                           size = 1000, 
                                           easiness_measure = "combined_easiness", 
                                           recalc = T){
  if(length(grep("p_cor", x  = ls(envir = globalenv()))) == 0){
    recalc = T
  }
  if(recalc){
    data <- data %>% filter(phrase_len >= max_pos + 9) 
    phrase_ids <- unique(data$g_phrase_id) %>% sample(size)
    p_cor_all <- map_dfr(3:10, function(x){
      get_all_correlations(data = data, 
                           phrase_N = x, 
                           easiness_measure = easiness_measure, 
                           method = "spearman", 
                           phrase_ids = phrase_ids)
    }) 
    p_cor_all_simul <- map_dfr(3:10, function(x){
      get_all_correlations(data = simul_data, 
                           phrase_N = x, 
                           easiness_measure = easiness_measure, 
                           method = "spearman", 
                           phrase_ids = phrase_ids)
    }) 
    p_cor_all <- p_cor_all %>% mutate(type = "wjd")
    p_cor_all_simul <- p_cor_all_simul %>% mutate(type = "sim")
    p_cor <- bind_rows(p_cor_all, p_cor_all_simul) %>% mutate(N = factor(N))
    assign("p_cor_all", p_cor_all, globalenv())
    assign("p_cor_all_simul", p_cor_all_simul, globalenv())
    assign("p_cor", p_cor, globalenv())
  }
  #wjd_ids <- unique(p_cor_all$phrase_id) %>% sample(size)
  #simul_ids <- unique(p_cor_all_simul$phrase_id) %>% sample(size)
  #return(p_cor)
  fit <- lm(estimate ~ N * type, data = p_cor) 
  #print(fit %>% broom::tidy())
  #print(fit %>% broom::glance())
  require(yarrr)
  q <- yarrr::pirateplot(formula  =  `Spearman's rho`  ~ type + N, 
                         data = p_cor %>% rename(`Spearman's rho` = estimate) %>% mutate(type = gsub("Simul", "sim", type)), 
                         theme = 1, inf.disp = "bean")
  return(q)
  q <- p_cor %>% 
    filter(easiness == easiness_measure) %>% 
    ggplot(aes(x = N, y = estimate, fill = type)) 
  q <- q + geom_boxplot(alpha = .5) 
  q <- q + geom_point(aes(colour = type), alpha = .1, position = position_dodge())
  q <- q + get_default_theme(default_text_size =  12, keep_legend = T)
  q <- q + labs(y = "Pearsons' correlation coefficient")
  q
}

plot_linear_betas <- function(data = wjd_inphrase_comb, 
                              MLA_main_types = c("lick", "line"),
                              easiness = "combined_easiness",
                              max_pos_range = 10:30, min_N = 3, 
                              p_thresh = .001,
                              alpha = 1){
  #browser()
  lb_wjd <- get_linear_betas(data = data %>% filter(type == "wjd"), 
                             min_N = min_N, 
                             easiness = easiness, 
                             MLA_main_type = MLA_main_types,
                             max_pos_range = max_pos_range) %>% mutate(type = "wjd")
  lb_simul <- get_linear_betas(data = data %>% filter(type == "simul"), 
                               min_N = min_N, 
                               easiness = easiness,
                               MLA_main_type = MLA_main_types,
                               max_pos_range = max_pos_range) %>% mutate(type = "simul")
  lb <- bind_rows(lb_wjd , lb_simul) %>% mutate(comb_type = sprintf("%s-%s", type, MLA_main_type))
  assign("lb", lb, globalenv())
  q <- lb %>% filter(p.value < p_thresh) %>% 
    ggplot(aes(x = max_pos, y = estimate)) 
  q <- q + geom_ribbon(aes(ymin = estimate-std.error, ymax = estimate + std.error, fill = type, group = comb_type), 
                       alpha = alpha) 
  q <- q + geom_line(aes(linetype = MLA_main_type, group = comb_type), colour = "black") 
  q <- q + facet_wrap(~N) 
  q <- q + get_default_theme(keep_legend = T)  
  q <- q + theme(legend.key.size = unit(0.5, "cm")) 
  q <- q + theme(legend.key.width = unit(1.0, "cm")) 
  q <- q + theme(legend.position = c(.85, .1), legend.background = element_rect(colour = "black")) 
  q <- q + geom_hline(yintercept = 0)
  q <- q + labs(x = "End Position", y = get_easiness_label(easiness))
  q
}

get_global_correlations <- function(data, N_range = 3:10, easiness_measures){
  ret <- map_dfr(N_range, function(k){
    data %>% 
      filter(N == k) %>% 
      select(easiness_measures) %>% 
      as.matrix() %>% 
      Hmisc::rcorr() %>% 
      flatten_corr_matrix() %>% 
      mutate(N = k)
  })
  ret %>% arrange(x, y, N)
}

get_effect_sizes <- function(data, max_pos = 30, easiness_measure = "combined_easiness"){
  easiness_measure <- sym(easiness_measure)
  se <- function(x) sd(x)/sqrt(length(x))
  tmp <- data %>% 
    filter(phrase_pos == 1, N >= 3) %>% 
    group_by(N) %>% 
    summarise(pos1 = mean(!!easiness_measure), .groups = "drop") %>% 
    ungroup() 
  tmp <- data %>% 
    filter(phrase_pos == max_pos, N >= 3) %>% 
    group_by(N) %>% 
    summarise(pos_last = mean(!!easiness_measure), .groups = "drop") %>% 
    ungroup() %>% 
    left_join(tmp)
  eff_sizes <- 
    data %>% 
    filter(phrase_pos <= max_pos, N >= 3) %>% 
    group_by(N, phrase_pos) %>%  
    summarise(m = mean(!!easiness_measure), .groups = "drop") %>% 
    ungroup() %>% 
    group_by(N) %>% 
    summarise(d = diff(range(m)), mean = mean(m), .groups = "drop") %>% 
    ungroup() %>% 
    left_join(tmp, by = "N")
  eff_sizes 
}

interval_between_phrase_comparisons <- function(fig_dir){
  ### intervals between phrases
  if(length(grep("wjd_all", x  = ls(envir = globalenv()))) == 0){
    return()
  }
  interval_in <- wjd_all %>% 
    filter(rev_phrase_pos != 1, N == 1) %>% 
    count(value)  %>% 
    mutate(freq = n/sum(n), type = "in phrases")
  interval_end <- wjd_all %>% 
    filter(rev_phrase_pos == 1, N == 1) %>% 
    count(value)  %>% 
    mutate(freq = n/sum(n), type = "between phrases")
  tmp <- 
    bind_rows(interval_in, interval_end) %>% 
    mutate(value = gsub("\\[", "", value)) %>% 
    mutate(value = as.numeric(gsub("\\]", "", value))) 
  q_interval_cmp <- tmp %>% ggplot(aes(x = value, y = freq, fill = type)) + 
    geom_col(colour = "black") + 
    facet_wrap(~type, ncol = 1) + 
    get_default_theme() + 
    scale_fill_manual(values = c( "#F2DFCE", "cadetblue3")) + 
    scale_y_continuous(labels=scales::percent) + 
    labs(x = "Interval", y = "Rel. Frequency")    
  save_plot(fig_dir, "interval_phrase_cmp.png", dpi = 600)
  mean_interval <- tmp %>% 
    group_by(type) %>% 
    summarise(mean_abs_int = sum(abs(value*freq)), .groups = "drop")
  
  q_diff_diff <- 
    wjd_all %>% 
    filter(N >= 3) %>% 
    group_by(N, in_phrase) %>% 
    summarise(extrinsic = mean(z_surprise), 
              intrinsic = mean(combined_easiness), .groups = "drop")  %>% 
    ungroup() %>% 
    mutate(in_phrase = factor(in_phrase, labels = c("between phrases", "in phrases")))  %>% 
    pivot_longer(cols = c(intrinsic, extrinsic)) %>% 
    ggplot(aes(x = factor(N), y = value, colour = in_phrase)) + 
    geom_point() + 
    geom_line(aes(group = in_phrase)) + 
    get_default_theme(keep_legend = T) + 
    labs(x = "N", y = "Mean Difficulty") + 
    facet_wrap(~name)
  save_plot(fig_dir, "inphrase_cmp_difficulties.png", dpi = 600)  
}

plot_phrase_position_count <- function(max_pos = NULL){
  tmp <- wjd_inphrase %>% 
    filter(phrase_len <= 200, N == 1, !is.na(MLA_main_type)) %>% 
    count(MLA_main_type, phrase_pos) %>% 
    bind_rows(
      wjd_inphrase %>% 
        filter(phrase_len <= 200, N == 1) %>% 
        count(phrase_pos) %>% 
        mutate(MLA_main_type = "all")
    )
  if(!is.null(max_pos)){
    q <- tmp %>%
      filter(phrase_pos <= max_pos) %>%   
      ggplot(aes(x = phrase_pos, y = n)) 
    
  }
  else{
    q <- tmp %>%   
      ggplot(aes(x = phrase_pos, y = n)) 
  }
  q <- q + geom_line(size = 1) 
  q <- q + get_default_theme() 
  q <- q + labs(x = "Phrase position", y = "Count") 
  q <- q + scale_y_log10(limits = c(1, 11400)) 
  q <- q + geom_smooth(method = "lm", se = F, color = "indianred") 
  q <- q + facet_wrap(~MLA_main_type) 
  q
}

MLA_pirate_plots <- function(fig_dir){
  messagef("### diffculty pirate plots over MLA")
  ## diffculty pirate plots over MLA
  q_ce <- yarrr::pirateplot(formula  =  combined_easiness ~ MLU + N, 
                            data = wjd_inphrase %>% filter(MLA_main_type %in% c("lick", "line"), 
                                                           N >= 3, 
                                                           abs(combined_easiness) < 3) %>% 
                              rename(MLU  = MLA_main_type), 
                            theme = 1, 
                            inf.disp = "bean", 
                            point.o = .005,
                            ylab = "Intrinsic Difficulty")
  q_ce <- recordPlot()
  save_png_plot(fig_dir, "pirate_intrinsic_lick_line.png", width = 8.89, height = 6.6, 
                units = "in", res = 600)
  q_ce
  dev.off()
  q_zs <- yarrr::pirateplot(formula  =  z_surprise ~ MLU + N, 
                            data = wjd_inphrase %>% filter(MLA_main_type %in% c("lick", "line"), 
                                                           N >= 3, 
                                                           abs(z_surprise) < 3) %>% 
                              rename(MLU  = MLA_main_type), 
                            theme = 1, 
                            inf.disp = "bean", 
                            point.o = .005,
                            ylab = "Extrinsic Difficulty")
  q_zs <- recordPlot()
  save_png_plot(fig_dir, "pirate_extrinsic_lick_line.png", width = 8.89, height = 6.6, 
                units = "in", res = 600)
  q_zs
  dev.off()
  
  
}
get_plot_file_name <- function(fig_dir, file_name){
  file.path(fig_dir, file_name)
}
save_plot <- function(fig_dir, file_name, dpi = 600){
  ggsave(file.path(fig_dir, file_name), dpi = 600)
  
}
save_png_plot <- function(fig_dir, file_name, ...){
  png(file.path(fig_dir, file_name), ...)
}

new_paper_version <- function(fig_dir = "docs/paper/figs/new"){
  ##Load  wjd_inphrase, wjd_inphrase_simul, wjd_inphrase_comb if no present.
  if(length(grep("wjd_inphrase", x  = ls(envir = globalenv()))) == 0){
    browser()
    messagef("Loading data...")
    load("data/inphrase_data.rda",envir = globalenv())
  }
  ###### SI #######
  messagef("### phrase position counts")
  ### phrase position counts
  q <- plot_phrase_position_count()
  save_plot(fig_dir, "phrase_pos_by_MLA.png", dpi = 600)
  
  messagef("### difficulty distributions")
  ### difficulty distributions
  q_wjd_int <- plot_distribution(wjd_inphrase, max_N = 10, easiness = "combined_easiness")  + ggtitle("WJD")
  save_plot(fig_dir, "wjd_intrinsic_dist.png", dpi = 600)
  q_wjd_ext <- plot_distribution(wjd_inphrase, max_N = 10, easiness = "z_surprise")  + ggtitle("WJD")
  save_plot(fig_dir, "wjd_extrinsic_dist.png", dpi = 600)
  q_simul_int <- plot_distribution(wjd_inphrase_simul, max_N = 10, easiness = "combined_easiness")  + ggtitle("Simulated")
  save_plot(fig_dir, "simul_intrinsic_dist.png", dpi = 600)
  q_simul_ext <- plot_distribution(wjd_inphrase_simul, max_N = 10, easiness = "z_surprise")  + ggtitle("Simulated")
  save_plot(fig_dir, "simul_extrinsic_dist.png", dpi = 600)
  
  messagef("### rhythm and melody phrase plots ")
  ### rhythm and melody phrase plots 
  q_rhythm_melody_int <- plot_easiness_by_type_MLA(wjd_inphrase_comb, 
                                                   MLA_main_types = c("rhythm", "melody"), 
                                                   easiness = "combined_easiness") 
  #q_rhythm_melody_int <- q_rhythm_melody_int + scale_fill_manual(values = c( "#F2DFCE", "cadetblue3"))  
  save_plot(fig_dir, "phrase_plot_intrinsic_rhythm_melody.png", dpi = 600)
  q_rhythm_melody_ext <- plot_easiness_by_type_MLA(wjd_inphrase_comb, 
                                                   MLA_main_types = c("rhythm", "melody"), 
                                                   easiness = "z_surprise")
  #q_rhythm_melody_ext <- q_rhythm_melody_ext + scale_fill_manual(values = c( "#F2DFCE", "cadetblue3"))  
  save_plot(fig_dir, "phrase_plot_extrinsic_rhythm_melody.png", dpi = 600)
  
  messagef("### no MLA  phrase plots ")
  ### no MLA  phrase plots
  q_all_int <- plot_easiness_by_type(wjd_inphrase_comb, smooth = "none", plot_type = "ribbon", easiness = "combined_easiness")
  save_plot(fig_dir, "phrase_plot_intrinsic_no_MLA.png", dpi = 600)
  q_all_ext <- plot_easiness_by_type(wjd_inphrase_comb, smooth = "none", plot_type = "ribbon", easiness = "z_surprise")
  save_plot(fig_dir, "phrase_plot_extrinsic_no_MLA.png", dpi = 600)
  
  #currently disabled  
  interval_between_phrase_comparisons()

  messagef("### effect sizes ")
  ### effect sizes 
  line_eff_int <- get_effect_sizes(wjd_inphrase %>% filter(MLA_main_type == "line")) %>% mutate(MLA_main_type = "line", easiness = "intrinsic")
  lick_eff_int <- get_effect_sizes(wjd_inphrase %>% filter(MLA_main_type == "lick")) %>% mutate(MLA_main_type = "lick", easiness = "intrinsic")
  line_eff_ext <- get_effect_sizes(wjd_inphrase %>% filter(MLA_main_type == "line"), easiness_measure = "z_surprise") %>% mutate(MLA_main_type = "line", easiness = "extrinsic")
  lick_eff_ext <- get_effect_sizes(wjd_inphrase %>% filter(MLA_main_type == "lick"), easiness_measure = "z_surprise") %>% mutate(MLA_main_type = "lick", easiness = "extrinsic")
  MLA_eff <- bind_rows(line_eff_int, lick_eff_int, line_eff_ext, lick_eff_ext)
  write.csv(MLA_eff %>% 
              mutate_if(is.numeric, round, 2) %>% 
              select(MLA_main_type, easiness, d, pos1, mean), file = "MLA_effect_size.csv", row.names = F)
  #temporarily disabled
  #MLA_pirate_plots(fig_dir)
  messagef("### correlations ")
  #### correlations
  corr_diff_wjd <- get_global_correlations(wjd_inphrase, 
                                           easiness_measures = c("int_variety", 
                                                                 "pitch_variety", 
                                                                 "dir_change", 
                                                                 "mean_int_size", 
                                                                 "int_range", 
                                                                 "mean_run_length", 
                                                                 "combined_easiness", "z_surprise"))
  corr_diff_simul <- get_global_correlations(wjd_inphrase_simul, 
                                             easiness_measures = c("int_variety", 
                                                                   "pitch_variety", 
                                                                   "dir_change", 
                                                                   "mean_int_size", 
                                                                   "int_range", 
                                                                   "mean_run_length", 
                                                                   "combined_easiness", "z_surprise"))
  corr_diff_ext_wjd <- corr_diff_wjd %>% 
    filter(x == "z_surprise" | y == "z_surprise") %>% 
    group_by(x, y) %>% 
    summarise(r = mean(r), .groups = "drop")
  corr_diff_ext_simul <- corr_diff_simul %>% filter(x == "z_surprise" | y == "z_surprise") %>% 
    group_by(x, y) %>% 
    summarise(r = mean(r), .groups = "drop")
  

  messagef("### phrase plots")
  ### phrase plots
  q_intrinsic <- plot_easiness_by_type_MLA(wjd_inphrase_comb, easiness = "combined_easiness") 
  save_plot(fig_dir, "phrase_plot_intrinsic_lick_line.png", dpi = 600)
  q_extrinsic <- plot_easiness_by_type_MLA(wjd_inphrase_comb, easiness = "z_surprise") 
  save_plot(fig_dir, "phrase_plot_extrinsic_lick_line.png", dpi = 600)
  
  messagef("### significant linear betas")
  #### significant linear betas
  q_beta_intrinsic <- plot_linear_betas(easiness = "combined_easiness", alpha = .5) 
  save_plot(fig_dir, "linear_beta_intrinsic_lick_line.png", dpi = 600)
  q_beta_extrinsic <- plot_linear_betas(easiness = "z_surprise" , alpha = .5) 
  save_plot(fig_dir, "linear_beta_extrinsic_lick_line.png", dpi = 600)
  
  messagef("### single phrase correlations pirate plots")
  ### single phrase correlations pirate plots

  q_cor_all_line  <- make_single_phrase_correlations(wjd_inphrase %>% filter(MLA_main_type == "line"), 
                                                     wjd_inphrase_simul %>% filter(MLA_main_type == "line"), 
                                                     recalc = F)
  q_cor_all_line <- recordPlot()
  png(file.path(fig_dir, "cor_all_intrinsic_line.png"),  width = 8.89, height = 6.6, 
      units = "in", res = 600)
  q_cor_all_line
  dev.off()
  q_cor_all_lick  <- make_single_phrase_correlations(wjd_inphrase %>% filter(MLA_main_type == "lick"), 
                                                     wjd_inphrase_simul %>% filter(MLA_main_type == "lick"), 
                                                     recalc = F)
  q_cor_all_lick <- recordPlot()
  save_png_plot(fig_dir, "cor_all_intrinsic_lick.png", width = 8.89, height = 6.6, 
      units = "in", res = 600)
  q_cor_all_lick
  dev.off()
  

}
