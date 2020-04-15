suppressWarnings(library(tidyverse))
suppressWarnings(library(ggplot2))
suppressWarnings(library(forcats))


base_feature_descriptions <- c(
  "abs_int_range"	= "Size of largest interval",
  "art_range"=	"Range of articulation",
  "cdpcx_density_1" = "Relative frequency of chordal roots",
  "cdpcx_density_10" = "Relative frequency of chordal sharp thirds",
  "cdpcx_density_3" = "Relative frequency of chordal thirds",
  "cdpcx_density_5" = "Relative frequency of chordal fifths",
  "cdpcx_density_6" = "Relative frequency of chordal sixths",
  "cdpcx_density_b2" = "Relative frequency of flat ninths",
  "cdpcx_density_T" = "Relative frequency of sharp elevenths",
  "durclass_abs_entropy" = "Entropy of absolute duration classes",
  "durclass_abs_hist_01_very_short" = "Relative frequency of very short tones (< 16th notes in 120 bpm)",
  "expressive" = "Relative frequency of expressive midlevel units",
  "f0_median_dev" = "Median deviation from nominal 12-TET pitch",
  "f0_abs_median_dev" = "Mean absolute median deviation from nominal 12-TET pitch",
  "f0_sd_median_dev" = "Standard deviation of median deviation from nominal 12-TET pitch",
  "fragment" = "Relative frequency of fragment midlevel units",
  "int_bigram_entropy" = "Entropy of interval combinations",
  "lick" = "Relative frequency of lick midlevel units",
  "line" = "Relative frequency of line midlevel units",
  "loudness_sd" = "Standard deviation of tone loudness",
  "pitch_entropy" = "Entropy of pitch distribution",
  "pitch_range" = "Pitch range",
  "pitch_std" = "Mean distance from mean pitch.",
  "rhythm" = "Relative frequency of rhythm midlevel units",
  "total_duration" = "Total duration of solo (in sec)",
  "void" = "Relative frequency of void midlevel units",
  "theme" = "Relative frequency of theme midlevel units",
  "quote" = "Relative frequency of quote midlevel units",
  "melody" = "Relative frequency of melody midlevel units"
)
#get_feature_description<-function(feature_name){
#  desc <- feature_descriptions[feature_name]
#  if(is.na(desc)){
#    desc <- "Sorry, no description available yet"
#  }
#  desc
#}
trim_var <- function(master, var, margin=.05){
  master <- master[!is.na(master[, var]),]
  if(margin <= 0){
    return(master)
  }
  q <- quantile(master[, var], seq(0, 1, margin), na.rm=T)
  lower <- q[2]
  upper <- q[length(q)-1]
  master[master[, var]>=lower & master[,var]<=upper,]
}
g_default_text_size<-12
default_grid_color <- "gray64"

get_dist_stats <- function(data, var_name){
  #browser()
  #data$variable <- as.data.frame(data)[, var_name]
  variable <- sym(var_name)
  stats <- 
    data %>% 
    filter(N >= 3) %>% 
    group_by(N) %>% 
    summarise(m = mean(!!variable, na.rm = T), 
              median = median(!!variable, na.rm = T), 
              sd = sd(!!variable, na.rm = T), 
              skewness = skewness(!!variable, na.rm = T))  
  stats$variable <- var_name
  as_tibble(stats)
}
get_default_theme <- function(x_rotate = 0, keep_legend = F, default_text_size = g_default_text_size){
  t <- theme_minimal()
  t <- t + theme(strip.text = element_text(size = round(default_text_size*.75), hjust=0))
  t <- t + theme(panel.border=element_blank())
  t <- t + theme(panel.grid.major	= element_line(colour=default_grid_color, size=.3))
  t <- t + theme(panel.grid.minor	= element_blank())
  t <- t + theme(text = element_text(size=default_text_size))
  t <- t + theme(axis.title.x = element_text(size = default_text_size, vjust = -.5))
  if (x_rotate != 0){
    t <- t + theme(axis.text.x = element_text(size=round(default_text_size *.85), angle=x_rotate, hjust=1))
  }
  else{
    t <- t + theme(axis.text.x = element_text(size=round(default_text_size *.85)))
    
  }
  t <- t + theme(plot.title = element_text(hjust=0))
  t <- t + theme(panel.spacing.x = unit(0.5, "cm"))
  t <- t + theme(panel.spacing.y = unit(0.5, "cm"))
  #t <- t + theme(legend.title=element_text(size=default_text_size))
  t <- t + theme(legend.title = element_blank())
  #t <- t + theme(legend.title.align=1)
  t <- t + theme(legend.text = element_text(size=round(default_text_size*.75)))
  if(!keep_legend){
    t <- t + theme(legend.position = "none")
  }
  t <- t + theme(legend.key.size=unit(0.5, "cm"))
  t <- t + theme(legend.key.width=unit(.1, "cm"))
  t
}

lm_eqn = function(m, degree) {
  return(lm_eqn_base(m))
  if(degree > 3){
    return(lm_eqn_base(m))
  }
  funs<-c(lm_eqn1, lm_eqn2, lm_eqn3)
  return(funs[degree][[1]](m))
}
lm_eqn_base = function(m) {
  
  l <- list(P = format(round(anova(m)$`Pr(>F)`[1], 3), digits=3, nsmall=3),
            aic = format(round(AIC(m), 3), digits=3, nsmall=3),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  eq <- substitute(italic(R)^2~"="~r2*","~~italic(p)~"="~P*","~~italic(AIC)~"="~aic,l)    
  
  as.character(as.expression(eq));                 
}

lm_eqn1 = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            P = format(round(anova(m)$`Pr(>F)`[1], 3), digits=3, nsmall=3),
            aic = format(round(AIC(m), 3), digits=3, nsmall=3),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2*","~~italic(p)~"="~P*","~~italic(AIC)~"="~aic,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(R)^2~"="~r2*","~~italic(p)~"="~P*","~~italic(AIC)~"="~aic,l)    
  }
  
  as.character(as.expression(eq));                 
}

lm_eqn2 = function(m) {
  
  l <- list(a0 = format(coef(m)[1], digits = 2),
            a1 = format(abs(coef(m)[2]), digits = 2),
            a2 = format(abs(coef(m)[3]), digits = 2),
            P = format(round(anova(m)$`Pr(>F)`[1], 3), digits=3, nsmall=3),
            aic = format(round(AIC(m), 3), digits=3, nsmall=3),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a0 + a2 %.% italic(x)*","~~italic(R)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a0 - a2 %.% italic(x)*","~~italic(R)^2~"="~r2,l)    
  }
  eq <- substitute(italic(y) == a0  + (a1) %.% italic(x)* + (a2) %.% italic(x)^2*","~~italic(R)^2~"="~r2*","~~italic(p)~"="~P*","~~italic(AIC)~"="~aic,l)
  as.character(as.expression(eq));                 
}

lm_eqn3 = function(m) {
  
  l <- list(a0 = format(coef(m)[1], digits = 2),
            a1 = format(coef(m)[2], digits = 2),
            a2 = format(coef(m)[3], digits = 2),
            a3 = format(coef(m)[4], digits = 2),
            P = format(round(anova(m)$`Pr(>F)`[1], 3), digits=3, nsmall=3),
            aic = format(round(AIC(m), 3), digits=3, nsmall=3),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(R)^2~"="~r2,l)    
  }
  eq <- substitute(italic(y) == a0 + (a1) %.% italic(x)* + (a2) %.% italic(x)^2* + (a3) %.% italic(x)^3*","~~italic(R)^2~"="~r2*","~~italic(p)~"="~P*","~~italic(AIC)~"="~aic,l)
  
  as.character(as.expression(eq));                 
}

quick_check<-function(data, 
                      var, 
                      x.axis = "recordingyear", 
                      y_lab = "", 
                      degree = 3, 
                      trim_prop = .00, 
                      annotate = T, 
                      se = F, 
                      aggregation = "",
                      residuals = F,
                      fit_color = default_color,
                      color_grouping = "",
                      point_size = 2,
                      alpha = .2,
                      hue = 40){
  if(is.character(data[, x.axis]) || is.factor(data[, x.axis])){
    data[, x.axis] <- as.numeric(as.character(data[, x.axis]))
  }
  #alpha <- .2
  if(residuals){
    f <- formula(sprintf("%s~%s", var, x.axis))
    data[, var] <- residuals(lm(f, data=data))
  }
  #point_size <- 2
  has_aggregation <- nchar(aggregation)
  if (has_aggregation){
    data <- data %>% 
      group_by_(x.axis) %>% 
      summarise_(tmp = sprintf("%s(%s)", aggregation, var)) %>% 
      as.data.frame()
    #print(master)
    data[, var] <- data$tmp
    data$tmp <- NULL
    #alpha <- 1
    #point_size <- 3
  }
  data <-trim_var(data,var, trim_prop)
  start <- min(data[, x.axis])
  end   <- max(data[, x.axis])
  tmp   <- data    
  f1    <- formula(paste("y", sprintf("poly(x, %d)", degree), sep="~"))
  tmp_aes <- aes_string(x=x.axis, y=var, colour="FALSE")
  has_color_grouping <- nchar(color_grouping) && toupper(color_grouping) != "NONE"
  has_performer_grouping <- FALSE
  if(has_color_grouping){
    has_performer_grouping <- length(levels(tmp[, color_grouping])) == 2
    if(has_performer_grouping){
      tmp$size_group <- as.numeric(tmp[, color_grouping])
      tmp_aes<-aes_string(x=x.axis, y=var, colour=color_grouping)
    }
    else{
      tmp_aes<-aes_string(x=x.axis, y=var, colour=color_grouping)
      #alpha<-1
    }
  }
  #tmp_aes<-aes_string(x=x.axis, y=var)
  q <- ggplot(tmp, tmp_aes) 
  q <- q + geom_point(alpha=alpha, size=point_size) 
  if(has_performer_grouping){
    q <- q + geom_point(data=tmp[tmp$size_group == 1,], size=point_size*1.25)
  }
  q <- q + geom_smooth(method="lm", formula = f1, aes(group=1), colour=fit_color, se=se) 
  f1 <- formula(paste(var, sprintf("poly(%s, %d)", x.axis, degree), sep="~"))
  m <- lm(f1, tmp)
  label1 <- lm_eqn(m, degree)
  dx <- diff(range(tmp[,x.axis]))
  dy <- diff(range(tmp[,var]))
  label2 <- paste("p=", format(anova(m)$`Pr(>F)`[1], digits=3), sep="") 
  #print(label2)    
  x.text <- start + .1*dx
  y.text <- min(tmp[, var], na.rm=T) - .15*dy
  #cat(x.text, y.text, "\n")
  if(annotate){
    q <- q + annotate("text", x=x.text, y=y.text, label=label1, parse=T, hjust=0) 
  }
  q <- q + scale_colour_hue(l=hue)
  if(has_color_grouping){
    if(has_performer_grouping){
      q <- q + scale_alpha_discrete(range=c(1,.2))
    }
    q <- q + get_default_theme(keep_legend=T)
  }
  else {
    q <- q + get_default_theme(keep_legend=F)
    q <- q + theme(legend.position="lower")
  }
  if(nchar(y_lab) == 0){
    y_lab <- var
  }    
  q <- q + labs(x="Recording Year", y=y_lab)

  #q<- q + geom_text(aes(x=start+.1*dx, y=min(tmp[, var], na.rm=T)+.1, label=label1, parse=T)) 
  return(q)
}

get_optimal_factors<-function(mydata, plot=F){
  require(nFactors)
  ev <- eigen(cor(mydata)) # get eigenvalues
  ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
                 rep=100,cent=.05)
  nS <- tryCatch(nScree(x=ev$values, aparallel=ap$eigen$qevpea), error=function(x) {list(Components=data.frame(noc=1, naf=1, nparallel=1, nkaiser=1))})
  if(plot){
    plotnScree(nS)
  }
  #print(nS$Components)
  return(nS$Components)
}
get_phrase_link <- function(phrase_id, phrase_N, phrase_max_pos = 15){
  ngrams <- 
    wjd_all %>% 
    filter(g_phrase_id == phrase_id, 
           N == as.integer(phrase_N),
           phrase_pos <= as.integer(phrase_max_pos) + as.integer(phrase_N) -1) %>% 
    arrange(phrase_pos) %>% 
    pull(value)
  #print(length(ngrams))
  ngram <- 
    map_int(1:length(ngrams), function(i) {
      pattern_to_int(ngrams[i])[1]
    }) %>% paste(collapse = ",")
  
  url <- sprintf("http://dig-that-lick.hfm-weimar.de/pattern_search/search?database_name=wjazzd&primary_pattern=%s&primary_transformation=interval&within_single_phrase=&generate_audio=True&generate_score=True", ngram)
  url
}
check_phrase <- function(phrase_id, phrase_len, phrase_N){
  if(length(phrase_id) > 1 ){
    ret <- map_chr(phrase_id, check_phrase, phrase_len, phrase_N)
    return(ret[nchar(ret)> 0])
  }
  pl <- wjd_all %>% filter(g_phrase_id == phrase_id) %>% pull(phrase_len) %>% unique()
  if(pl < (phrase_len + phrase_N -1 )){
    return("")
  }
  phrase_id
}
filter_phrases <- function(phrase_data, phrase_len, phrase_N, size = 50){
  #browser()
  good_phrases <- check_phrase(phrase_data %>% pull(phrase_id), phrase_len, phrase_N) 
  if(length(good_phrases) > size){
    good_phrases <- sample(good_phrases, size = size)
  }
  browser()
  #pl <- wjd_all %>% filter(g_phrase_id %in% good_phrases) %>% distinct(g_phrase_id, phrase_len) %>% select(phrase_id = g_phrase_id, phrase_len)
  phrase_data %>% filter(phrase_id %in% good_phrases) 
  #%>% left_join(pl, by = "phrase_id")
}

make_phrase_selection <- function(cor_data, easiness_measure = "combined_easiness", size = 50, r_thresh = .7, phrase_N = 5, phrase_len = 15){
  #browser()
  easy_first <- cor_data %>% filter(estimate > r_thresh, N == phrase_N, easiness == easiness_measure) %>% select(phrase_id, estimate, easiness, N)
  easy_last  <- cor_data %>% filter(estimate < -r_thresh, N == phrase_N, easiness == easiness_measure) %>% select(phrase_id, estimate, easiness, N)
  easy_flat  <- 
    cor_data %>% 
    filter(N >3, easiness == easiness_measure) %>% 
    group_by(phrase_id) %>% 
    mutate(max_r = max(abs(estimate))) %>% 
    ungroup() %>% 
    filter(max_r < .3, N == phrase_N ) %>% 
    distinct(phrase_id, .keep_all = T) %>% 
    select(phrase_id, estimate, easiness, N)
  
  easy_first <- filter_phrases(easy_first, phrase_len, phrase_N, size) 
  easy_last  <- filter_phrases(easy_last, phrase_len, phrase_N, size)
  #browser()
  easy_flat  <- filter_phrases(easy_flat, phrase_len, phrase_N, size)
  easy_first$type <- "easy_first"
  easy_last$type  <-  "easy_last"
  easy_flat$type  <-  "easy_flat"
  
  phrases_select <-bind_rows(easy_first, easy_last, easy_flat)
  phrases_select$thresh <- r_thresh

  #browser()
  phrases_select$url <- map_chr(phrases_select$phrase_id, get_phrase_link, phrase_N, phrase_len) 
  #browser()
  phrases_select
}