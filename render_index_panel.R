

render_sv_plots <- function(x,region, characteristic, axis_label,plot = FALSE, trend_plots = FALSE){
  characteristic <- character_id
  region <- region_def
  
  x <- lapply(x, function(x){
    
    x[,'PARAMETER'] <- x[,characteristic]
    x[,'REGION'] <- x[,region]
    return(x)
    
  })      
  
  df <- x$station_mean
  
  
  p.i <- df %>%
    ggplot(aes(x = parameter_status, y = PARAMETER, fill = stream_class)) +
    geom_boxplot(outlier.alpha = 0.25)+
    facet_grid(~REGION)+
    xlab(' ')+
    ylab(' ')+
    theme_bw()+
    scale_y_continuous(trans = log_trans(), breaks = log_breaks())+
    theme(legend.position = 'center',
          panel.grid.major = element_line(size = 0.1),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.border = element_rect(fill = "transparent",
                                      color = "black")) +
    ggtitle('All Seasons:Station-Year Means: All Programs')
  
  p.ii <- df %>%
    filter(!is.na(og_status)) %>% 
    ggplot(aes(x = parameter_status, y = PARAMETER, fill = stream_class)) +
    geom_boxplot(outlier.alpha = 0.25)+
    facet_grid(~REGION)+
    xlab(' ')+
    ylab(axis_label)+
    theme_bw()+
    scale_y_continuous(trans = log_trans(), breaks = log_breaks())+
    theme(legend.position = 'center',
          panel.grid.major = element_line(size = 0.1),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.border = element_rect(fill = "transparent",
                                      color = "black")) +
    ggtitle('All Seasons:Station-Year Means: MBI Development Stations')
  
  p.iii <- df %>%
    filter(str_detect(project_name, pattern = 'REF|REFERENCE|Ref')) %>% 
    ggplot(aes(x = parameter_status, y = PARAMETER, fill = stream_class)) +
    geom_boxplot(outlier.alpha = 0.25)+
    facet_grid(~REGION)+
    xlab('Benthic Macro Parameter Status')+
    ylab(' ')+
    theme_bw()+
    scale_y_continuous(trans = log_trans(), breaks = log_breaks())+
    theme(legend.position = 'center',
          panel.grid.major = element_line(size = 0.1),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.border = element_rect(fill = "transparent",
                                      color = "black")) +
    ggtitle('All Seasons:Station-Year Means: All Reference Stations')
  
  j_key <- tibble(bioregion = c('MV', "PR", "BG", "MT"), 
                  primary = c("MVIR", "PENNYROYAL", "BLUEGRASS", "MOUNTAINS"))
  

  x.reference <- df %>% 
    filter(str_detect(project_name, pattern = 'REF|REFERENCE|Ref'))
  
  x.hybrids <- df %>% 
    mutate(t_condition = ifelse(primary %in% 'MVIR'& INDEX_RATING_CLASS %in% 'PENNYROYAL','MVIR_riffles', NA)) %>%
    mutate(t_condition = ifelse(primary %in% 'PENNYROYAL' & INDEX_RATING_CLASS %in% 'MVIR', 'PR_20jabs', t_condition)) %>% 
    filter(!is.na(t_condition)) %>% 
    select(-t_condition)

  cuts <- repo$lookups$mbi$ratings %>%
    filter(rating %in% 'Good') %>%
    filter(INDEX_NAME %in% c('MBI-H', 'MBI-W')) %>%
    select('stream_class' = stream_type, bioregion, lower_bound) %>%
    left_join(j_key)

  cuts.i <- cuts %>%
    filter(bioregion %in% c('MV', 'PR')) %>%
    mutate(primary = ifelse(primary %in% 'MVIR', 'PENNYROYAL', 'MVIR')) %>%
    mutate(bioregion = ifelse(bioregion %in% 'MV', 'PR', 'MV')) %>%
    rename('tran_lb' = lower_bound)

  cuts <- cuts %>%
    left_join(cuts.i)
  # 
  # 
  # 
  v <- out_list$station_mean %>%
    filter(programs %in% 'all programs') %>%
    filter(constraints %in% 'transitions included') %>%
    filter(stream_class %in% 'all')
    # mutate(ninezero = v$`0.9`, ninefive = v$`0.95`) %>% 


  v$ninezero <- v$`0.9`
  v$ninefive <- v$`0.95`
  
  v <- v %>% 
    select(matches(region_def), ninezero, ninefive, old_sv)
  
  p.iv <- df %>%
    left_join(cuts) %>%
    # left_join(old_sv) %>% 
    left_join(v) %>% 
    ggplot(aes(x = PARAMETER, y = INDEX_SCORE))+
    geom_point(color = 'black', size = 2, alpha = 0.5)+
    geom_point(data = x.hybrids, aes(x = PARAMETER, y = INDEX_SCORE), size = 3.5, color = 'blue', alpha = 0.5)+
    geom_point(data = x.reference, aes(x = PARAMETER, y = INDEX_SCORE),size = 2, color = 'red', alpha = 0.5)+
    scale_x_continuous(trans = log_trans(), breaks = log_breaks())+
    facet_grid(rows= vars(REGION), cols = vars(stream_class))+
    geom_hline(aes(yintercept = lower_bound))+ 
    geom_hline(aes(yintercept = tran_lb), linetype = 'dotted', size = 1)+
    # geom_vline(aes(xintercept = ninezero), linetype = 'solid', size = 0.75, color = 'red')+
    geom_vline(aes(xintercept = ninefive), linetype = 'solid', size = 0.75, color = 'red')+
    geom_vline(aes(xintercept = old_sv), linetype = 'solid',  size = 1, color = 'blue') +
    theme_grey()+
    theme(panel.grid.major = element_line(size = 0.1),
          panel.grid.minor = element_blank(), 
          panel.grid.major.x = element_blank())+
    xlab(axis_label)+
    ggtitle('Station-Year Means: Red = Reference; Blue = MVIR RK/MH and PR MAC20')
  
  # p.iv
  
  p_design <- "
               441111
               442222
               443333
               "
  p <- p.i+p.ii+p.iii+p.iv+
    patchwork::plot_layout(design = p_design)
  
  file_label <- str_replace(characteristic, pattern = ',', replacement = ' ')
  
  if(trend_plots == TRUE){
    trend_chem[,'PARAMETER'] <- trend_chem[,characteristic]
    # trend_chem[,'REGION'] <- x[,region]
    
    x_list <- list()
    for(i in unique(stations$primary)){
      
      x_list[[i]] <- trend_chem %>% 
        left_join(stations) %>% 
        filter(primary %in% i) %>% 
        mutate(month = month(activity_date, abbr = TRUE, label = TRUE)) %>% 
        ggplot(aes(x = month, y = PARAMETER, color = year))+
        geom_point(size = 4.25, alpha = 0.75)+
        scale_colour_gradientn(colours = viridis::viridis(n = 7))+
        ylab(axis_label)+
        facet_grid(~STATION_NAME)+
        guides(colour=FALSE)+
        ggtitle(i)
      
    }
    
    p_design <- "
               1111
               2222
               3333
               4444
               "
    p <- x_list[[1]] + x_list[[2]] + x_list[[3]] + x_list[[4]] +
      patchwork::plot_layout(design = p_design)
    
    file_label <- str_replace(characteristic, pattern = ',', replacement = ' ')
    ggsave(filename = paste0('V:\\DOWWQB\\Assessment\\CALM\\Screening-Values_2023\\summaries\\trends\\',file_label,'.png'),height = 9, width = 18, units = 'in',  plot = p,device = 'png')
    
  }else{
    if(!plot %in% TRUE){
      file_label <- str_replace(characteristic, pattern = ',', replacement = ' ')
      ggsave(filename = paste0(plot_path,file_label,'.png'),height = 9, width = 18, units = 'in',  plot = p,device = 'png')
    }else{
      p
    }
  }
}





