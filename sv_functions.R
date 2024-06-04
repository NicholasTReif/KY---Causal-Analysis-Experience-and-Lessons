get_quantiles <- function(x, programs = NULL, q_value, which_characteristic, which_ratings, drop_transitions = FALSE, which_groups, screening_region = FALSE){
  x_list <- list()
  options(digits = 3)
  x[,'PARAMETER'] <- x[,which_characteristic]
  
  if(!is.null(which_ratings)){
    if(!'all' %in% which_ratings){
        x <- x %>% 
          filter(INDEX_RATING %in% which_ratings)
    }
  }
  
  if(drop_transitions %in% TRUE){
    x <- x %>% 
      filter(is.na(secondary))
  }
  if(!is.null(programs)){
    if(!is.null(x$PROJECT_NAME)){
      x <- x %>% 
        filter(str_detect(PROJECT_NAME, pattern = programs))
    }
    if(!is.null(x$project_name)){
      x <- x %>% 
        filter(str_detect(project_name, pattern = programs))
    }
    if(any(str_detect(programs, pattern = '\\|'))){
      program_id <- str_sub(programs,start = 1, end = str_locate(programs, pattern = '\\|')[1]-1)
    }else{
      program_id <- programs
    }
  }else{
    program_id <- 'all programs'
  }

  grouping_vars <- lapply(which_groups, as.symbol)
  
  x.i <- x %>%
    select(bio_date, matches(which_groups),PARAMETER) %>% 
    distinct()
  
  q_list <- list()
  
  for(i in 1:length(q_value)){
    qx <- x.i %>%
      select(matches(which_groups), PARAMETER) %>% 
      filter(!is.na(PARAMETER)) %>% 
      group_by_(.dots = grouping_vars) %>% 
      summarise(value = quantile(PARAMETER, q_value[i], na.rm = TRUE),
                quantile = q_value[i],
                sv_char = which_characteristic)
    
    q_list[[i]] <- qx
    
    if(i == length(q_value)){
      q.i <- bind_rows(q_list)
      
      n <- x.i %>%
        filter(!is.na(PARAMETER)) %>% 
        group_by_(.dots = grouping_vars) %>%
        summarise(n = n())
      
      q.ii <- q.i %>%
        left_join(n) %>%
        mutate(programs = program_id) %>% 
        mutate(ratings = str_flatten(which_ratings, collapse = ',')) %>% 
        select(-sv_char) %>% 
        spread(key = quantile, value = value) %>%
        arrange_(.dots = grouping_vars)
    }
  }
  return(q.ii)
}

define_region <- function(x, which_characteristic){
  if(which_characteristic %in% 'PHOSPHORUS'){
    sv_region <- 'phosphorus_region'
  }
  if(which_characteristic %in% 'NITROGEN'){
    sv_region <- 'nitrogen_region'
  }
  
  for(i in 1:nrow(x)){
    if(x$primary[i] %in% 'BLUEGRASS'){
      if(x$ECOREGION[i] %in% c('71d', '71k')){
        x[i,sv_region] <- 'BG Outer (sans 71L)'
      }else{
        x[i,sv_region] <- 'BG Inner (71L)'
      }
    }
    if(x$primary[i] %in% 'PENNYROYAL'){
      if(which_characteristic %in% 'NITROGEN'){
        if(x$ECOREGION[i] %in% c('71e')){
          x[i,sv_region] <- 'PR 71e'
        }else{
          x[i,sv_region] <- 'PR sans 71e' 
        }
      }
      if(which_characteristic %in% 'PHOSPHORUS'){
        x[i, sv_region] <- 'Pennyroyal'
      }
    }
    if(x$primary[i] %in% c('MOUNTAINS', 'MISS. VALLEY INTERIOR RIVER', 'MVIR')){
      if(str_detect(x$primary[i] ,pattern = 'MISS.|MVIR')){
        x[i,sv_region] <- 'MVIR'
      }else{
        x[i,sv_region] <- str_to_title(x$primary[i])
      }
    }
  }
  
  return(x)
}

summarize_analyte <- function(x, transition_pars, region_class, stream_class, analyte){
  x_list <- list()
  y_list <- list()
  z_list <- list()
  for (i in transition_pars){
    if(i %in% 'transitions excluded'){
      drop = TRUE
    }else{
      drop = FALSE
    }
    if(stream_class %in% TRUE){
      group_ids <- c(region_class, 'stream_class')
    }else{
      group_ids <- c(region_class)
    }
    
    x_list[[i]] <- x %>%
      get_quantiles(q_value = c(0.05,0.25, 0.5, 0.75, 0.9, 0.95),
                    drop_transitions = drop, 
                    which_characteristic = analyte,
                    which_ratings = c('Good', 'Excellent'),
                    which_groups = group_ids) %>%
      mutate(constraints = i)
    
    y_list[[i]] <- x %>%
      filter(!is.na(og_status)) %>% 
      get_quantiles(drop = drop, 
                    programs = 'REF|Ref|REFERENCE', 
                    q_value = c(0.05,0.25, 0.5, 0.75, 0.9,0.95),
                    which_characteristic = analyte,
                    which_ratings = 'all',
                    which_groups = group_ids) %>%
      mutate(constraints = i, og_status = 'mbi development')
    
    z_list[[i]] <-  x %>%
      get_quantiles(drop = drop, 
                    programs = 'REF|Ref|REFERENCE', 
                    q_value = c(0.05,0.25, 0.5, 0.75, 0.9, 0.95),
                    which_characteristic = analyte,
                    which_ratings = 'all',
                    which_groups = group_ids) %>%
      mutate(constraints = i, og_status = 'all reference stations')
    
    if(length(x_list) == length(transition_pars)){
      x_list  <- bind_rows(x_list)
      y_list  <- bind_rows(y_list)
      z_list  <- bind_rows(z_list)
      
      x_list <- bind_rows(x_list, y_list, z_list)
      
    }
  }
  return(x_list)
}

excel_wrapper <- function(x,file_path, file_id = NULL, set_width){
  options(openxlsx.dateFormat="mm/dd/yyyy")
  options(openxlsx.datetimeFormat="mm/dd/yyyy")

  wb <- createWorkbook()
  modifyBaseFont(wb, fontSize = 8)
  
  if(!any(class(x) %in% 'list')){
    addWorksheet(wb, 'Sheet1')
    writeDataTable(wb, sheet = 'Sheet1', x, startCol = 1, startRow = 1, 
                   tableStyle = 'TableStyleMedium15', colNames = TRUE, withFilter = TRUE)
    setColWidths(wb, 'Sheet1', widths = set_width, cols = 1:50)
    if(!is.null(file_id)){
      saveWorkbook(wb, paste0(file_path, file_id, '.xlsx'))
    }else{openXL(wb)}
  }
  if(any(class(x) %in% 'list')){
    for (df_name in names(x)){
      x.i <- df_name
      addWorksheet(wb, x.i)
      writeDataTable(wb, sheet = x.i, x[[x.i]], startCol = 1, startRow = 1, 
                     tableStyle = 'TableStyleMedium15', colNames = TRUE, withFilter = TRUE)
      setColWidths(wb, x.i, widths = set_width, cols = 1:50)
      print(paste("formatting", x.i, sep = " "))
    }
    if(!is.null(file_id)){
      saveWorkbook(wb, paste0(file_path, file_id, '.xlsx'))
    }else{openXL(wb)}
  }
}

check_packages <- function(packages){
  packages <- packages
  
  x <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}
