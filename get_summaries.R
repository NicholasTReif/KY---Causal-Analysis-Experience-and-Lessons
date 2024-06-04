rm(list = ls(all = TRUE))
options(error = expression(NULL))
options(readr.show_col_types = FALSE)

options(scipen = 999)
library(foreign)
library(tidyverse)

setwd('v:/dowwqb/github/kwade_repository/')
source("./secondary_functions.R")
source('./screening_value_analysis/processing_scripts/sv_functions.R')
source('./screening_value_analysis/processing_scripts/render_sv_panel.R')


xo     <- readRDS('./screening_value_analysis/rds/kdas_same_day_sampling 2024-01-25.RDS')
wd     <- c('v:/dowwqb/github/kwade_repository/screening_value_analysis/rds/')
x.chem <- readRDS(paste0(wd,'/chemistry_', '2024-01-25', '.RDS'))

d <- get_new_rds(type = 'raw')

d <- rename_reports(d)

reference_stations <- d$`Project Field Activities` %>% 
   filter(str_detect(PROJECT_NAME, pattern = 'MBI RECAL')) %>% 
   select(STATION_NAME, PROJECT_NAME) %>% distinct() 

stations <- d %>% 
   get_sv_stations() %>% 
   filter(STATION_NAME %in% xo$STATION_NAME) %>%
   mutate(stream_class = ifelse(CATCHMENT_AREA < 5, 'headwater', 'wadeable'), 
          primary = factor(primary, levels = c('MVIR', "PENNYROYAL", "BLUEGRASS", 'MOUNTAINS'))) %>% 
   distinct()

trend_stations <- d$`Project Stations` %>% 
   filter(str_detect(STATION_CATEGORIES, pattern = 'REFERENCE TREND')) %>% 
   select(STATION_NAME) %>% distinct() %>% 
   .$STATION_NAME

trend_chem <- x.chem %>% 
   filter(STATION_NAME %in% trend_stations)

pchem <- x.chem %>%  
   filter(STATION_NAME %in% xo$STATION_NAME)%>% 
   filter(!is.na(activity_date))

lat_long <- xo %>% 
   select(STATION_NAME, latitude, longitude)

repo <- list()
repo$lookups <- readRDS('./generic_project/repo/lookups/lookup_repository.RDS')

chars <- colnames(xo)[c(71:72,74,77:82)]
region <- c(rep('primary',4), rep('nitrogen_region',3), 'phosphorus_region', 'primary')
units <- c(rep(' [mg/L]', 2), ' mhos/cm', rep(' [mg/L]', 6))
key <- tibble('chars' = chars,'units'=units, 'region' = region)
# -----------------------------------------------------------------------------------------------------------------------------------------

for(i in 1:nrow(key)){

   region_def <- key$region[i]
   character_id <- key$chars[i]
   axis_id <- paste0(character_id, " ", key$units[i])
   
   x_list <- list()
   
   
   xi <- xo %>% 
      mutate(og_status = ifelse(STATION_NAME %in% reference_stations$STATION_NAME, 'Reference MBI', NA)) %>% 
      select(bio_date, chem_date, og_status, project_name, STATION_NAME, LOCALE_NAME, 
             INDEX_SCORE,INDEX_RATING,INDEX_RATING_CLASS, matches('primary|secondary|tertiary|ECOREGION|region|stream_class|CATCHMENT_AREA'), RBP_SCORE,
             RBP_RATING,matches('CONDUCTIVITY|AS N|AS P|Sulfate|TOC|CACO3|CaCO3')) %>% 
      mutate(nitrogen_region = factor(nitrogen_region, 
                                      levels = c("MVIR", 'PR 71e', 'PR sans 71e', 'BG Inner (71L)','BG Outer (sans 71L)', 'Mountains'))) %>% 
      mutate(phosphorus_region = factor(phosphorus_region, 
                                        levels = c("MVIR", 'Pennyroyal', 'BG Inner (71L)','BG Outer (sans 71L)', 'Mountains'))) %>% 
      mutate(parameter_status = ifelse(INDEX_RATING %in% c('Good', 'Excellent'), 'FS', 
                                       ifelse(INDEX_RATING %in% c('Good/Fair', 'Fair/Good'), 'ALOER',
                                              ifelse(INDEX_RATING %in% 'Fair', 'PS',
                                                     ifelse(INDEX_RATING %in% c("Poor", 'Very Poor'), 'NS', INDEX_RATING))))) %>% 
      mutate(parameter_status = factor(parameter_status,levels = c('NS', 'PS','ALOER','FS' ))) %>%
      filter(!RBP_RATING %in% 'Poor')

   # if(region_def %in% c('phosphorus_region', 'nitrogen_region')){
   #    file_label <-  paste0(character_id, ' apr-oct')
   #       pchem.i <- pchem %>%
   #          filter(month > 3 & month < 11)
   # 
   #       xi <- xi %>%
   #          filter(month(bio_date) > 3 & month(bio_date) < 11)
   # }else{
   #    pchem.i <- pchem
      file_label <- paste0(character_id, ' fg_habitat')
   # }

   x_list[['paired']] <- xi
   
   
   pchem.i[,'result'] <- pchem.i[,character_id]
   xi[,'result'] <- xi[,character_id]
   
   
   xii <- xi %>% 
      mutate(year = year(bio_date)) %>% 
      select(bio_date,chem_date, year, parameter_status, og_status:RBP_RATING) %>%
      distinct() %>% 
      full_join(pchem.i) %>%
      filter(!is.na(INDEX_RATING)) %>% 
      select(activity_date, bio_date,chem_date,year, parameter_status, og_status:RBP_RATING, result)
   
   x.mean <- xii %>% 
      group_by(STATION_NAME, year) %>% 
      mutate(result = mean(result,na.rm = TRUE), 
             bio_date = max(bio_date), 
             chem_date = max(chem_date)) %>% 
      ungroup %>%
      mutate(result = ifelse(is.nan(result), NA, result)) %>% 
      select(bio_date, chem_date:RBP_RATING,result) %>% 
      distinct()
   
   x.max <- xii %>% 
      group_by(STATION_NAME, year) %>% 
      mutate(result = max(result, na.rm = TRUE),
             bio_date = max(bio_date), 
             chem_date = max(chem_date)) %>% 
      ungroup %>%
      mutate(result = ifelse(is.infinite(result), NA, result)) %>% 
      select(bio_date, chem_date:RBP_RATING, result) %>% 
      distinct()
   
   xii[,character_id] <- xii[,'result']
   x_list[['complete']] <- xii %>% select(-result)
   
   x.mean[,character_id] <- x.mean[,'result']
   x_list[['station_mean']] <- x.mean %>% select(-result)
   
   x.max[,character_id] <- x.max[,'result']
   x_list[['station_max']] <- x.max %>% select(-result)
   
   
   out_list <- list()
   
   for(input_id in names(x_list)){
      
      out_list[['paired_raw']] <- xi
      out_list[['complete_raw']] <- xii
      
      x.i <- x_list[[input_id]] %>% 
         summarize_analyte(analyte = character_id, 
                           region_class = region_def, stream_class = FALSE, 
                           transition_pars = c('transitions included', 'transitions excluded')) %>% 
         mutate(stream_class = 'all') 
      
      
      out_list[[input_id]] <- x_list[[input_id]] %>% 
         summarize_analyte(analyte = character_id, 
                           region_class = region_def, stream_class = TRUE, 
                           transition_pars = c('transitions included', 'transitions excluded')) %>% 
         bind_rows(x.i)
   }
   
   render_sv_plots(x = x_list, region = region_def, characteristic = character_id, axis_label = axis_id, trend_plots = FALSE)

   # file_label <- '-no constraints'
   excel_wrapper(out_list, set_width = 20, file_path = 'V:\\DOWWQB\\Assessment\\CALM\\Screening-Values_2023\\summaries\\fg_habitat\\', file_id = paste0(file_label))
}











