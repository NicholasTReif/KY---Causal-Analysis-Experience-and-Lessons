rm(list = ls(all = TRUE))
options(error = expression(NULL))
options(readr.show_col_types = FALSE)

options(scipen = 999)
library(foreign)
library(tidyverse)
library(patchwork)

setwd('C:/Users/nicholas.reif/Documents/GitHub/KY---Causal-Analysis-Experience-and-Lessons')
source('./sv_functions.R')
source('./render_index_panel.R')


repo               <- readRDS('./rds/repo_lookups.RDS')
xo                 <- readRDS('./rds/subset_test.RDS')
pchem.i            <- readRDS('./rds/chemistry_subset.RDS')
stations           <- readRDS('./rds/stations.RDS')
reference_stations <- readRDS('./rds/reference_stations.RDS')


check_packages(c("tidyverse", "openxlsx",'lubridate', 'writexl', 'scales', 'foreign', 'patchwork'))

lat_long <- xo %>% 
   select(STATION_NAME, latitude, longitude)

chars  <- colnames(xo)[c(71:72,74,77:82)]
region <- c(rep('primary',4), rep('nitrogen_region',3), 'phosphorus_region', 'primary')
units  <- c(rep(' [mg/L]', 2), ' mhos/cm', rep(' [mg/L]', 6))
key    <- tibble('chars' = chars,'units'=units, 'region' = region)


figure_save_location <- paste0(getwd(),'/figures/')
report_save_location <- paste0(getwd(),'/reports/')
# -----------------------------------------------------------------------------------------------------------------------------------------

for(i in 1:nrow(key)){
    i <- 1
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
      mutate(parameter_status = factor(parameter_status,levels = c('NS', 'PS','ALOER','FS' )))


      file_label <- paste0(character_id)

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
   plot_path <- paste0(getwd(), './figures/')
   render_sv_plots(x = x_list, region = region_def, characteristic = character_id, axis_label = axis_id, trend_plots = FALSE)

   excel_wrapper(out_list, set_width = 20, file_path = report_save_location, 
                 file_id = paste0(file_label))
}











