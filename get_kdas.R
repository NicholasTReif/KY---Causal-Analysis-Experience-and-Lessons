rm(list = ls(all = TRUE))
options(error = expression(NULL))
options(readr.show_col_types = FALSE)
library(foreign)

setwd('v:/dowwqb/github/kwade_repository/')
source("./secondary_functions.R")
source("V:/DOWWQB/github/kwade_reporting/r_functions/filter_by.R")
source('./screening_value_analysis/processing_scripts/sv_functions.R') #get_quantiles() #define_region()
check_packages(packages = c("tidyverse", "readxl","openxlsx",'lubridate', 'writexl', 'readxl', 'knitr', 'gridExtra'))

repo  <- readRDS('./generic_project/repo/lookups/lookup_repository.RDS')

# last_run <- Sys.Date()
last_run <- '2024-01-25'   
wd        <- c('v:/dowwqb/github/kwade_repository/screening_value_analysis/rds/')
bio       <- readRDS(paste0(wd,'/mbi_', last_run, '.RDS'))
habitat   <- readRDS(paste0(wd,'/habitat_', last_run, '.RDS'))
chemistry <- readRDS(paste0(wd,'/chemistry_', last_run, '.RDS'))

pool       <- read_excel('v:/dowwqb/github/kwade_repository/screening_value_analysis/archive/Include-Population 03-22-24.xlsx')
samplers   <- read_excel('v:/dowwqb/github/kwade_repository/screening_value_analysis/archive/sampler-review_include-exclude_11-14-23.xlsx')

pool.i <- pool %>%
   select(STATION_NAME, activity_date, `Exclude Final`, `Comment for final decision`) %>% 
   distinct()

sampler.i <- samplers %>%
   select(STATION_NAME, activity_date,Collector:`ID by`, matches('Exclude'), matches('Final|final')) %>% 
   rename('s.Exclude Final'= `Exclude Final`, 's.comment for final decision' = `Comment for final decision`)

d <- get_new_rds(type = 'raw')
d <- rename_reports(d)

stations <- d %>% 
   get_sv_stations() %>% 
   mutate(stream_class = ifelse(CATCHMENT_AREA < 5, 'headwater', 'wadeable'))


x.habitat <- habitat %>% 
   # rename('habitat_date' = activity_date) %>% 
   select(-source, -CATCHMENT_AREA, -stream_class, -primary, -ECOREGION, -bioregion)


x.chem <- chemistry %>% 
   mutate(chem_date = activity_date) %>% 
   select(-activity_date, -month, -source, -PROJECT_NAME) %>% #only includes year as part of the join key.
   mutate(year = year(chem_date))


x.bio <- bio %>%
   left_join(pool.i) %>% 
   filter(`Exclude Final` %in% 'No') %>% distinct() %>%  
   left_join(sampler.i) %>% 
   filter(is.na(`Exclude Final klm`)| `Exclude Final klm` %in% 'No')
   
   
x.bio <- x.bio %>% 
   left_join(x.habitat) %>% 
   left_join(x.chem) %>% 
   select(-`Collector`:-`s.comment for final decision`, -narrative_assessment)

x <- x.bio %>% 
   left_join(stations) %>% 
   filter(!STREAM_FLOW %in% c('HIGH', 'FLOOD', 'RUNOFF EVENT')) %>%
   filter(CATCHMENT_AREA < 200) %>%
   group_by(STATION_NAME, bio_date) %>% 
   mutate(time_lag = as.numeric(abs(chem_date - bio_date))) %>% 
   filter(time_lag == min(time_lag, na.rm = TRUE)) %>%
   ungroup %>% 
   select(`Exclude Final`, `Comment for final decision`,project_name, matches(colnames(stations)),
          matches('date|time'), everything(), -activity_date) %>%
   arrange(INDEX_RATING_CLASS, STATION_NAME, bio_date) %>% 
   distinct()

x <- x %>% 
   define_region(which_characteristic = 'PHOSPHORUS') %>% 
   define_region(which_characteristic = 'NITROGEN') %>% 
   mutate(primary      = factor(primary, levels = c('MVIR', 'PENNYROYAL', "BLUEGRASS", "MOUNTAINS")), 
          INDEX_RATING_CLASS = factor(INDEX_RATING_CLASS, levels = c('MVIR', 'PENNYROYAL', "BLUEGRASS", "MOUNTAINS")), 
          ECOREGION    = factor(ECOREGION), 
          stream_class = factor(stream_class), 
          INDEX_RATING = factor(INDEX_RATING, levels = c('Very Poor', 'Poor', 'Fair', 'Fair/Good','Good/Fair', 'Good', 'Excellent')), 
          RBP_RATING   = factor(RBP_RATING, levels = c("Poor", 'Fair', 'Good'))) %>% 
   select(`Exclude Final`:ECOREGION, matches('_region'), everything()) %>%
   # filter(`PHOSPHORUS, TOTAL (AS P)` < 6.0 | is.na(`PHOSPHORUS, TOTAL (AS P)`)) %>%  #removing two sites based on sv discussions
   arrange(project_name, primary, bio_date, STATION_NAME)
   
saveRDS(x, paste0(getwd(), '/screening_value_analysis/rds/kdas_same_day_sampling ', Sys.Date(), '.RDS'))


excel_wrapper(x, set_width = 15)


# x.year <- chemistry %>% 
#    left_join(x.bio) %>%
#    left_join(stations) %>%
#    distinct() %>% 
#    mutate(time_lag = abs(chem_date - bio_date)) %>% 
#    filter(!STREAM_FLOW %in% c('HIGH', 'FLOOD', 'RUNOFF EVENT')) %>%
#    filter(CATCHMENT_AREA < 200) %>%
#    arrange(STATION_NAME, year)
# 
# saveRDS(x.year, paste0(getwd(), '/screening_value_analysis/rds/kdas_same_year_sampling ', '2024-01-11', '.RDS'))
# 

#0-15 day chemistry biology sample date differences



