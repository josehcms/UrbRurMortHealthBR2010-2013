###################################################################
### Title: Process census raw data for mortality estimation
### Author: Jose H C Monteiro da Silva
### Github: josehcms
### Last version: 2020-12-14
###################################################################

### 1. Housekeeping #----------------------------------------------
rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)

###################################################################

### 2. Read raw data #---------------------------------------------
person_dat <- 
  readRDS(
    "DATA/PESBRCENSUS2010.rds"
  )

mort_dat <- 
  readRDS(
    "DATA/MORTBRCENSUS2010.rds"
  )

hshold_dat <- 
  readRDS(
    "DATA/DOMBRCENSUS2010.rds"
  )

###################################################################

### 3. Adjust variable names and merge data with urb-rur info #----

## 3.1 Person data
person_dat <- 
  merge(
    person_dat[,
               list(
                 hshold_id   = V0300,
                 reg         = as.numeric( substr( V0011, 1, 1 ) ),
                 person_wght = V0010,
                 sex         = ifelse( V0601 == 1, 'm', 'f' ),
                 age         = ifelse( as.numeric( V6036 ) > 89,
                                       90,
                                       as.numeric( V6036 ) ),
                 dsblty_census = ifelse( ( V0614 %in% c( 1, 2 ) ) | 
                                           ( V0615 %in% c( 1, 2 ) ) | 
                                           ( V0616 %in% c( 1, 2 ) ), 
                                         1, 0 )
                 )
               ] ,
    hshold_dat[, .( hshold_id = V0300, 
                    urb = ifelse( V1006 == 1, 'urb', 'rur' ) ) ],
    by = 'hshold_id',
    all.x = T
  ) %>%
  .[, 
    list(
      pop              = round( sum( person_wght ) ),      
      px_dsblty_census = round( sum( person_wght * dsblty_census ) )
    ),
    .( reg, urb, sex, age )
    ]
 
## 3.2 Mortality data
mort_dat <- 
  merge(
    mort_dat[ !is.na( idade ),
              list(
                hshold_id   = V0300,
                reg         = as.numeric( substr( V0011, 1, 1 ) ),
                person_wght = V0010,
                sex         = ifelse( V0704 == 1, 'm', 'f' ),
                age         = ifelse( as.numeric( idade ) > 89,
                                      90,
                                      as.numeric( idade ) )
                )
             ],
    hshold_dat[, .( hshold_id = V0300, 
                    urb = ifelse( V1006 == 1, 'urb', 'rur' ) ) ],
    by = 'hshold_id',
    all.x = T
    ) %>%
  .[, 
    list(
      deaths = round( sum( person_wght ) )
      ),
    .( reg, urb, sex, age )
    ]

## 3.3 merge and save 
census_dat <- 
  merge(
    person_dat,
    mort_dat,
    by = c( 'reg', 'urb', 'sex', 'age' )
  )

write.table(
  census_dat,
  file = 'DATA/PROCESSED/population_deaths_region_processed.csv',
  row.names = F
)
##################################################################

### THE END