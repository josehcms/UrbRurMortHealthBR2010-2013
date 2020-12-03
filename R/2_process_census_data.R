###################################################################
### Title: Process census raw data for mortality estimation
### Author: Jose H C Monteiro da Silva
### Github: josehcms
### Last version: 2020-02-10
###################################################################

### 1. Housekeeping #----------------------------------------------
rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)

###################################################################


### 2. Read raw data #---------------------------------------------
person.dat <- 
  readRDS(
    "DATA/PESBRCENSUS2010.rds"
  )

mort.dat <- 
  readRDS(
    "DATA/MORTBRCENSUS2010.rds"
  )

hshold.dat <- 
  readRDS(
    "DATA/DOMBRCENSUS2010.rds"
  )

deathcov.dat <- 
  fread( "DATA/DeathCoverageByRegionBR2010.csv" )

###################################################################

### 3. Adjust variable names and merge data with urb-rur info #----

person.dat <- 
  merge(
    person.dat[,
               list(
                 hshold.id   = V0300,
                 reg         = as.numeric( substr( V0011, 1, 1 ) ),
                 person.wght = V0010,
                 sex         = ifelse( V0601 == 1, 'm', 'f' ),
                 # age         = as.numeric( paste0( cut( V6036,
                 #                                        breaks = c( 0, 1, seq( 5, 80, 5 ), Inf ),
                 #                                        labels = c( 0, 1, seq( 5, 80, 5 ) ),
                 #                                        right  = FALSE
                 #                                        )
                 #                                   )
                 #                           ),
                 age         = ifelse( as.numeric( V6036 ) > 89,
                                       90,
                                       as.numeric( V6036 ) ),
                 dsblty.census = ifelse( ( V0614 %in% c( 1, 2 ) ) | ( V0615 %in% c( 1, 2 ) ) | ( V0616 %in% c( 1, 2 ) ), 1, 0 )
                 )
               ] ,
    hshold.dat[, .( hshold.id = V0300, 
                    urb = ifelse( V1006 == 1, 'urb', 'rur' ) ) ],
    by = 'hshold.id',
    all.x = T
  ) %>%
  .[, 
    list(
      pop           = round( sum( person.wght ) ),      
      dsblty.census = round( sum( person.wght * dsblty.census ) )
    ),
    .( reg, urb, sex, age )
    ]
 
mort.dat <- 
  merge(
    mort.dat[!is.na(idade),
             list(
               hshold.id   = V0300,
               reg         = as.numeric( substr( V0011, 1, 1 ) ),
               person.wght = V0010,
               sex         = ifelse( V0704 == 1, 'm', 'f' ),
               # age         = as.numeric( paste0( cut( as.numeric( idade ),
               #                                        breaks = c( 0, 1, seq( 5, 80, 5 ), Inf ),
               #                                        labels = c( 0, 1, seq( 5, 80, 5 ) ),
               #                                        right  = FALSE
               #                                        )
               #                                   )
               #                           )
               age         = ifelse( as.numeric( idade ) > 89,
                                     90,
                                     as.numeric( idade ) )
               )
             ],
    hshold.dat[, .( hshold.id = V0300, urb = ifelse( V1006 == 1, 'urb', 'rur' ) ) ],
    by = 'hshold.id',
    all.x = T
    ) %>%
  .[, 
    list(
      deaths = round( sum( person.wght ) )
      ),
    .( reg, urb, sex, age )
    ]

census.dat <- 
  merge(
    person.dat,
    mort.dat,
    by = c( 'reg', 'urb', 'sex', 'age' )
  )

##################################################################

### 4. Add ggbseg coverage level for death counts correction #-----

census.dat <- 
  merge(
    census.dat,
    deathcov.dat,
    by = c( 'reg', 'sex' )
  )
###################################################################

### 5. Apply SEG adjusted correction by region #-------------------

# 5.1 Correct when death coverages are below 1 for ages 15-74
census.dat[, deaths.ggbseg := ifelse( ggbseg > 1 | age < 15 | age > 74, deaths, round( deaths / ggbseg )) ]

# 5.2 Compute age specific mortality rates
census.dat[, mx := round( deaths.ggbseg / pop, 5 ) ]

# 5.3 Save adjusted data
census.dat <- 
  census.dat[,
             list(
               pop           = sum( pop ),
               dsblty.census = sum( dsblty.census ),
               deaths        = sum( deaths ),
               deaths.ggbseg = sum( deaths.ggbseg )
             ),
             by = c( 'reg', 'urb', 'sex', 'age' )
             ]
      
saveRDS( census.dat, file = 'DATA/BRCENSUS2010AdjDeathsSingleAges.rds')

##################################################################

### THE END