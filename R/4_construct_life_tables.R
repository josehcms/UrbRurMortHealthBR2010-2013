###################################################################
### Title: Urban-Rural life tables and Sullivan Life tables
### Author: Jose H C Monteiro da Silva
### Github: josehcms
### Last version: 2020-04-16
###################################################################

### 1. Housekeeping #----------------------------------------------
rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)
require(DemoTools)

###################################################################

### 2. Read data #------------------------------------------------

# 2.1 2010 National Census data
census_dat <- 
  fread( 'DATA/PROCESSED/population_deaths_region_processed.csv' ) 

# 2.2 DDM results
ddm_res_reg <- 
  fread( 'DATA/PROCESSED/ddm_results_region.csv' ) %>%
  .[ ages_sel == '15-55' & id != 0,
     .( reg = id, 
        sex,
        ggbseg ) ]

ddm_res_urb <- 
  fread( 'DATA/PROCESSED/ddm_results_urbrur.csv' ) %>%
  .[ ages_sel == '15-55',
     .( urb = id, 
        sex,
        ggbseg ) ]

# Merge ddm and deaths data
mx_reg <- 
  merge(
    census_dat,
    ddm_res_reg,
    by = c( 'reg', 'sex' )
  ) %>%
  .[ , age := cut( age,
                   breaks = c( 0, 1, seq( 5, 80, 5 ), Inf ),
                   labels = c( 0, 1, seq( 5, 80, 5 ) ),
                   right  = FALSE ) %>% paste0 %>% as.numeric ] %>%
  .[ , deaths := ifelse( age >= 15, deaths / ggbseg, deaths ) ] %>%
  .[ , 
     .(
       deaths           = sum( as.numeric( deaths ) ),
       pop              = sum( pop ),
       px_dsblty_census = sum( px_dsblty_census )
     ),
     .( urb, sex, age ) ] %>%
  .[ , 
     `:=`(
       mx = deaths / pop,
       px_dsblty_census = px_dsblty_census / pop
       ) 
     ]

mx_urb <- 
  merge(
    census_dat,
    ddm_res_urb,
    by = c( 'urb', 'sex' )
  ) %>%
  .[ , age := cut( age,
                   breaks = c( 0, 1, seq( 5, 80, 5 ), Inf ),
                   labels = c( 0, 1, seq( 5, 80, 5 ) ),
                   right  = FALSE ) %>% paste0 %>% as.numeric ] %>%
  .[ , deaths := ifelse( age >= 15, deaths / ggbseg, deaths ) ] %>%
  .[ , 
     .(
       deaths           = sum( as.numeric( deaths ) ),
       pop              = sum( pop ),
       px_dsblty_census = sum( px_dsblty_census )
     ),
     .( urb, sex, age ) ] %>%
  .[ , 
     `:=`(
       mx = deaths / pop,
       px_dsblty_census = px_dsblty_census / pop
     ) 
  ]

###################################################################

### 3. Construct life table for BR2010 census #--------------------

# 3.1 Using Demotools Function and region ddm
lt_reg <- data.table()

for( sexsel in c( 'm', 'f' ) ){
  for( urbsel in c( 'urb', 'rur' ) ){
    lt_reg <-
      rbind(
        lt_reg,
        lt_abridged(
          nMx = mx_reg[ sex == sexsel & urb == urbsel ]$mx,
          Age = mx_reg[ sex == sexsel & urb == urbsel ]$age,
          Sex = sexsel 
        ) %>%
          as.data.table %>%
          .[,
            list(
              urb = urbsel,
              sex = sexsel,
              age = Age,
              mx  = round( nMx, 6 ),
              qx  = round( nqx, 6 ),
              lx  = round(  lx, 0 ),
              nLx = round( nLx, 0 ),
              ex  = round(  ex, 1 )
            )
          ]
      )
  }
}

# 3.2 Using Demotools Function and urbrur ddm
lt_urb <- data.table()

for( sexsel in c( 'm', 'f' ) ){
  for( urbsel in c( 'urb', 'rur' ) ){
    lt_urb <-
      rbind(
        lt_urb,
        lt_abridged(
          nMx = mx_urb[ sex == sexsel & urb == urbsel ]$mx,
          Age = mx_urb[ sex == sexsel & urb == urbsel ]$age,
          Sex = sexsel 
        ) %>%
          as.data.table %>%
          .[,
            list(
              urb = urbsel,
              sex = sexsel,
              age = Age,
              mx  = round( nMx, 6 ),
              qx  = round( nqx, 6 ),
              lx  = round(  lx, 0 ),
              nLx = round( nLx, 0 ),
              ex  = round(  ex, 1 )
            )
          ]
      )
  }
}
##################################

### 4. Adjust and save #----------

lt_dat <- 
  rbind(
    lt_reg[ , type := 'reg' ],
    lt_urb[ , type := 'urbrur' ]
    )

write.table( lt_dat,
             'DATA/PROCESSED/life_tables.csv')
##################################
