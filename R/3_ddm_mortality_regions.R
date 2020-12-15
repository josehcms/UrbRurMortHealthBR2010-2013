###################################################################
### Title: Run Death Distribution Methods for census mortality data
### Author: Jose H C Monteiro da Silva
### Github: josehcms
### Last version: 2020-12-14
###################################################################

### 1. Housekeeping #----------------------------------------------
rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)
require(DDM)
require(ggplot2)
###################################################################

### 2. Load data #-------------------------------------------------

## 2.1 2000 census for pop1
load('DATA/DATA_POP_2000.RData')
dat2000 <- 
  rbind(
    TAB[ , .( ufcode = UF,
              reg = substr( UF, 1, 1 ),
              urb = ifelse( RUR_URB == '1', 'urb', 'rur' ),
              sex = ifelse( SEXO == 1, 'm', 'f' ),
              age = ifelse( as.numeric( IDADE ) == 1,
                            0,
                            as.numeric( IDADE ) ),
              pop = as.numeric( N_PES ) ) ] %>%
      .[ , .( pop1 = sum( pop ) ),
         .( reg, sex, age ) ],
    TAB[ , .( ufcode = UF,
              reg = '0',
              urb = ifelse( RUR_URB == '1', 'urb', 'rur' ),
              sex = ifelse( SEXO == 1, 'm', 'f' ),
              age = ifelse( as.numeric( IDADE ) == 1,
                            0,
                            as.numeric( IDADE ) ),
              pop = as.numeric( N_PES ) ) ] %>%
      .[ , .( pop1 = sum( pop ) ),
         .( reg, sex, age ) ]
  )
rm( TAB )

## 2.2 2010 census for pop2
dat2010 <- 
  rbind(
    fread( 'DATA/PROCESSED/population_deaths_region_processed.csv' ) %>%
      .[ , .( reg = as.character( reg ),
              urb, sex, 
              age = cut( as.numeric( age ),
                         breaks = c( seq( 0, 80, 5 ), Inf ),
                         labels = seq( 0, 80, 5 ),
                         right  = FALSE ) %>% 
                paste0 %>% as.numeric,
              pop, 
              deaths ) ] %>%
      .[ , .( pop2 = sum( pop ),
              deaths = sum( deaths ) ),
         .( reg, sex, age ) ],
    fread( 'DATA/PROCESSED/population_deaths_region_processed.csv' ) %>%
      .[ , .( reg = '0',
              urb, sex, 
              age = cut( as.numeric( age ),
                         breaks = c( seq( 0, 80, 5 ), Inf ),
                         labels = seq( 0, 80, 5 ),
                         right  = FALSE ) %>% 
                paste0 %>% as.numeric,
              pop, 
              deaths ) ] %>%
      .[ , .( pop2 = sum( pop ),
              deaths = sum( deaths ) ),
         .( reg, sex, age ) ]
  )
###################################################################

### 3. Prepare data for DDM evaluation #---------------------------

ddm_dat <- 
  merge(
    dat2000,
    dat2010,
    by = c( 'reg', 'sex', 'age' )
  ) %>%
  .[ , date1 := as.Date( '2000-07-31' ) ] %>%
  .[ , date2 := as.Date( '2010-07-31' ) ] %>%
  .[ , deaths := 
       ( deaths / pop2 ) * 
       sqrt( pop1 * pop2 ) ] %>%
  .[ , .(
    cod = reg,
    sex, age,
    date1, pop1,
    date2, pop2,
    deaths ) ]

###################################################################

### 4. Run DDM function for several age groups #-------------------

ages_sel <- 
  list(
    seq( 15, 50, 5 ),
    seq( 15, 55, 5 ),
    seq( 15, 60, 5 ),
    seq( 15, 65, 5 ),
    seq( 15, 70, 5 ),
    seq( 15, 75, 5 ),
    seq( 20, 55, 5 ),
    seq( 20, 60, 5 ),
    seq( 20, 65, 5 ),
    seq( 20, 70, 5 ),
    seq( 20, 75, 5 ),
    seq( 25, 60, 5 ),
    seq( 25, 65, 5 ),
    seq( 25, 70, 5 ),
    seq( 25, 75, 5 ),
    seq( 30, 65, 5 ),
    seq( 30, 70, 5 ),
    seq( 30, 75, 5 ),
    seq( 35, 70, 5 ),
    seq( 35, 75, 5 )
  )


ddm_res_list <- 
  lapply( 1:20, function( i ){
    
    select_ages <- 
      ages_sel[[i]]
    
    out <- 
      rbind(
        ddm( ddm_dat[ sex == 'f'], 
             exact.ages.seg = select_ages, 
             exact.ages.ggb = select_ages ) %>%
          as.data.table() %>%
          .[ , sex := 'f' ] %>%
          .[ , ages_sel := paste0( lower, '-', upper ) ],
        ddm( ddm_dat[ sex == 'm'], 
             exact.ages.seg = select_ages, 
             exact.ages.ggb = select_ages ) %>%
          as.data.table() %>%
          .[ , sex := 'm' ] %>%
          .[ , ages_sel := paste0( lower, '-', upper ) ]
      )
    
    return( out )
  })

ddm_res <- 
  do.call( rbind, ddm_res_list )

## save results
write.table( ddm_res,
             'DATA/PROCESSED/ddm_results_region.csv',
             row.names = F 
             )

# ddm_res[, 
#         list(
#           maxddm  = max( ggbseg ),
#           meanddm = mean( ggbseg ),
#           medddm  = median( ggbseg ),
#           minddm  = min( ggbseg )
#           ),
#         .( id, sex ) ]
###################################################################

### THE END