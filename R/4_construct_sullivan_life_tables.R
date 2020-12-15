###################################################################
### Title: Sulivan Life Table
### Author: Jose H C Monteiro da Silva
### Github: josehcms
### Last version: 2020-12-15
###################################################################

### 1. Housekeeping #----------------------------------------------
rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)
require(DemoTools)
require(DemoDecomp)

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

# Merge ddm and deaths data
mx_dat <- 
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

pns_dat <- 
  fread( 'DATA/PROCESSED/pns_prevalence_rates_processed.csv' ) %>%
  .[ , -c( 'pop' ) ]


hx_dat <- 
  merge(
    mx_dat,
    pns_dat,
    by = c( 'urb', 'sex', 'age' ),
    all = T
  ) %>%
  .[ , `:=`(
    px_dsblty = ifelse( is.na( px_dsblty ),
                        0, 
                        px_dsblty ),
    px_cardio = ifelse( is.na( px_cardio ),
                        0, 
                        px_cardio ),
    px_osteop = ifelse( is.na( px_osteop ),
                        0, 
                        px_osteop ),
    px_diabetes = ifelse( is.na( px_diabetes ),
                          0, 
                          px_diabetes ),
    px_dsblty_census = px_dsblty_census,
    mx = mx
  ) ] %>%
  melt(
    id.vars = c( 'urb', 'sex', 'age', 'mx' ),
    measure.vars = c( 'px_dsblty_census', 'px_diabetes', 'px_cardio',
                      'px_osteop' ),
    value.name = 'px',
    variable.name = 'dsblty'
  )
###################################################################

### 3. Run Sullivan Life Table #-----------------------------------

# 3.1 Sulli LT function
sulli_lt_calc <- 
  function( x = c( 0, 1, seq( 5, 80, 5 ) ),
            mx, px, sex ){
    
    stopifnot( length( x ) == length( mx ) &
                 length( x ) == length( px ) &
                  ( sex %in% c( 'm', 'f' ) ) )
    
    # Step 1: construct life table from mx
    lt <-
      lt_abridged( Age = x,
                   nMx = mx,
                   sex = sex,
                   radix = 1 )
    
    # Step 2: Compute Lx adjusted for disability prevalence
    lt$Lx_Sulli <- 
      round( lt$nLx * ( 1 - px ), 6 )
    
    lt$Tx_Sulli <- 
      round( rev( cumsum( rev( lt$Lx_Sulli ) ) ), 6 )
    
    # Step 3: Estimate Health Expectancy
    lt$hx <- 
      round( lt$Tx_Sulli / lt$lx , 1 )
    
    # step 4: Return result
    setDT( lt )
    out_lt <- 
      lt[, .( age = Age, 
              sex = sex, 
              mx = round( nMx, 6 ),
              ax = round( nAx, 3 ),
              qx = round( nqx, 6 ),
              lx = round(  lx, 6 ),
              # dx = round( ndx, 6 ),
              # Lx = round( nLx, 6 ),
              # Tx = round(  Tx, 6 ),
              # Lx_Sulli = round( Lx_Sulli, 6 ),
              # Tx_Sulli = round( Tx_Sulli, 6 ),
              hx = round( hx, 1 ),
              ex = round( ex, 1 ) ) ]
    
    return( out_lt )
  }

# 3.2 Save Sullivan results
lt_sulli <- data.table()
for( pxsel in c( 'px_dsblty_census', 'px_diabetes', 
                 'px_cardio', 'px_osteop' ) ){
  for( sexsel in c( 'm', 'f' ) ){
    for( urbsel in c( 'urb', 'rur' ) ){
      
      mx_aux <- 
        hx_dat[ sex == sexsel & 
                  urb == urbsel & 
                  dsblty == pxsel ]$mx
      px_aux <- 
        hx_dat[ sex == sexsel & 
                  urb == urbsel & 
                  dsblty == pxsel ]$px
      
      lt_sulli <-
        rbind(
          lt_sulli,
          sulli_lt_calc( mx  = mx_aux,
                         px  = px_aux,
                         sex = sexsel ) %>%
            .[ , dsblty := pxsel ] %>%
            .[ , urb := urbsel ] 
        )
    }
  }
}

# 3.3 Save table
results_tab <- 
  lt_sulli[ age %in% c( 20, 40, 60 )] %>%
  .[, hxex := paste0( hx, ' (', round( hx/ex, 2 ), ')' ) ] %>%
  dcast( dsblty + age ~ sex + urb, value.var = 'hxex' )

write.table( x = results_tab,
             file = 'FIGURES/table_hx_ex.csv',
             row.names = F )

### THE END