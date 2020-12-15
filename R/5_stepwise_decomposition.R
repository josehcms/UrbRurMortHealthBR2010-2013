###################################################################
### Title: StepWise Decomposition
### Author: Jose H C Monteiro da Silva
### Github: josehcms
### Last version: 2020-12-14
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
  ) ]



# pars mx and then prev
calc_h20_males <- function(pars){
  
  # step 1: convert from vec into something we can use
  N        <- length(pars)
  dim(pars)<- c( N / 2, 2 ) # rows, columns
  
  # step 2: compute life table
  lt <-
    lt_abridged( Age = c( 0, 1, seq( 5, 80, 5 ) ),
                 nMx = pars[,1],
                 sex = 'm' )

  # lt <- 
  #   LifeTable( x = seq( 20, 80, 5 ),
  #              mx = pars[,1], sex = 'male' )$lt 
  # step 3: use prevalence rates to construct health expectancy   
  lt$nLx.Sulli <- 
    round( lt$nLx * ( 1 - pars[,2] ), 6 )
  
  lt$Tx.Sulli <- 
    round( rev( cumsum( rev( lt$nLx.Sulli ) ) ), 6 )
  
  lt$hx <- 
    round( lt$Tx.Sulli / lt$lx , 1 )
  
  # step 4: return result
  
  return( lt$hx[6] )
}

decompres_males_list <- 
  lapply( c( 'px_diabetes', 'px_cardio', 'px_osteop', 
             'px_dsblty_census' ),
          function( px ){
            pars_urb <- c(
              hx_dat[ sex == 'm' & urb == 'urb' ]$mx,
              hx_dat[ sex == 'm' & urb == 'urb', get( px ) ]
            )
            
            pars_rur <- c(
              hx_dat[ sex == 'm' & urb == 'rur' ]$mx,
              hx_dat[ sex == 'm' & urb == 'rur', get( px ) ]
            )
            
            dec_setpwise <- 
              stepwise_replacement( func = calc_h20_males, 
                                    pars1 = pars_urb, 
                                    pars2 = pars_rur, 
                                    symmetrical = TRUE )
            
            dim( dec_setpwise ) = c( length( pars_rur ) / 2, 2 )
            
            decomp_res <- 
              data.table( dec_setpwise ) %>%
              setnames( c( 'mx_cntrb', 'px_cntrb' ) ) %>%
              .[ , x := c( 0, 1, seq( 5, 80, 5 ) ) ] %>%
              .[ , dsblty := px ] %>%
              .[ , .( x, 
                      sex = 'm',
                      mx_cntrb, px_cntrb, dsblty ) ]
            
            return( decomp_res )
          } )

decompres_males <- 
  do.call( rbind, decompres_males_list )

calc_h20_females <- function(pars){
  
  # step 1: convert from vec into something we can use
  N        <- length(pars)
  dim(pars)<- c( N / 2, 2 ) # rows, columns
  
  # step 2: compute life table
  lt <-
    lt_abridged( Age = c( 0, 1, seq( 5, 80, 5 ) ),
                 nMx = pars[,1],
                 sex = 'f' )
  
  # lt <- 
  #   LifeTable( x = seq( 20, 80, 5 ),
  #              mx = pars[,1], sex = 'male' )$lt 
  # step 3: use prevalence rates to construct health expectancy   
  lt$nLx.Sulli <- 
    round( lt$nLx * ( 1 - pars[,2] ), 6 )
  
  lt$Tx.Sulli <- 
    round( rev( cumsum( rev( lt$nLx.Sulli ) ) ), 6 )
  
  lt$hx <- 
    round( lt$Tx.Sulli / lt$lx , 1 )
  
  # step 4: return result
  
  return( lt$hx[6] )
}

decompres_females_list <- 
  lapply( c( 'px_diabetes', 'px_cardio', 'px_osteop', 
             'px_dsblty_census' ),
          function( px ){
            pars_urb <- c(
              hx_dat[ sex == 'f' & urb == 'urb' ]$mx,
              hx_dat[ sex == 'f' & urb == 'urb', get( px ) ]
            )
            
            pars_rur <- c(
              hx_dat[ sex == 'f' & urb == 'rur' ]$mx,
              hx_dat[ sex == 'f' & urb == 'rur', get( px ) ]
            )
            
            dec_setpwise <- 
              stepwise_replacement( func = calc_h20_females, 
                                    pars1 = pars_urb, 
                                    pars2 = pars_rur, 
                                    symmetrical = TRUE )
            
            dim( dec_setpwise ) = c( length( pars_rur ) / 2, 2 )
            
            decomp_res <- 
              data.table( dec_setpwise ) %>%
              setnames( c( 'mx_cntrb', 'px_cntrb' ) ) %>%
              .[ , x := c( 0, 1, seq( 5, 80, 5 ) ) ] %>%
              .[ , dsblty := px ] %>%
              .[ , .( x, 
                      sex = 'f',
                      mx_cntrb, px_cntrb, dsblty ) ]
            
            return( decomp_res )
          } )

decompres_females <- 
  do.call( rbind, decompres_females_list )


decompres <- 
  rbind(
    decompres_females,
    decompres_males
  )

write.table( decompres,
             file = 'DATA/PROCESSED/stepwisedecomp_results.csv',
             row.names = F )


### THE END