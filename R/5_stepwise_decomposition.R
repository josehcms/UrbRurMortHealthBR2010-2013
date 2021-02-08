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

### plot 
dic_dsbType <-
  c(
    'px_cardio'        = 'Carviovascular\nDiseases', 
    'px_osteop'        = 'Osteopathies', 
    'px_diabetes'      = 'Diabetes', 
    'px_dsblty_census' = 'Census\nDisabilities'
  )
dic_age <- 
  c('0'='0-4','5'='5-9','10'='10-14','15'='15-19','20'='20-24','25'='25-29','30'='30-34','35'='35-39',
    '40'='40-44','45'='45-49','50'='50-54','55'='55-59','60'='60-64','65'='65-69','70'='70-74','75'='75-79',
    '80'='+80')
decompres
HthDecomp <- 
  decompres %>%
  melt(
    id.vars = c( 'x', 'sex', 'dsblty' ),
    measure.vars = c( 'mx_cntrb', 'px_cntrb' ),
    value.name = 'cntrb',
    variable.name = 'type'
    ) %>%
  .[,
    list(
      age  = x, 
      sex  = factor( sex,
                     levels = c( 'm', 'f' ),
                     labels = c( 'Males', 'Females' ) 
                     ),
      type = factor( type,
                     levels = c( 'mx_cntrb', 'px_cntrb' ),
                     labels = c( 'Mortality', 'Health' ) 
            ),
      dsblty.type  = factor( dic_dsbType[ as.character( dsblty ) ], 
                             levels = c( 'Carviovascular\nDiseases',
                                         'Diabetes',
                                         'Osteopathies',
                                         'Census\nDisabilities' )
                             ),
      d = cntrb
      )
    ]

HthDecomp
# 8.2 Generate differences to input into decomposition graphs
HthDecompText.dat <-
  HthDecomp[ age >= 20,
             list(
               diff  = sum( d ),
               mdiff = sum( d[ type == 'Mortality' ] ),
               hdiff = sum( d[ type == 'Health' ] ),
               x = 20,
               y = 1.20
             ),
             by = c( 'sex', 'dsblty.type' )
  ]

# 8.3 Plot
require( ggplot2 )
HthDecompPlot <- 
  ggplot(
    data = HthDecomp[ age >= 20 ]
  ) +
  geom_col(
    aes( x = age , y = d , fill = type ),
    position = 'stack',
    color    = 'black'
  ) +
  scale_fill_manual(
    values = c( 'Mortality' = 'gray20', 'Health' = 'gray60' ),
    labels = c( 'Differences due to Mortality Rates', 
                'Differences due to Disease/Disability Prevalence Rates' ),
    name   = ''
  ) +
  facet_grid(
    sex ~ dsblty.type,
    scales = 'free'
  ) +
  scale_y_continuous(
    limits = c( -0.75, 1.5 ),
    breaks = seq( -1, 3, 0.25 ),
    name   = 'Rural-Urban differences in h(x)'
  ) +
  scale_x_continuous(
    limits = c( 17.5, 82.5 ),
    breaks = seq( 20, 80, 5 ),
    labels = dic_age[ as.character( seq( 20, 80, 5 ) ) ],
    name   = '')+
  theme_bw() + 
  theme(
    legend.position  = 'top',
    legend.direction = 'vertical',
    legend.text      = element_text( size = 12, color = 'black' ),
    axis.title       = element_text( size = 12, color = 'black' ),
    axis.text.x      = element_text( size = 10, color = 'black', angle = 90, vjust = 0.5, hjust=1 ),
    axis.text.y      = element_text( size = 10, color = 'black' ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line( size = 0.25, linetype = 5, color = 'gray90' ),
    strip.text       = element_text( size  = 12, color = 'black' ),
    plot.caption     = element_text( hjust = 0,  size  = 12 )
  ) +
  geom_text(
    data = HthDecompText.dat,
    aes( x     = x, 
         y     = y,
         label = paste0( 'h(20) Rural - Urban: ',
                         format( round( unique( diff ), 1 ), nsmall = 1 ),
                         '\n',
                         'Mortality contribution: ',
                         format( round( unique( mdiff ), 1 ), nsmall = 1 ),
                         '\n',
                         'Health contribution: ',
                         format( round( unique( hdiff ), 1 ), nsmall = 1 ) 
         )
    ),
    hjust = 0,
    size  = 2.4
  )

ggsave(
  filename = 'FIGURES/5_plot3_decomp.png',
  height = 6, width = 8 
)
HthDecompPlot
### THE END