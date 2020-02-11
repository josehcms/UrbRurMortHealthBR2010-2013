###################################################################
### Title: Plots for paper on Rural-Urban health differentials
### Author: Jose H C Monteiro da Silva
### Github: josehcms
### Last version: 2020-02-11
###################################################################

### 1. Housekeeping #----------------------------------------------
rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)
require(ggplot2)

###################################################################

### 2. Read data #------------------------------------------------

# 2.1 Andreev's decomposition data
HthDecomp <- 
  readRDS( 
    file = 'DATA/HealthDecompAndreev.rds'
    )
###################################################################

### 3. Update variables labels for plots #-------------------------

# 3.1 Variables dictionaries

dic.type <- 
  c( 
    'lambda' = 'Mortality',
    'gama'   = 'Health'
  )

dic.sex <- 
  c( 
    'm' = 'Males',
    'f' = 'Females'
  )

dic.dsbType <-
  c(
    'dsblty.cardio'   = 'Carviovascular\nDiseases', 
    'dsblty.osteop'   = 'Osteopathies', 
    'dsblty.diabetes' = 'Diabetes', 
    'dsblty.census'   = 'Census\nDisabilities'
  )

dic.age <- 
  c('0'='0-4','5'='5-9','10'='10-14','15'='15-19','20'='20-24','25'='25-29','30'='30-34','35'='35-39',
    '40'='40-44','45'='45-49','50'='50-54','55'='55-59','60'='60-64','65'='65-69','70'='70-74','75'='75-79',
    '80'='+80')


# 3.2 Change variables labels
HthDecomp[,
          `:=`(
            sex  = factor( dic.sex[as.character(sex)],
                           levels = c( 'Males', 'Females' ) 
                           ),
            type = factor( dic.type[as.character(type)], 
                           levels = c( 'Mortality', 'Health' ) 
                           ),
            dsblty.type  = factor( dic.dsbType[as.character(dsblty.type)], 
                                   levels = c( 'Carviovascular\nDiseases', 
                                               'Osteopathies', 'Diabetes', 'Census\nDisabilities' )
                                   )
            )
          ]
###################################################################

### 4. Decomposition plot #----------------------------------------

# 4.1 Generate differences to input into decomposition graphs
HthDecompText.dat <-
  HthDecomp[ age < 85,
             list(
               diff  = sum( d ),
               mdiff = sum( d[ type == 'Mortality' ] ),
               hdiff = sum( d[ type == 'Health' ] ),
               x = 20,
               y = 2.0
             ),
             by = c( 'sex', 'dsblty.type' )
             ]

HthDecompPlot <- 
  ggplot(
    data = HthDecomp
    ) +
  geom_col(
    aes( x = age , y = d , fill = type ),
         position = 'stack',
         color    = 'black'
    ) +
  scale_fill_manual(
    values = c( 'Mortality' = 'black', 'Health' = 'gray60' ),
    name   = ''
    ) +
  facet_grid(
    sex ~ dsblty.type,
    scales = 'free'
    ) +
  scale_y_continuous(
    limits = c( -1, 2.5 ),
    breaks = seq( -1, 3, 0.50 ),
    name   = 'Rural-Urban differences in DFLE'
    ) +
  scale_x_continuous(
    limits = c( 17.5, 82.5 ),
    breaks = seq( 20, 80, 5 ),
    labels = dic.age[ as.character( seq( 20, 80, 5 ) ) ],
    name   = '')+
  theme_bw() + 
  theme(
    legend.position  = 'top',
    legend.direction = 'horizontal',
    legend.text      = element_text( size = 14, color = 'black' ),
    axis.title       = element_text( size = 14, color = 'black' ),
    axis.text.x      = element_text( size = 12, color = 'black', angle = 90, vjust = 0.5, hjust=1 ),
    axis.text.y      = element_text( size = 12, color = 'black' ),
    panel.grid.minor = element_blank(),
    strip.text       = element_text( size  = 14, color = 'black' ),
    plot.caption     = element_text( hjust = 0,  size  = 12 )
    ) +
  geom_text(
    data = HthDecompText.dat,
    aes( x     = x, 
         y     = y,
         label = paste0( 'DFLE(20) Rural - Urban: ',
                         round( unique( diff ), 1 ),
                         '\n',
                         'Mortality contribution: ',
                         round( unique( mdiff ), 1 ),
                         '\n',
                         'Health contribution: ',
                         round( unique( hdiff ), 1 ) 
                         )
         ),
    hjust = 0
    )
###################################################################

x11()
HthDecompPlot
