###################################################################
### Title: Plots for paper on Rural-Urban health differentials
### Author: Jose H C Monteiro da Silva
### Github: josehcms
### Last version: 2020-04-16
###################################################################

### 1. Housekeeping #----------------------------------------------
rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)
require(ggplot2)

###################################################################

### 2. Read data #------------------------------------------------

dic.age <- 
  c('0'='0-4','5'='5-9','10'='10-14','15'='15-19','20'='20-24','25'='25-29','30'='30-34','35'='35-39',
    '40'='40-44','45'='45-49','50'='50-54','55'='55-59','60'='60-64','65'='65-69','70'='70-74','75'='75-79',
    '80'='+80')

ddm_res <- 
  fread( 'DATA/PROCESSED/ddm_results_region.csv' ) %>%
  .[ ages_sel == '15-55' & id %in% 1:5 ]

census.dat <- 
  fread(
    file = "DATA/PROCESSED/population_deaths_region_processed.csv"
  ) %>%
  merge(
    ddm_res[ , .( reg = id, sex, ggbseg ) ]
  ) %>%
  .[ , age_grp := cut( age,
                       breaks = c( seq( 0, 80, 5 ), Inf ),
                       labels = seq( 0, 80, 5 ),
                       right = FALSE,
                       include.lowest = TRUE ) %>%
       as.character %>% as.numeric ] %>%
  .[ , age_grp2 := dic.age[ paste0( age_grp ) ] ] %>%
  .[ , deaths_cor := ifelse( age >= 20, deaths / ggbseg, deaths ) ] %>%
  .[ , 
     .( deaths = sum( deaths_cor ),
        pop = sum( pop ) ),
     .( urb, sex, age_grp, age_grp2 ) ] %>%
  .[ , mx := deaths / pop ] %>%
  .[ , sex := factor( sex,
                      levels = c( 'm', 'f' ),
                      labels = c( 'Males', 'Females' ) ) ] 


plot1.mx <-
  ggplot(
    data = census.dat
  ) +
  geom_smooth( 
    aes( 
      x = age_grp, 
      y = mx,
      color    = urb, 
      linetype = urb
    ),
    span = 0.36,
    se = F,
    size = 1.25
  ) +
  geom_point( 
    aes( 
      x = age_grp, 
      y = mx,
      color = urb, 
      shape = urb
    ),
    size = 1.75
  ) +
  facet_wrap(
    ~ sex,
    nrow = 1
  ) + 
  scale_y_continuous(
    trans  = "log10",
    name   = "Mortality Rates\n(Log10 Scale)"
  ) +
  scale_x_continuous(
    limits = c( 0, 80 ),
    breaks = seq( 0, 85, 5 ),
    labels = dic.age[ as.character( seq( 0, 85, 5 ) ) ],
    name   = ""
  ) +
  annotation_logticks( sides = 'l' ) +
  theme_bw() +
  scale_color_manual(
    values = c( 'urb' = 'black', 'rur' = 'gray60' ),
    labels = c( 'urb' = 'Urban', 'rur' = 'Rural' ),
    name = ''
  ) +
  scale_linetype_manual(
    values = c( 'urb' = 1, 'rur' = 5 ),
    labels = c( 'urb' = 'Urban', 'rur' = 'Rural' ),
    name   = ''
  ) + 
  scale_shape_manual(
    values = c( 'urb' = 19, 'rur' = 4 ),
    labels = c( 'urb' = 'Urban', 'rur' = 'Rural' ),
    name   = ''
  ) + 
  labs( 
    caption = '*Mortality curves smoothed by aplying local polynomial regression fitting' 
  ) +
  theme(
    legend.position  = 'top',
    legend.direction = 'horizontal',
    legend.text      = element_text( size = 13, color = 'black' ),
    axis.title       = element_text( size = 13, color = 'black' ),
    axis.text.x      = element_text( size = 11, color = 'black', angle = 90, vjust = 0.5, hjust=1 ),
    axis.text.y      = element_text( size = 11, color = 'black' ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line( size = 0.25, linetype = 5, color = 'gray90' ),
    strip.text       = element_text( size  = 13, color = 'black' ),
    plot.caption     = element_text( hjust = 1,  size  = 9 )
  )

ggsave(
  'FIGURES/1_plot1_mx.png', 
  width  = 8, 
  height = 4, 
  dpi    = 300
)
