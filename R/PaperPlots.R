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

# 2.1 Andreev's decomposition data
HthDecomp <- 
  readRDS( 
    file = 'DATA/HealthDecompAndreev.rds'
    )

# 2.2 Sulli life table, prevalence data
ltSulli <- 
  readRDS(
    file = "DATA/LifeTableSulliAdult.rds"
  )

# 2.3 Census mortality data
census.dat <- 
  readRDS(
    file = "DATA/BRCENSUS2010AdjDeaths.rds"
  )

# 2.4 Life table data
lt <- 
  readRDS(
    file = "DATA/LifeTable.rds"
  )
###################################################################

### 3. Variables dictionaries #------------------------------------

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



###################################################################

### 4. Graph 1 - Mortality rates #---------------------------------
census.dat[ age < 5, age:= 0 ]

newcensus.dat <- 
  census.dat[,
             list(
               deaths.ggbseg = sum( deaths.ggbseg ),
               pop           = sum( pop ),
               deaths        = sum( deaths )
               ),
             .( urb, sex, age )
             ]

dat.plot1 <- 
  newcensus.dat[,
                list(
                  urb, 
                  sex = factor( dic.sex[ sex ], 
                                levels = c( 'Males', 'Females' ) ), 
                  age,
                  mx.ggbseg = deaths.ggbseg / pop,
                  mx        = deaths / pop 
                  )
                ]
               

plot1.mx <-
  ggplot(
    data = dat.plot1 
  ) +
  geom_smooth( 
    aes( 
      x = age, 
      y = mx.ggbseg,
      color    = urb, 
      linetype = urb
    ),
    span = 0.36,
    se = F,
    size = 1.25
  ) +
  geom_point( 
    aes( 
      x = age, 
      y = mx.ggbseg,
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
    name  = ""
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
  'FIGURES/1_plot1_mx.eps', 
  width  = 8, 
  height = 4, 
  dpi    = 300
)
###################################################################

### 5. Table 1 - Life expectancy #---------------------------------
tab1.ex <- 
  lt[age %in% c(0,20,40,60),.(urb,age,sex,ex)] %>% 
  dcast(age~urb+sex,value.var='ex') %>%
  .[,
    list(
      age,
      urb_m,
      rur_m,
      diffurb_m = rur_m-urb_m,
      urb_f,
      rur_f,
      diffurb_f = rur_f-urb_f,
      diffsex_urb = urb_f-urb_m,
      diffsex_rur = rur_f-rur_m
      )
    ]

write.table( tab1.ex, 'FIGURES/2_table1_ex.csv', 
            row.names = F, dec = '.', sep = ';' )
###################################################################

### 6. Prevalences plot #------------------------------------------

# 6.1 Change labels
newltSulli <- 
  ltSulli %>% copy

newltSulli[,
        `:=`(
          sex  = factor( dic.sex[as.character(sex)],
                         levels = c( 'Males', 'Females' ) 
                         ),
          dsblty.type  = factor( dic.dsbType[as.character(dsblty.type)], 
                                 levels = c( 'Carviovascular\nDiseases', 
                                             'Diabetes', 
                                             'Osteopathies',
                                             'Census\nDisabilities' 
                                             )
                                 )
          )
        ]

DsbPrevPlot <-
  ggplot(
    data = newltSulli 
  ) +
  geom_line( 
    data = newltSulli,
    aes( 
      x = age, 
      y = 100 * ( dsblty.prev ),
      color    = urb, 
      linetype = urb
    ),
    size = 1.25
  ) +
  geom_point( 
    data = newltSulli,
    aes( 
      x = age, 
      y = 100 * ( dsblty.prevUns ),
      color    = urb,
      shape    = urb
    ),
    size = 2
  ) +
  facet_grid(
    sex ~ dsblty.type,
    scales = 'free'
  ) +
  scale_y_continuous(
    limits = c( 0, 80 ), 
    breaks = seq( 0, 80, 10),
    name = 'Disease/Disability Prevalence (%)' 
  ) +
  scale_x_continuous(
    limits = c( 17.5, 82.5 ), 
    breaks = seq( 20, 80, 5 ), 
    labels = dic.age[ as.character( seq( 20, 80, 5 ) ) ],
    name = ''
  ) +
  theme_bw() +
  scale_color_manual(
    values = c( 'urb' = 'black', 'rur' = 'gray60' ),
    labels = c( 'urb' = 'Urban', 'rur' = 'Rural' ),
    name = ''
  ) +
  scale_shape_manual(
    values = c( 'urb' = 19, 'rur' = 4 ),
    labels = c( 'urb' = 'Urban', 'rur' = 'Rural' ),
    name   = ''
  ) +
  scale_linetype_manual(
    values = c( 'urb' = 1, 'rur' = 5 ),
    labels = c( 'urb' = 'Urban', 'rur' = 'Rural' ),
    name   = ''
  ) + 
  theme(
    legend.position  = 'top',
    legend.direction = 'horizontal',
    legend.text      = element_text( size = 13, color = 'black' ),
    axis.title       = element_text( size = 13, color = 'black' ),
    axis.text.x      = element_text( size = 10, color = 'black', angle = 90, vjust = 0.5, hjust=1 ),
    axis.text.y      = element_text( size = 10, color = 'black' ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line( size = 0.25, linetype = 5, color = 'gray90' ),
    strip.text       = element_text( size  = 13, color = 'black' ),
    plot.caption     = element_text( hjust = 0,  size  = 12 )
  )

ggsave(
  'FIGURES/3_plot2_prevalence.eps', 
  width  = 8, 
  height = 4, 
  dpi    = 300
)
###################################################################

### 7. Table 2 - Health expectancy #--------------------------------
tab2.sulli <-
  ltSulli[ age %in% c(0,20,40,60), 
           .( urb, sex, age, type = dsblty.type, ex, ex.Sulli,
              hx = paste0( format( ex.Sulli, nsmall = 1 ), 
                           " (", 
                           format( round( ex.Sulli / ex, 2 ), nsmall = 2 ), 
                           ")" ) 
              ) ] %>%
  dcast(type+age~urb+sex,value.var='hx') %>%
  .[,
    list(
      type,
      age,
      urb_m,
      rur_m,
      urb_f,
      rur_f
    )]

write.table( tab2.sulli, 'FIGURES/4_table2_health.csv', 
             row.names = F, dec = '.', sep = ';' )
###################################################################


### 8. Decomposition plot #----------------------------------------

# 8.1 Change variables labels
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
                                                'Diabetes', 
                                               'Osteopathies',
                                               'Census\nDisabilities' )
            )
          )
          ]
# 8.2 Generate differences to input into decomposition graphs
HthDecompText.dat <-
  HthDecomp[ age < 85,
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
    labels = dic.age[ as.character( seq( 20, 80, 5 ) ) ],
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
  'FIGURES/5_plot3_decomp.eps', 
  width  = 8, 
  height = 5, 
  dpi    = 300
  )

###################################################################


