rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)
require(DDM)
require(ggplot2)

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

dat2010 <- 
  rbind(
    readRDS( 'DATA/BRCENSUS2010AdjDeathsSingleAges.rds' ) %>%
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
    readRDS( 'DATA/BRCENSUS2010AdjDeathsSingleAges.rds' ) %>%
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

dat_ddm <- 
  merge(
    dat2000,
    dat2010,
    by = c( 'reg', 'sex', 'age' )
  ) %>%
  .[ , date1 := as.Date( '2000-07-31' ) ] %>%
  .[ , date2 := as.Date( '2010-07-31' ) ] %>%
  .[ , deaths := ( deaths / ( pop2 ) ) * sqrt( pop1 * pop2 ) ] %>%
  .[ , .(
    cod = reg,
    sex, age,
    date1, pop1,
    date2, pop2,
    deaths ) ]


ddm_res <- 
  rbind(
    ddm( dat_ddm[ sex == 'm' ], 
         exact.ages.ggb = seq( 5, 65, 5 ), 
         exact.ages.seg = seq( 15, 65, 5 ),
         deaths.summed = FALSE ) %>%
      as.data.table %>%
      .[ , sex := 'm' ] %>%
      .[ , reg := id ] %>%
      .[ , .( reg, sex, ggbseg, ggb ) ],
    ddm( dat_ddm[ sex == 'f' ], 
         exact.ages.ggb = seq( 5, 65, 5 ), 
         exact.ages.seg = seq( 15, 65, 5 ),
         deaths.summed = FALSE ) %>%
      as.data.table %>%
      .[ , sex := 'f' ] %>%
      .[ , reg := id ] %>%
      .[ , .( reg, sex, ggbseg, ggb ) ]
  )
  
dat2010adj <- 
  rbind(
    readRDS( 'DATA/BRCENSUS2010AdjDeathsSingleAges.rds' ) %>%
      .[ , .( reg = as.character( reg ),
              urb, sex, 
              age = cut( as.numeric( age ),
                         breaks = c( 0, 1, seq( 5, 80, 5 ), Inf ),
                         labels = c( 0, 1, seq( 5, 80, 5 ) ),
                         right  = FALSE ) %>% 
                paste0 %>% as.numeric,
              pop, 
              deaths ) ] %>%
      .[ , .( pop2 = sum( pop ),
              deaths = sum( deaths ) ),
         .( reg, urb, sex, age ) ],
    readRDS( 'DATA/BRCENSUS2010AdjDeathsSingleAges.rds' ) %>%
      .[ , .( reg = '0',
              urb, sex, 
              age = cut( as.numeric( age ),
                         breaks = c( seq( 0, 80, 5 ), Inf ),
                         labels = c( seq( 0, 80, 5 ) ),
                         right  = FALSE ) %>% 
                paste0 %>% as.numeric,
              pop, 
              deaths ) ] %>%
      .[ , .( pop2 = sum( pop ),
              deaths = sum( deaths ) ),
         .( reg, urb, sex, age ) ]
  ) %>%
  merge(
    ddm_res,
    by = c( 'reg', 'sex' )
  ) %>%
  .[ , deaths_adj := ifelse( age >= 15 & age <= 65,
                             deaths / ggbseg,
                             deaths ) ] %>%
  .[ reg != 0 , 
     list(
       deaths_adj = sum( deaths_adj ),
       deaths     = sum( deaths ),
       pop        = sum( pop2 )
     ),
     .( urb, sex, age ) ]

dat2010adj[ , mx := deaths_adj / ( pop + deaths_adj / 2 ) ]

dat2010adj_reg <- 
  dat2010adj %>% copy
require(DemoTools)
require(MortalityLaws)

lt_abridged(
  Deaths    = dat2010adj[ urb == 'rur' & sex == 'm' ]$deaths_adj,
  Exposures = dat2010adj[ urb == 'rur' & sex == 'm' ]$pop,
  Age       = dat2010adj[ urb == 'rur' & sex == 'm' ]$age,
  radix     = 1,
  Sex = 'm',
  )$ex[1] -
  lt_abridged(
  Deaths    = dat2010adj[ urb == 'urb' & sex == 'm' ]$deaths_adj,
  Exposures = dat2010adj[ urb == 'urb' & sex == 'm' ]$pop,
  Age       = dat2010adj[ urb == 'urb' & sex == 'm' ]$age
)$ex[1]

lt_abridged(
  Deaths    = dat2010adj[ urb == 'rur' & sex == 'f' ]$deaths_adj,
  Exposures = dat2010adj[ urb == 'rur' & sex == 'f' ]$pop,
  Age       = dat2010adj[ urb == 'rur' & sex == 'f' ]$age
)$ex[1] -
lt_abridged(
  Deaths    = dat2010adj[ urb == 'urb' & sex == 'f' ]$deaths_adj,
  Exposures = dat2010adj[ urb == 'urb' & sex == 'f' ]$pop,
  Age       = dat2010adj[ urb == 'urb' & sex == 'f' ]$age
)$ex[1]

ggbRes <- 
  rbind(
    ggbMakeColumns( dat_ddm[sex == 'f' & cod == 'rur'] ),
    ggbMakeColumns( dat_ddm[sex == 'f' & cod == 'urb'] ),
    ggbMakeColumns( dat_ddm[sex == 'm' & cod == 'rur'] ),
    ggbMakeColumns( dat_ddm[sex == 'm' & cod == 'urb'] )
  )

ggplot() +
  geom_text( data = ggbRes,
             aes( x = rightterm, y = leftterm, label = age ),
             size = 3 ) +
  geom_smooth( data = ggbRes[ age %in% 15:65 ],
               aes( x = rightterm, y = leftterm ),
               method = 'lm', se = F,
               size = 1, color = 'navy' ) +
  geom_abline( intercept = 0, slope = 1, 
               size = 0.12, color = 'red', linetype = 'longdash' ) +
  facet_wrap( vars( sex, cod ), nrow = 2 ) +
  theme_light() 


segRes <- 
  rbind(
    ggbsegMakeColumns( dat_ddm[sex == 'f' & cod == '1'],
                       agesFit.ggb = seq( 15, 65, 5 ) ),
    ggbsegMakeColumns( dat_ddm[sex == 'f' & cod == '2'],
                       agesFit.ggb = seq( 15, 65, 5 ) ),
    ggbsegMakeColumns( dat_ddm[sex == 'f' & cod == '3'],
                       agesFit.ggb = seq( 15, 65, 5 ) ),
    ggbsegMakeColumns( dat_ddm[sex == 'f' & cod == '4'],
                       agesFit.ggb = seq( 15, 65, 5 ) ),
    ggbsegMakeColumns( dat_ddm[sex == 'f' & cod == '5'],
                       agesFit.ggb = seq( 15, 65, 5 ) ),
    ggbsegMakeColumns( dat_ddm[sex == 'm' & cod == '1'],
                       agesFit.ggb = seq( 15, 65, 5 ) ),
    ggbsegMakeColumns( dat_ddm[sex == 'm' & cod == '2'],
                       agesFit.ggb = seq( 15, 65, 5 ) ),
    ggbsegMakeColumns( dat_ddm[sex == 'm' & cod == '3'],
                       agesFit.ggb = seq( 15, 65, 5 ) ),
    ggbsegMakeColumns( dat_ddm[sex == 'm' & cod == '4'],
                       agesFit.ggb = seq( 15, 65, 5 ) ),
    ggbsegMakeColumns( dat_ddm[sex == 'm' & cod == '5'],
                       agesFit.ggb = seq( 15, 65, 5 ) )
  )

segRes[age %in% 10:70 ] %>%
  ggplot() +
  geom_hline( yintercept = 1.00, 
              color = 'tomato3', size = 0.15, linetype = 'longdash' ) +
  geom_hline( yintercept = 0.80, 
              color = 'tomato3', size = 0.15, linetype = 'longdash' ) +
  geom_hline( yintercept = 0.60, 
              color = 'tomato3', size = 0.15, linetype = 'longdash') +
  geom_point( aes( x = age, y = Cx,
                   color = sex,
                   shape = cod ) ) +
  
  scale_color_manual( values = c( 'f' = 'red',
                                  'm' = 'blue' ),
                      labels = c( 'f' = 'Females',
                                  'm' = 'Males' ),
                      name = '' ) +
  scale_shape_manual( values = c( '1' = 19,
                                  '2' = 10,
                                  '3' = 12,
                                  '4' = 14,
                                  '5' = 15 ) ) +
  scale_y_continuous( limits = c( 0, 1 ),
                      breaks = seq( 0, 1, 0.10 ) ) +
  scale_x_continuous( limits = c( 5, 70 ),
                      breaks = seq( 5, 70, 5 ) ) +
  facet_wrap( ~ cod, nrow = 1 ) +
  theme_light() +
  theme( legend.position = 'top' ) 
