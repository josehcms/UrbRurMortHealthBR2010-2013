rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)
require(DDM)
require(ggplot2)

load('DATA/DATA_POP_2000.RData')
dat2000 <- 
  TAB[ , .( ufcode = UF,
            reg = substr( UF, 1, 1 ),
            urb = ifelse( RUR_URB == '1', 'urb', 'rur' ),
            sex = ifelse( SEXO == 1, 'm', 'f' ),
            age = ifelse( as.numeric( IDADE ) == 1,
                          0,
                          as.numeric( IDADE ) ),
            pop = as.numeric( N_PES ) ) ] %>%
  .[ , .( pop1 = sum( pop ) ),
     .( urb, sex, age ) ]
rm( TAB )

dat2010 <- 
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
     .( urb, sex, age ) ]

dat_ddm <- 
  merge(
    dat2000,
    dat2010,
    by = c( 'urb', 'sex', 'age' )
  ) %>%
  .[ , date1 := as.Date( '2000-07-31' ) ] %>%
  .[ , date2 := as.Date( '2010-07-31' ) ] %>%
  .[ , deaths := ( deaths / ( pop2 ) ) * sqrt( pop1 * pop2 ) ] %>%
  .[ , .(
    cod = urb,
    sex, age,
    date1, pop1,
    date2, pop2,
    deaths ) ]


ddm_res <- 
  rbind(
    ddm( dat_ddm[ sex == 'm' ], 
         exact.ages.ggb = seq( 15, 55, 5 ), 
         exact.ages.seg = seq( 15, 55, 5 ),
         deaths.summed = FALSE ) %>%
      as.data.table %>%
      .[ , sex := 'm' ] %>%
      .[ , urb := id ] %>%
      .[ , .( urb, sex, ggbseg, ggb ) ],
    ddm( dat_ddm[ sex == 'f' ], 
         exact.ages.ggb = seq( 5, 65, 5 ), 
         exact.ages.seg = seq( 15, 55, 5 ),
         deaths.summed = FALSE ) %>%
      as.data.table %>%
      .[ , sex := 'f' ] %>%
      .[ , urb := id ] %>%
      .[ , .( urb, sex, ggbseg, ggb ) ]
  )

dat2010adj <- 
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
     .( urb, sex, age ) ] %>%
  merge(
    ddm_res,
    by = c( 'urb', 'sex' )
  ) %>%
  .[ , deaths_adj := ifelse( age >= 15 & age <= 65,
                             deaths / ggbseg,
                             deaths ) ] %>%
  .[ , 
     list(
       deaths_adj = sum( deaths_adj ),
       deaths     = sum( deaths ),
       pop        = sum( pop2 )
     ),
     .( urb, sex, age ) ]

saveRDS( dat2010adj, file = 'DATA/BRCENSUS2010AdjDeathsUrb.rds')

dat2010adj[ , mx := deaths_adj / ( pop + deaths_adj / 2 ) ]

dat2010adj_rur <- 
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

datplotmx <- 
  rbind(
    dat2010adj_reg[, 
                   .( urb, sex, age,
                      mx = deaths / 
                        ( pop + ( deaths / 2 ) ),
                      type = 'Observed' ) ],
    dat2010adj_reg[, 
                   .( urb, sex, age,
                      mx = deaths_adj / 
                        ( pop + ( deaths_adj / 2 ) ),
                      type = 'GGB-SEG (Regions)' ) ],
    dat2010adj_rur[, 
                   .( urb, sex, age,
                      mx = deaths_adj / 
                        ( pop + ( deaths_adj / 2 ) ),
                      type = 'GGB-SEG (Urban-Rural)' ) ]
    )

ltres <- data.table()

for( s in c( 'm', 'f' ) ){
  for( ur in c( 'urb', 'rur' ) ){
    for( ty in c( 'GGB-SEG (Urban-Rural)',
                  'GGB-SEG (Regions)',
                  'Observed' ) ){
      aux <- 
        datplotmx[ sex == s & urb == ur & type == ty ] %>%
        copy
      
      lt <- 
        lt_abridged(
          nMx = aux$mx,
          Age = aux$age,
          sex = s,
          radix = 1
        )
      
      q1545 <- 
        ( lt[ lt$Age == 15, ]$lx - lt[ lt$Age == 60, ]$lx ) /
        lt[ lt$Age == 15, ]$lx
      
      e15 <- lt[ lt$Age == 15, ]$ex
      e30 <- lt[ lt$Age == 30, ]$ex
      e45 <- lt[ lt$Age == 45, ]$ex
      e60 <- lt[ lt$Age == 60, ]$ex
      
      ltres <- 
        rbind(
          ltres,
          data.table(
            urb = ur,
            type = ty,
            sex = s,
            q1545,
            e15, e30, e45, e60
          )
        )
    }
  }
}

ltres %>%
  dcast( type~sex+urb, value.var = 'e15' )

ggplot( data = datplotmx ) +
  geom_point( aes( x = as.numeric( paste0( age ) ), 
                   y = mx,
                   color = type ) ) +
  geom_line( aes( x = as.numeric( paste0( age ) ), 
                  y = mx,
                  color = type, linetype = type ),
             size = 0.90 ) +
  scale_y_log10() +
  facet_wrap( vars( sex, urb ) ) +
  theme_bw()


segRes <- 
  rbind(
    ggbsegMakeColumns( dat_ddm[sex == 'f' & cod == 'urb'],
                       agesFit.ggb = seq( 15, 55, 5 ) ),
    ggbsegMakeColumns( dat_ddm[sex == 'f' & cod == 'rur'],
                       agesFit.ggb = seq( 15, 55, 5 ) ),
    ggbsegMakeColumns( dat_ddm[sex == 'm' & cod == 'urb'],
                       agesFit.ggb = seq( 15, 55, 5 ) ),
    ggbsegMakeColumns( dat_ddm[sex == 'm' & cod == 'rur'],
                       agesFit.ggb = seq( 15, 55, 5 ) )
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
  scale_shape_manual( values = c( 'urb' = 19,
                                  'rur' = 10 ) ) +
  scale_y_continuous( limits = c( 0, 1 ),
                      breaks = seq( 0, 1, 0.10 ) ) +
  scale_x_continuous( limits = c( 5, 70 ),
                      breaks = seq( 5, 70, 5 ) ) +
  facet_wrap( ~ cod, nrow = 1 ) +
  theme_light() +
  theme( legend.position = 'top' ) 

