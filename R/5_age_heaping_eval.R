
require( data.table ); require( tidyverse ); require( readr )

dat_census <- 
  fread('DATA/PROCESSED/population_deaths_region_processed.csv') %>%
  .[ , .( pop = sum( pop ), deaths = sum( deaths ) ),
     .( urb, sex, age ) ] %>%
  .[ , pop_prop :=  ifelse( sex == 'm',
                            - pop / sum( pop ),
                            pop / sum( pop ) ),
     .( urb ) ] %>%
  .[ , deaths_prop :=  ifelse( sex == 'm', 
                               - deaths / sum( deaths ),
                               deaths / sum( deaths ) ),
     .( urb ) ] %>%
  setorder( urb, sex, age )


ggplot() +
  geom_point( data = dat_census[ urb == 'rur' ],
              aes( x = deaths_prop, y = age,
                   color = urb ), size = 1.5, 
              alpha = 0.90 ) +
  geom_point( data = dat_census[ urb == 'urb' ],
              aes( x = deaths_prop, y = age,
                   color = urb ), size = 1.5, 
              alpha = 0.25 ) +
  geom_segment( data = dat_census[ urb == 'rur' ],
                aes( x = 0, xend = deaths_prop,
                     y = age, yend = age,
                     color = urb  ),
                size = 1.25, alpha = 0.90 ) +
  geom_segment( data = dat_census[ urb == 'urb' ],
                aes( x = 0, xend = deaths_prop,
                     y = age, yend = age,
                     color = urb  ),
                size = 1.25, alpha = 0.25 ) +
  scale_color_manual( values = c( 'skyblue', 'gray5' ) ) +
  labs( title = 'Age Distribution - Deaths' ) +
  theme_light() 


ggplot() +
  geom_point( data = dat_census[ urb == 'rur' ],
              aes( x = pop_prop, y = age,
                   color = urb ), size = 1.5, 
              alpha = 0.90 ) +
  geom_point( data = dat_census[ urb == 'urb' ],
              aes( x = pop_prop, y = age,
                   color = urb ), size = 1.5, 
              alpha = 0.25 ) +
  geom_segment( data = dat_census[ urb == 'rur' ],
                aes( x = 0, xend = pop_prop,
                     y = age, yend = age,
                     color = urb  ),
                size = 1.25, alpha = 0.90 ) +
  geom_segment( data = dat_census[ urb == 'urb' ],
                aes( x = 0, xend = pop_prop,
                     y = age, yend = age,
                     color = urb  ),
                size = 1, alpha = 0.25 ) +
  labs( title = 'Age Distribution - Population' ) +
  scale_color_manual( values = c( 'skyblue', 'gray5' ) ) +
  theme_light() 

require( DemoTools )

check_heaping_myers( Value = dat_census[ sex == 'm' & urb == 'urb' & age < 90 ]$pop, 
                     Age = dat_census[ sex == 'm' & urb == 'urb' & age < 90 ]$age, 
                     ageMin = 15, ageMax = 89 )

check_heaping_myers( Value = dat_census[ sex == 'm' & urb == 'rur' & age < 90 ]$pop, 
                     Age = dat_census[ sex == 'm' & urb == 'rur' & age < 90 ]$age, 
                     ageMin = 15, ageMax = 89 )


check_heaping_whipple( Value = dat_census[ sex == 'm' & urb == 'urb' & age < 90 ]$pop, 
                       Age = dat_census[ sex == 'm' & urb == 'urb' & age < 90 ]$age, 
                       ageMin = 15, ageMax = 70,
                       digit = 0 )

check_heaping_whipple( Value = dat_census[ sex == 'm' & urb == 'rur' & age < 90 ]$pop, 
                       Age = dat_census[ sex == 'm' & urb == 'rur' & age < 90 ]$age, 
                       ageMin = 15, ageMax = 70,
                       digit = 0 )

check_heaping_whipple( Value = dat_census[ sex == 'm' & urb == 'urb' & age < 90 ]$pop, 
                       Age = dat_census[ sex == 'm' & urb == 'urb' & age < 90 ]$age, 
                       ageMin = 20, ageMax = 65,
                       digit = c( 0, 5 ) )

check_heaping_whipple( Value = dat_census[ sex == 'm' & urb == 'rur' & age < 90 ]$pop, 
                       Age = dat_census[ sex == 'm' & urb == 'rur' & age < 90 ]$age, 
                       ageMin = 20, ageMax = 65,
                       digit = c( 0, 5 ) )


check_heaping_whipple( Value = dat_census[ sex == 'f' & urb == 'urb' & age < 90 ]$pop, 
                       Age = dat_census[ sex == 'f' & urb == 'urb' & age < 90 ]$age, 
                       ageMin = 20, ageMax = 65,
                       digit = c( 0, 5 ) )

check_heaping_whipple( Value = dat_census[ sex == 'f' & urb == 'rur' & age < 90 ]$pop, 
                       Age = dat_census[ sex == 'f' & urb == 'rur' & age < 90 ]$age, 
                       ageMin = 20, ageMax = 65,
                       digit = c( 0, 5 ) )

dat_census[, .( ratio = sum( deaths[age >=80] ) / sum( deaths[age >=60] ) ),.(urb,sex)]


getHMDitemavail('SWE', username = 'zecosta.monteiro@gmail.com',password = 'papai123')
readHMDweb()