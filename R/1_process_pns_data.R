###################################################################
### Title: Read and structure BR National Health Survey 2013 data
### Author: Jose H C Monteiro da Silva
### Github: josehcms
### Last version: 2020-12-14
###################################################################

### 1. Housekeeping #----------------------------------------------
rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)
###################################################################

### 2. Read data and select variables #----------------------------

# 2.1 household data
hshold_filter <- 
  c( 
    "V0024", "UPA_PNS", "V0006_PNS", "V0026", "A002", "A003",
    "A004", "A005", "A016", "A017", "A01801", "A01803", 
    "A01807", "A01817", "A019", "A020", "B001", "B003",
    "B004", "V0028", "V00281", "V00282", "V00283", "V00293"
  )

hshold_dat <- 
  fread( 'DATA/DOMPNS2013.csv' ) %>%
  select( hshold_filter )

# 2.2 Person data
person_filter <- 
  c(
    "V0024", "UPA_PNS", "V0006_PNS", "Q002", "Q030", "Q060",
    "Q063", "Q068", "Q074", "Q079", "Q084", "Q088", "Q092",
    "Q116", "Q120", "Q121", "Q124", "Q128", "X001", "X003",
    "V0029", "V00291", "V0028", "V00281", "C006", "C008",
    "V00283", "V00293", "N003", "N020", "N022", "N023",
    'J001', 'J002', 'J004', 'J007', 'J008', 'J010', 'J011',
    'J036', 'J058', 'K013', 'K022', 'M01101', 'M01102', 
    'O021', 'O024', 'O025', 'X008', 'X02003', 'X007', 'Q001',
    'Q029'
  )


person_dat <- 
  fread( 'DATA/PESPNS2013.csv' ) %>% 
  select( person_filter )

###################################################################

### 3. Adjust variables for analysis #-----------------------------

# 3.1 Household data adjusts
hshold_dat[, hshold_wght := as.numeric( V00281 ) ]

hshold_dat[, hshold_id := paste0( UPA_PNS,V0006_PNS ) ]

# 3.2 Person data adjusts 
person_dat[, 
           age := as.numeric( paste0( cut( as.numeric( C008 ), 
                                           breaks = c( 0, 1, 
                                                       seq( 5, 80, 5 ), 
                                                       Inf ), 
                                           labels = c( 0, 1, 
                                                       seq( 5, 80, 5 ) ),
                                           right = FALSE ) ) ) ]

person_dat[, person_wght := as.numeric( V00291 ) ]
person_dat[, hshold_id := paste0( UPA_PNS, V0006_PNS ) ]

# 3.3 Create vars for diferent disabilities
person_dat[,
           `:=`(
             dsblty_movement = ifelse( N003 == 4 | N003 == 5, 1, 0 ),
             dsblty_listen   = ifelse( N020 == 4 | N020 == 5, 1, 0 ),
             dsblty_vision   = ifelse( N023 == 4 | N023 == 5, 1, 0 ),
             dsblty_cardio   = ifelse( Q002 == 1 | Q060 == 1 | 
                                         Q063 == 1 | Q068==1, 1, 0 ),
             dsblty_osteop   = ifelse( Q079 == 1 | Q084 == 1 | 
                                         Q088 == 1, 1, 0 ),
             dsblty_diabetes = ifelse( Q030 == 1, 1, 0 )
           )
]

person_dat[, dsblty_pns := ifelse( dsblty_movement == 1 | 
                                     dsblty_listen == 1 | 
                                     dsblty_vision == 1, 1, 0 ) ]

# 3.4 Merge person and household data on urban-rural and recode
pns_dat <- 
  merge(
    person_dat[, .( hshold_id, age, 
                    sex = C006, 
                    person_wght, 
                    dsblty_pns, 
                    dsblty_cardio, 
                    dsblty_osteop, 
                    dsblty_diabetes ) ],
    hshold_dat[, .( hshold_id, urb = V0026 ) ],
    by = "hshold_id",
    all.x = T
  ) %>%
  .[ !is.na( person_wght ) , ] %>%
  .[, urb := ifelse( urb == 1, 'urb', 'rur' ) ] %>%
  .[, sex := ifelse( sex == 1, 'm', 'f' ) ]

###################################################################

### 4. Apply weights to person data and compute prevalences #------

# 4.1 By Urban-Rural, Sex and Age
pns_prev_dat <- 
  pns_dat[,
          list(
            pop        = round( sum( person_wght ) ),
            px_dsblty  = round( sum( person_wght * dsblty_pns ) / 
                                  sum( person_wght ), 5 ),
            px_cardio  = round( sum( person_wght * dsblty_cardio, 
                                     na.rm = TRUE ) / 
                                  sum( person_wght ), 5 ), 
            px_osteop   = round( sum( person_wght * dsblty_osteop, 
                                      na.rm = TRUE  ) / 
                                   sum( person_wght ), 5 ), 
            px_diabetes = round( sum( person_wght * dsblty_diabetes, 
                                      na.rm = TRUE  ) / 
                                   sum( person_wght ), 5 )
          ),
          by = c( 'urb', 'sex', 'age' )
  ] %>%
  setorder( urb, sex, age )

write.table( pns_prev_dat,
             file = 'DATA/PROCESSED/pns_prevalence_rates_processed.csv',
             row.names = F )

# 4.2 By Urban-Rural and sex
# pnsSexPrev.dat <- 
#   pns_dat[age >= 15 & age <= 64,
#           list(
#             pop             = round( sum( person.wght ) ),
#             dsblty.pns      = round( sum( person.wght * dsblty.pns ) / sum( person.wght ), 5 ),
#             dsblty.cardio   = round( sum( person.wght * dsblty.cardio, na.rm = TRUE ) / sum( person.wght ), 5 ), 
#             dsblty.osteop   = round( sum( person.wght * dsblty.osteop, na.rm = TRUE  ) / sum( person.wght ), 5 ), 
#             dsblty.diabetes = round( sum( person.wght * dsblty.diabetes, na.rm = TRUE  ) / sum( person.wght ), 5 )
#           ),
#           by = c( 'urb', 'sex' )
#   ] %>%
#   setorder( urb, sex )
# 
# saveRDS( pnsSexPrev.dat, file = 'DATA/PNS2013SexPrev.rds' )

##################################################################

### THE END