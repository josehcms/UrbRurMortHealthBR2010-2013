###################################################################
### Title: Urban-Rural life tables and Sullivan Life tables
### Author: Jose H C Monteiro da Silva
### Github: josehcms
### Last version: 2020-02-10
###################################################################

### 1. Housekeeping #----------------------------------------------
rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)
require(DemoTools)

###################################################################

### 2. Read data #------------------------------------------------

# 2.1 2010 National Census data
census.dat <- 
  readRDS(
    file = "DATA/BRCENSUS2010AdjDeaths.rds"
  )

# 2.2 2013 national health survey data
pns.dat <- 
  readRDS(
    file = "DATA/PNS2013SexAgePrev.rds"
  )


###################################################################

### 3. Construct life table for BR2010 census #--------------------

lt.dat <- data.table()

for( sexsel in c( 'm', 'f' ) ){
  for( urbsel in c( 'urb', 'rur' ) ){
    lt.dat <-
      rbind(
        lt.dat,
        lt_abridged(
          Deaths    = census.dat[ sex == sexsel & urb == urbsel ]$deaths.ggbseg,
          Exposures = census.dat[ sex == sexsel & urb == urbsel ]$pop,
          Age       = census.dat[ sex == sexsel & urb == urbsel ]$age,
          Sex       = sexsel 
        ) %>%
          as.data.table %>%
          .[,
            list(
              urb = urbsel,
              sex = sexsel,
              age = Age,
              mx  = round( nMx, 6 ),
              qx  = round( nqx, 6 ),
              lx  = round(  lx, 0 ),
              nLx = round( nLx, 0 ),
              ex  = round(  ex, 1 )
              )
            ]
      )
  }
}

###################################################################

### 4. Merge life table and disability data #----------------------

lt.dat <- 
  merge(
    lt.dat,
    pns.dat[, .( urb, sex, age, dsblty.cardio, dsblty.osteop, dsblty.diabetes ) ],
    by = c( 'urb', 'sex', 'age' )
  ) %>%
  merge(
    census.dat[, .( urb, sex, age, dsblty.census = round( dsblty.census / pop, 5 ) ) ],
    by = c( 'urb', 'sex', 'age' )
  )
###################################################################

### 5. Construct Sullivan Life Table #-----------------------------

# 5.1 Sullivan LT function
lt_sullivan <- 
  function( age, nLx, lx, AgePrev ){
    
    stopifnot( all.equal( length( age ), length( nLx ), length( AgePrev ), length( lx ) ) )

    # person-years lived without disability/disease    
    nLx.Sulli <- 
      round( nLx * (1 - AgePrev), 0 )
    
    Tx.Sulli <- 
      round( rev( cumsum( rev( nLx.Sulli ) ) ), 6 )
    
    ex.Sulli <- 
      round( Tx.Sulli / lx , 1 )
    
    Tx <- 
      round( rev( cumsum( rev( nLx ) ) ), 6 )
    
    ex <- 
      round( Tx / lx , 1 )
    
    ltSulli <- 
      data.table( 
        age         = age,
        lx          = lx,
        dsblty.prev = AgePrev,
        nLx         = nLx,
        nLx.Sulli   = nLx.Sulli,
        ex          = ex,
        ex.Sulli    = ex.Sulli
        )
    
    return(ltSulli)
  }

# 5.2 Data on melted format:

ltMelt.dat <- 
  melt(lt.dat,
       id.vars       = c( 'urb', 'sex', 'age', 'mx', 'qx', 'lx', 'nLx', 'ex' ),
       measure.vars  = c( 'dsblty.cardio', 'dsblty.osteop', 'dsblty.diabetes', 'dsblty.census' ),
       variable.name = 'dsblty.type',
       value.name    = 'dsblty.prev'
       )

# 5.3 Smooth disability data using loess:
ltMelt.dat[,
           dsblty.prev := round( loess( dsblty.prev ~ age, degree = 1, span = 0.4 )$fitted, 5 ),
           by = c( 'urb', 'sex', 'dsblty.type' )
           ]

ltMelt.dat[, 
           dsblty.prev := ifelse( dsblty.prev < 0, 0, dsblty.prev )
           ]

# 5.4 Construct Sullivan LT for all disability types

ltSulli.dat <- 
  data.table()

for( urbsel in c( 'urb', 'rur' ) ){
  for( sexsel in c( 'm', 'f' ) ){
    for(dsbType in c( 'dsblty.cardio', 'dsblty.osteop', 'dsblty.diabetes', 'dsblty.census' ) ){
      ltSulli.dat <- 
        rbind(
          ltSulli.dat,
          lt_sullivan(
            age     = ltMelt.dat[ urb == urbsel & sex == sexsel & dsblty.type == dsbType ]$age,
            nLx     = ltMelt.dat[ urb == urbsel & sex == sexsel & dsblty.type == dsbType ]$nLx,
            lx      = ltMelt.dat[ urb == urbsel & sex == sexsel & dsblty.type == dsbType ]$lx,
            AgePrev = ltMelt.dat[ urb == urbsel & sex == sexsel & dsblty.type == dsbType ]$dsblty.prev
          ) %>%
            .[,
              list(
                urb = urbsel,
                sex = sexsel,
                age,
                mx  = ltMelt.dat[ urb == urbsel & sex == sexsel & dsblty.type == dsbType ]$mx,
                qx  = ltMelt.dat[ urb == urbsel & sex == sexsel & dsblty.type == dsbType ]$qx,
                lx  = round(  lx, 0 ),
                nLx = round( nLx, 0 ),
                ex  = round(  ex, 1 ),
                dsblty.type = dsbType,
                dsblty.prev = dsblty.prev,
                nLx.Sulli = nLx.Sulli,
                ex.Sulli
                )
              ]
          ) 
    }
  }
}

# 5.5 Save LT data
saveRDS( ltSulli.dat, file = 'DATA/LifeTableSulli.rds')

##################################################################

### THE END

