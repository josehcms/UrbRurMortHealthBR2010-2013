###################################################################
### Title: Urban-Rural life tables and Sullivan Life tables
### Author: Jose H C Monteiro da Silva
### Github: josehcms
### Last version: 2020-04-16
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

# 3.1 Using Demotools Function
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

saveRDS( lt.dat, file = 'DATA/LifeTable.rds')

# # Alternative way - only for adult population aged 20+
# 
# LifeTable <- function( Age, Deaths, Exposures ){
#   x = Age
#   nMx = Deaths / Exposures
#   nmax <- length(x)
#   #nMx = nDx/nKx
#   n <- c(diff(x), 999)          		  # width of the intervals
#   nax <- n/2;		            	        # default to .5 of interval
#   nax[nmax] <- 1/nMx[nmax] 	  	      # e_x at open age interval
#   nqx <- (n * nMx) / (1 + (n - nax) * nMx)
#   nqx<-ifelse(nqx > 1, 1, nqx);		    # necessary for high nMx
#   nqx[nmax] <- 1.0
#   lx <- c(1, cumprod(1 - nqx))*100000;   	  # survivorship lx
#   lx <- lx[1:length(nMx)]
#   ndx <- lx * nqx;
#   nLx <- n * lx - nax * ndx;      	 # equivalent to n*l(x+n) + (n-nax)*ndx
#   nLx[nmax] <- lx[nmax] * nax[nmax]
#   Tx <- rev(cumsum(rev(nLx)))
#   ex <- ifelse( lx[1:nmax] > 0, Tx/lx[1:nmax], NA);
#   lt <- data.table(Age = x, nqx = nqx, lx = lx, nLx = nLx, Tx = Tx, ex = ex, nMx = nMx)
#   return(lt)
# }
# 
# lt.dat <- data.table()
# 
# for( sexsel in c( 'm', 'f' ) ){
#   for( urbsel in c( 'urb', 'rur' ) ){
#     lt.dat <-
#       rbind(
#         lt.dat,
#         LifeTable(
#           Deaths    = census.dat[ sex == sexsel & urb == urbsel & age %in% 20:80 ]$deaths.ggbseg,
#           Exposures = census.dat[ sex == sexsel & urb == urbsel & age %in% 20:80 ]$pop,
#           Age       = census.dat[ sex == sexsel & urb == urbsel & age %in% 20:80 ]$age
#         ) %>%
#           as.data.table %>%
#           .[,
#             list(
#               urb = urbsel,
#               sex = sexsel,
#               age = Age,
#               mx  = round( nMx, 6 ),
#               qx  = round( nqx, 6 ),
#               lx  = round(  lx, 0 ),
#               nLx = round( nLx, 0 ),
#               ex  = round(  ex, 1 )
#             )
#             ]
#       )
#   }
# }

###################################################################

### 4. Merge life table and disability data #----------------------

lt.dat <- 
  merge(
    lt.dat,
    pns.dat[, .( urb, sex, age, dsblty.cardio, dsblty.osteop, dsblty.diabetes ) ],
    by = c( 'urb', 'sex', 'age' ),
    all.x = TRUE
  ) %>%
  merge(
    census.dat[, .( urb, sex, age, dsblty.census = round( dsblty.census / pop, 5 ) ) ],
    by = c( 'urb', 'sex', 'age' ),
    all.x = TRUE
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
        lx          = lx / 100000,
        dsblty.prev = AgePrev,
        nLx         = nLx / 100000,
        nLx.Sulli   = nLx.Sulli / 100000,
        Tx          = Tx / 100000,
        Tx.Sulli    = Tx.Sulli / 100000,
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
       ) %>%
  .[, dsblty.prev := ifelse( is.na( dsblty.prev ), 0, dsblty.prev ) ]

# 5.3 Smooth disability data using loess:
ltMelt.dat[,
           dsblty.prevUns := dsblty.prev # UnSmothed
           ]

ltMelt.dat[,
           dsblty.prev := round( loess( dsblty.prevUns ~ age, degree = 1, span = 0.4 )$fitted, 5 ),
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
                lx  = round(  lx, 5 ),
                nLx = round( nLx, 5 ),
                Tx  = round(  Tx, 5 ),
                ex  = round(  ex, 1 ),
                dsblty.type = dsbType,
                dsblty.prev = dsblty.prev,
                nLx.Sulli = round( nLx.Sulli, 5 ),
                Tx.Sulli  = round( Tx.Sulli, 5 ),
                ex.Sulli
                )
              ]
          ) 
    }
  }
}

# 5.5 Add Unsmoothed prev data to Sullidat
ltSulli.dat <- 
  merge(
    ltSulli.dat,
    ltMelt.dat[, .( urb, sex, age, dsblty.type, dsblty.prevUns ) ],
    by = c( 'urb', 'sex', 'age', 'dsblty.type' )
  )

# 5.6 Save LT data
saveRDS( ltSulli.dat, file = 'DATA/LifeTableSulliAdult.rds')

##################################################################

### THE END

