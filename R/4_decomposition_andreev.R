###################################################################
### Title: Decomposition of Ex and DFLEx
### Author: Jose H C Monteiro da Silva
### Github: josehcms
### Last version: 2020-02-11
###################################################################

### 1. Housekeeping #----------------------------------------------
rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)
require(DemoTools)

###################################################################

### 2. Read data #------------------------------------------------

# 2.1 Sullivan Life Table and General Life Table
ltSulli <- 
  readRDS(
    file = "DATA/LifeTableSulliAdult.rds"
  )

###################################################################

### 3. Decomposition of disease free life expectancy #-------------

# 3.1 Create function for decomposition of health differences
HealthDecomp.Andreev <-
  function(
    age,
    lx1,
    lx2,
    qx1,
    qx2,
    nLx1,
    nLx2,
    pi1,
    pi2,
    age.min = 20,
    age.max = 80
  ){
    
    xmin = age.min
    xmax = age.max
    
    setDecompDat <- 
      function( age, lx, qx, nLx, pi, age.min = xmin, age.max = xmax ){
        
        require(data.table)
        
        stopifnot( 
          all_equal( 
            length( age ), length( lx ), length( qx ), 
            length( nLx ), length( pi )
            ) 
          )
        
        dat <- data.table( age, lx, qx, nLx, pi )
        
        dat[ , hx:= rev( cumsum( rev( nLx * ( pi ) ) ) ) / lx ]
        
        dat[ , Px:= nLx / lx ]
        
#        dat[ , lx := lx / 100000 ]
        
        dat <- 
          dat[ age %in% age.min:age.max ]
        
        return(dat)
      }
    
    dat1 <- 
      setDecompDat( age, lx = lx1, qx = qx1, nLx = nLx1, pi = pi1 )
    
    dat2 <- 
      setDecompDat( age, lx = lx2, qx = qx2, nLx = nLx2, pi = pi2 )
    
    lambda <- 
      data.table(
        age  = seq( age.min ,age.max, 5 ),
        d    = as.numeric( NA ),
        type = 'lambda'
      )
    
    gama   <- 
      data.table(
        age  = seq( age.min, age.max , 5 ),
        d    = as.numeric( NA ),
        type = 'gama'
      )
    
    for( age.count in seq( age.min , age.max , 5) ){
      
      if ( ( age.count + 5 ) == ( age.max + 5 ) ){
        
        hx1_age2 = 0
        hx2_age2 = 0
        
      } else{
        
        hx1_age2 = dat1[ age == age.count + 5 ]$hx
        hx2_age2 = dat2[ age == age.count + 5 ]$hx
      }
      
      lambda[ age == age.count ]$d <- 
        0.25 * ( dat1[ age == age.count ]$lx + dat2[ age == age.count ]$lx ) * 
        ( dat2[ age == age.count ]$Px - dat1[ age == age.count ]$Px ) * 
        ( dat1[ age == age.count ]$pi + dat2[ age == age.count ]$pi ) +
        0.5 * ( hx1_age2 * dat2[ age == age.count ]$lx + hx2_age2 * dat1[ age == age.count]$lx ) * 
        ( dat1[ age == age.count ]$qx - dat2[ age == age.count ]$qx )
      
      gama[ age == age.count ]$d <- 
        0.25 * ( dat1[ age == age.count ]$lx + dat2[ age == age.count ]$lx ) *
        ( dat1[ age == age.count ]$Px + dat2[ age == age.count ]$Px ) *
        ( dat2[ age == age.count ]$pi - dat1[ age == age.count ]$pi )
    }
    
    deltax <- rbind(lambda,gama)
    
    cat( paste0( 'Delta sum: ',
                   round ( sum( deltax$d ) , 4 ),
                 '\n' ) 
         )
    
    cat( paste0( 'hx diff = hx2 - hx1: ',
                   round ( dat2$hx[1] - dat1$hx[1] , 4 ),
                 '\n' )
         )
    
    cat( paste0( 'Health contribution: ',
                   round ( gama$d %>% sum , 4 ),
                 '\n' ) 
         )
    
    cat( paste0( 'Mortality contribution: ',
                   round ( lambda$d %>% sum , 4 ),
                 '\n\n' ) 
         )
    
    return( deltax )
    
  }

# 3.2 Create data by applying decomposition function
HthDecomp.dat <- data.table()

for( sexsel in c( 'm', 'f' ) ){
  for( prevsel in c( 'dsblty.cardio', 'dsblty.osteop', 'dsblty.diabetes', 'dsblty.census' ) ){
    HthDecomp.dat <- 
      rbind(
        HthDecomp.dat,
        HealthDecomp.Andreev(
          age  =     ltSulli[ urb == 'urb' & sex == sexsel & dsblty.type == prevsel ]$age,
          lx1  =     ltSulli[ urb == 'urb' & sex == sexsel & dsblty.type == prevsel ]$lx,
          lx2  =     ltSulli[ urb == 'rur' & sex == sexsel & dsblty.type == prevsel ]$lx,
          qx1  =     ltSulli[ urb == 'urb' & sex == sexsel & dsblty.type == prevsel ]$qx,
          qx2  =     ltSulli[ urb == 'rur' & sex == sexsel & dsblty.type == prevsel ]$qx,
          nLx1 =     ltSulli[ urb == 'urb' & sex == sexsel & dsblty.type == prevsel ]$nLx,
          nLx2 =     ltSulli[ urb == 'rur' & sex == sexsel & dsblty.type == prevsel ]$nLx,
          pi1  = 1 - ltSulli[ urb == 'urb' & sex == sexsel & dsblty.type == prevsel ]$dsblty.prev,
          pi2  = 1 - ltSulli[ urb == 'rur' & sex == sexsel & dsblty.type == prevsel ]$dsblty.prev
        ) %>%
          .[, sex := sexsel ] %>%
          .[, dsblty.type := prevsel ]
      )
  }
}

# 3.3 Save data
saveRDS( HtdDecomp.dat, file = 'DATA/HealthDecompAndreev.rds' )
###################################################################

### THE END
