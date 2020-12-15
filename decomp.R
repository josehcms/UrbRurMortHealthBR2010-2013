rm(list = ls())
graphics.off()

require(dplyr)
require(data.table)
require(DemoTools)
require(MortalityLaws)
###################################################################

### 2. Read data #------------------------------------------------

# 2.1 2010 National Census data
census.dat <- 
  readRDS(
    file = "DATA/BRCENSUS2010AdjDeathsUrb.rds"
  )

# 2.2 2013 national health survey data
pns.dat <- 
  readRDS(
    file = "DATA/PNS2013SexAgePrev.rds"
  )

dat <- 
  merge(
    census.dat,
    pns.dat[, - c( 'pop' ) ],
    by = c( 'urb', 'sex', 'age' ),
    all = T
  ) %>%
  .[ , `:=`(
    dsblty.pns = ifelse( is.na( dsblty.pns ),
                         0, 
                         dsblty.pns ),
    dsblty.cardio = ifelse( is.na( dsblty.cardio ),
                            0, 
                            dsblty.cardio ),
    dsblty.osteop = ifelse( is.na( dsblty.osteop ),
                            0, 
                            dsblty.osteop ),
    dsblty.diabetes = ifelse( is.na( dsblty.diabetes ),
                            0, 
                            dsblty.diabetes ),
    mx = deaths_adj / pop 
  )]

pars_urb <- c(
  dat[ sex == 'm' & urb == 'urb' & age >= 20 ]$mx,
  dat[ sex == 'm' & urb == 'urb' & age >= 20 ]$dsblty.osteop
)

pars_rur <- c(
  dat[ sex == 'm' & urb == 'rur' & age >= 20 ]$mx,
  dat[ sex == 'm' & urb == 'rur' & age >= 20 ]$dsblty.osteop
)

# pars mx and then prev
calc_hx <- function(pars){
  
  # step 1: convert from vec into something we can use
  N        <- length(pars)
  dim(pars)<- c(N / 2, 2) # rows, columns
  
  # step 2: compute life table
  # lt <- 
  #   lt_abridged( Age = c( 0, 1, seq( 5, 80, 5 ) ),
  #                nMx = pars[,1],
  #                sex = 'm' )
  # 
  lt <- 
    LifeTable( x = seq( 20, 80, 5 ),
               mx = pars[,1], sex = 'male' )$lt 
  # step 3: use prevalence rates to construct health expectancy   
  lt$nLx.Sulli <- 
    round( lt$Lx * ( 1 - pars[,2] ), 6 )
  
  lt$Tx.Sulli <- 
    round( rev( cumsum( rev( lt$nLx.Sulli ) ) ), 6 )
  
  lt$hx <- 
    round( lt$Tx.Sulli / lt$lx , 1 )
  
  # step 4: return result
  
  return( lt$hx[1] )
}

calc_hx(pars_urb)
calc_hx(pars_rur)
require(DemoDecomp)
dec_s <- stepwise_replacement( func = calc_hx, 
                               pars1 = pars_urb, 
                               pars2 = pars_rur, 
                               symmetrical = TRUE )
dim(dec_s) = c( length( pars_rur ) / 2, 2 )

sum(dec_s[,1])
sum(dec_s[,2])

decomp_res <- 
  data.table( dec_s ) %>%
  setnames( c( 'mx', 'px' ) ) %>%
  .[ , x := seq( 20, 80, 5 ) ]
decomp_res
sum(dec_s)
data(Mxc1)
data(Mxc2)
# we'll want to pass in these dimensions
dims  <- dim(Mxc1)
# we need parameters in vec form
Mxc1v <- c(Mxc1)
Mxc2v <- c(Mxc2)
dims  <- dim(Mxc1)
# we need parameters in vec form
Mxc1v <- c(Mxc1)
Mxc2v <- c(Mxc2)
B     <- stepwise_replacement(func = Mxc2e0abrvec, 
                              pars1 = Mxc1v, pars2 = Mxc2v, dims = dims, 
                              # authors' recommendations:
                              symmetrical = TRUE, direction = "up")
dim(B) <- dims