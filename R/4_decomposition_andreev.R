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
    file = "DATA/LifeTableSulli.rds"
  )

###################################################################

### 3. Decomposition of life expectancy differentials #------------


###################################################################

### 4. Decomposition of disease free life expectancy #-------------

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


HthDecomp.dat <- data.table()

for( sexsel in c( 'm', 'f' ) ){
  for( prevsel in c( 'dsblty.cardio', 'dsblty.osteop', 'dsblty.diabetes', 'dsblty.census' ) ){
    HthDecomp.dat <- 
      rbind(
        HthDecomp.dat,
        HealthDecomp.Andreev(age = ltSulli[ urb == 'urb' & sex == sexsel & dsblty.type == prevsel ]$age,
                             lx1 = ltSulli[ urb == 'urb' & sex == sexsel & dsblty.type == prevsel ]$lx,
                             lx2 = ltSulli[ urb == 'rur' & sex == sexsel & dsblty.type == prevsel ]$lx,
                             qx1 = ltSulli[ urb == 'urb' & sex == sexsel & dsblty.type == prevsel ]$qx,
                             qx2 = ltSulli[ urb == 'rur' & sex == sexsel & dsblty.type == prevsel ]$qx,
                             nLx1 = ltSulli[ urb == 'urb' & sex == sexsel & dsblty.type == prevsel ]$nLx,
                             nLx2 = ltSulli[ urb == 'rur' & sex == sexsel & dsblty.type == prevsel ]$nLx,
                             pi1 = 1-ltSulli[ urb == 'urb' & sex == sexsel & dsblty.type == prevsel ]$dsblty.prev,
                             pi2 = 1-ltSulli[ urb == 'rur' & sex == sexsel & dsblty.type == prevsel ]$dsblty.prev
        ) %>%
          .[, sex := sexsel ] %>%
          .[, dsblty.type := prevsel ]
      )
  }
}

text_data <- 
  HthDecomp.dat %>%
  copy %>%
  .[,type_diff:=sum(d),
    .(dsblty.type,sex,type)] %>%
  .[,.(type,dsblty.type,sex,type_diff)] %>% 
  unique %>%
  dcast(dsblty.type+sex~type,value.var=c('type_diff')) %>%
  .[,diff:=gama+lambda] %>%
  setnames(c('dsblty.type','sex','mdiff','hdiff','diff')) %>%
  .[,`:=`(
    x = 20,
    y = 2.0
  )]

    
dic_age <- 
  c('0'='0-4','5'='5-9','10'='10-14','15'='15-19','20'='20-24','25'='25-29','30'='30-34','35'='35-39',
    '40'='40-44','45'='45-49','50'='50-54','55'='55-59','60'='60-64','65'='65-69','70'='70-74','75'='75-79',
    '80'='+80')


require(ggplot2)
ggplot(data=HthDecomp.dat) +
  geom_col(aes( x=age , y=d , fill=type ),
           position = 'stack',
           color = 'black')+
  scale_fill_manual(values=c('lambda'='black',
                             'gama'='gray60'),
                    name='') +
  facet_grid(sex~dsblty.type,scales='free')+
  scale_y_continuous(limits = c(-1,2.5), 
                     breaks = seq(-1,3,0.50), 
                     name = 'Rural-Urban differences in DFLE')+
  scale_x_continuous(limits = c(17.5,72.5), 
                     breaks = seq(20,70,5), 
                     labels = dic_age[as.character(seq(20,70,5))],
                     name='')+
  theme_bw() + 
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(size=14,color='black'),
        axis.title = element_text(size=14,color='black'),
        axis.text.x = element_text(size=12,color='black',angle = 90,vjust = 0.5,hjust=1),
        axis.text.y = element_text(size=12,color='black'),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=14,color='black'),
        plot.caption = element_text(hjust=0,size=12))+
  geom_text(data=text_data,
            aes(x=x,y=y,
                label= paste0('DFLE(20) Rural-Urban: ',
                              round(unique(diff),2),
                              '\n',
                              'Mortality difference: ',
                              round(unique(mdiff),2),
                              '\n',
                              'Health difference: ',
                              round(unique(hdiff),2))),
            hjust=0)

    

    
###################################################################