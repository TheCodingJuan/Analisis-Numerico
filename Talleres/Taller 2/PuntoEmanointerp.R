metodo <- "fmm"  # metodos : ("fmm","natural","periodic") 

graf <- function(arr1,arr2, color)
{
  points(arr1,arr2, pch=7, cex=0.5, col = color, asp=1,xlab="X", ylab="Y", main="Diagrama ")
}

# esto, dada una lista, encuentra los 3 mejores puntos para lagrange, tomando los dos extremos y alguno intermedio
findMin <- function(lisx, lisy, liminf, limsup)
{
  errmin <- 0
  pt     <- 0
  prim   <- TRUE
  intersec  <- 0
  bstintrsc <- 0
  
  for( i in liminf:limsup)
  {
    valx <-          lisx[liminf]
    valx <- c( valx, lisx[i] )
    valx <- c( valx, lisx[limsup] )
    
    valy <-          lisy[liminf]
    valy <- c( valy, lisy[i] )
    valy <- c( valy, lisy[limsup] )
    
    sumerr <- 0    
    
    ttam <- limsup - liminf
    ret  <- spline( valx , valy , n = ttam , method = metodo)
    ry   <- ret$y
    
    intersec <- 0
    
    #lines(spline(valx, valy, n = ttam, method = metodo), col = 3)
    
    for( j in 1:ttam )
    {
      if (lisy[ liminf + j ] ==  ry[ j ])
      {
        intersec <- intersec + 1
        #cat (lisy[ liminf + j ]  ,"-----",  ry[ j ],"\n")
      }
      sumerr <- sumerr + abs( lisy[ liminf + j ] -  ry[ j ] ) 
    }
    if ( !is.nan( sumerr ) && ( prim || sumerr < errmin ) )
    {
      errmin <- sumerr
      pt     <- i 
      prim   <- FALSE
      bstintrsc <- intersec
    }
  }
  #cat("best intersectioon : " , bstintrsc," \n")
  #cat ("final : ",errmin ,"    ",pt,"\n")
  return( c(pt,errmin,bstintrsc) )
}

puntos <- function(x,y)
{
  totintersect <- 0
  increm <- 1.4
  errorinicial  <- 1
  erroraceptado <- errorinicial
  puntos <- c(1)
  top    <- 1 
  min    <- c(Inf,Inf,Inf,Inf,Inf)# de, medio , hasta, valor, puntos intersectantes 
  i      <- 3
  prim   <- TRUE
  len    <- 0 # longitud del tramo actual 
  
  while ( top != length(x) )
  {
    if ( i - top >= 1)
    {
      ret <- findMin(x,y,top,i) 
      
      if ( abs(min[4] - ret[2]) <= erroraceptado || prim) # si se encuentran otros valores que minimicen el error, se guardan. o si es el primer valor revisado
      {
        min <- c( top ,ret[1], i, ret[2],ret[3])
        prim = FALSE
        erroraceptado <- erroraceptado + increm*(len/length(x)) # para darle mas 
        len <- 1 # se reinicia la longitud actual 
      }
      else
      {
        len <- len + 1 # aumenta la longitud del tramo actual
      }
      
      if ( i >= length(x) )
      {
        nx <- c( x[ min[1] ], x[ min[2] ] , x[ min[3] ] )
        ny <- c( y[ min[1] ], y[ min[2] ] , y[ min[3] ] )
        lines(spline(nx, ny, n = 200, method = metodo ), col = 2)
        
        erroraceptado <- errorinicial # reiniciar el error aceptado
        
        cat("desde : ",min[1],"\tpunto medio : ",min[2], "\thasta : ",min[3], "\terror : ",min[4],"\tpuntos intersectantes : ",min[5],"\n")
        totintersect <- totintersect + min[5]
        if ( min[2] == puntos[ length( puntos ) ] ) 
        {
          puntos <- c( puntos,min[ 3 ] ) # min[1] en teoria es el ultimo de los anteriores, entonces ya esta 
        }
        else
        {
          puntos <- c( puntos,min[ 2:3 ] )
        }
        
        top <- min[3]
        i   <- top
        
        min  <- c(Inf,Inf,Inf,Inf,Inf)# de, medio , hasta, valor, interseccion
        prim <- TRUE
      }
      else 
      {
        i <- i + 1  
      }
    }
    else 
    {
      i <- i + 1  
    }
    
    if ( i >= length(x) ) # por si acaso :v 
    {
      i <- length(x)
    }
  }
  cat("\n\nlen : ",length(puntos),"\ttotal de puntos intersectantes : ",totintersect,"\n",puntos,"\n")
}

# datos originales 
#x=c(14.65, 14.71, 14.6, 14.8, 15.2, 15.6, 15.7, 17.0, 17.6, 17.52, 17.3, 16.8, 15.4, 14.83, 14.4, 14.5, 15.0, 15.1, 15.0, 14.9, 14.6, 14.3, 14.0, 13.9, 13.8, 13.5, 13.1, 13.0, 13.3, 13.2, 13.1, 12.9, 12.4, 11.9, 11.7, 11.6, 11.3, 10.9, 10.7, 10.6, 10.6, 10.1, 9.7, 9.4, 9.3, 9.6, 9.9, 10.1, 10.2, 10.3, 9.10, 8.6, 7.5, 7.0, 6.7, 6.6, 7.70, 8.00, 8.10, 8.40,9.20, 9.30, 10, 10.2, 10.3, 10.0, 9.50)                                                                                                       
#y=c(14.7, 14.33, 13.4, 12.33, 11.0, 10.5, 10.22, 8.20, 7.10, 6.70, 6.60, 6.80, 8.30, 8.80, 9.30, 8.80, 6.30, 5.50, 5.00, 4.70, 4.60, 4.50, 4.90, 5.40, 5.80, 6.90, 8.20, 7.60, 5.80, 4.50, 4.30, 3.90, 4.20, 5.70, 7.00, 7.90, 8.20, 7.30, 6.70, 5.50, 5.10, 4.60, 4.7, 5.0, 5.5, 7.2, 7.8, 8.60, 9.40, 10.0, 10.7, 9.9, 9.0, 9.1, 9.3, 9.7, 11.7, 12.3, 12.5, 13.0,13.91, 14.9, 16, 16.4, 16.8, 10.7, 11.0)     

# reorganizacion de datos 
x = c(14.65, 14.71, 14.6, 14.8, 15.2, 15.6, 15.7, 17.0, 17.6, 17.52, 17.3, 
      16.8, 15.4, 14.83, 14.4, 14.5, 
      15.0, 15.1, 15.0, 14.9, 14.6, 14.3, 14.0, 13.9, 13.8, 13.5, 13.1, 13.0, 
      13.3, 13.2, 13.1, 12.9, 12.4, 11.9, 11.7, 11.6, 11.3, 10.9, 
      10.7, 10.6, 10.6, 10.1, 9.7, 9.4, 9.3, 9.6, 9.9, 10.1, 10.2, 10.3,  10.0, 9.5, 
      8.6, 7.5, 7.0, 6.7, 6.6, 7.7, 
      8.0, 8.1, 8.4, 9.2, 9.3, 10, 10.2, 10.3)

y = c(14.7, 14.33, 13.4, 12.33, 11.0, 10.5, 10.22, 8.2, 7.1, 6.7, 6.6, 
      6.8, 8.3, 8.8, 9.3, 8.8, 
      6.3, 5.5, 5.0, 4.7, 4.6, 4.5, 4.9, 5.4, 5.8, 6.9, 8.2, 7.6, 
      5.8, 4.5, 4.3, 3.9, 4.2, 5.7, 7.0, 7.9, 8.2, 7.3, 
      6.7, 5.5, 5.1, 4.6, 4.7, 5.0, 5.5, 7.2, 7.8, 8.6, 9.4, 10.0,  10.7, 11.0, 
      9.9, 9.0, 9.1, 9.3, 9.7, 11.7, 
      12.3, 12.5, 13.0, 13.91, 14.9, 16, 16.4, 16.8)


plot(x,y,type="l",asp=1,main="Mano")
div <- 20
#for ( i in 0:length(x)/div )
#{
#  (findMin(x,y,div*i,div*i+1) )
#}

## de cada uno de estos cortes deberian terminar quedando unos 3 puntos 

# esta parte ya muestra la mano, por mal que quede, pero es una mano 
c1 = 9
c2 = 15
c3 = 27
c4 = 37
c5 = 52
c6 = 56
c7 = length(x)

a <- 1
b <- c1 /2 
#puntos(x[a:b],y[a:b])


#x = c(14.65, 14.71, 14.6,  14.8, 15.2, 15.6,  15.7, 17.0, 17.6, 17.52, 17.3) 

#y = c(14.7 , 14.33, 13.4, 12.33, 11.0, 10.5, 10.22,  8.2,  7.1,   6.7,  6.6) 


puntos(x,y)