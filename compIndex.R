compIndex <- function(index,notBurnableMasckPG,cld.mask.t,cld.mask.a,Artoi,DoI){
  
  message(paste('Obtaining data for index', index))
  
  ###########
  ## TERRA ##
  ###########
  
  bandInd.t <- rsat_get_raster(Artoi, "MOD09GA", index)
  bandInd.t <- bandInd.t * cld.mask.t
  
  ##########
  ## AQUA ##
  ##########

  bandInd.a <- rsat_get_raster(Artoi, "MYD09GA", index)
  bandInd.a <- bandInd.a * cld.mask.a
  
  # Composite image
  message('Creating composite images')
  bandInd <- cover(bandInd.t, bandInd.a)
  # equalize extents
  notBurnableMasckPG <- raster::intersect(notBurnableMasckPG, bandInd[[1]])
  extent(notBurnableMasckPG) <- extent(bandInd)
  # extract not burnable pixels
  bandInd <- bandInd * notBurnableMasckPG
  
  # first day of analysis
  firstDay <- as.numeric(gsub("X", "", names(cld.mask.t)[1]))-1
  
  # add first day information to obtain previous and posterior time series 
  bandInd_doi <- addLayer(bandInd,(DoI-firstDay))
  bandInd_pre <- calc(bandInd_doi,getTSpre)
  bandInd_pos <- calc(bandInd_doi,getTSpos)
  
  # Obtain difference between previous and posterior scenes
  delta_bandInd <- calc(bandInd_pre, median) - calc(bandInd_pos, median)
  
  # clamp values
  if(index!='MIRBI'){
    delta_bandInd <- clamp(delta_bandInd, lower=-1, upper=1, useValues=F)
  }
  
  names(delta_bandInd) <- paste("d",index)
  
  return(delta_bandInd)
  
}

## Helper function to obtain time series for the previous scene

getTSpre <- function(x){
  
  ts <- x[1:(length(x)-1)]
  dOcH <- x[length(x)]
  
  if (!is.na(dOcH)){
    pre <- ts[1:dOcH]
    
    pre <- pre[!is.na(pre)]
    
    if (length(pre)>= 8){
      
      pre <- pre[(length(pre)-7):length(pre)]
      
    } else {
      pre <- c(pre,rep(NA,(8-length(pre))))
      
    }
    return(pre)
    
  } else {NA}
  
}


## Helper function to obtain time series for the posterior scene

getTSpos <- function(x){
  
  ts <- x[1:(length(x)-1)]
  dOcH <- x[length(x)]
  
  if (!is.na(dOcH)){
    pos <- ts[(dOcH+1):length(ts)]
    
    pos <- pos[!is.na(pos)]
    
    if (length(pos)>= 8){
      
      pos <- pos[1:8]
      
    } else {
      pos <- c(pos,rep(NA,(8-length(pos))))
      
    }
    
    return(pos)
  } else {NA}
}