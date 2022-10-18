Z.cov.int.nokm<-function(finalVariables,actFires){
  
  xy<-coordinates(actFires)/1e6
  
  x<-xy[,1]
  y<-xy[,2]
  
  actfires.ppp<-ppp(x,y,c(min(x),max(x)),c(min(y),max(y)))
  
  #########EStimación y predicción
  
  Q<-quadscheme(actfires.ppp)
  fit1<-ppm(Q ~ polynom(x,y,2))
  
  coord.indice<-coordinates(finalVariables[[1]])/1e6
  
  p1<-predict(fit1,locations=data.frame(x=coord.indice[,1],y=coord.indice[,2]),type="trend")
  
  pred.intensity <- setValues(finalVariables[[1]],p1)
  
  return(4 * pred.intensity/1e6)
}