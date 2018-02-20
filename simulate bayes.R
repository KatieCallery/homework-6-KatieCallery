library(viridis)
palette(rev(viridis(10,option="A")))

sim_bayes<-function(p=0.5,N=10,y_lim=2.5)
{
  success<-0
  curve(dbeta(x,1,1),xlim=c(0,1),
  ylim=c(0,y_lim),xlab='p',ylab='Posterior Density',col="black",lty=2,
  lwd=5,cex.lab=1.4,cex.axis=1.3)
  
  #legend('topright',legend=c('Prior','Updated Posteriors','Final Posterior'),lty=c(2,1,1),col=c(1,1,'red'))
  
  for(i in 1:N)
  {
    if(runif(1,0,1)<=p)
      success<-success+1
    
    curve(dbeta(x,success+1,(i-success)+1),add=TRUE,col=i,lwd=i)
    print(paste(success,"successes and ",i-success," failures"))
  }
  curve(dbeta(x,success+1,(i-success)+1),add=TRUE,col=i,lwd=i)
}

sim_bayes(p=0.6,N=8)
