med_plots <-function (d,cont,int, cols){
    #med_plots.r by Matthew Fritz and David MacKinnon, 2007,
    #Dept. of Psychology, Arizona State University
    #This program plots the mediated effect for a single mediator mediation
    #model and adds labels for the different effects.
  
    #Modified by Eric N. Smith, 2016
    #Dept. of Psychology, Stanford University
    
    #To read in file, type: source("C:/med_plots.r")
    #To run file, type: med_plots(cont,int)
    #where cont=0 if x is dichotomous           and
    #      cont=1 if x is continuous            and
    #where int=0 if there is no XM interaction  and
    #      int=1 if there is an XM interaction
    
    #Read in the data file
    data1 <- d[,cols]
    names(data1) <- c("V1","V2","V3")
    data1 <- data1[complete.cases(data1),]
    attach(data1)
    
    #Rename the corresponding variables in the data set to x, m, and y
    
    x<-V1
    m<-V2
    y<-V3
    
    #Runs the regression analyses from Equations 1, 2, and 3
    
    #Equation 1
    fit.yx<-glm(y~x)
    sum.yx<-summary(fit.yx)
    c<-sum.yx$coefficients[2,1]
    i1<-sum.yx$coefficients[1,1]
    
    #Equation 2
    fit.mx<-lm(m~x)
    sum.mx<-summary(fit.mx)
    a<-sum.mx$coefficients[2,1]
    i2<-sum.mx$coefficients[1,1]
    
    #Equation 3
    if(int==0) {
      fit.yxm<-lm(y~x+m)
      sum.yxm<-summary(fit.yxm)
      b<-sum.yxm$coefficients[3,1]
      cpr<-sum.yxm$coefficients[2,1]
      i3<-sum.yxm$coefficients[1,1]
    } else {
      fit.yxm<-lm(y~x+m+(x*m))
      sum.yxm<-summary(fit.yxm)
      b<-sum.yxm$coefficients[3,1]
      cpr<-sum.yxm$coefficients[2,1]
      i3<-sum.yxm$coefficients[1,1]
      g<-sum.yxm$coefficients[4,1]}
    
    
    
    #Plotting the effects
    meanx<-mean(x)
    sdx<-sd(x)
    mmin<-min(m)
    mmax<-max(m)
    ymin<-min(y)
    ymax<-max(y)
    
    
    plot(m,y,type="n",ylab=" ",xlab="M",xlim=range(mmin-((mmax-
                                                          mmin)/5),mmax+((mmax-mmin)/5)),
         ylim=range(ymin-((ymax-ymin)/5),ymax+((ymax-ymin)/5)))
    points(m,y, pch=19)
    mtext("Y",side=2,las=1,adj=5)
    par(family="serif",font=4,cex=1.13)
    
    #Dichotomous X with no XM interaction condition
    if(cont==0 && int==0) {
      #Effect of X on M (a)
      vert1<-i2+a*0
      vert2<-i2+a*1
      abline(v=vert1)
      abline(v=vert2)
      arrows(vert1,ymin-((ymax-ymin)/5),vert2,ymin-((ymax-
                                                     ymin)/5),length=.1,code=3)
      text(((vert2-vert1)/2+vert1),ymin-((ymax-ymin)/5),"a",pos=3)
      
      #Effect of X on Y (c)
      hor1<-i1+c*0
      hor2<-i1+c*1
      abline(h=hor1)
      abline(h=hor2)
      arrows(mmin-((mmax-mmin)/5),hor1,mmin-((mmax-
                                              mmin)/5),hor2,length=.1,code=3)
      text(mmin-((mmax-mmin)/5),((hor2-hor1)/2+hor1),"c",pos=4)
      
      #Simple regression lines
      abline((i3+cpr*0),b,col=1,lty=1,lwd=3)
      abline((i3+cpr*1),b,col=1,lty=2,lwd=3)
      
      #Legend
      legend("topleft",legend=c("Y=i3+c’X+bM (X=0)",
                                 "Y=i3+c’X+bM (X=1)"),
              lty=1:2,lwd=3,bty="n") }
    
    #Dichotomous X with an XM interaction condition
    if(cont==0 && int==1) {
      #Effect of X on M (a)
      vert1<-i2+a*0
      vert2<-i2+a*1
      abline(v=vert1)
      abline(v=vert2)
      arrows(vert1,ymin-((ymax-ymin)/5),vert2,ymin-((ymax-
                                                     ymin)/5),length=.1,code=3)
      text(((vert2-vert1)/2+vert1),ymin-((ymax-ymin)/5),"a",pos=3)
      
      #Effect of X on Y (c)
      hor1<-i1+c*0
      hor2<-i1+c*1
      abline(h=hor1)
      abline(h=hor2)
      arrows(mmin-((mmax-mmin)/5),hor1,mmin-((mmax-
                                              mmin)/5),hor2,length=.1,code=3)
      text(mmin-((mmax-mmin)/5),((hor2-hor1)/2+hor1),"c",pos=4)
      
      #Simple regression lines
      abline((i3+cpr*0),b+g*0,col=1,lty=1,lwd=3)
      abline((i3+cpr*1),b+g*1,col=1,lty=2,lwd=3)
      abline((i3+cpr*1),b+g*0,col=1,lty=3,lwd=3)
      
      #Legend
      legend("topleft",legend=c("Y=i3+c’X+bM+gXM (X=0)","Y=i3+c’X+bM+gXM
                                 (X=1)",
                                 "Y=i3+c’X+bM (X=1)"),lty=1:3,lwd=3,bty="n") }
    #Continuous X with no XM interaction condition
    if(cont==1 && int==0) {
      #Effect of X on M (a)
      vert1<-i2+a*meanx
      vert2<-i2+a*(meanx+1)
      abline(v=vert1)
      abline(v=vert2)
      arrows(vert1,ymin-((ymax-ymin)/5),vert2,ymin-((ymax-
                                                     ymin)/5),length=.1,code=3)
      text(((vert2-vert1)/2+vert1),ymin-((ymax-ymin)/5),"a",pos=3)
      
      #Effect of X on Y (c)
      hor1<-i1+c*meanx
      hor2<-i1+c*(meanx+1)
      abline(h=hor1)
      abline(h=hor2)
      arrows(mmin-((mmax-mmin)/5),hor1,mmin-((mmax-
                                              mmin)/5),hor2,length=.1,code=3)
      text(mmin-((mmax-mmin)/5),((hor2-hor1)/2+hor1),"c",pos=4)
      
      #Simple regression lines
      abline((i3+cpr*(meanx+sdx)),b,col=1,lty=1,lwd=3)
      abline((i3+cpr*meanx),b,col=1,lty=2,lwd=3)
      abline((i3+cpr*(meanx-sdx)),b,col=1,lty=3,lwd=3)
      
      #Legend
      legend("topleft",legend=c("Y=i3+c’X+b(MeanX+1sd)","Y=i3+c’X+b(MeanX)",
                                 "Y=i3+c’X+b(MeanX-1sd)"),lty=1:3,lwd=3,bty="n") }
    #Continuous X with an XM interaction condition
    if(cont==1 && int==1) {
      #Effect of X on M (a)
      vert1<-i2+a*meanx
      vert2<-i2+a*(meanx+1)
      abline(v=vert1)
      abline(v=vert2)
      arrows(vert1,ymin,vert2,ymin,length=.1,code=3)
      text(((vert2-vert1)/2+vert1),ymin,"a",cex=1.13,pos=3)
      
      #Effect of X on Y (c)
      hor1<-i1+c*meanx
      hor2<-i1+c*(meanx+1)
      abline(h=hor1)
      abline(h=hor2)
      arrows(mmin,hor1,mmin,hor2,length=.1,code=3)
      text(mmin,((hor2-hor1)/2+hor1),"c",cex=1.13,pos=4)
      
      #Simple regression lines
      abline((i3+cpr*(meanx+sdx)),b+(g*(meanx+sdx)),col=1,lty=1,lwd=3)
      abline((i3+cpr*meanx),b+(g*(meanx)),col=1,lty=2,lwd=3)
      abline((i3+cpr*(meanx-sdx)),b+(g*(meanx-sdx)),col=1,lty=3,lwd=3)
      
      #Legend
      legend("topleft",legend=c("Y=i3+c’(MeanX+1sd)+bM+gM*(Meanx+1sd)",
                                 "Y=i3+c’(MeanX)+bM+gM*(MeanX)","Y=i3+c’(MeanX-1sd)+bM+gM*(MeanX-
                                                                                           1sd)"),
              lty=1:3,lwd=3,bty="n") }
    detach(data1)
}
