The LPC model
-------------

![](https://raw.githubusercontent.com/dominicnoy/LPC/master/LPC.png)

-   A\_n is the asynchrony at iteration n,
-   C is a constant metronome interval,
-   M\_n is the motor delay, and
-   T\_n is the Time Keeper interval.
-   M\_n and T\_n are random variables with M\_n ~ NV(\_M, ^2\_M) and
    T\_n ~ NV(\_T, ^2\_T)

Simulation Function
-------------------

    #Simulation function
    Simulate<-function(N, nseq, alpha, st, sm)
    {
      As<-matrix(NA, nrow=N, ncol=nseq)
      for(o in 1:nseq)
        {
          M=rnorm(N+2)
          T=rnorm(N+2)
          
          Z=st*T[1:(N+1)]-sm*M[2:(N+2)]+sm*M[1:(N+1)]
          AA=rep(0,N+2)
          for(I in 1:(N+1))
            {
              AA[I+1]<-(1-alpha)*AA[I]+Z[I]
            }
          As[,o]<-AA[3:(N+2)]
        }
      As
    }

    #Set parameter values for simulation  
    set.seed(1234567)
    outc<-as.data.frame(Simulate(N=100, nseq=1, alpha=0.5, st=10, sm=5))

    outc$n<-c(1:dim(outc)[1])
    names(outc)<-c("A_n", "n")

    require(ggplot2)

    ## Loading required package: ggplot2

    ggplot(outc, aes(x=n, y=A_n)) +
      geom_point()+
      geom_line()+
      #scale_x_continuous(breaks=seq(from=1, to=n, by=4)+
      theme_bw() +
      guides(color=FALSE)+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            axis.text=element_text(size=15),
            axis.title=element_text(size=15))+
      ylab("A_n")+
      xlab("Iteration number n")

![](LPCSimulation_files/figure-markdown_strict/LPC%20simple%20plot-1.png)
