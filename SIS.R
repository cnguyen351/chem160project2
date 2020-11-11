alp.m <- 0.000006 #transmission rate:male person-1 day-1
alp.f <- 0.0000009 #transmission rate:female person-1 day-1
gam.m <- 0.05 #recovery rate:male day-1
gam.f <- 0.007 #recovery rate:female day-1
Sm <- 14000 #susceptible males
Sf <- 9000  #susceptible females
Im <- 1000 #infected males
If <- 1000 #infected females
Sm.hist <- c() #Initialize vectors to hold pop. size as time goes by
Sf.hist <- c()
Im.hist <- c()
If.hist <- c()
for (day in 1:2000) { #2000 day time period
  Sm.hist[day] <- Sm #Each time step will update current value of pop. sizes
  Sf.hist[day] <- Sf
  Im.hist[day] <- Im
  If.hist[day] <-If
  delta.Sm <- (gam.m*Im-alp.m*Sm*If) #Equations for change in number of susceptible
  delta.Sf <- (gam.f*If-alp.f*Sf*Im)
  delta.Im <- (alp.m*Sm*If-gam.m*Im) #Equations for change in number of infected
  delta.If <- (alp.f*Sf*Im-gam.f*If)
  Sm <- Sm + delta.Sm #Update population sizes
  Sf <- Sf + delta.Sf
  Im <- Im + delta.Im
  If <- If + delta.If
  Sm <- max(Sm,0) #Make sure population sizes stay in the positive
  Sf <- max(Sf,0)
  Im <- max(Im,0)
  If <- max(If,0)
}
plot(Sm.hist, type="l", ylim=c(0,14000), xlab="Time (days)", ylab="Number of individuals") #Plot each pop. pool
lines(Sf.hist,col=2)
lines(Im.hist,col=3)
lines(If.hist,col=4)