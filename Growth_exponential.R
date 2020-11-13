growth_exponential.model <- function(simstart, simende)
{
  Pool1 <- rep(NA, simende)
  Pool2 <- rep(NA, simende)
  Pool1[simstart] <- 100
  Pool2[simstart] <- 1
  for (day in simstart:(simende - 1)) {
    F1 <- 0.045*Pool2[day]
    Pool1[day + 1] <- Pool1[day] - F1
    Pool2[day + 1] <- Pool2[day] + F1
    }
return(data.frame(day = simstart:simende, Pool1 = Pool1[simstart:simende], Pool2 = Pool2[simstart:simende] ))
}
growth_exponential.model
Ergebnis <- growth_exponential.model(simstart=1, simende=100)
Ergebnis
plot(Ergebnis$Pool1~Ergebnis$day)
plot(Ergebnis$Pool2~Ergebnis$day)
