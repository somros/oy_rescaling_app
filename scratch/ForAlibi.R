ABCs <- c(100000,80000,60000,40000,40000)
OYcap <- 200000
r <- OYcap/sum(ABCs)
w <- c(4,5,1,3,2)

ABCPred <- function(mult) { r <- r^(1/(w*mult)); return(ABCs*r) }

fn <- function(mult)
{
  ABC1 <- ABCPred(mult)
  obj <- (OYcap-sum(ABC1))^2
  
  
}

xx <- optimize(f=fn,interval=c(-1,10))
mult <- xx$minimum
r <- r^(1/(w*mult))
print(mult)
print(r)
print(ABCs*r)
print(sum(ABCs*r))

  
mult <- seq(from=0,to=10,by=0.1)
capdiff <- NULL
for (Imult in 1:length(mult))
{
 ABC1 <-  ABCPred(mult[Imult])
 capdiff <- c(capdiff, (OYcap-sum(ABC1))^2)
 
  
}  

plot(mult,capdiff)
