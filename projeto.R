pdf = function(x, alpha, L,H){
    (alpha * L^(alpha) * x^(-alpha-1))/(1 - (L/H)^(alpha))
}

sim.IT = function(n,alpha, L, H){
    x=0
    t=0
    while(t<n){
        t=t+1
        u=runif(1,0,1)
        up = -(u*(1-((L/H)^alpha))-1)
        down = L^alpha
        x[t]= (up/down)^(1/(-alpha))
        
    }
    x    
}

## por enquanto g=1 (função constante). Podemos arranjar uma distribuição exponencial para otimizar os resultados
sim.AR = function(n,alpha, L,H, lambda){
    f =function (x){ (alpha * L^(alpha) * x^(-alpha-1))/(1 - (L/H)^(alpha)) }
    M = optimize(f, c(L,L+0.0000001),maximum = T)$objective
    ##g = function(x){ lambda*exp(-lambda*x)}
    g = 1
    ##gITM=function(x){log(1-x)/(-lambda)}
    v = vector()
    for(i in 1:n){
        u <- 1
        a <- 0
        while(u> a){
            x.c   <- runif(1,2,3)
            a <- f(x.c)/(M*g) 
            u     <- runif(1,0,1)          
        }
        v <- c(v,x.c)
    }
    v
}


#(d) Since 15 samples is too small to make any conclusion, we cannot conclude that the code is not generating samples correctly. We can see that it has more samples in the beginning than in the end as we want so it can be likely that it is generating samples from the assumed distribution altough not conclusivly

library(arm)
library(Cairo)
set.seed(2447)
alpha=0.25
L=2
H=3
set.IT = sim.IT(12000,alpha,L,H)
set.AR = sim.AR(1200,alpha,L,H,3)
gTest = function(x,lambda){ lambda*exp(-lambda*x)}
par(mfrow=c(1,2))
hist (set.IT[1:15],main="ITM Truncated Pareto",xlim=c(L,H),freq = F)
curve(pdf(x,alpha,L,H) ,add = T, xlim=c(L,H))    
hist(set.AR[1:15],main="ARM Truncated Pareto", freq = F)
curve(pdf(x,alpha,L,H) ,add = T, xlim=c(2,4))

set.seed(2447)
alpha=0.25
L=2
H=3
set.IT = sim.IT(12000,alpha,L,H)
set.AR = sim.AR(12000,alpha,L,H,3)
par(mfrow=c(1,2))
hist (set.IT,main="ITM Truncated Pareto",xlim=c(L,H),freq = F)
curve(pdf(x,alpha,L,H) ,add = T, xlim=c(2,4))
hist(set.AR,main="ARM Truncated Pareto", xlim=c(L,H),freq = F)
curve(pdf(x,alpha,L,H) ,add = T, xlim=c(2,4))    
curve(1.2967*1+(x-x),add=T,xlim=c(2,4))
