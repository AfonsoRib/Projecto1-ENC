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
sim.AR = function(n,alpha, L,H){
    f =function (x){ (alpha * L^(alpha) * x^(-alpha-1))/(1 - (L/H)^(alpha)) }
    g = function(x){exp(-x)}
    h = function(x){f(x)/g(x)}
    M = optimize(h, c(L,H),maximum = T)$objective

    g.CDF=function(x){-exp(-x)+1}
    g.ITM = function(x){-log(1-x)}
    v = vector()
    for(i in 1:n){
        u <- 1
        a <- 0
        while(u> a){
            x.c   <- g.ITM(runif(1,g.CDF(2),g.CDF(3))) # O g.CDF aqui serve para não estar a gerar valores desnecessários através da distribuição exponencial
            a <- f(x.c)/(M*g(x.c)) 
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
set.AR = sim.AR(12000,alpha,L,H)
gTest = function(x,lambda){ lambda*exp(-lambda*x)}
par(mfrow=c(1,2))
hist (set.IT[1:15],main="ITM Truncated Pareto",xlim=c(L,H),freq = F)
curve(pdf(x,alpha,L,H) ,add = T, xlim=c(L,H))    
hist(set.AR[1:15],main="ARM Truncated Pareto",xlim=c(L,H),freq = F)
curve(pdf(x,alpha,L,H) ,add = T, xlim=c(L,H))

set.seed(2447)
alpha=0.25
L=2
H=3
set.IT = sim.IT(12000,alpha,L,H)
set.AR = sim.AR(12000,alpha,L,H)
par(mfrow=c(1,2))
hist (set.IT,main="ITM Truncated Pareto",xlim=c(L,H),freq = F)
curve(pdf(x,alpha,L,H) ,add = T, xlim=c(2,4))
hist(set.AR,main="ARM Truncated Pareto", xlim=c(L,H),freq = F)
curve(pdf(x,alpha,L,H) ,add = T, xlim=c(2,4))    
curve(1.2967*1+(x-x),add=T,xlim=c(2,4))


library(Pareto)
par(mfrow=c(1,2))
hist (set.IT,main="ITM Truncated Pareto",xlim=c(L,H),freq = F)
curve(pdf(x,alpha,L,H) ,add = T, xlim=c(2,4))
hist (rPareto(12000,2,0.25,truncation=3),main="ITM Truncated Pareto",xlim=c(L,H),freq = F)
curve(pdf(x,alpha,L,H) ,add = T, xlim=c(2,4))


