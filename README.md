* Here is the R script to examine the change in the homicide rate between 2023 and 2024 for Maryland:

```R
# did hrate in Maryland increase or decrease from 2023 to 2024?

# step 1: number of homicides in Maryland for each year

h23 <- 548
p23 <- 6180253
r23 <- h23/p23
r23*1e5

h24 <- 491
p24 <- 6263220
r24 <- h24/p24
r24*1e5

# step 2: calculate percent change statistic

((r24-r23)/r23)*100

# step 3: calculate confidence interval for the difference

d23 <- rbeta(n=1e6,shape1=1/2+h23,shape2=1/2+p23-h23)
d24 <- rbeta(n=1e6,shape1=1/2+h24,shape2=1/2+p24-h24)
delta <- (d24-d23)*1e5
mean(delta)
median(delta)
quantile(delta,c(0.025,0.975))

################################################
# step 4a: count deaths due to external cause of undetermined intent (broadly defined)

un23 <- 992
un24 <- 649

# step 5a: calculate upper bounds on homicide rate for each year

ubh23 <- h23+un23
ubh24 <- h24+un24

ubr23 <- ubh23/p23
ubr23*1e5
ubr24 <- ubh24/p24
ubr24*1e5

# step 6a: compare identification regions of the 2 rates

c(r23*1e5,ubr23*1e5)
c(r24*1e5,ubr24*1e5)

################################################
# step 4b: count deaths due to external cause of undetermined intent (narrowly defined)

un23 <- 32
un24 <- 38

# step 5b: calculate upper bounds on homicide rate for each year

ubh23 <- h23+un23
ubh24 <- h24+un24

ubr23 <- ubh23/p23
ubr23*1e5
ubr24 <- ubh24/p24
ubr24*1e5

# step 6b: compare identification regions of the 2 rates

c(r23*1e5,ubr23*1e5)
c(r24*1e5,ubr24*1e5)

# step 7b: calculate confidence intervals for each bound
# note: 95% confidence interval represents the p(CI for lower bound traps
# population lower bound *and* CI for upper bound traps population upper bound) = 0.95

d23lb <- d23
d23ub <- rbeta(n=1e6,shape1=1/2+h23+un23,shape2=1/2+p23-h23-un23)
quantile(d23lb*1e5,c(0.0125,0.9875))
quantile(d23ub*1e5,c(0.0125,0.9875))

d24lb <- d24
d24ub <- rbeta(n=1e6,shape1=1/2+h24+un24,shape2=1/2+p24-h24-un24)
quantile(d24lb*1e5,c(0.0125,0.9875))
quantile(d24ub*1e5,c(0.0125,0.9875))

# step 8b: suppose we assume that all of the undetermined intent cases in 2024 were
# homicides and that none of the undetermined intent cases in 2023 were homicides.
# this would create the smallest possible difference between the 2 rates (unlikely 
# but the data are not strong enough to rule it out).

quantile((d24ub-d23lb)*1e5,c(0.025,0.975))
```

* Here is the output:

```Rout
> # did hrate in Maryland increase or decrease from 2023 to 2024?
> 
> # step 1: number of homicides in Maryland for each year
> 
> h23 <- 548
> p23 <- 6180253
> r23 <- h23/p23
> r23*1e5
[1] 8.866951
> 
> h24 <- 491
> p24 <- 6263220
> r24 <- h24/p24
> r24*1e5
[1] 7.839418
> 
> # step 2: calculate percent change statistic
> 
> ((r24-r23)/r23)*100
[1] -11.58834
> 
> # step 3: calculate confidence interval for the difference
> 
> d23 <- rbeta(n=1e6,shape1=1/2+h23,shape2=1/2+p23-h23)
> d24 <- rbeta(n=1e6,shape1=1/2+h24,shape2=1/2+p24-h24)
> delta <- (d24-d23)*1e5
> mean(delta)
[1] -1.027092
> median(delta)
[1] -1.027116
> quantile(delta,c(0.025,0.975))
       2.5%       97.5% 
-2.04050340 -0.01217423 
> 
> ################################################
> # step 4a: count deaths due to external cause of undetermined intent (broadly defined)
> 
> un23 <- 992
> un24 <- 649
> 
> # step 5a: calculate upper bounds on homicide rate for each year
> 
> ubh23 <- h23+un23
> ubh24 <- h24+un24
> 
> ubr23 <- ubh23/p23
> ubr23*1e5
[1] 24.91807
> ubr24 <- ubh24/p24
> ubr24*1e5
[1] 18.2015
> 
> # step 6a: compare identification regions of the 2 rates
> 
> c(r23*1e5,ubr23*1e5)
[1]  8.866951 24.918074
> c(r24*1e5,ubr24*1e5)
[1]  7.839418 18.201500
> 
> ################################################
> # step 4b: count deaths due to external cause of undetermined intent (narrowly defined)
> 
> un23 <- 32
> un24 <- 38
> 
> # step 5b: calculate upper bounds on homicide rate for each year
> 
> ubh23 <- h23+un23
> ubh24 <- h24+un24
> 
> ubr23 <- ubh23/p23
> ubr23*1e5
[1] 9.384729
> ubr24 <- ubh24/p24
> ubr24*1e5
[1] 8.446135
> 
> # step 6b: compare identification regions of the 2 rates
> 
> c(r23*1e5,ubr23*1e5)
[1] 8.866951 9.384729
> c(r24*1e5,ubr24*1e5)
[1] 7.839418 8.446135
> 
> # step 7b: calculate confidence intervals for each bound
> # note: 95% confidence interval represents the p(CI for lower bound traps
> # population lower bound *and* CI for upper bound traps population upper bound) = 0.95
> 
> d23lb <- d23
> d23ub <- rbeta(n=1e6,shape1=1/2+h23+un23,shape2=1/2+p23-h23-un23)
> quantile(d23lb*1e5,c(0.0125,0.9875))
   1.25%   98.75% 
8.046601 9.744258 
> quantile(d23ub*1e5,c(0.0125,0.9875))
    1.25%    98.75% 
 8.542098 10.289144 
> 
> d24lb <- d24
> d24ub <- rbeta(n=1e6,shape1=1/2+h24+un24,shape2=1/2+p24-h24-un24)
> quantile(d24lb*1e5,c(0.0125,0.9875))
   1.25%   98.75% 
7.074061 8.661556 
> quantile(d24ub*1e5,c(0.0125,0.9875))
   1.25%   98.75% 
7.651381 9.298145 
> 
> # step 8b: suppose we assume that all of the undetermined intent cases in 2024 were
> # homicides and that none of the undetermined intent cases in 2023 were homicides.
> # this would create the smallest possible difference between the 2 rates (unlikely 
> # but the data are not strong enough to rule it out).
> 
> quantile((d24ub-d23lb)*1e5,c(0.025,0.975))
      2.5%      97.5% 
-1.4536700  0.6144469 
>
```
