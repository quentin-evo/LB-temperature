Keep it cool: R script
================

``` r
tpds <- read.csv("~/PATH-TO/combined-data.txt")
```

Packages

``` r
library(ggplot2)
library(tidybayes)
library(MCMCglmm)
```

    ## Loading required package: Matrix

    ## Loading required package: coda

    ## Loading required package: ape

### 1\. Effect of Temperature on the mating behaviour

#### 1\. a. Temperature and male density

Run a Generalize Linear Model: Regression of the mean number over the
average redd temperature.

``` r
p<-list(G= list(), R=list(V=1,nu=0.002)) # Set priors
itr<-40

# Run model
mm.dt<-MCMCglmm(round(mean.male)~mean.temp,data=tpds[complete.cases(tpds$depth),], rcov= ~units, family = "poisson",
                prior=p, nitt = 13000*itr ,thin = 10*itr, burnin = 3000*itr)
```

Summary and diagnostic plots

``` r
summary(mm.dt)
```

    ## 
    ##  Iterations = 120001:519601
    ##  Thinning interval  = 400
    ##  Sample size  = 1000 
    ## 
    ##  DIC: 49.74914 
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean  l-95% CI u-95% CI eff.samp
    ## units    0.0324 0.0001816   0.1358     1000
    ## 
    ##  Location effects: round(mean.male) ~ mean.temp 
    ## 
    ##             post.mean l-95% CI u-95% CI eff.samp pMCMC  
    ## (Intercept)   1.61992  0.18525  3.14247    896.4 0.040 *
    ## mean.temp    -0.09543 -0.31569  0.11944    894.5 0.398  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
plot(mm.dt)
```

![](Report-LB-spawning-Bayesian-method_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](Report-LB-spawning-Bayesian-method_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

Half-eye plot of the posterior distributions of the fixed effect
(Temperature)

``` r
# Make a dataframe containing the posterior estimates 
df.dt<-data.frame(Value = c(mm.dt$Sol[,"(Intercept)"],
                            (mm.dt$Sol[,"(Intercept)"]+ mm.dt$Sol[,"mean.temp"])),
                        Effect =c(rep("Intercept",nrow(mm.dt$Sol)),rep("Temperature",nrow(mm.dt$Sol))))
```

``` r
ggplot(df.dt, aes(y = Effect, x = Value)) +  
  stat_halfeye(.width = 0.95,fill = "lightblue", color = "#003333",point_interval = mode_hdi) + theme_bw()
```

![](Report-LB-spawning-Bayesian-method_files/figure-gfm/ggplot-1.png)<!-- -->

#### 1.b. Temperature and and spawning behaviours (courthip intensity and aggression)

Two models are being made: - “mm.aggt” : Number of aggression per redd
(corrected by lenght of the video record) as a response variable -
“mm.court” : Number of courtship events per redd (corrected by lenght
of the video record) as a response The posterior mode structure (p) is
the same as in part 1.a.

``` r
itr<-40

mm.aggt<-MCMCglmm(round(corfreq.agg)~mean.temp,data=tpds[complete.cases(tpds$depth),], rcov= ~units, family = "poisson",
                prior=p, nitt = 13000*itr ,thin = 10*itr, burnin = 3000*itr) # Aggression

mm.crt<-MCMCglmm(round(corfreq.court)~mean.temp,data=tpds[complete.cases(tpds$depth),], rcov= ~units, family = "poisson",
                  prior=p, nitt = 13000*itr ,thin = 10*itr, burnin = 3000*itr) # Courtship
```

``` r
summary(mm.aggt)
```

    ## 
    ##  Iterations = 120001:519601
    ##  Thinning interval  = 400
    ##  Sample size  = 1000 
    ## 
    ##  DIC: 56.99251 
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean  l-95% CI u-95% CI eff.samp
    ## units    0.1023 0.0003919   0.4423     1000
    ## 
    ##  Location effects: round(corfreq.agg) ~ mean.temp 
    ## 
    ##             post.mean l-95% CI u-95% CI eff.samp pMCMC  
    ## (Intercept)   2.07571  0.17209  3.55362     1000 0.018 *
    ## mean.temp    -0.16976 -0.40666  0.07279     1000 0.160  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(mm.crt)
```

    ## 
    ##  Iterations = 120001:519601
    ##  Thinning interval  = 400
    ##  Sample size  = 1000 
    ## 
    ##  DIC: 42.80951 
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units   0.04856 0.000218    0.214    947.4
    ## 
    ##  Location effects: round(corfreq.court) ~ mean.temp 
    ## 
    ##             post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)   0.56509 -1.23905  2.71581     1000 0.572
    ## mean.temp    -0.01727 -0.32095  0.24181     1000 0.928

Plot the posterior densities of the fixed effect (Temperature)

``` r
df.behav<-data.frame(Value = c(mm.aggt$Sol[,"(Intercept)"],
                            (mm.aggt$Sol[,"(Intercept)"]+ mm.aggt$Sol[,"mean.temp"]),
                            mm.crt$Sol[,"(Intercept)"],
                              (mm.crt$Sol[,"(Intercept)"]+ mm.crt$Sol[,"mean.temp"])),
                  Effect =c(rep("Intercept",nrow(mm.aggt$Sol)),rep("Temperature",nrow(mm.aggt$Sol)),
                            rep("Intercept",nrow(mm.crt$Sol)),rep("Temperature",nrow(mm.crt$Sol))), 
                  Behaviour = c(rep("Aggression",nrow(mm.aggt$Sol)*2) , rep("Courtship",nrow(mm.crt$Sol)*2)))
df.behav$Value.obs<-exp(df.behav$Value) # Posterior estimates on the observed scale
```

``` r
ggplot(df.aggt, aes(y = Effect, x = Value.obs)) +  
  stat_halfeye(.width = 0.95,fill = "lightblue", color = "#003333",point_interval = mode_hdci) + theme_bw() + 
  facet_wrap(~Behaviour, ncol = 1, scales = "free") + xlab("Number of behavioural events") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 17, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12, vjust = 2),
        axis.text.y = element_text(size = 14,face = "bold"),
        strip.background = element_rect(fill = "white", colour = "white"), 
        strip.text.x = element_text(size = 12,face = "bold", hjust = 0.05))
```

![](Report-LB-spawning-Bayesian-method_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### 2\. Effect of Depth on the mating behaviour

#### 2.a. Depth & Densities of males

The models are the same as in part 1, expect that the response is
variable is “depth”.

``` r
itr<-50
mm.dd<-MCMCglmm(round(mean.male)~depth - 1,data=tpds[complete.cases(tpds$depth),], rcov= ~units, family = "poisson",
                prior=p, nitt = 13000*itr ,thin = 10*itr, burnin = 3000*itr)
```

``` r
summary(mm.dd)
```

    ## 
    ##  Iterations = 150001:649501
    ##  Thinning interval  = 500
    ##  Sample size  = 1000 
    ## 
    ##  DIC: 49.19044 
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean  l-95% CI u-95% CI eff.samp
    ## units   0.03102 0.0002039   0.1184     1000
    ## 
    ##  Location effects: round(mean.male) ~ depth - 1 
    ## 
    ##              post.mean l-95% CI u-95% CI eff.samp  pMCMC    
    ## depthShallow    1.0645   0.6837   1.4273   1000.0 <0.001 ***
    ## depthDeep       0.6102  -0.1190   1.3372    833.3   0.12    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
plot(mm.dd)
```

![](Report-LB-spawning-Bayesian-method_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->![](Report-LB-spawning-Bayesian-method_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

Posterior density plots

``` r
df.dd<-data.frame(Value = c(mm.dd$Sol[,"depthShallow"],
                            (mm.dd$Sol[,"depthDeep"])),
                  Effect =c(rep("Shallow",nrow(mm.dd$Sol)),rep("Deep",nrow(mm.dd$Sol))))
```

``` r
ggplot(df.dd, aes(y = Effect, x = exp(Value))) +  
  stat_halfeyeh(.width = 0.95,fill = "lightblue", color = "#003333",point_interval = mode_hdi) + theme_bw()
```

![](Report-LB-spawning-Bayesian-method_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

There seem to be a trend for lower densities of males on deep redds.
Let´s check how strong is this effect compared to the total variance in
density by calculating the marginal \(R^{2}_{lnnorm(m)}\) from Nakagawa
& Schielzeth (2013).

``` r
itr<-70
mm.dd2<-MCMCglmm(round(mean.male)~depth,data=tpds[complete.cases(tpds$depth),], rcov= ~units, family = "poisson",
                prior=p, nitt = 13000*itr ,thin = 10*itr, burnin = 3000*itr)

Vard<-numeric(1000)
for(i in 1:1000) { Var <- var(as.vector(mm.dd2$Sol[i,]%*% t(mm.dd2$X))) 
Vard[i] <- Var 
}

R2md<- Vard /( Vard + mm.dd2$VCV[,1] + log((1/exp(mm.dd2$Sol[,1])) + 1)) 
```

``` r
posterior.mode(R2md)
```

    ##        var1 
    ## 0.002657647

… only about 0.3% of the variance in density explained by the effect of
the depth

#### 2.b. Depth & spawning behaviours (courtship intensity & aggression events)

The models are the same as in part 1, expect that the response is
variable is “depth”.

``` r
itr<-70

mm.aggd<-MCMCglmm(round(corfreq.agg)~depth - 1,data=tpds[complete.cases(tpds$depth),], rcov= ~units, family = "poisson",
                  prior=p, nitt = 13000*itr ,thin = 10*itr, burnin = 3000*itr) # Aggression

mm.crd<-MCMCglmm(round(corfreq.court)~depth - 1,data=tpds[complete.cases(tpds$depth),], rcov= ~units, family = "poisson",
                 prior=p, nitt = 13000*itr ,thin = 10*itr, burnin = 3000*itr) # Courtship
```

``` r
summary(mm.aggd)
```

    ## 
    ##  Iterations = 210001:909301
    ##  Thinning interval  = 700
    ##  Sample size  = 1000 
    ## 
    ##  DIC: 54.8208 
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean  l-95% CI u-95% CI eff.samp
    ## units   0.06483 0.0002797   0.2944    740.7
    ## 
    ##  Location effects: round(corfreq.agg) ~ depth - 1 
    ## 
    ##              post.mean l-95% CI u-95% CI eff.samp pMCMC   
    ## depthShallow    1.1209   0.7076   1.4944     1127 0.002 **
    ## depthDeep       0.1082  -0.7498   1.0769     1000 0.736   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(mm.crd)
```

    ## 
    ##  Iterations = 210001:909301
    ##  Thinning interval  = 700
    ##  Sample size  = 1000 
    ## 
    ##  DIC: 41.21374 
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean  l-95% CI u-95% CI eff.samp
    ## units   0.04496 0.0002441   0.1847     1000
    ## 
    ##  Location effects: round(corfreq.court) ~ depth - 1 
    ## 
    ##              post.mean l-95% CI u-95% CI eff.samp pMCMC  
    ## depthShallow   0.59975  0.05873  1.01735   1000.0 0.018 *
    ## depthDeep     -0.14064 -1.31530  0.81529    894.1 0.846  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Density plots

``` r
dd.behav<-data.frame(Value = c(mm.aggd$Sol[,"depthShallow"],
                               (mm.aggd$Sol[,"depthDeep"]),
                               mm.crd$Sol[,"depthShallow"],
                               (mm.crd$Sol[,"depthDeep"])),
                     Effect =c(rep("Shallow",nrow(mm.aggd$Sol)),rep("Deep",nrow(mm.aggd$Sol)),
                               rep("Shallow",nrow(mm.crd$Sol)),rep("Deep",nrow(mm.crd$Sol))), 
                     Behaviour = c(rep("Aggression",nrow(mm.aggd$Sol)*2) , rep("Courtship",nrow(mm.crd$Sol)*2)))
dd.behav$Value.obs<-exp(dd.behav$Value) # Posterior estimates on the observed scale

eye.behav.depth<-ggplot(dd.behav, aes(y = Effect, x = Value.obs)) +  
  geom_halfeyeh(.width = 0.95,fill = "lightblue", color = "#003333",point_interval = mode_hdci) + theme_bw() + 
  facet_wrap(~Behaviour, ncol = 1) + xlab("Number of events") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 17, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12, vjust = 2),
        axis.text.y = element_text(size = 14,face = "bold"),
        strip.background = element_rect(fill = "white", colour = "white"), 
        strip.text.x = element_text(size = 12,face = "bold", hjust = 0.05))
```

``` r
eye.behav.depth
```

![](Report-LB-spawning-Bayesian-method_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

Now calculate the \(R^{2}_{lnnorm(m)}\) as in Part 2.a. for Aggression
and Courtship, respectively.

``` r
# R2 of aggression 
Varag<-numeric(1000)
for(i in 1:1000) { Var <- var(as.vector(mm.aggd2$Sol[i,]%*% t(mm.aggd2$X))) 
Varag[i] <- Var 
}

R2mag<- Varag /( Varag + mm.aggd2$VCV[,1] + log((1/exp(mm.aggd2$Sol[,1])) + 1)) 
```

``` r
posterior.mode(R2mag)
```

    ##     var1 
    ## 0.521561

``` r
# R2 of courtship model

mm.crd2<-MCMCglmm(round(corfreq.court)~depth,data=tpds[complete.cases(tpds$depth),], rcov= ~units, family = "poisson",
                 prior=p, nitt = 13000*itr ,thin = 10*itr, burnin = 3000*itr) # Courtship


Varagc<-numeric(1000)
for(i in 1:1000) { Var <- var(as.vector(mm.crd2$Sol[i,]%*% t(mm.crd2$X))) 
Varagc[i] <- Var 
}
R2mc<- Varagc /( Varagc + mm.crd2$VCV[,1] + log((1/exp(mm.crd2$Sol[,1])) + 1)) 
```

``` r
posterior.mode(R2mc)
```

    ##         var1 
    ## -0.000132722

### 3\. Effects of temperature on development

``` r
dredd <- read.table("~/PATH-TO/deeptemperature.txt", h=T)
```

``` r
dredd$Date<- as.Date(dredd$Date, "%m/%d/%y")

ggplot(dredd, aes(x=Date, y=Degree_day, colour = Redd)) +
  geom_line(size = 2) +
  scale_colour_manual( values = c("#003399","#99CCFF")) +
  ylab("CTUs (°C)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m") +
  geom_hline(yintercept = 440, colour = "darkgrey", size = 1) + 
  geom_hline(yintercept = 650, colour = "darkgrey", size = 1) + 
  geom_vline(xintercept=as.numeric(dredd$Date[63]), linetype = 2, colour = "lightgrey", size = 1) +
  #geom_vline(xintercept = "2019-09-27")
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
```

![](Report-LB-spawning-Bayesian-method_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->
