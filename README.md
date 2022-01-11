# BayesianMixedEffectModel-DenimProduction-Analysis
The *denim* dataset concerns the amount of waste in material cutting for a jeans manufacturer due to five suppliers. 

**Plot the data and comment.**

```R
> library(ggplot2)
> ggplot(denim, aes(x=supplier, y=waste))+geom_point()
```

<img src="https://raw.githubusercontent.com/yjjjjxx/BayesianMixedEffectModel-DenimProduction-Analysis/main/Screen%20Shot%202021-11-15%20at%2001.34.09.png" style="zoom:50%;" />

The plot can indicate that there is no obvious difference of waste in material cutting for these suppliers, except there are two outliers in supplier 1 and 2. 

**Fit the one-way ANOVA model using *INLA* using the default prior. Comment on the fit.**

```R
> library(INLA)

> formula <- waste ~ f(supplier, model="iid")
> result <- inla(formula, family="gaussian", data=denim)
> summary(result)

Call:
   "inla(formula = formula, family = \"gaussian\", data = denim)" 
Time used:
    Pre = 5.33, Running = 6.34, Post = 0.213, Total = 11.9 
Fixed effects:
             mean    sd 0.025quant 0.5quant 0.975quant  mode kld
(Intercept) 6.977 1.014      4.981    6.977      8.971 6.977   0

Random effects:
  Name	  Model
    supplier IID model

Model hyperparameters:
                                           mean       sd 0.025quant 0.5quant 0.975quant    mode
Precision for the Gaussian observations 1.0e-02 2.00e-03      0.008     0.01   1.40e-02    0.01
Precision for supplier                  2.2e+04 2.39e+04   1653.538 14893.73   8.52e+04 4519.20

Expected number of effective parameters(stdev): 1.00(0.003)
Number of equivalent replicates : 94.93 

Marginal log-Likelihood:  -367.01 
```

We can find that the intercept of 6.977 is similar as the previous plot. We need to compute SDs for interpretative purposes. The posterior mean for σ~ε~ is $\frac{1}{\sqrt{0.010}}=10$. However, the precision for the supplier term is very large and so the posterior mean for σ~α~ is close to zero. We know from the plots of the data that we could not possibly be this confident about a conclusion of no variation between suppliers. Something is wrong.

**Refit the model but with more informative priors. Make a density plot of the error and supplier SD posterior densities.**

```R
> sdres <- sd(denim$waste)
> pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))
> formula <- waste ~ f(supplier, model="iid", hyper = pcprior)
> result <- inla(formula, family="gaussian", data=denim)
> result <- inla.hyperpar(result)
```

```R
> sigmaalpha <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[2]])
> sigmaepsilon <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[1]])
> ddf <- data.frame(rbind(sigmaalpha,sigmaepsilon),errterm=gl(2,2048,labels = c("alpha","epsilon")))
> ggplot(ddf, aes(x,y, linetype=errterm))+geom_line()+xlab("waste")+ylab("density")+xlim(0,16)
```

<img src="https://raw.githubusercontent.com/yjjjjxx/BayesianMixedEffectModel-DenimProduction-Analysis/main/Screen%20Shot%202021-11-17%20at%2011.47.26.png" style="zoom:50%;" />

From the plot, we can find that the distribution of SD alpha is concentrated extremely left side (skewed to right).

**Calculate summaries of the posteriors from the model fit.**

```R
> summary(result)

Call:
   "inla(formula = formula, family = \"gaussian\", data = denim)" 
Time used:
    Pre = 2.33, Running = 0.237, Post = 0.184, Total = 2.75 
Fixed effects:
            mean    sd 0.025quant 0.5quant 0.975quant  mode kld
(Intercept) 7.05 1.519      4.092    7.035     10.109 7.016   0

Random effects:
  Name	  Model
    supplier IID model

Model hyperparameters:
                                            mean       sd 0.025quant 0.5quant 0.975quant  mode
Precision for the Gaussian observations 1.10e-02 2.00e-03      0.008    0.010      0.014 0.010
Precision for supplier                  7.22e+05 1.06e+09      0.025    0.423    206.778 0.053

Expected number of effective parameters(stdev): 2.42(1.10)
Number of equivalent replicates : 39.27 

Marginal log-Likelihood:  -367.69 
```

**Report 95% credible intervals for the SDs using the summary output. Compute the posterior modes for the error and supplier SDs and compare these to the posterior means.**

The 95% credible interval for SD of supplier is [0.025, 206.778] and the 95% credible interval for SD of error is [0.008, 0.014]. The mode for the error SD is 0.010 which is pretty similar to its mean (indicate normal distribution). The mode for supplier SD is 0.053 which is pretty smaller than its mean (indicate right-skewed distribution). 

**Remove the two outliers from the data and repeat the analysis. Comment on any interesting differences.**

```R
# Remove two outliers 
> denim1 <- subset(denim, waste < 40)

> sdres <- sd(denim1$waste)
> pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))
> formula <- waste ~ f(supplier, model="iid", hyper = pcprior)
> result <- inla(formula, family="gaussian", data=denim1)
> result <- inla.hyperpar(result)
```

```R
> sigmaalpha <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[2]])
> sigmaepsilon <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[1]])
> ddf <- data.frame(rbind(sigmaalpha,sigmaepsilon),errterm=gl(2,2048,labels = c("alpha","epsilon")))
> ggplot(ddf, aes(x,y, linetype=errterm))+geom_line()+xlab("waste")+ylab("density")+xlim(0,15)
```

<img src="https://raw.githubusercontent.com/yjjjjxx/BayesianMixedEffectModel-DenimProduction-Analysis/main/Screen%20Shot%202021-11-17%20at%2012.22.34.png" style="zoom:50%;" />

From the plot, we can find that the density distribution of SD alpha became normally distributed. 

```R
> summary(result)

Call:
   "inla(formula = formula, family = \"gaussian\", data = denim1)" 
Time used:
    Pre = 2.45, Running = 0.292, Post = 0.21, Total = 2.95 
Fixed effects:
             mean    sd 0.025quant 0.5quant 0.975quant  mode kld
(Intercept) 6.147 1.564      3.028    6.122      9.385 6.077   0

Random effects:
  Name	  Model
    supplier IID model

Model hyperparameters:
                                            mean       sd 0.025quant 0.5quant 0.975quant  mode
Precision for the Gaussian observations    0.027 4.00e-03      0.020    0.027      0.036 0.027
Precision for supplier                  5585.727 4.98e+06      0.021    0.159      4.775 0.060

Expected number of effective parameters(stdev): 3.79(0.866)
Number of equivalent replicates : 24.51 

Marginal log-Likelihood:  -317.42 
```

From the summary results, we can find that the mean of error SD is slightly increased and it is not statistically different with the previous results. The mean of supplier SD is greatly reduced than previous and the upper bound of 95% credible interval also become much smaller. 



**Use the *denim* dataset again for this question but conduct the analysis using STAN.**

```R
> library(rstan)
> library(faraway)
> data("denim")
```

**Fit the one-way ANOVA model using *STAN* with the default prior. Produce diagnostic plots for the three parameters: the mean and standard deviations of the supplier and error effects.**

```R
> denimdat <- list(N=95, J=5, response=denim$waste, predictor=as.numeric(denim$supplier))
> fit <- stan(file = "/Users/yjjjjxx/Desktop/STOR557_21Fall/oneway.stan", data = denimdat)
```

```R
# Diagnostic plot for μ
> traceplot(fit, pars="mu", inc_warmup = FALSE)
```

<img src="https://raw.githubusercontent.com/yjjjjxx/BayesianMixedEffectModel-DenimProduction-Analysis/main/Screen%20Shot%202021-11-15%20at%2021.17.52.png" style="zoom:50%;" />

The four chains exhibit strong mixing as they all vary randomly around a constant level with no obvious dependence.

```R
# Diagnostic plot for SD
> traceplot(fit, pars="sigmaalpha", inc_warmup = FALSE)
```

<img src="https://raw.githubusercontent.com/yjjjjxx/BayesianMixedEffectModel-DenimProduction-Analysis/main/Screen%20Shot%202021-11-15%20at%2021.20.01.png" style="zoom:50%;" />

The cluster of four chains are near 0 and they also vary randomly which can show nice mixing. 

```R
# Diagnostic plot for error effects
> traceplot(fit, pars="sigmaepsilon", inc_warmup = FALSE)
```

<img src="https://raw.githubusercontent.com/yjjjjxx/BayesianMixedEffectModel-DenimProduction-Analysis/main/Screen%20Shot%202021-11-15%20at%2021.22.23.png" style="zoom:50%;" />

Similarly, the four chains exhibit strong mixing as they all vary randomly around a constant level with no obvious dependence.

**Report the posterior mean,95% credible intervals and effective sample size for the three parameters.**

```R
> print(fit, pars=c("mu","sigmaalpha","sigmaepsilon","a"))
Inference for Stan model: oneway.
4 chains, each with iter=2000; warmup=1000; thin=1; 
post-warmup draws per chain=1000, total post-warmup draws=4000.

             mean se_mean   sd 2.5%  25%  50%   75% 97.5% n_eff Rhat
mu           7.27    0.25 2.22 3.53 6.11 7.07  8.08 13.13    78 1.06
sigmaalpha   2.88    0.32 2.95 0.11 1.03 2.08  3.62 11.55    84 1.06
sigmaepsilon 9.98    0.02 0.75 8.66 9.46 9.93 10.45 11.60  2466 1.00
a[1]         5.87    0.04 1.74 1.95 4.76 5.98  7.09  8.95  1979 1.00
a[2]         7.84    0.04 1.68 4.84 6.70 7.73  8.88 11.53  2237 1.00
a[3]         6.09    0.04 1.83 2.00 5.02 6.25  7.29  9.42  2109 1.00
a[4]         7.30    0.06 1.74 3.96 6.21 7.25  8.39 10.99   757 1.01
a[5]         8.39    0.09 2.13 4.91 6.90 8.10  9.66 13.15   556 1.01

Samples were drawn using NUTS(diag_e) at Mon Nov 15 21:15:46 2021.
For each parameter, n_eff is a crude measure of effective sample size,
and Rhat is the potential scale reduction factor on split chains (at 
convergence, Rhat=1).
```

The posterior mean is 7.27 which is what we would expect. The 95% credible interval for mean is [3.53, 13.13] and effective sample size is 78. The 95% credible interval for SD is [0.11, 11.55] and effective sample size is 84. The 95% credible interval for error SD is [8.66, 11.60] and effective sample size is 2466. 

**Make a plot of the posterior densities of the supplier and error effects.Estimate the probability that the supplier SD is bigger than the error SD.**

```R
> library(reshape2)
> postsig <- extract(fit, pars=c("sigmaalpha","sigmaepsilon"))
> ref <- melt(postsig,value.name="waste")
> ggplot(data=ref,aes(x=waste, linetype=L1)) + geom_density()+xlim(0,15)+scale_linetype(name="SD",labels=c("supplier","error"))
```

<img src="https://raw.githubusercontent.com/yjjjjxx/BayesianMixedEffectModel-DenimProduction-Analysis/main/Screen%20Shot%202021-11-16%20at%2010.24.35.png" style="zoom:50%;" />

```R
# Estimate the probability that the supplier SD is bigger than the error SD
> mean(postsig$sigmaalpha > postsig$sigmaepsilon)
[1] 0.07625
```

**Plot the posterior distributions of the five suppliers. Which supplier tends to produce the least waste and which the most? What is the probability that the best supplier is better than the worst supplier?**

```R
> opre <- rstan::extract(fit, pars="a")
> ref <- melt(opre, value.name="waste")
> ggplot(data=ref,aes(x=waste, linetype=factor(Var2)))+geom_density()+scale_linetype(name="supplier",labels=LETTERS[1:5])
```

<img src="https://raw.githubusercontent.com/yjjjjxx/BayesianMixedEffectModel-DenimProduction-Analysis/main/Screen%20Shot%202021-11-16%20at%2011.03.27.png" style="zoom:50%;" />

```R
> mean(ref[ref[,2]==1,3])
[1] 5.76439
> mean(ref[ref[,2]==2,3])
[1] 7.86258
> mean(ref[ref[,2]==3,3])
[1] 5.986478
> mean(ref[ref[,2]==4,3])
[1] 7.183805
> mean(ref[ref[,2]==5,3])
[1] 8.314281
```

Supplier 1 tends to produce the least waste and supplier 5 tends to produce the most waste. 

```R
> mean(ref[ref[,2]==1,3]>ref[ref[,2]==5,3])
[1] 0.18325
```

The probability that the best supplier is better than the worst supplier is 0.18325. 

**A plot of the data reveals two obvious outliers. Repeat the analysis without these two points and report on any interesting differences with the full data.**

```R
# Remove two outliers 
> denim1 <- subset(denim, waste < 40)

> denimdat1 <- list(N=93, J=5, response=denim1$waste, predictor=as.numeric(denim1$supplier))
> fit1 <- stan(file = "/Users/yjjjjxx/Desktop/STOR557_21Fall/oneway.stan", data = denimdat1)

> print(fit1, pars=c("mu","sigmaalpha","sigmaepsilon","a"))
Inference for Stan model: oneway.
4 chains, each with iter=2000; warmup=1000; thin=1; 
post-warmup draws per chain=1000, total post-warmup draws=4000.

             mean se_mean   sd 2.5%  25%  50%   75% 97.5% n_eff Rhat
mu           6.36    0.15 2.00 2.49 5.21 6.22  7.27 11.30   174 1.05
sigmaalpha   4.02    0.24 2.83 0.56 2.19 3.25  4.88 12.54   145 1.04
sigmaepsilon 6.23    0.01 0.49 5.37 5.89 6.21  6.53  7.30  1862 1.00
a[1]         3.41    0.04 1.43 0.62 2.54 3.42  4.39  6.10  1036 1.01
a[2]         5.96    0.03 1.23 3.58 5.11 5.96  6.79  8.48  2230 1.00
a[3]         5.15    0.03 1.30 2.49 4.31 5.18  5.99  7.55  2166 1.00
a[4]         7.13    0.02 1.28 4.64 6.25 7.14  7.97  9.60  3185 1.00
a[5]         9.18    0.06 1.83 5.61 7.98 9.20 10.41 12.69   799 1.01

Samples were drawn using NUTS(diag_e) at Tue Nov 16 11:56:56 2021.
For each parameter, n_eff is a crude measure of effective sample size,
and Rhat is the potential scale reduction factor on split chains (at 
convergence, Rhat=1).
```

It is interesting to find that the posterior mean for σ~α~ is larger than the previous result and the posterior mean for σ~ε~ is smaller than the previous result. 

```R
> library(reshape2)
> postsig1 <- extract(fit1, pars=c("sigmaalpha","sigmaepsilon"))
> ref1 <- melt(postsig1,value.name="waste")
> ggplot(data=ref1,aes(x=waste, linetype=L1)) + geom_density()+xlim(0,20)+scale_linetype(name="SD",labels=c("supplier","error"))
```

<img src="https://raw.githubusercontent.com/yjjjjxx/BayesianMixedEffectModel-DenimProduction-Analysis/main/Screen%20Shot%202021-11-16%20at%2013.08.00.png" style="zoom:50%;" />

```R
> mean(postsig1$sigmaalpha > postsig1$sigmaepsilon)
[1] 0.16725
```

Compared to the previous plot, the SD for supplier is much more normally distributed right now. 

```R
> opre1 <- rstan::extract(fit1, pars="a")
> ref1 <- melt(opre1, value.name="waste")
> ggplot(data=ref1,aes(x=waste, linetype=factor(Var2)))+geom_density()+scale_linetype(name="supplier",labels=LETTERS[1:5])
```

<img src="https://raw.githubusercontent.com/yjjjjxx/BayesianMixedEffectModel-DenimProduction-Analysis/main/Screen%20Shot%202021-11-16%20at%2013.19.24.png" style="zoom:50%;" />

Compared to the previous plot, the distributions for each supplier are now much more separated. 

```R
> mean(ref1[ref1[,2]==1,3])
[1] 3.413299
> mean(ref1[ref1[,2]==2,3])
[1] 5.959834
> mean(ref1[ref1[,2]==3,3])
[1] 5.145292
> mean(ref1[ref1[,2]==4,3])
[1] 7.13234
> mean(ref1[ref1[,2]==5,3])
[1] 9.181514
```

Supplier 1 still tends to produce the least waste (become smaller) and supplier 5 still tends to produce the most waste (become larger).

```R
> mean(ref1[ref1[,2]==1,3]>ref1[ref1[,2]==5,3])
[1] 0.013
```

The probability that the best supplier is better than the worst supplier became smaller than previous result.

**Instead of removing the two outliers, change the error distribution from normal to *t*3. Repeat the analysis and indicate how this changes the conclusions. Discuss which approach to handling the outliers is best.**

```R
> fit2 <- stan(file = "/Users/yjjjjxx/Desktop/STOR557_21Fall/onewayt.stan", data = denimdat)

> print(fit2, pars=c("mu","sigmaalpha","sigmaepsilon","a"))
Inference for Stan model: onewayt.
4 chains, each with iter=2000; warmup=1000; thin=1; 
post-warmup draws per chain=1000, total post-warmup draws=4000.

              mean se_mean   sd  2.5%  25%   50%   75% 97.5% n_eff Rhat
mu            5.95    0.13 2.56  0.47 4.67  5.98  7.39 11.33   416 1.00
sigmaalpha    5.55    0.48 4.09  1.82 3.14  4.41  6.36 20.24    74 1.06
sigmaepsilon  4.35    0.01 0.49  3.45 4.02  4.31  4.65  5.37  2117 1.00
a[1]          1.97    0.02 1.04 -0.01 1.30  1.94  2.61  4.09  2749 1.00
a[2]          6.05    0.02 1.11  3.94 5.29  6.02  6.75  8.34  3867 1.00
a[3]          5.11    0.02 1.14  2.82 4.38  5.12  5.87  7.31  2737 1.00
a[4]          7.33    0.02 1.11  5.20 6.59  7.33  8.05  9.51  2567 1.00
a[5]         10.69    0.04 1.90  6.98 9.46 10.66 11.90 14.49  2244 1.00

Samples were drawn using NUTS(diag_e) at Tue Nov 16 15:07:25 2021.
For each parameter, n_eff is a crude measure of effective sample size,
and Rhat is the potential scale reduction factor on split chains (at 
convergence, Rhat=1).
```

We can find that the posterior mean for σ~α~ is much larger than the first result and the posterior mean for σ~ε~ is much smaller than the first result. Also, they are pretty similar right now. 

```R
> postsig2 <- extract(fit2, pars=c("sigmaalpha","sigmaepsilon"))
> ref2 <- melt(postsig2,value.name="waste")
> ggplot(data=ref2,aes(x=waste, linetype=L1)) + geom_density()+xlim(0,30)+scale_linetype(name="SD",labels=c("supplier","error"))
```

<img src="https://raw.githubusercontent.com/yjjjjxx/BayesianMixedEffectModel-DenimProduction-Analysis/main/Screen%20Shot%202021-11-16%20at%2016.43.48.png" style="zoom:50%;" />

```R
> mean(postsig2$sigmaalpha > postsig2$sigmaepsilon)
[1] 0.519
```

```
> opre2 <- rstan::extract(fit2, pars="a")
> ref2 <- melt(opre2, value.name="waste")
> ggplot(data=ref2,aes(x=waste, linetype=factor(Var2)))+geom_density()+scale_linetype(name="supplier",labels=LETTERS[1:5])
```

<img src="https://raw.githubusercontent.com/yjjjjxx/BayesianMixedEffectModel-DenimProduction-Analysis/main/Screen%20Shot%202021-11-16%20at%2016.51.28.png" style="zoom:50%;" />

We can see that the differences between supplier 1 and supplier2  are even greater (much more separated).

```R
> mean(ref2[ref2[,2]==1,3])
[1] 1.972797
> mean(ref2[ref2[,2]==2,3])
[1] 6.049117
> mean(ref2[ref2[,2]==3,3])
[1] 5.112895
> mean(ref2[ref2[,2]==4,3])
[1] 7.326804
> mean(ref2[ref2[,2]==5,3])
[1] 10.68935
```

From my perspective, both two method can improve the analysis results. The posterior density distribution of supplier became normal distributed. I would prefer t3 distribution method  to handle the outlier for the reason that we can see much more clear desnsity distrbution for each supplier (much more separated). 


