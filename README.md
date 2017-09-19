---
title: "Equations and example of simple CFA and SEM"
output: html_document
---
Here we have a simple example of a confirmatory factor analysis (CFA) with four items that compose a single construct in this example visual.  The first box to check is the model's identification.  We cannot assess how well the model we are describing is doing unless we know that the model is identified.  If the model is underidentified than there are more degrees of freedom than parameters being estimated.  We can calculate the degrees of freedom using the following formula q(q+1)/2 where q = the number of items.  Therefore we have, 4*(4+1)/2 = 10 degrees of freedom and are estimating a total of 8 parameters, which are described below. 
```{r, message=FALSE, warning=FALSE}
library(lavaan)
data(HolzingerSwineford1939)
dat = HolzingerSwineford1939
head(dat)
```
$${x_{1} = \lambda_{11}(\xi_{1})+ \delta_{1}}~~~ (1.1)$$ 
$${x_{2} = \lambda_{21}(\xi_{1})+ \delta_{2}}~~~ (1.2)$$ 
$${x_{3} = \lambda_{31}(\xi_{1})+ \delta_{3}}~~~ (1.3)$$ 
$${x_{4} = \lambda_{41}(\xi_{1})+ \delta_{4}}~~~ (1.4)$$
\[
\begin{bmatrix}
    \theta_{11} & 0  & 0 & 0\\
    0  & \theta_{22}  & 0 & 0 \\
    0 & 0 & \theta_{33} & 0 \\
    0 & 0 & 0 & \theta_{44}\\
\end{bmatrix} {(2)}
\]
\[
\begin{bmatrix}
    \phi_{1}\\
\end{bmatrix}{(3)}
\]

Equations 1.1 through 1.4 are the equations for the items.  In this example, each item has a fixed intercept at 1, with a regression weight of lambda multiplied by the latent variable xi.  Then we add the unique error term for that item delta.  

Equation 2 is the variance covariance matrix for the deltas (i.e. the error terms).  In this model, I am assuming that error terms are uncorrelated and the variances are estimated by the model.

Equation 3 is the variance covariance matrix for the phis, which in this case there is only one, because we are only estimating the variance for the latent construct visual and no other covariances or variances, because there are on other latent constructs in this example.    

Now we can conduct the CFA with one construct loading onto four items using the code below.
```{r}
model1 = 'visual =~ x1 + x2 + x3 +x4'
fit1 <-lavaan:::cfa(model1, data=dat)
summary(fit1, fit.measures = TRUE)
```
Number of observations = Total number of data points, which is 301
Estimator = This says that I used the maximum likelihood estimator to find the parameter estimates
Minimum Function Test Statistic = Test statistic to assess if the model implied specifications and data are statistically significantly different.  This statistic is almost always significant meaning we need to look to other fit indices such as CFI, TFI, RMSEA, and SRMR to assess the model’s fit.

Model test baseline model = Tests whether the model is a better fit for the data than a null or empty model.  

CFI/TFI = A value of .935 means that our model is 93.5% better than the null. 
RMSEA = This is the average amount of residual or error the model between the data implied and the model covariance structure.
SRMR = Standardized version of the RMSEA.

Latent variables = These are interpreted just like a covariate in a regression model.  The first item is 1, because we need one item to serve as the comparison for all other items.  Therefore, we are only estimating 8 parameters not 9, because the parameter estimate for item one is fixed.  

Finally, we can plot the model to help us visualize what it looks like using the semPlot package.
```{r, message=FALSE, warning=FALSE}
library(semPlot)
semPaths(fit1, title = FALSE, curvePivot = TRUE)
```
Now we can move to an example using SEM.  Here we will expand the model to include an additional latent variable textual and then add the SEM part, which evaluates the direct effect that visual has on textual.  

The measurement equations are the same, except that the four textual items are indicated by y's instead of x's.  The only new set of equations for this model is equation 7 where we have the regression coefficient beta(12) indicating the direct effect that we are hypothesizing that visual has on textual.
```{r, message=FALSE, warning=FALSE}
model2 = 'visual =~ x1 + x2 + x3 +x4
          textual =~ x5 + x6 + x7 +x8
          visual ~ textual'
fit2 = sem(model2, data = dat)
summary(fit2)
semPaths(fit2, title = FALSE, curvePivot = TRUE)
```
$${x_{1} = \lambda_{11}(\xi_{1})+ \delta_{1}}~~~ (4.1)$$ 
$${x_{2} = \lambda_{21}(\xi_{1})+ \delta_{2}}~~~ (4.2)$$ 
$${x_{3} = \lambda_{31}(\xi_{1})+ \delta_{3}}~~~ (4.3)$$ 
$${x_{4} = \lambda_{41}(\xi_{1})+ \delta_{4}}~~~ (4.4)$$
$${y_{1} = \lambda_{11}(\xi_{1})+ \delta_{1}}~~~ (4.5)$$ 
$${y_{2} = \lambda_{21}(\xi_{1})+ \delta_{2}}~~~ (4.6)$$ 
$${y_{3} = \lambda_{31}(\xi_{1})+ \delta_{3}}~~~ (4.7)$$ 
$${y_{4} = \lambda_{41}(\xi_{1})+ \delta_{4}}~~~ (4.8)$$
\[
\begin{bmatrix}
    \theta_{11} & 0  & 0 & 0 & 0 & 0 & 0 & 0\\
    0  & \theta_{22}  & 0 & 0 & 0 & 0 & 0 & 0 \\
    0 & 0 & \theta_{33} & 0 & 0 & 0 & 0 & 0 \\
    0 & 0 & 0 & \theta_{44} & 0 & 0 & 0 & 0\\
    0 & 0 & 0 & 0 & \theta_{55} & 0 & 0 & 0\\
    0 & 0 & 0 & 0 & 0 & \theta_{66} & 0 & 0\\
    0 & 0 & 0 & 0 & 0 & 0 &  \theta_{77} & 0\\
    0 & 0 & 0 & 0 & 0 & 0 & 0 & \theta_{88}\\
\end{bmatrix} {(5)}
\]

\[
\begin{bmatrix}
    \phi_{11}       & 0 \\
    0       & \phi_{22} \\
\end{bmatrix} {(6)}
\] 

\[
\begin{bmatrix}
    0      & \beta_{12}  \\
    0       & 0 \\
\end{bmatrix} {(7)}
\]
