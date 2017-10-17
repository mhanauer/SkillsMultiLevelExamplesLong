---
title: "Using Multilevel Modeling for Longitudinal Models in R"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
In this example, I am documenting how to formulate an equation for random intercepts and slopes in multilevel modeling for longitudinal models along with the r code using the nlme package with interpretation of the output.  In this current data set there are five variables id, time, interven (i.e. an intervention that students are participating in), and score (i.e. the score the student received on the intervention).    Overall there are 100 participants over 10 time points.  In this demonstration we will want to predict a student's score on an intervention while accounting for the nesting of students over time and make the model more complicated further into this example.  Multilevel modeling for longitudinal data analysis makes sense, because it can account for the correlation between time points by allowing each person to have their own intercept (mean value) as well as their own trajectory (i.e. slope).  After accounting for the variance within students for their differences in intercepts and slopes, it is likely that the assumption of independent error terms will be met.
```{r}
id = rep(1:100, each = 10)
time = rep(1:10, 100)
time = time-1
set.seed(12345)
score = c(rnorm(5000, 20, 5), rnorm(5000, 0, 5))
score = as.data.frame(round(scale(score, center = TRUE, scale = FALSE),0))
colnames(score) = c("score")
interven = c(rep(1, 5000), rep(0,5000))
gpa = round(abs(rnorm(10000,2,1)),2)
dat = as.data.frame(cbind(id, interven, time, gpa, score)); head(dat)
```
Graphing
```{r}
install.packages("lattice")
```



Here I try to break down what each component of the equation means.

gamma00 is the average or mean score across the time points.  The indexes indicate where the parameter falls in each level.  For example, the first 0 in the gamma00 means that it is the intercept for level one.  The second zero means that it is the mean or intercept for level two.  However, level two intercepts can vary by u0j, which will be discussed next. 

u0j this is the random deviation from the intercept for each time point.  This is a level two error term, which is why it only has the subscript j instead of i and j.  It has a j, because j represents all of the time points (i.e. from 1 to j time points).  For example the unique deviation from the intercept for time point one would be u01.  

Beta10 is the average regression coefficient for the change associated with the time variable (timeij).  It is the average change we would see given a one unit change in the independent variable (in this case one time point increase).  The first 1 index means that it is the first slope coefficient in level one and the second 0 means that it is the average slope coefficient (i.e. no effect of any level two variables), because in this model the slope for time is not influenced by any level two variables.   

eij is the individual level one error term for each individual over each time point.
 $$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{10}(time_{ij}) + e_{ij}}~~~ (1.1)$$
$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + u_{0j}} ~~~ (1.2)$$

$$Mixed~model: ~~~{y_{ij} = \gamma_{00} + \beta_{10}(time_{ij}) + u_{0j} + e_{ij}} ~~~(1.3)$$
Here is the r-code for the multilevel model described in equations 1 using the nlme package.  There is the fixed part, which will have the average intercept and the parameter estimate for the time variable. To model the random intercepts, we use the 1 to signify that this is a random intercepts model where we want intercepts for each student (i.e. the id variable).   
```{r, message=FALSE, warning=FALSE}
library(nlme)
model1 = lme(fixed = score ~ time, random = ~1 | id, data = dat)
sumModel1 = summary(model1); sumModel1
```
We are using REML, because it accounts for the included parameters correctly in the standard errors (i.e. making them larger by accounting for the reduced degrees of freedom). 

AIC, BIC, and logLik are all model comparison statistics.

Fixed effects: The time variable is the rate of change in the score related to one unit or one time point increase holding all other variables constant.


Correlation (inter): The correlation between the fixed intercept and fixed slope for time. If the value is above zero, then we could say that as the intercept increases so does the slope and the opposite for a negative relationship. In our case, as the intercept or mean for score move up, we see a decrease in the slope for time.

Random effects: These are the standard deviations for the two error terms the standard deviation for random intercepts and the error term for the individual students (i.e. the variance for the residuals).

Standardized residuals = These are the residuals scaled by the standard deviation.

Now we can estimate the random slope model.  The random slope model adds one unique component which is u1j.  This is the random level two error for the slope allowing the slope for time to vary for each person.  It is indexed with j, because it can vary for each person.  It is multiplied by the covariate value of time, because it is a slope therefore we need to multiply it by the covariate for which it is measuring the slope.

$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}(interven_{ij}) + e_{ij}}~~~ (2.1)$$

$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + u_{0j}} ~~~ (2.2)$$

$$ Level~2~Slope:~~~{\beta_{1j} = \gamma_{10} + u_{1j}} ~~~ (2.2)$$
$$ Mixed~model:~~~{y_{ij} = \gamma_{00} + \gamma_{10}(interven_{ij}) +u_{0j} + u_{1j}(interven_{ij}) + e_{ij}}~~~ (2.3)$$
For this model, in nlme, instead of using one, we now use the term that we want to have a random slope which is time.
```{r}
model2 = lme(fixed = score ~ time, random = ~ time | id, data = dat)
sumModel2 = summary(model2); sumModel2
```
We could include one of two types of variables.  Time varying and time invariant.  Time invariant variables are ones that do not vary over time and therefore are level two variables.  In our example, this means that they do not vary for the student.  These types of variables would be ethnicity and gender.  Additionally, we could include time varying variables, such as a student's GPA, which will likely change from year to year.

To include the time varying covariate gpa, we first include it in the level one part, because it is a level one variable.  Then for each student to have a slope for gpa, we create another level two slope equation which has the fixed or average slope for gpa over all the students (gamma20) and the variation in gpa for each student (u2j).

We could also include a time invariant variable intervention (interven) (i.e. a level two variable) that affects a student average score over time (i.e. random intercepts) and their average trajectory on gpa over time (random slopes).  To do this we add the time invariant covariate interven to the level two intercept (gamma01), level two slopes for b1j (gamma11) the time slope and the level two slope for b2j (gamma21) the gpa slope.

$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}(time_{ij}) +\beta_{2j}(gpa_{ij}) + e_{ij}}~~~ (3.1)$$
$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} +\gamma_{01}(interven_{j})+ u_{0j}} ~~~ (3.2)$$
$$ Level~2~Slope~\beta_{1j}:~~~{\beta_{1j} = \gamma_{10} +\gamma_{11}(interven_{j}) + u_{1j}} ~~~ (3.3)$$

$$ Level~2~Slope~\beta_{2j}:~~~{\beta_{2j} = \gamma_{20}+ \gamma_{21}(interven_{j}) + u_{2j}} ~~~ (3.4)$$

$$ Mixed~model:~~~{y_{ij} = \gamma_{00} + \gamma_{01}(interven_{j})+ \gamma_{10}(time_{ij}) + \gamma_{11}(interven_{j})(time_{ij})  +\gamma_{20}(gpa_{ij}) + \gamma_{21}(interven_{j})(gpa_{ij}) +u_{0j} + u_{1j}(time_{ij}) +u_{2j}(gpa_{ij}) + e_{ij}}~~~ (3.5)$$

To include the time invariant (interven) and time variant (gpa) variables and model the effect that interven has on the slopes of both time and gpa, we need to add several new items to the r code.  First, we need to add the interven variable representing the average effect overall for the intervention, gpa, which is the average effect gpa has on the score outcome variable, and the interaction between interven and time, measuring the impact the intervention has over time on the outcome variable, the interaction between interven and gpa, measuring the impact interven has on gpa’s influence score outcome variable.  Additionally, to get the random slope coefficients for gpa, we need to add it to the random section of the code along with time.
```{r}
model4 = lme(fixed = score ~ time+ interven + gpa + interven*time + gpa*time, random = ~ time + gpa | id, data = dat)
sumModel4 = summary(model4); sumModel4
```
Another way to model longitudinal data with a multilevel model is to change the structure of the error variance.  In the previous model we assumed that the error terms were uncorrelated after accounting for the variance between and within the students; however, there may still be some correlation between the time points.  Therefore, we could use a common error structure where the correlation between adjacent time points are accounted for.  Let us go back to the original model with just time and random intercepts to demonstrate r-code for this scenario.  The new component is the correlation = corAR1(), which tells r to use a autoregression 1 error structure, which is what I just described.
```{r}
model5 = lme(fixed = score ~ time, random = ~ time | id, correlation = corAR1(), data = dat)
sumModel5 = summary(model5); sumModel5
```
The parameter phi is .79, which is a good indicator that adjacent time points for each person are related.

References:
Finch, W. H., Bolin, J. E., & Kelley, K. (2014). Multilevel modeling using R. Crc Press.
