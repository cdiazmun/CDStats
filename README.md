# CDStats
This guide will show you how to perform basic statistical analysis using the `iris` dataset embedded in `r`.

```
head(iris)

Sepal.Length Sepal.Width Petal.Length Petal.Width Species
1          5.1         3.5          1.4         0.2  setosa
2          4.9         3.0          1.4         0.2  setosa
3          4.7         3.2          1.3         0.2  setosa
4          4.6         3.1          1.5         0.2  setosa
5          5.0         3.6          1.4         0.2  setosa
6          5.4         3.9          1.7         0.4  setosa
```

# Comparing only 2 groups of a single factor (A vs B)

The very first thing that we need to do is to check the normality of the data. The data that you want to do statistics with, is normally stored within a column, inside a `DataFrame`. We will use the *Shapiro* test to evaluate the normality. If the test returns a high *p*-value (> 0.1) it means that the data is normally distributed. 

```
# In general, for all data in a column (Sepal.Length)
shapiro.test(iris$Sepal.Length)

# Individual calculation for each group we want to compare in the factor (Species)
shapiro.test(iris$Sepal.Length[iris$Species == "virginica"])
```

We can also perform another test to check the normality, a *D'Agostino* test. For this one we would need at least 20 values to work successfully. `Omnibus` is the *p*-value that we should look at in this occasion. 

```
library(fBasics) # Package needed for dagoTest.

dagoTest(iris$Sepal.Length)
```

**If the data is normally distributed**, next thing we need to do is to check the equality of variances. In this case, if *p* < 0.05 it means that the variances are unequal between the two groups.

```
var.test(iris$Sepal.Length[iris$Species=="setosa"],
         iris$Sepal.Length[iris$Species=="versicolor"])
```

Once we have determined the equality of variances, we can perform a *t-test* to see if there are significant differences between the two groups. Change the attribute `var.equal` to `TRUE` or `FALSE` depending on the result of the previous test. Of course, is it generally agreed by the scientific community that a *p*-value below 0.05 depicts a significant difference of the data between the two groups. 

```
t.test(iris$Sepal.Length[iris$Species=="setosa"],
       iris$Sepal.Length[iris$Species=="versicolor"],
       var.equal=TRUE)

Two Sample t-test

data:  iris$Sepal.Length[iris$Species == "setosa"] and iris$Sepal.Length[iris$Species == "versicolor"]
t = -10.521, df = 98, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -1.1054165 -0.7545835
sample estimates:
mean of x mean of y 
    5.006     5.936 
```

**If the data is not normally distributed**, we need to perform a *Wilcoxon rank-sum* test.

```
wilcox.test(iris$Sepal.Length[iris$Species=="setosa"],
            iris$Sepal.Length[iris$Species=="versicolor"])

Wilcoxon rank sum test with continuity correction

data:  iris$Sepal.Length[iris$Species == "setosa"] and iris$Sepal.Length[iris$Species == "versicolor"]
W = 168.5, p-value = 8.346e-14
alternative hypothesis: true location shift is not equal to 0
```

# Comparing more than 2 groups in a single factor (A vs B vs C vs ...)

We start the same way as before, which means that we need to check the distribution of the data to see if it follows or not a normal distribution. Before, we used the *Shapiro* test and we kid of trusted the *p*-value returned. But we can also check the distribution more visually by means of a *Q-Q plot*. 

```
# Count is a vector of three strings, which are the groups we want to compare
Count <- levels(iris$Species)

# We generate a plot for each group to see if the data follows the theoretical distribution
par(mfrow =c(1 ,1))
for(i in 1:3)
  { qqnorm (iris$Sepal.Length[iris$Species == Count [i]], main = Count [i])
  qqline (iris$Sepal.Length[iris$Species == Count [i]])
  }
```
![image](https://github.com/cdiazmun/CDStats/assets/38433538/7405345c-d7a2-4a07-af9d-d51d9c873154)

Again, **if the samples are normally distributed**, we have to check the equality of variances. Before we did that through the `rstats` function `var.test`, but we can also use other tests, if more appropriate for our data, such as the *Levene* test. If *p* < 0.05 the variances are unequal. 

```
library(car) # Package needed for the leveneTest.

leveneTest(Sepal.Length ~ Species, data=iris)
```

**If the samples are normally distributed and have equal variances**, perform the *ANOVA*.
```
iris.anova <- aov(Sepal.Length ~ Species, data=iris) 

# See the results of the ANOVA.
summary (iris.anova)

            Df Sum Sq Mean Sq F value Pr(>F)    
Species       2  63.21  31.606   119.3 <2e-16 ***
Residuals   147  38.96   0.265                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
As a sanity-check, we can perform an analysis of the residuals to check the validity of the ANOVA. It should follow the theoretical correlation.

```
iris.res <- iris.anova$residuals 
qqnorm (iris.res, main =" Residuals from Pressure-Time anova model ") # Clean the plot space before executing this
qqline (iris.res)
```
![image](https://github.com/cdiazmun/CDStats/assets/38433538/d80b5e79-f8b2-4132-8d32-4164292cdb0c)

But the ANOVA only told us that if there was significant differences on the `Sepal.Length` according to the `Species`, in general. If we want to see how *virginica* compares to *setosa* and *versicolor* independently, we can perform a pairwise comparison by means of a *Tukey* test.

```
TukeyHSD(iris.anova, conf.level = 0.95)

Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = Sepal.Length ~ Species, data = iris)

$Species
                      diff       lwr       upr p adj
versicolor-setosa    0.930 0.6862273 1.1737727     0
virginica-setosa     1.582 1.3382273 1.8257727     0
virginica-versicolor 0.652 0.4082273 0.8957727     0
```

Now, **if the data followed a normal distribution but variances were not equal**, we need to perform a One-way analysis of means. Alternatively, we can perform a pairwise *t-test* by setting the `var.equal=FALSE` argument. In the latter case, we can also use the argument `alternative` depending on whether our alternative hypothesis is *two-sided* (default), *greater* or *less*.

```
oneway.test(Sepal.Length~Species, data = iris)

pairwise.t.test(iris$Sepal.Length, iris$Species, var.equal=FALSE)
```

Finally, **if the data is not normally distributed**, we can perform a *Kruskal-Wallis* rank sum test. To perform the pairwise comparisons, we could use again the *Wilcoxon* test as we did previously. 

```
kruskal.test(Sepal.Length ~ Species , data = iris)

Kruskal-Wallis rank sum test

data:  Sepal.Length by Species
Kruskal-Wallis chi-squared = 96.937, df = 2, p-value < 2.2e-16
```

# Comparing more than one factor

In the `iris` dataset, we only have the `Species` column that we can use as factor, and we already know that this factor plays a significant influence on the `Sepal.Length`, for example. So, we are going to add an extra column with a completely redundant factor that as is meaningless, it should not play any significant role in the data. 

```
iris$Treatment <- rep(c("A", "B"), 75)
```

In this example, we are testing the effect of `Species` and `Treatment` SEPARATELY, so NOT in combination. 

```
iris.anova2 <- aov(Sepal.Length ~ Species+Treatment, data=iris)
summary (iris.anova2)

             Df Sum Sq Mean Sq F value Pr(>F)    
Species       2  63.21  31.606 118.458 <2e-16 ***
Treatment     1   0.00   0.002   0.006  0.937    
Residuals   146  38.95   0.267                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

However, if we changed the `+` sign for the `*` one, we will be testing the effect of `Species` and `Treatment` both SEPARATELY and IN COMBINATION. 

```
iris.anova3 <- aov(Sepal.Length ~ Species*Treatment, data=iris)
summary (iris.anova3)

                   Df Sum Sq Mean Sq F value Pr(>F)    
Species             2  63.21  31.606 118.429 <2e-16 ***
Treatment           1   0.00   0.002   0.006  0.937    
Species:Treatment   2   0.52   0.262   0.982  0.377    
Residuals         144  38.43   0.267                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

Furthermore, this can be scaled to include as many levels as wanted. It is generally recommended to start with the most saturated model in which all effects are studied and then proceed with a backwards elimination, progressively removing effects when they are not found significant.

```
iris.anova4 <- aov(Sepal.Length ~ Species*Sepal.Width*Petal.Length*Petal.Width, data=iris)
summary (iris.anova4)

                                             Df Sum Sq Mean Sq F value Pr(>F)    
Species                                        2  63.21  31.606 342.959 <2e-16 ***
Sepal.Width                                    1  10.95  10.953 118.846 <2e-16 ***
Petal.Length                                   1  14.04  14.038 152.329 <2e-16 ***
Petal.Width                                    1   0.41   0.409   4.438 0.0371 *  
Species:Sepal.Width                            2   0.38   0.192   2.088 0.1282    
Species:Petal.Length                           2   0.54   0.272   2.948 0.0561 .  
Sepal.Width:Petal.Length                       1   0.13   0.127   1.379 0.2425    
Species:Petal.Width                            2   0.23   0.114   1.242 0.2922    
Sepal.Width:Petal.Width                        1   0.20   0.202   2.192 0.1413    
Petal.Length:Petal.Width                       1   0.01   0.012   0.127 0.7221    
Species:Sepal.Width:Petal.Length               2   0.18   0.091   0.991 0.3740    
Species:Sepal.Width:Petal.Width                2   0.03   0.016   0.178 0.8369    
Species:Petal.Length:Petal.Width               2   0.05   0.025   0.273 0.7616    
Sepal.Width:Petal.Length:Petal.Width           1   0.03   0.030   0.325 0.5694    
Species:Sepal.Width:Petal.Length:Petal.Width   2   0.15   0.075   0.819 0.4431    
Residuals                                    126  11.61   0.092                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

After watching at the levels and combination of levels that have a significance effect on Sepal.Length, we can change those that doesn't have an combinatory effect from "*" (interaction) to "+" (simple addition to the model).

```
iris.anova5 <- aov(Sepal.Length ~ Species + Sepal.Width*Petal.Length + Petal.Width, data=iris)
summary (iris.anova5)

                         Df Sum Sq Mean Sq F value Pr(>F)    
Species                    2  63.21  31.606 340.309 <2e-16 ***
Sepal.Width                1  10.95  10.953 117.928 <2e-16 ***
Petal.Length               1  14.04  14.038 151.152 <2e-16 ***
Petal.Width                1   0.41   0.409   4.404 0.0376 *  
Sepal.Width:Petal.Length   1   0.28   0.275   2.966 0.0872 .  
Residuals                143  13.28   0.093                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

We can always perform the Tukey tests to see the pairwaise comparison of the different combinations. However, this only works when the combinations are factors, and not numbers.

```
TukeyHSD(iris.anova3, conf.level = 0.95)

Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = Sepal.Length ~ Species * Treatment, data = iris)

$Species
                      diff       lwr       upr p adj
versicolor-setosa    0.930 0.6853168 1.1746832     0
virginica-setosa     1.582 1.3373168 1.8266832     0
virginica-versicolor 0.652 0.4073168 0.8966832     0

$Treatment
           diff        lwr       upr     p adj
B-A 0.006666667 -0.1600787 0.1734121 0.9371219

$`Species:Treatment`
                            diff         lwr        upr     p adj
versicolor:A-setosa:A      0.968  0.54594478  1.3900552 0.0000000
virginica:A-setosa:A       1.480  1.05794478  1.9020552 0.0000000
setosa:B-setosa:A         -0.036 -0.45805522  0.3860552 0.9998740
versicolor:B-setosa:A      0.856  0.43394478  1.2780552 0.0000005
virginica:B-setosa:A       1.648  1.22594478  2.0700552 0.0000000
virginica:A-versicolor:A   0.512  0.08994478  0.9340552 0.0078939
setosa:B-versicolor:A     -1.004 -1.42605522 -0.5819448 0.0000000
versicolor:B-versicolor:A -0.112 -0.53405522  0.3100552 0.9726630
virginica:B-versicolor:A   0.680  0.25794478  1.1020552 0.0001057
setosa:B-virginica:A      -1.516 -1.93805522 -1.0939448 0.0000000
versicolor:B-virginica:A  -0.624 -1.04605522 -0.2019448 0.0004971
virginica:B-virginica:A    0.168 -0.25405522  0.5900552 0.8595152
versicolor:B-setosa:B      0.892  0.46994478  1.3140552 0.0000001
virginica:B-setosa:B       1.684  1.26194478  2.1060552 0.0000000
virginica:B-versicolor:B   0.792  0.36994478  1.2140552 0.0000036
```

# $\\alpha$-diversity

I will start with a disclaimer. You are not going to find here the most precise and mathematically accurate definition of this or any other diversity metric. If you are looking for something like that, I rather recommend you to go to a guide or vignette on how to use the `vegan()` package, for example. I am going to build a made-up `DataFrame` that serves as an intuitive example to comprehend in a more practical way what the $\\alpha$-diversity is telling us about our sample (microbial community). 

So, let's create our `DataFrame` in which every column will be a microbial genus and every row a different sample. The numerical values can represent relative abundances or absolute numbers (typically sequence reads). Let's simulate the latter. 

```
Lactobacillus <- c(0,20,0,10,15)
Acetobacter <- c(4,8,4,10,3)
Escherichia <- c(2,30,0,0,0)
Salmonella <- c(20,10,0,10,8)
samples <- c("A", "B", "C", "D", "E")

micro <- data.frame(Lactobacillus, Acetobacter, Escherichia, Salmonella,
                    row.names = samples)
micro

  Lactobacillus Acetobacter Escherichia Salmonella
A             0           4           2         20
B            20           8          30         10
C             0           4           0          0
D            10          10           0         10
E            15           3           0          8
```

We are going to calculate a few diversity indexes on that easy table in which every organism is detected in different ratios in each of the samples. Again, I am not going to explain how these indexes work, there are clear explanations online or simply typing `?diversity()` and reading the description. Actually, the function `diversity()` is part of the `vegan()` package, a community ecology package designed by Jari Oksanen. As described in the package documentation, "the functions in the vegan package contain tools for diversity analysis, ordination methods and tools for the analysis of dissimilarities". Let's start calculating some stuff then.
```
Shannon <- diversity(micro, index = "shannon")
Shannon

        A         B         C         D         E 
0.6870920 1.2546261 0.0000000 1.0986123 0.9291688
```
Sample C has a Shannon index of `0.0` because there's only one microorganism present in the sample, but B has more than `1.0` because there are a lot of different microorganisms in the sample. So, the largest the Shannon index is, the higher the microbial diversity in our sample. However, *Shannon* is not the only metric that exists to calculate the diversity of a sample, we can also use other approximations, such as the *Simpson* method. I have always encountered a very high correlation between the two indexes, so as it says in the documentation... "[...] these indices are all very closely related (Hill 1973), and there is no reason to despise one more than others (but if you are a graduate student, don't drag me in, but obey your Professor's orders)". I may add that the Simpson index fluctuates between 0.0 and 1.0, so it may be more intuitive to understand the result and give it a biological meaning.  
```
Simpson <- diversity(micro, index = "simpson")
Simpson

        A         B         C         D         E 
0.3786982 0.6833910 0.0000000 0.6666667 0.5591716
```
A measure that is also important to evaluate when studying a microbial community is the *eveness*. Given a diversity in the sample, that diversity can be distributed in a more equal or unequal way. We use the *Pielou* index to calculate that. A calculated value of *Pielou's evenness* ranges from 0 (no evenness) to 1 (complete evenness). 
```
Pielou <- Shannon/log(specnumber(micro))
Pielou

        A         B         C         D         E 
0.6254181 0.9050214       NaN 1.0000000 0.8457659
```
As we can see, the eveness of the sample depends on the $\\alpha$-diversity previously calculated. The evennes of sample D is `1.0` (max) because all the microorganisms present are equally distributed, but A is `~0.6` because the distribution of the microorganisms is very unequal. Since in sample C there is only one microorganism appearing, the diversity index was `0.0` and therefore, the calculation does not make any sense. 

# $\\beta$-diversity

This diversity metric is going to describe the dissimilarity of the samples regarding their microbial composition. The first thing we need to do to execute the functions to calculate these $\\beta$-diversity indexes is to add a factor as a `column` of our `DataFrame`, so that we can use it as a factor. We are going to generate a new `DataFrame`with the same microorganisms but a different distribution and since these indexes work better with a big set of data, I am going to repeat the samples three times to build some statistical power. Let's imagine that we have a few samples inoculated with a starter made of *Lactobacillus* and *Acetobacter* and others that are not inoculated.

```
Lactobacillus <- rep(c(0,20,0,10,15,2),3)
Acetobacter <- rep(c(0,30,4,14,28,3),3)
Escherichia <- rep(c(21,0,10,2,0,20),3)
Salmonella <- rep(c(20,1,40,0,1,25),3)
Starter <- rep(c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE),3)
Sample <- c("A", "B", "C", "D", "E", "F",
            "G", "H", "I", "J", "K", "L",
            "M", "N", "O", "P", "Q", "R")

micro2 <- data.frame(Lactobacillus, Acetobacter, Escherichia, Salmonella, Starter, Sample,
                    row.names = Sample)
head(micro2)

  Lactobacillus Acetobacter Escherichia Salmonella Starter Sample
A             0           0          21         20   FALSE      A
B            20          30           0          1    TRUE      B
C             0           4          10         40   FALSE      C
D            10          14           2          0    TRUE      D
E            15          28           0          1    TRUE      E
F             2           3          20         25   FALSE      F
```
The first calculation we are going to do is a *PERMANOVA* (Permutational Multivariate Analysis of Variance) to evaluate how a factor (in this case a different sample, which putatively in your case have different 






