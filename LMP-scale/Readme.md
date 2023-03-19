* To read .RData files use the **readRDS()** function of R. For example to load the *squared_expectations.RData* file, use the code: **readRDS("https://github.com/saransh-g-1/Statistical-Inference/blob/LMP-rohan/LMP-scale/squared_expectations.RData")**

* The *squared_expectations.RData* file contains the simulated values of $\mathbb{E}[Z_{(i,N)}^2]$ for values of $N$ from $2$ to $1000$

* The *results.RData* file contains the obtained values of the below test statistics for different values of $\theta$ (scale parameter) and different values of $m$ and $n$ where $m$ is the number of $Y$ observations and $n$ is the number of X observations:
  + Capon's test statistic
  + Klotz test statistic
  + Mood's test statistic
  + Savage test statistic
 
* The model setup under consideration is $$X_1,\cdots,X_n \sim^{\text{iid}} F\left(\frac{x}{\theta}\right) \text{   and   } Y_1,\cdots,Y_m \sim^{\text{iid}} F(x)$$
