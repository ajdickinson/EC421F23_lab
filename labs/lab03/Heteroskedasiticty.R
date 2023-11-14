library(pacman)
p_load(tidyverse, fixest)
theme_set(hrbrthemes::theme_ipsum())


### Heteroskedasticity
set.seed(12345)
n=1000
data = tibble(
  x=rnorm(n, 2, 1),
  group=c(rep(0,n/2), rep(1,n/2)),
  error = rnorm(n, 0, 1),
  y_homo =  error,
  y_hetero1= error*x,
  y_hetero2= error*abs(x-2),
  y_hetero3= error*(group+1)*3,  
) 
#write_csv(data, file="Data_Heteroskedasticity.csv")

ggplot(data=data, aes(x=x, y=y_homo)) + geom_point()
ggplot(data=data, aes(x=x, y=y_hetero1)) + geom_point()
ggplot(data=data, aes(x=x, y=y_hetero2)) + geom_point()
ggplot(data=data, aes(x=x, y=y_hetero3, color=factor(group))) + geom_point(alpha=0.5)



####### GQ tests #######

### Step 1. Sort data by X
data = data %>%
  arrange(x)

## Step 2. Split the sample into thirds
  # First third index
  first_third = nrow(data)/3
  # Third third index
  third_third = floor((nrow(data)/3) * 2) + 2
  # Number of rows
  n = nrow(data)

# Split data
data_1 = data[1:first_third, ]
data_2 = data[third_third:n, ]


### Step 3. Run regressions
fit1 = lm(y_hetero2 ~ x, data_1 )  %>% summary()
rss1 = sum(resid(fit1)^2)

fit2 = lm(y_hetero2 ~ x, data_2 )  %>% summary()
rss2 = sum(resid(fit2)^2)

## Step 4. Conduct hypothesis testing (RSS2/RSS1)
## Null hypothesis: Homoskedasticity. Alt hypotehsis: Heteroskedasticity
## Degrees of freedom = # of observation (in restricted data) - # of parameters: n-k
tstat = rss2/rss1
pf(q = tstat, df1 = 333-1, df2 = 333-1, lower.tail = F)



####### White's test #######

## Step 1. Regress Y on X
fit = lm(y_hetero2 ~ x, data)

## Step 2. Find squared residuals
data = data %>% mutate(resid = resid(fit)^2)

### Step 3. Regress:
## residual squared w/ ALL X's, X^2, and cross product between Xs
fit = lm(resid ~ x + I(x^2), data) %>% summary() 

### Step 4. Find p-values. Conduct hypothesis testing (R^2 * N)
### Degrees of Freedom = # of variables in step 3.
## Null hypothesis: Homoskedasticity. Alt hypotehsis: Heteroskedasticity
teststat = fit$r.squared * n
pchisq(q = teststat, df = 2, lower.tail = F)



