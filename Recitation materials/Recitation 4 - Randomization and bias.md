---
title: "Recitation 4 Randomization and Selection Bias"
author: "Seamus Wagner"
date: "September 17, 2021"
output: pdf_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.width=8, fig.height=5, fig.align = "center", echo=FALSE, warning=FALSE, message=FALSE)
```

#Selection Bias
For the first section of recitation, I will go over selection bias and lab 2 question 1 as there was a bit of confusion between what you were supposed to take from it versus what most of you did takeaway. Note, I did alter the grades on 1.7 since the question states to mention your expectations and why, not whether they were grounded in what we hoped or not.  

The main culprit for this question was 1.7, where you were asked to interpret what happened in 1.6 as you increased the number of observations and observed the bias. Nearly everyone mentioned the law of large numbers as why the bias was going to 0. What we need to remember though, is that the ATE was 0 for this case. If the ATE were positive or negative, the law of large numbers should not trend toward 0 if the sample is biased, as the underlying distribution is no longer normal in this sense. If we observe selection bias, we should see steady estimates as the number of observations increases. 

Below is the example I put on many of your responses for how to check this (I forgot to add the -.1 and - (-.1) for your examples, so they probably didn't work). The first few simulations have too few observations to trend toward what we would expect the bias to be. It is important to remember that the error is the estimate - ATE. The ATE for the question at the time was 0. So the error = estimate. 
```{r}
library(data.table)
library(gridExtra)
library(dplyr)
library(ggplot2)
sample.int(.Machine$integer.max, 1) 
```

```{r}
rm(list=ls())
set.seed(961752487)
sim <- function(n_obs) {
  sim_data <- data.table(
  Y1 = rnorm(n_obs, .1),
  Y0 = rnorm(n_obs))
  sim_data[, p := plogis(Y1 - Y0)]
  sim_data[, D := rbinom(.N, 1, p)]
  sim_data[, Y := D * Y1 + (1 - D) * Y0]
  #sim_data[, diff := Y1 - Y0]
  #sim_data[, mean(Y1-Y0)]
  sim_data[, sum(D * Y) / sum(D) - sum((1 - D) * Y) / sum(1 - D)] - .1
}
sim(1e3)
sim(1e4)
sim(1e5)
sim(1e6)
sim(1e7)
```
See here that once we get to a larger number of observations, but the bias seems pretty consistent here, and we are getting underestimates. Also note that we subtract out the known ATE at the end of our difference in means. So the NDIM minus the known difference in means produces underestimates when selection bias is included. Now let's try this with a negative ATE. 

```{r}
sim2 <- function(n_obs) {
  sim_data <- data.table(
  Y1 = rnorm(n_obs, -.1),
  Y0 = rnorm(n_obs))
  sim_data[, p := plogis(Y1 - Y0)]
  sim_data[, D := rbinom(.N, 1, p)]
  sim_data[, Y := D * Y1 + (1 - D) * Y0]
  sim_data[, sum(D * Y) / sum(D) - sum((1 - D) * Y) / sum(1 - D)] - (-.1)
}

sim2(1e3)
sim2(1e4)
sim2(1e5)
sim2(1e6)
sim2(1e7)

```
Here we see pretty consistent bias and overestimates this time. Note that we also accounted for the negative ATE at the end of our NDIM code. These estimates should be 0 if no bias were present given that we account for the ATE not being zero in our NDIM calculation. 


What this leaves us with is the knowledge that with enough number of observations, we can see that bias is influencing our estimations, and that where the weka law of large numbers comes in, it is that a decent number of observations is needed to see that the estimates do not trend toward zero when bias is present. We observe bias because the assignment to treatment is no longer independent of the values of Y1 and Y0. With higher Y1 values will be disproportionately assigned to treatment compared to lower Y1 values. Further, higher Y0 values disproportionately assigned to control compared to lower Y0 values. 

#Randomization

Moving into the lab specific stuff for lab three. This should be a shorter lab and much of the code comes from the demos and slides. We will start with complete versus simple randomization then move onto p values. 

Simple randomization takes forever to run as the number of observations get remotely interesting. We start with resetting a seed (not necessary, I just do it sometimes). Then we create a matrix of complete randomization for all combinations of 0 and 1 for eight participants. We then create a subset matrix of only those cases where exactly half of the observations are treated. One way to check that it worked properly is to check the dimensions of the matrix next to the R command for combinations. 
```{r}
#Seed not necessary for this chunk but I am resetting it for the second part of
# this recitation
set.seed(169372705) 

#Matrix of complete randomization for eight participants
randomizations <- data.matrix(CJ(
D01 = 0:1, D02 = 0:1, D03 = 0:1, D04 = 0:1,
D05 = 0:1, D06 = 0:1, D07 = 0:1, D08 = 0:1))

#Subset matrix of only cases where exactly half are treated.
complete_randomizations <- randomizations[
rowSums(randomizations) == 4, ]

#Checking to make sure it works
dim(complete_randomizations)

#Continued checking to make sure it is correct
dim(cbind(combn(8, 4)))

```

```{r}
#Brief example here of how long these things can take
system.time(
  randomizations <- data.matrix(CJ(
    D01 = 0:1, D02 = 0:1, D03 = 0:1, D04 = 0:1,
    D05 = 0:1, D06 = 0:1, D07 = 0:1, D08 = 0:1)))
    
system.time(
  randomizations1 <- data.matrix(CJ(
    D01 = 0:1, D02 = 0:1, D03 = 0:1, D04 = 0:1,
    D05 = 0:1, D06 = 0:1, D07 = 0:1, D08 = 0:1,
    D0.1 = 0:1, D0.2 = 0:1, D0.3 = 0:1, D0.4 = 0:1,
    D0.5 = 0:1, D0.6 = 0:1, D0.7 = 0:1, D0.8 = 0:1,
    D0.1. = 0:1, D0.2. = 0:1, D0.3. = 0:1, D0.4. = 0:1,
    D0.5. = 0:1, D0.6. = 0:1, D0.7. = 0:1, D0.8. = 0:1
    ))
)

#system.time(
#  randomizations2 <- data.matrix(CJ(
#    D01 = 0:1, D02 = 0:1, D03 = 0:1, D04 = 0:1,
#    D05 = 0:1, D06 = 0:1, D07 = 0:1, D08 = 0:1,
#    D0.1 = 0:1, D0.2 = 0:1, D0.3 = 0:1, D0.4 = 0:1,
#    D0.5 = 0:1, D0.6 = 0:1, D0.7 = 0:1, D0.8 = 0:1,
#    D0.1. = 0:1, D0.2. = 0:1, D0.3. = 0:1, D0.4. = 0:1,
#    D0.5. = 0:1, D0.6. = 0:1, D0.7. = 0:1, D0.8. = 0:1,
#     D01. = 0:1, D02. = 0:1, D03. = 0:1, D04. = 0:1,
#    D05. = 0:1, D06. = 0:1, D07. = 0:1, D08. = 0:1
#    ))
#)
rm(list=ls())
#Why use gc?
gc()
#?gc
```
As we mentioned in class, these get excessively large quickly and adding another eight participants exceeds the maximum integer size for number of rows that my laptop can take. 

The options we tend to take is sampling from all possible randomizations by either taking exactly half or using some probability selection. If we go with simple randomization (exactly half), the sample function is best for matricies.

```{r}
randomizations <- data.matrix(CJ(
D01 = 0:1, D02 = 0:1, D03 = 0:1, D04 = 0:1,
D05 = 0:1, D06 = 0:1, D07 = 0:1, D08 = 0:1))
dim(randomizations)

# There are other, equally if not more efficient ways to do these as well. Think
# about what would make this more efficient in a function or loop/apply sequence.

#One way of randomly selecting half of the rows in matrix without replacement
subset_mat <- randomizations[sample(nrow(randomizations),size=128,replace=FALSE),]
dim(subset_mat)

#Another way
subset_mat1 <- as.matrix(data.frame(randomizations) %>% sample_n(128))

#Yet another way, but doesn't like nested class switching
rand1 <- data.table(randomizations)
subset_mat2 <- as.matrix(rand1[, .SD[sample(x = .N, size = 128)]])
dim(subset_mat2)

#Make sure to read whether replacement is T or F by default in the methods you choose. 
```

For the p-values, t-tests, at standard errors and such, take a close look at the code from William's demo. This is one test of the function writing we have been practicing, as you will start nesting functions you wrote within others and gain a grasp on what each function does. I am happy to go over these with you all. There are also canned functions for most of these, but test them against your written ones to illustrate what William went over on Friday to see how they may differ and why canned functions are something to make sure you fully understand before using them and then defending their use in your own research. 

I am happy to go over any code people have for this question or do some live coding with suggestions from the group. 

I am going to bring in some data using some of the functions I am not sharing as recitation but to go over what you sohuld be getting and how to interpret them. 
```{r}
setwd("C:/Users/spw51/OneDrive/Desktop")
dt <- read.csv("sim_p_vals.csv")
dt <- data.table(dt)

dt[, mean(dt$classical_p < .05)]
dt[, mean(dt$randomization_p < .05)]

fig1 <- ggplot(dt, aes(x = classical_p)) +
  geom_histogram() +
  xlab("Classical p values") +
  ylab("Count") +
  ggtitle("Classical p values") +
  theme_bw() 

fig2 <-  ggplot(dt, aes(x = randomization_p)) +
  geom_histogram() +
  xlab("Randomization p values") +
  ylab("Count") +
  ggtitle("Randomization p values") +
  theme_bw() 

grid.arrange(fig1,fig2, ncol = 2)

```

```{r}
rm(list=ls())
setwd("C:/Users/spw51/OneDrive/Desktop")
dt <- read.csv("sim_p_vals1.csv")
dt <- data.table(dt)

dt[, mean(dt$classical_p < .05)]
dt[, mean(dt$randomization_p < .05)]

fig1 <- ggplot(dt, aes(x = classical_p)) +
  geom_histogram() +
  xlab("Classical p values") +
  ylab("Count") +
  ggtitle("Classical p values") +
  theme_bw() 

fig2 <- ggplot(dt, aes(x = randomization_p)) +
  geom_histogram() +
  xlab("Randomization p values") +
  ylab("Count") +
  ggtitle("Randomization p values") +
  theme_bw() 
grid.arrange(fig1,fig2, ncol = 2)

```
