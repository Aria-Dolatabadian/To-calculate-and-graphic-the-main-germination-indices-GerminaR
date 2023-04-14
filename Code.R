if (!require("remotes"))
  install.packages("remotes")
remotes::install_github("Flavjack/GerminaR")


library(GerminaR)
library(dplyr)
library(knitr)
library(ggplot2)

fb <- read.csv('germ.csv') %>%
      mutate(across(c(nacl, temp, rep), as.factor))


fb %>% 
   head(10) %>% 
   kable(caption = "Prosopis dataset")


#Calculate the germination indices and perform the ANOVA and the mean comparison tests.

# germination analysis (ten variables)

gsm <- ger_summary(SeedN = "seeds"
                   , evalName = "D"
                   , data = fb
                   )

# Prosopis data set processed

gsm %>% 
  head(10) %>% 
  mutate(across(where(is.numeric), ~round(., 2))) %>% 
  kable(caption = "Function ger_summary performe ten germination indices")

write.csv(gsm, file = "Function ger_summary performe ten germination indices.csv", row.names = FALSE)

#Punctual analysis of germination
#Germination percentage



## Germination Percentage (GRP)

# analysis of variance

av <- aov(grp ~ nacl*temp + rep, data = gsm)

# mean comparison test

mc_grp <- ger_testcomp(aov = av
                       , comp = c("temp", "nacl")
                       , type = "snk"
                       )

# data result

mc_grp$table %>% 
   kable(caption = "Germination percentage mean comparision")


write.csv(mc_grp$table, file = "Germination percentage mean comparision.csv", row.names = FALSE)

# bar graphics for germination percentage

grp <- mc_grp$table %>% 
   fplot(data = .
       , type = "bar"
       , x = "temp"
       , y = "grp"
       , group = "nacl"
       , ylimits = c(0, 120, 30)
       , ylab = "Germination ('%')"
       , xlab = "Temperature"
       , glab = "NaCl (MPa)"
       , error = "ste"
       , sig = "sig"
       , color = F
       )

grp


#Mean germination time


## Mean Germination Time (MGT)

# analysis of variance

av <- aov(mgt ~ nacl*temp + rep, data = gsm)

# mean comparison test

mc_mgt <- ger_testcomp(aov = av
                       , comp = c("temp", "nacl")
                       , type = "snk")

# data result

mc_mgt$table %>% 
   kable(caption = "Mean germination time comparison")



write.csv(mc_mgt$table, file = "Mean germination time comparison.csv", row.names = FALSE)

# bar graphics for mean germination time

mgt <- mc_mgt$table %>% 
   fplot(data = .
       , type = "bar" 
       , x = "temp"
       , y = "mgt"
       , group = "nacl"
       , ylimits = c(0,10, 1)
       , ylab = "Mean germination time (days)"
       , xlab = "Temperature"
       , glab = "NaCl (MPa)"
       , sig = "sig"
       , error = "ste"
       , color = T
       )

mgt


#Cumulative analysis of germination

#In time analysis for NaCl



# data frame with percentage or relative germination in time by NaCl

git <- ger_intime(Factor = "nacl"
                  , SeedN = "seeds"
                  , evalName = "D"
                  , method = "percentage"
                  , data = fb
                  )

# data result

git %>% 
   head(10) %>% 
   kable(caption = "Cumulative germination by nacl factor")



write.csv(git, file = "Cumulative germination by nacl factor.csv", row.names = FALSE)


# graphic germination in time by NaCl

nacl <- git %>% 
   fplot(data = .
        , type = "line"
        , x = "evaluation"
        , y = "mean"
        , group = "nacl"
        , ylimits = c(0, 110, 10)
        , ylab = "Germination ('%')"
        , xlab = "Day"
        , glab = "NaCl (MPa)"
        , color = T
        , error = "ste"
        )
nacl


#In time analysis for temperature

# data frame with percentage or relative germination in time by temperature

git <- ger_intime(Factor = "temp"
                  , SeedN = "seeds"
                  , evalName = "D"
                  , method = "percentage"
                  , data = fb) 

# data result

git %>% 
   head(10) %>% 
   kable(caption = "Cumulative germination by temperature factor")

write.csv(git, file = "Cumulative germination by temperature factor.csv", row.names = FALSE)

# graphic germination in time by temperature

temp <- git %>% 
   fplot(data = .
        , type = "line"
        , x = "evaluation"
        , y = "mean"
        , group = "temp"
        , ylimits = c(0, 110, 10)
        , ylab = "Germination ('%')"
        , xlab = "Day"
        , glab = "Temperature"
        , color = F
        ) 
temp


library(ggplot2)

git <- ger_intime(Factor = "temp"
                  , SeedN = "seeds"
                  , evalName = "D"
                  , method = "percentage"
                  , data = fb
                  ) 

ggplot <- git %>% 
   fplot(data = .
        , type = "line"
        , x = "evaluation"
        , y = "mean"
        , group = "temp"
        , ylimits = c(0, 110, 10)
        , ylab = "Germination ('%')"
        , xlab = "Day"
        , glab = "Temperature"
        , color = T
        ) +
  scale_x_continuous(n.breaks = 10, limits = c(0, 11)) 

ggplot



