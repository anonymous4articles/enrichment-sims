library(tidyverse)
sample.vec <- function(x, ...) x[sample(length(x), ...)]


sims <- 100
time <- 40
output <- list(sims)

for(sim in 1:sims){


# variance of the satisfaction update distribution for each task
avar <- .1
bvar <- .2
cvar <- .3
dvar <- .4
evar <- .5
fvar <- .6
gvar <- .7
hvar <- .8
ivar <- .9
jvar <- 1

# intertia for each task

aauto <- 0.5
bauto <- 0.5
cauto <- 0.5
dauto <- 0.5
eauto <- 0.5
fauto <- 0.5
gauto <- 0.5
hauto <- 0.5
iauto <- 0.5
jauto <- 0.5

# b1, strength of the effect of the update on future satisfaction
b1 <- 0.8

# initial conditions for satisfaction
# random: runif(10, min = -0.5, 0.5)

allsat <- seq(from = 0, to = 1, by = 0.1)
asat <- allsat[1]
bsat <- allsat[2]
csat <- allsat[3]
dsat <- allsat[4]
esat <- allsat[5]
fsat <- allsat[6]
gsat <- allsat[7]
hsat <- allsat[8]
isat <- allsat[9]
jsat <- allsat[10]



# container
# 10 satisfaction scores
# 1 vector for the choice
# 1 vector for the update 
# vector for time
# = 13
store <- matrix(, ncol = 23, nrow = time)
count <- 0

for(i in 1:time){
  count <- count + 1
  store[count, 23] <- sim
  
  
  
  
  

# first time point --------------------------------------------------------

  
  if(i == 1){
    
    # time
    store[count, 1] <- i
    # satisfaction for task A
    store[count, 2] <- asat
    # satisfaction for task B
    store[count, 3] <- bsat
    # satisfaction for task C
    store[count, 4] <- csat
    # satisfaction for task D
    store[count, 5] <- dsat
    # satisfaction for task E
    store[count, 6] <- esat
    # satisfaction for task F
    store[count, 7] <- fsat
    # satisfaction for task G
    store[count, 8] <- gsat
    # satisfaction for task H
    store[count, 9] <- hsat
    # satisfaction for task I
    store[count, 10] <- isat
    # satisfaction for task J
    store[count, 11] <- jsat
    
    

    # likelihood for task A
    likea <- 1 / 1 + exp(store[count, 2])
    # likelihood for task B
    likeb <- 1 / 1 + exp(store[count, 3])
    # likelihood for task C
    likec <- 1 / 1 + exp(store[count, 4])
    # likelihood for task D
    liked <- 1 / 1 + exp(store[count, 5])
    # likelihood for task E
    likee <- 1 / 1 + exp(store[count, 6])
    # likelihood for task F
    likef <- 1 / 1 + exp(store[count, 7])
    # likelihood for task G
    likeg <- 1 / 1 + exp(store[count, 8])
    # likelihood for task H
    likeh <- 1 / 1 + exp(store[count, 9])
    # likelihood for task I
    likei <- 1 / 1 + exp(store[count, 10])
    # likelihood for task J
    likej <- 1 / 1 + exp(store[count, 11])
    
    
    
    # Select task with greatest likelihood
    best_tasks <- data.frame(
      "value" = c(likea, likeb, likec, liked, likee,
                  likef, likeg, likeh, likei, likej),
      "task" = c(1:10)
    ) %>% 
      mutate(rank = dense_rank(desc(value))) %>% 
      filter(rank == 1) %>% 
      pull(task)
    
    choice <- sample.vec(best_tasks, 1)
    
    store[count, 12] <- choice
    
    # Experience the task
    # and receive a satisfaction update
    sat_update <- c(rep(0, 10))
    if(choice == 1){
      sat_update[1] <- rnorm(1, 0, avar)
    }else if (choice == 2){
      sat_update[2] <- rnorm(1, 0, bvar)
    }else if (choice == 3){
      sat_update[3] <- rnorm(1, 0, cvar)
    }else if (choice == 4){
      sat_update[4] <- rnorm(1, 0, dvar)
    }else if (choice == 5){
      sat_update[5] <- rnorm(1, 0, evar)
    }else if (choice == 6){
      sat_update[6] <- rnorm(1, 0, fvar)
    }else if (choice == 7){
      sat_update[7] <- rnorm(1, 0, gvar)
    }else if (choice == 8){
      sat_update[8] <- rnorm(1, 0, hvar)
    }else if (choice == 9){
      sat_update[9] <- rnorm(1, 0, ivar)
    }else if (choice == 10){
      sat_update[10] <- rnorm(1, 0, jvar)
    }
    
    # update for A
    store[count, 13] <- sat_update[1]
    # update for B -- which could be zero if she didn't sample B
    store[count, 14] <- sat_update[2]
    store[count, 15] <- sat_update[3]
    store[count, 16] <- sat_update[4]
    store[count, 17] <- sat_update[5]
    store[count, 18] <- sat_update[6]
    store[count, 19] <- sat_update[7]
    store[count, 20] <- sat_update[8]
    store[count, 21] <- sat_update[9]
    store[count, 22] <- sat_update[10]
    
    
    
    
    
    
# other time points -------------------------------------------------------
    
    
  }else{
  
    

    # time
    store[count, 1] <- i
    # satisfaction for task A
    # function of
    # a at the prior period and the update (if A occured)
    store[count, 2] <- aauto*store[count - 1, 2] + b1*store[count - 1, 13]
    # satisfaction for task B
    store[count, 3] <- bauto*store[count - 1, 3] + b1*store[count - 1, 14]
    # satisfaction for task C
    store[count, 4] <- cauto*store[count - 1, 4] + b1*store[count - 1, 15]
    # satisfaction for task D
    store[count, 5] <- dauto*store[count - 1, 5] + b1*store[count - 1, 16]
    # satisfaction for task E
    store[count, 6] <- eauto*store[count - 1, 6] + b1*store[count - 1, 17]
    # satisfaction for task F
    store[count, 7] <- fauto*store[count - 1, 7] + b1*store[count - 1, 18]
    # satisfaction for task G
    store[count, 8] <- gauto*store[count - 1, 8] + b1*store[count - 1, 19]
    # satisfaction for task H
    store[count, 9] <- hauto*store[count - 1, 9] + b1*store[count - 1, 20]
    # satisfaction for task I
    store[count, 10] <- iauto*store[count - 1, 10] + b1*store[count - 1, 21]
    # satisfaction for task J
    store[count, 11] <- jauto*store[count - 1, 11] + b1*store[count - 1, 22]
    
    
    
    
    # likelihood for task A
    likea <- 1 / 1 + exp(store[count, 2])
    # likelihood for task B
    likeb <- 1 / 1 + exp(store[count, 3])
    # likelihood for task C
    likec <- 1 / 1 + exp(store[count, 4])
    # likelihood for task D
    liked <- 1 / 1 + exp(store[count, 5])
    # likelihood for task E
    likee <- 1 / 1 + exp(store[count, 6])
    # likelihood for task F
    likef <- 1 / 1 + exp(store[count, 7])
    # likelihood for task G
    likeg <- 1 / 1 + exp(store[count, 8])
    # likelihood for task H
    likeh <- 1 / 1 + exp(store[count, 9])
    # likelihood for task I
    likei <- 1 / 1 + exp(store[count, 10])
    # likelihood for task J
    likej <- 1 / 1 + exp(store[count, 11])
    
    
    
    # Select task with greatest likelihood
    best_tasks <- data.frame(
      "value" = c(likea, likeb, likec, liked, likee,
                  likef, likeg, likeh, likei, likej),
      "task" = c(1:10)
    ) %>% 
      mutate(rank = dense_rank(desc(value))) %>% 
      filter(rank == 1) %>% 
      pull(task)
    
    choice <- sample(best_tasks, 1)
    
    store[count, 12] <- choice
    
    # Experience the task
    # and receive a satisfaction update
    sat_update <- c(rep(0, 10))
    if(choice == 1){
      sat_update[1] <- rnorm(1, 0, avar)
    }else if (choice == 2){
      sat_update[2] <- rnorm(1, 0, bvar)
    }else if (choice == 3){
      sat_update[3] <- rnorm(1, 0, cvar)
    }else if (choice == 4){
      sat_update[4] <- rnorm(1, 0, dvar)
    }else if (choice == 5){
      sat_update[5] <- rnorm(1, 0, evar)
    }else if (choice == 6){
      sat_update[6] <- rnorm(1, 0, fvar)
    }else if (choice == 7){
      sat_update[7] <- rnorm(1, 0, gvar)
    }else if (choice == 8){
      sat_update[8] <- rnorm(1, 0, hvar)
    }else if (choice == 9){
      sat_update[9] <- rnorm(1, 0, ivar)
    }else if (choice == 10){
      sat_update[10] <- rnorm(1, 0, jvar)
    }
    
    # update for A
    store[count, 13] <- sat_update[1]
    # update for B -- which could be zero if she didn't sample B
    store[count, 14] <- sat_update[2]
    store[count, 15] <- sat_update[3]
    store[count, 16] <- sat_update[4]
    store[count, 17] <- sat_update[5]
    store[count, 18] <- sat_update[6]
    store[count, 19] <- sat_update[7]
    store[count, 20] <- sat_update[8]
    store[count, 21] <- sat_update[9]
    store[count, 22] <- sat_update[10]
    
    
  }
  
  
  
}



output[[sim]] <- store


}

df <- do.call(rbind, output)
df <- data.frame(df)
names(df) <- c('time', 'asat', 'bsat', 'csat', 'dsat', 'esat', 'fsat', 'gsat', 'hsat', 'isat', 'jsat',
               'choice', 'aupdate', 'bupdate', 'cupdate', 'dupdate', 'eupdate', 'fupdate', 'gupdate', 'hupdate', 'iupdate', 'jupdate',
               'simulation')

satdf <- df %>%
  select(time, simulation,
         choice,
         contains("sat")) %>% 
  pivot_longer(cols = contains('sat'),
               names_to = "task",
               values_to = "sat") %>% 
  mutate(task = str_replace(task, "sat", ""))

updf <- df %>% 
  select(time, simulation,
         contains("update")) %>% 
  pivot_longer(cols = contains('update'),
               names_to = "task",
               values_to = "update") %>% 
  mutate(task = str_replace(task, 'update', ''))

dd <- left_join(satdf, updf)

write.csv(dd, file = "data/modeloutput.csv", row.names = F)
