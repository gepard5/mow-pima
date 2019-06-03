PimaData <- read.csv(file="./pima_dataset.csv", header=TRUE, sep=",")
g_columns <- ncol(PimaData) - 1
g_maxIter <- 20
g_popSize <- 20
g_runNumber <- 20
g_elitism <- 0.2
g_crossoverChance <- 0.8
g_mutationChance <- 0.2
g_tree_levels <- 4

source("./functions.r")

library(GA)

test_attribute_tree <- function(m_maxIter, m_popSize, m_runNumber, m_elitism, m_crossoverChance, m_mutationChance, m_tree_levels) {
    tree_len <- 2 ** m_tree_levels - 1
    GA <- ga(type = "real-valued",
             fitness = fitness_function, lower = rep(1, tree_len),
             upper = rep(g_columns, tree_len), popSize = m_popSize,
             maxiter=m_maxIter, run=m_runNumber, parallel=TRUE,
             elitism = base::max(1, round(m_popSize*m_elitism) ),
             seed=1234,
             pcrossover = m_crossoverChance, pmutation = m_mutationChance)
    summary(GA)
}


test_attr_thresh_tree <- function(m_maxIter, m_popSize, m_runNumber, m_elitism, m_crossoverChance, m_mutationChance, m_tree_levels) {
    tree_len <- (2 ** m_tree_levels - 1) * 2
    m_upper <- rep(1, tree_len)
    m_lower <- rep(1, tree_len)
    for( i in 1:(2 ** m_tree_levels - 1))
    {
        m_upper[[2*i -1]] <- g_columns
        m_upper[[2*i]] <- 1
        m_lower[[2*i - 1]] <- 1
        m_lower[[2*i ]] <- 0
    }
    GA <- ga(type = "real-valued",
             fitness = fitness_thresh_function, lower = m_lower,
             upper = m_upper, popSize = m_popSize,
             maxiter=m_maxIter, run=m_runNumber, parallel=TRUE,
             elitism = base::max(1, round(m_popSize*m_elitism) ),
             seed=1234,
             pcrossover = m_crossoverChance, pmutation = m_mutationChance)
    summary(GA)
}



print("Test attribute tree")
test_attribute_tree(g_maxIter, g_popSize, g_runNumber, g_elitism, g_crossoverChance, g_mutationChance, g_tree_levels)
print("Test attribute threshold tree")
test_attr_thresh_tree(g_maxIter, g_popSize, g_runNumber, g_elitism, g_crossoverChance, g_mutationChance, g_tree_levels)
