g_min_precision = 0.75
g_penalty = 0.1


find.treshold.function <- function(pd.attributes, pd.classes){
  pd.attr.class <- cbind(pd.attributes, pd.classes)

  if (is.null(pd.attr.class)){
    pd.final.treshold <- 0

  } else if (sum(pd.attr.class[,'pd.classes']) %in% c(0, length(pd.attr.class[, 'pd.classes']))){
    pd.final.treshold <- min(pd.attr.class[, 'pd.attributes'])

  } else {
    pd.attr.class.sorted <- pd.attr.class[order(pd.attr.class[,'pd.attributes']),]

    pd.test.tresholds <- c()
    pd.amount.of.attr <- length(pd.attr.class.sorted[, 'pd.attributes'])

    for (i in 1:(pd.amount.of.attr-1)){
      pd.test.tresholds[i] = (pd.attr.class.sorted[i,'pd.attributes']+pd.attr.class.sorted[i+1,'pd.attributes'])/2
    }
    pd.treshold.entropy <- rbind()

    for (treshold in pd.test.tresholds){
      pd.low <- sum(pd.attr.class.sorted[,'pd.attributes']<=treshold)
      pd.up <- sum(pd.attr.class.sorted[,'pd.attributes']>treshold)
      pd.neg.low <- sum(pd.attr.class.sorted[,'pd.attributes']<=treshold & pd.attr.class.sorted[,'pd.classes']==0)
      pd.pos.low <- sum(pd.attr.class.sorted[,'pd.attributes']<=treshold & pd.attr.class.sorted[,'pd.classes']==1)
      pd.neg.up <- sum(pd.attr.class.sorted[,'pd.attributes']>treshold & pd.attr.class.sorted[,'pd.classes']==0)
      pd.pos.up <- sum(pd.attr.class.sorted[,'pd.attributes']>treshold & pd.attr.class.sorted[,'pd.classes']==1)

      if (pd.neg.low == 0) {
        pd.entropy.low.neg <- 0
      } else {
        pd.entropy.low.neg <- -(pd.neg.low/pd.low)*log2(pd.neg.low/pd.low)
      }

      if (pd.pos.low == 0) {
        pd.entropy.low.pos <- 0
      } else {
        pd.entropy.low.pos <- -(pd.pos.low/pd.low)*log2(pd.pos.low/pd.low)
      }

      if (pd.neg.up == 0) {
        pd.entropy.up.neg <- 0
      } else {
        pd.entropy.up.neg <- -(pd.neg.up/pd.up)*log2(pd.neg.up/pd.up)
      }

      if (pd.pos.up == 0) {
        pd.entropy.up.pos <- 0
      } else {
        pd.entropy.up.pos <- -(pd.pos.up/pd.up)*log2(pd.pos.up/pd.up)
      }

      pd.entropy.low <- pd.entropy.low.neg + pd.entropy.low.pos
      pd.entropy.up <- pd.entropy.up.neg + pd.entropy.up.pos
      pd.entropy <- (((pd.low/pd.amount.of.attr)*pd.entropy.low) + ((pd.up/pd.amount.of.attr)*pd.entropy.up))

      pd.treshold.entropy <- rbind(pd.treshold.entropy, c(treshold, pd.entropy))
    }

    pd.final.treshold <- pd.treshold.entropy[which.min(pd.treshold.entropy[,2]),1]
  }

  return(pd.final.treshold)
}

prepare_leafs <- function(data, a, b, depth, index, result_list) {
    n <- length(a)
    head_length <- 2 ** depth
    first_part <- head(a, head_length)
    last_part <- tail(a, -1*head_length)
    first_values <- head(b, head_length)
    last_values <- tail(b, -1*head_length)
    thresh_value <- first_values[[index]]
    cn <- colnames(data)[[first_part[[index]]]]
    left_set <- data[data[, cn] >= thresh_value, ]
    right_set <- data[data[, cn] < thresh_value, ]
    if( length(last_part) == 0 ) {
        right_positive <- nrow(right_set[right_set$Class == 1, ])
        left_positive <- nrow(left_set[left_set$Class == 1, ])
        right_negative <- nrow(right_set[right_set$Class == 0, ])
        left_negative <- nrow(left_set[left_set$Class == 0, ])
        right_index <- 2*index
        left_index <- 2*index - 1
        result_list[[right_index]] <- result_list[[right_index]] + right_positive - right_negative
        result_list[[left_index]] <- result_list[[left_index]] + left_positive - left_negative
    }
    else {
       result_list <- prepare_leafs( left_set, last_part, last_values, depth+1, 2*index-1, result_list)
       result_list <- prepare_leafs( right_set, last_part, last_values, depth+1, 2*index, result_list )
    }

    return(result_list)
}


classify <- function( observ, a, b, class, index, depth) {
    head_length <- 2 ** depth
    first_part <- head(a, head_length)
    last_part <- tail(a, -1*head_length)
    first_values <- head(b, head_length)
    last_values <- tail(b, -1*head_length)
    thresh_value <- first_values[[index]]
    col <- first_part[[index]]
    if( length(last_part) == 0 ) {
        if( observ[[col]] >= thresh_value ) {
            return(class[[index*2]])
        }
        else {
            return(class[[index*2-1]])
        }
    }
    else {
        if( observ[[col]] >= thresh_value ) {
            return(classify( observ, last_part, last_values, class, 2*index, depth+1))
        }
        else {
            return(classify( observ, last_part, last_values, class, 2*index-1, depth+1))
        }
    }
    return(0)
}

get_thresholds <- function(data, attributes, index, depth, result) {
    head_length <- 2 ** depth
    first_part <- head(attributes, head_length)
    last_part <- tail(attributes, -1*head_length)
    global_index <- 2 ** depth - 1 + index
    thresh <- find.treshold.function( data[, first_part[[index]]], data[, 9])
    result[[global_index]] <- thresh
    if( length(last_part) == 0) {
        return(result)
    }
    else{
        result <- get_thresholds( data[data[, first_part[[index]]] >= thresh,  ], last_part, 2*index, depth+1, result )
        result <- get_thresholds( data[data[, first_part[[index]]] < thresh,  ], last_part, 2*index-1, depth+1, result )
        return(result)
    }
}


classify_function <- function( train_data, test_data, attr_list, values_list) {
        n <- length(attr_list) + 1
        raw_classes <- prepare_leafs(train_data, attr_list, values_list, 0, 1, as.list(rep(0, n)))
        classes_list <- as.list(rep(0, n))
        for (i in 1:n) {
            if( raw_classes[[i]] > 0) {
                classes_list[[i]] <- 1
            }
            else {
                classes_list[[i]] <- 0
            }
        }
        tp <- 0
        positive <- 0
        all <- nrow(test_data)
        good <- 0
        class_column <- ncol(train_data)
        for(i in 1:nrow(test_data) )
        {
                obs <- classify( test_data[i, ], attr_list, values_list, classes_list, 1, 0)
                if( obs == test_data[i, class_column] ) {
                        good <- good + 1
                }
                if( obs == 1 ) {
                        positive <- positive + 1
                        if( test_data[i, class_column] == 1 ) {
                                tp <- tp + 1
                        }
                }
        }
        if( positive == 0 ) {
                return(0)
        }
        if( positive == 0 ) {
                return (0)
        }
        else {
                penalty <- 1
                if( good/all < g_min_precision ) {
                        penalty <- g_penalty
                }
                return (tp*penalty)
        }
}

fitness_function <- function(attr_list) {
        n <- length(attr_list) + 1
        values_list <- get_thresholds( train_data, attr_list, 1, 0, as.list(rep(0, length(attr_list))))
        return(classify_function(train_data, test_data, attr_list, values_list))
}


colMax <- function(data) { return(max(data, na.rm=TRUE)) }

fitness_thresh_function <- function(attr_thresh_list) {
        sample <- sample.int(n = nrow(PimaData), size = floor(.8*nrow(PimaData)))
        train_data <- PimaData[sample, ]
        test_data <- PimaData[-sample, ]
        n <- length(attr_thresh_list) / 2
        attr_list <- as.list(rep(0, n))
        values_list <- as.list(rep(0, n))
        for (i in 1:n) {
                column <- attr_thresh_list[[2*i-1]]
                attr_list[[i]] <- column
                print("ColMax")
                print(colMax(train_data[, column]))
                values_list[[i]] <- attr_thresh_list[[2*i]] * colMax(train_data[, column])
        }
        return(classify_function(train_data, test_data, attr_list, values_list))
}

library(GA)

test_attribute_tree <- function(m_maxIter, m_popSize, m_runNumber, m_elitism, m_crossoverChance, m_mutationChance, m_tree_levels) {
    tree_len <- 2 ** m_tree_levels - 1
    GA <- ga(type = "real-valued",
             fitness = fitness_function, lower = rep(1, tree_len),
             upper = rep(g_columns, tree_len), popSize = m_popSize,
             maxiter=m_maxIter, run=m_runNumber,
             elitism = base::max(1, round(m_popSize*m_elitism) ),
             seed=1234,
             pcrossover = m_crossoverChance, pmutation = m_mutationChance)
    return(GA)
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
             maxiter=m_maxIter, run=m_runNumber,
             elitism = base::max(1, round(m_popSize*m_elitism) ),
             seed=1234,
             pcrossover = m_crossoverChance, pmutation = m_mutationChance)
    return(GA)
}


