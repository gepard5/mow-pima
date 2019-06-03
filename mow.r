PimaData <- read.csv(file="./pima_dataset.csv", header=TRUE, sep=",")

# funkcja wyszukujaca prog testu nierownosciowego
find.treshold.function <- function(pd.attributes, pd.classes){
  # macierz z wektorów atrybutow i klas
  pd.attr.class <- cbind(pd.attributes, pd.classes)

  if (is.null(pd.attr.class)){
    # jesli wektory sa puste
    pd.final.treshold <- 0

  } else if (sum(pd.attr.class[,'pd.classes']) %in% c(0, length(pd.attr.class[, 'pd.classes']))){
    # jesli wszystkie atrybuty sa tej samej klasy
    pd.final.treshold <- min(pd.attr.class[, 'pd.attributes'])

  } else {
    # jesli wektory niepuste i atrybuty roznych klas
    # posortowana macierz atrybutow i ich klas
    pd.attr.class.sorted <- pd.attr.class[order(pd.attr.class[,'pd.attributes']),]

    # wyznaczenie progow test
    pd.test.tresholds <- c()
    pd.amount.of.attr <- length(pd.attr.class.sorted[, 'pd.attributes'])

    for (i in 1:(pd.amount.of.attr-1)){
      pd.test.tresholds[i] = (pd.attr.class.sorted[i,'pd.attributes']+pd.attr.class.sorted[i+1,'pd.attributes'])/2
    }

    # wyznaczenie entropii dla kazdego progu
    pd.treshold.entropy <- rbind()

    for (treshold in pd.test.tresholds){
      pd.low <- sum(pd.attr.class.sorted[,'pd.attributes']<=treshold)
      pd.up <- sum(pd.attr.class.sorted[,'pd.attributes']>treshold)
      pd.neg.low <- sum(pd.attr.class.sorted[,'pd.attributes']<=treshold & pd.attr.class.sorted[,'pd.classes']==0)
      pd.pos.low <- sum(pd.attr.class.sorted[,'pd.attributes']<=treshold & pd.attr.class.sorted[,'pd.classes']==1)
      pd.neg.up <- sum(pd.attr.class.sorted[,'pd.attributes']>treshold & pd.attr.class.sorted[,'pd.classes']==0)
      pd.pos.up <- sum(pd.attr.class.sorted[,'pd.attributes']>treshold & pd.attr.class.sorted[,'pd.classes']==1)

      # zabezpieczenie przed log(0)
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

    # wybor najlepszego progu (najmniejszej entropii)
    pd.final.treshold <- pd.treshold.entropy[which.min(pd.treshold.entropy[,2]),1]
  }

  return(pd.final.treshold)
}

fitness_mow <- function(data, a, b, depth, index, result_list) {
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
       result_list <- fitness_mow( left_set, last_part, last_values, depth+1, 2*index-1, result_list)
       result_list <- fitness_mow( right_set, last_part, last_values, depth+1, 2*index, result_list )
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


fitness_function <- function(attr_list) {
      sample <- sample.int(n = nrow(PimaData), size = floor(.8*nrow(PimaData)))
        train_data <- PimaData[sample, ]
        test_data <- PimaData[-sample, ]
        n <- length(attr_list) + 1
        values_list <- get_thresholds( train_data, attr_list, 1, 0, as.list(rep(0, length(attr_list))))
        raw_classes <- fitness_mow(train_data, attr_list, values_list, 0, 1, as.list(rep(0, n)))
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
        for(i in 1:nrow(test_data) )
        {
                obs <- classify( test_data[i, ], attr_list, values_list, classes_list, 1, 0)
                if( obs == test_data[i, 9] ) {
                        good <- good + 1
                }
                if( obs == 1 ) {
                        positive <- positive + 1
                        if( test_data[i, 9] == 1 ) {
                                tp <- tp + 1
                        }
                }
        }
        if( positive == 0 ) {
                return(0)
        }
        print("fitness_function")
        print(tp)
        print(positive)
        return(good/all)
}

test_attr <- list(1, 2, 3)
thres <- get_thresholds(PimaData, test_attr, 1, 0, list(0, 0, 0))

result <- fitness_mow(PimaData, list(1, 2, 3), list(10, 5, 7), 0, 1, as.list(rep(0, 4)))

classes_list <- as.list(rep(0, 4))
for (i in 1:4) {
    if( result[[i]] > 0) {
        classes_list[[i]] <- 1
    }
    else {
        classes_list[[i]] <- 0
    }
}


class <- classify( PimaData[1, ], list(1, 2, 3), list(10, 5, 7), classes_list, 1, 0)



library(GA)
GA <- ga(type = "real-valued", fitness = fitness_function, lower = c(1, 1, 1, 1, 1, 1, 1), upper = c(8, 8, 8, 8, 8, 8, 8), popSize = 20, maxiter=20, run=20, parallel=TRUE)
summary(GA)
