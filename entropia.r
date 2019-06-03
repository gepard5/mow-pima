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

# wektory atrybutow i klas
pd.attributes <- c(100,2,3,4,5,6)
pd.classes <- c(0,0,1,0,0,0)

# uzycie funkcji
result <- find.treshold.function(pd.attributes = pd.attributes, pd.classes = pd.classes)
print(result)