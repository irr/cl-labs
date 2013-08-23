# source("bayes.R")
# bayes_query(5.9, 3.0, 5.1, 1.8)

library(e1071)

bayes_init <<- function() {
    data(iris)
    model <<- naiveBayes(Species ~ ., iris)
}

bayes_query <<- function(sl, sw, pl, pw) { 
    return(as.character(predict(model, 
        data.frame(Sepal.Length=sl,Sepal.Width=sw,Petal.Length=pl,Petal.Width=pw))))
}

if (!exists(as.character(substitute(model)))) {
    bayes_init()
}