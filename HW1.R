pollutantmean <-function(directory, pollutant, ID = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'ID' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'ID' vector (ignoring NA values)
  ## NOTE: Do not round the results
  
  all_data <- data.frame()
  origin <- "C:/Users/Tony/Data-Science/"
  
  for (i in  ID) {
    
    if (i < 10) {
      p_loc <- paste(origin, directory, "/00", i, ".csv", sep = "")
    }
    
    else if (i >= 10 & i < 100) {
      p_loc <- paste(origin, directory, "/0", i, ".csv", sep = "")
    }
    
    else {
      p_loc <- paste(origin, directory, "/", i, ".csv", sep = "")
    }
    
    all_data <- rbind(all_data, read.csv(p_loc))
  }
  
  new_data <- all_data[complete.cases(all_data),]

  mean(new_data[[pollutant]])
}

complete <- function(directory, ID = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the csv files
  
  ## 'ID' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame form:
  ## ID nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'ID' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  all_data <- data.frame()
  origin <- "C:/Users/Tony/Data-Science/"
  
  for (i in  ID) {
    
    if (i < 10) {
      p_loc <- paste(origin, directory, "/00", i, ".csv", sep = "")
    }
    
    else if (i >= 10 & i < 100) {
      p_loc <- paste(origin, directory, "/0", i, ".csv", sep = "")
    }
    
    else {
      p_loc <- paste(origin, directory, "/", i, ".csv", sep = "")
    }
    
    all_data <- rbind(all_data, read.csv(p_loc))
  }
  
  new_data <- all_data[complete.cases(all_data),]
  
  complete_cases <- aggregate(new_data,
                               list(id = new_data[['ID']]),
                               length)
  
  complete_cases <- complete_cases[c("id", "ID")]
  
  names(complete_cases)[2] <- "nobs"
  
  complete_cases
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the csv files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compete the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  all_data <- data.frame()
  origin <- "C:/Users/Tony/Data-Science/"
  ID = 1:332
  
  for (i in  ID) {
    
    if (i < 10) {
      p_loc <- paste(origin, directory, "/00", i, ".csv", sep = "")
    }
    
    else if (i >= 10 & i < 100) {
      p_loc <- paste(origin, directory, "/0", i, ".csv", sep = "")
    }
    
    else {
      p_loc <- paste(origin, directory, "/", i, ".csv", sep = "")
    }
    
    all_data <- rbind(all_data, read.csv(p_loc))
  }
  
  new_data <- all_data[complete.cases(all_data),]
  new_data <- new_data[c("sulfate", "nitrate", "ID")]
  
  complete_cases <- aggregate(new_data,
                              list(id = new_data[['ID']]),
                              length)
  complete_cases <- complete_cases[c("id", "ID")]
  names(complete_cases)[2] <- "nobs"
  
  final_data = vector()
  
  for (i in complete_cases$id) {

    if (complete_cases[ which(complete_cases$id == i),'nobs']
        > threshold) {
      cor_data <- new_data[new_data$ID == i,]
      cor_data <- cor_data[c("sulfate", "nitrate")]
      final_data <- append(final_data, cor(cor_data,
                                           use = "everything"))
    }
  }

  final_data = unique(final_data[! final_data %in% 1.0])
  
  final_data
}
