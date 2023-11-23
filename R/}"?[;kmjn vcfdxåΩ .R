library(devtools)
library(dplyr)
install_github('Mattciao96/ritalic')
library(ritalic)
library(readxl)
HERB_JURI <- read_excel("~/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/D9DECC6F-30D1-4929-B158-885FCE7A1C04/HERB_JURI.xls")

HERB_JURI_d <- HERB_JURI %>% 
  select(`Erbario lichenologico Juri Nascimbene`) %>% 
  distinct()
test <- HERB_JURI_d[600:900, ]

nomi <- test$`Erbario lichenologico Juri Nascimbene`

imma2 <- ritalic::lich_match('	
Xantoria fallax')

     
      
a5 <- imma[576,]
a1 <- imma[2,]
a2 <- imma[10,]
a3 <- imma[265,]
a4 <- imma[390,]

ds <- rbind(a1,a2,a3,a4,imma2,a5)

dat <- lich_data(nom$accepted_name)
lich_checklist('Puglia')
lich
lich_checklist('Dolomites')

allinea <- lich_match(test$`Erbario lichenologico Juri Nascimbene`)

class <- lich_classification(allinea$accepted_name)

ritalic::lich_occurrences(
  'Cetraria islandica (L.) Ach. subsp. islandica')

lich_occurrences()


class(sp)
library(jsonlite)
library(httr)
lich_traits2 <-function(sp_names) {
  
  # sp_names must be a vector
  if (!is.character(sp_names) && !is.vector(sp_names)) {
    stop("sp_string must be a string or a vector")
  } else if (is.character(sp_names)) {
    sp_names <- 
      c(sp_names)
  }
  # replace Na with empty values
  sp_names <- ifelse(is.na(sp_names), "", sp_names)
  
  # create a vector with only unique species names
  unique_sp_names <- unique(sp_names)
  
  # for each unique name call the match api the result is put in a dataframe
 
  for (i in 1:length(unique_sp_names)) {
    # these parameters are used to retry the iteration after a 429 code
    
    success <- FALSE
    while (!success) {
      
      sp_name <- unique_sp_names[i];
      sp_name <- URLencode(sp_name)
      
      url <- "https://italic.units.it/api/v1/traits/"
      url <- paste(url, sp_name, sep = '')
      
      response <- GET(url)
      
      # Deal with api errors
      # 500 server not available (blocks the function)
      # 429 API usage limit exceeded
      if (response$status_code == 500) {
        stop("Impossible to connect to the server, please try again later")
      } else if (response$status_code == 429) {
        # wait the end of the api cooldown and retry
        wait_api_cooldown()
      } else if (response$status_code == 200) {
        success <- TRUE
      } else {
        stop("An unknown error occurred, please try again later")
      }
      
    }
    
    # If status_code = 200 everything is fine
    data <- fromJSON(rawToChar(response$content))
    
    input <- as.data.frame(data[1])
    traits <- data[2]
    # replace null with na
    traits <- lapply(traits$traits, function(x) if (is.null(x)) '' else x)
    traits <- as.data.frame(traits)
    return(traits)
    
    
    warnings <-  as.data.frame(data[3])
    
    result <- cbind(input,traits,warnings)
    
    if (i == 1) {
      result_merged <- result
    } else {
      result_merged <- rbind(result_merged, result)
    }
    
  
    
  }
  
  # at the end of the cycle, the original array is rebuilt
  ordered_dataframe <- reconstruct_order2(sp_names, result_merged, 1)
  return(ordered_dataframe)
  
}

soto = lich_traits2('8')
soto = soto$traits
soto = as.data.frame(soto)

reconstruct_order2 <-  function(original_vector, dataframe, column_with_vector_values) {
  # Use match() function to get indices of original values in vector
  print(original_vector)
  print(dataframe)
  print(column_with_vector_values)
  return()
  ordered_dataframe <- data.frame(matrix(nrow = length(original_vector), ncol = ncol(dataframe)))
  colnames(ordered_dataframe) <- colnames(dataframe)
  
  # Copy values for duplicates
  for(i in 1:length(original_vector)) {
    ordered_dataframe[i,] <- dataframe[dataframe[,column_with_vector_values] == original_vector[i],]
  }
  return(ordered_dataframe)
}


nomi_gudo <-  ritalic::lich_match(nomi)
eco <- ritalic::lich_ecology_test(nomi_gudo$accepted_name)
