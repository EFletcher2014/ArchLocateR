library(tidyverse)
library(tidytext)
library(tidyselect)
library(openNLP)
library(tidyr)
library(NLP)
library(dplyr)
library(ggplot2)
library(scales)
library(readr)

#functions
read_in_file <- function(f){
  if (grepl(".docx", f)) {
    return(textreadr::read_docx(files[z]))
  } else if (grepl(".txt", f)) {
    return(read_file(f))
  }
}

locate_coords_in_doc <- function(tib) {
  temp <- NULL #will become a list of coordinates for each document
  for (z in 1:nrow(tib)) {
    temp[z] <-
      list(which(
        wordsTibble$isCoord == TRUE &
          wordsTibble$doc == tib$document[z]
      ))
  }
  return(temp)
}

locate_bkwrds_coords_in_doc <- function(tib) {
  temp <- NULL #will become a list of backwards coordinates for each document
  for (z in 1:nrow(tib)) {
    temp[z] <-
      list(which(
        wordsTibble$isBkwrdsCoord == TRUE &
          wordsTibble$doc == tib$document[z]
      ))
  }
  return(temp)
}

collapse_coords <- function(coordz, bkwrds_flag, index, document) {
  
  #Clean up to fix coordinates broken up during tokenizing
  i <- 1
  while(i <= (length(coordz[[1]])/2)) { #loop through coords for each document. coords contains a start and end, so we only need half of its length
    if(bkwrds_flag){
      indices = fullTextTibble$bkwrdsCoordIndicesInWordCorpus[[index]]
      alreadyHandled <- FALSE
      for (j in 1:(length(fullTextTibble$coords[index][[1]])/2)) {
        alreadyHandled <- ((coordz[[1]][i, 1][[1]] >= fullTextTibble$coords[index][[1]][j, 1][[1]]) & (coordz[[1]][i, 1][[1]] <= fullTextTibble$coords[index][[1]][j, 2][[1]])) | ((coordz[[1]][i, 2][[1]] >= fullTextTibble$coords[index][[1]][j, 1][[1]]) & (coordz[[1]][i, 2][[1]] <= fullTextTibble$coords[index][[1]][j, 2][[1]]))
        if(alreadyHandled) {
          break
        }  
      }
      if(alreadyHandled) {
       #print("indices")
        for (k in 1:(length(indices))) {
         #print(wordsTibble[indices[k],])
        }
        #print("SKIPPED")
        #print(str_c(coordz[[1]][i, 1][[1]], " ", coordz[[1]][i, 2][[1]]))
        
        coordsTokens <- filter(
          wordsTibble,
          wordsTibble$doc == document 
          & wordsTibble$start >= coordz[[1]][i, 1][[1]] 
          & wordsTibble$start <= coordz[[1]][i, 2][[1]], 
          .preserve = TRUE
        )
        #print(coordsTokens)
        #print("index")
        #print(wordsTibble[indices[i],])
        wordsTibble$isBkwrdsCoord[indices[i]] <<- FALSE
        #print("coordinates here")
        #print(coordz[[1]][-(i),])
        
        temp <- fullTextTibble$bkwrds_coords
        temp[[index]] <- coordz[[1]][-(i),]
        coordz[[1]] <- temp[[index]]
        
        fullTextTibble <<-
          mutate(fullTextTibble, bkwrds_coords = temp)
        
        
        fullTextTibble <<-
          mutate(fullTextTibble, coordIndicesInWordCorpus = locate_coords_in_doc(fullTextTibble),
                 bkwrdsCoordIndicesInWordCorpus = locate_bkwrds_coords_in_doc(fullTextTibble))

        next
      }
    
    } else {
      indices = fullTextTibble$coordIndicesInWordCorpus[[index]]
    }
    
    for (k in 1:(length(indices))) {
      #print(wordsTibble[indices[k],])
    }
    
    
    #Gather chunks of the coordinate--words which have starts between the coordinate start and end
    coordsTokens <- filter(
      wordsTibble,
      wordsTibble$doc == document 
      & wordsTibble$start >= coordz[[1]][i, 1][[1]] 
      & wordsTibble$start <= coordz[[1]][i, 2][[1]], 
      .preserve = TRUE
    )
    
    #collapse those chunks into one and store it in wordsTibble in place of the first chunk, then remove the extra chunks
    if(nrow(coordsTokens)>1) {
      #print(nrow(coordsTokens))
      fullCoord <- str_c(coordsTokens$word, collapse = "")
      #print("")
      #print(str_c(coordz[[1]][i, 1][[1]], " ", coordz[[1]][i, 2][[1]]))
      #print(coordsTokens)
      #print(str_c("coord ", fullCoord))
      #print(wordsTibble[indices[i],])
      #print(str_c("old word ", wordsTibble$word[indices[i]]))
      wordsTibble$word[indices[i]] <<- fullCoord
      #print(str_c("index ", indices[i]))
      #print(str_c("new word ", wordsTibble$word[indices[i]]))
      wordsTibble <<- anti_join(wordsTibble, slice(coordsTokens, 2:n()), by = c('start', 'doc'))
      
      #relocate the rest of the coordinates because collapsing the coordinates changed the setup of wordsTibble
      #Add temp to fullTextTibble so every document is accompanied by a list of indices of coordinates it contains
      fullTextTibble <<-
        mutate(fullTextTibble, coordIndicesInWordCorpus = locate_coords_in_doc(fullTextTibble),
               bkwrdsCoordIndicesInWordCorpus = locate_bkwrds_coords_in_doc(fullTextTibble))
    
    }
    i <- i + 1
  }
}

findFirstWordOfSent <- function(startPoint) {
  startWord <- which(wordsTibble$start == startPoint)
  
  while(length(startWord) == 0) {
    startPoint <- startPoint + 1
    startWord <- which(wordsTibble$start == startPoint)
  }
  return(startPoint)
}

find_nouns <- function(bkwrds_flag, stopNouns) {
  for(z in 1:nrow(fullTextTibble)) {
    
    if(bkwrds_flag){
      indices = fullTextTibble$bkwrdsCoordIndicesInWordCorpus
      print("backwards")
      
      #loop through coordinates to find the closest noun to each
      for(x in 1:length(indices[[z]])) {
        print(wordsTibble$word[indices[[z]][x]])
        
        #find starting point--beginning of the sentence containing coordinate
        tempString <- substr(fullTextTibble$text, 1, wordsTibble$start[indices[[z]][x]])
        sentenceIndices <- str_locate_all(tempString, pattern = "(\\.\\s)|(\n\n)")
        
        #if no sentences this far in the document, start at the beginning
        if(nrow(sentenceIndices[[1]]) == 0) {
          startPoint <- 1
        } else {
          startPoint <- sentenceIndices[[1]][nrow(sentenceIndices[[1]])]
        }
        startPoint <- findFirstWordOfSent(startPoint)
        wordNum <- which(wordsTibble$start == startPoint)
        print(wordsTibble$word[wordNum])
        sentNum <- 1
        fin <- indices[[z]][[x]]
        print(fin)
        
        while (wordsTibble$closestNoun[indices[[z]][x]] == "") {
          #print("stuck here 1")
          while(wordNum < fin && wordsTibble$doc[wordNum] == fullTextTibble$document[z]
                && (wordsTibble$POS[wordNum] %in% c("NN", "NNS", "NNP", "NNPS")
                    || (wordsTibble$POS[wordNum] %in% c("JJ") 
                        && wordsTibble$POS[wordNum+1] %in% c("NN", "NNS", "NNP", "NNPS")))
                && !(wordsTibble$word[wordNum] %in% stopNouns)
                && !(grepl(regex("\\d+cm", ignore_case = TRUE), wordsTibble$word[wordNum], ignore.case = TRUE))) {
            
            #we want to be able to store a string of nouns, or nouns and an adjective, so collapse them into one string and then increment wordNum to check the next (preceding) word
            wordsTibble$closestNoun[indices[[z]][x]] <<- str_c(wordsTibble$closestNoun[indices[[z]][x]], wordsTibble$word[wordNum], sep = " ")
            wordNum <- wordNum + 1
            #print("stuck here 2")
          }
          
          wordNum <- wordNum + 1
          if(wordNum >= fin){
            print("MOVING BACK A SENTENCE")
            fin <- which(wordsTibble$start==startPoint)
            
            #if no sentences this far in the document, start at the beginning
            if(nrow(sentenceIndices[[1]])-sentNum == 0) {
              startPoint <- 1
            } else {
              startPoint <- sentenceIndices[[1]][nrow(sentenceIndices[[1]])-sentNum]
            }
            
            startPoint <- findFirstWordOfSent(startPoint)
            wordNum <- which(wordsTibble$start==startPoint)
            print(wordNum)
            print(wordsTibble$word[wordNum])
            print(fin)
            sentNum <- sentNum + 1
          }
        }
      }
    } else {
      indices = fullTextTibble$coordIndicesInWordCorpus
      
      #loop through coordinates to find the closest noun to each
      for(x in 1:length(indices[[z]])) {
        
        #Index for which word to check, starting at the coordinate and moving out until the end of the text is reached
        #Tie breaks by choosing the word which occurs before the coordinate because it is assumed that most sentences will work that way
        wordNum <- 1
        
        #while this coordinate still has no associated noun and we have not finished the document, continue looking for a noun
        while(wordsTibble$closestNoun[indices[[z]][x]] == "" && 
              ((indices[[z]][x] - wordNum > 0 
                && wordsTibble$doc[indices[[z]][x] - wordNum] == fullTextTibble$document[z]) 
               || (indices[[z]][x] + wordNum <= nrow(wordsTibble) 
                   && wordsTibble$doc[indices[[z]][x] + wordNum] == fullTextTibble$document[z]))) {
          
          #Check preceding words first
          #if we have not reached the beginning of the document and the current word is a noun (or we already have a noun but it is preceded by an adjective), save it 
          while(indices[[z]][x]-wordNum > 0 
                && wordsTibble$doc[indices[[z]][x] - wordNum] == fullTextTibble$document[z]
                && (wordsTibble$POS[indices[[z]][x] - wordNum] %in% c("NN", "NNS", "NNP", "NNPS")
                    || (wordsTibble$closestNoun[indices[[z]][x]] != "" 
                        && wordsTibble$POS[indices[[z]][x] - wordNum] %in% c("JJ")))
                && !(wordsTibble$word[indices[[z]][x] - wordNum] %in% stopNouns)
                && !(grepl(regex("\\d+cm", ignore_case = TRUE), wordsTibble$word[indices[[z]][x] - wordNum], ignore.case = TRUE))) {
            
            #we want to be able to store a string of nouns, or nouns and an adjective, so collapse them into one string and then increment wordNum to check the next (preceding) word
            wordsTibble$closestNoun[indices[[z]][x]] <<- str_c(wordsTibble$word[indices[[z]][x]-wordNum], wordsTibble$closestNoun[indices[[z]][x]], sep = " ")
            wordNum <- wordNum + 1
          }
          
          #if the word before the coordinate did not work, try the word after it.
          if(wordsTibble$closestNoun[indices[[z]][x]] == "") {
            #if we have not reached the end of the document and the current word is a noun not in stopNouns (or an adjective followed by a noun which isn't a stop noun), save it 
            while(indices[[z]][x] + wordNum <= nrow(wordsTibble) 
                  && wordsTibble$doc[indices[[z]][x] + wordNum] == fullTextTibble$document[z]
                  && (wordsTibble$POS[indices[[z]][x] + wordNum] %in% c("NN", "NNS", "NNP", "NNPS")
                      || (wordsTibble$closestNoun[indices[[z]][x]] != "" 
                          && wordsTibble$POS[indices[[z]][x] - wordNum] %in% c("JJ"))
                      || (wordsTibble$POS[indices[[z]][x] + wordNum] %in% c("JJ") 
                          && wordsTibble$POS[indices[[z]][x] + wordNum + 1] %in% c("NN", "NNS", "NNP", "NNPS")
                          && !(wordsTibble$word[indices[[z]][x] + wordNum + 1] %in% stopNouns)))
                  && !(wordsTibble$word[indices[[z]][x] + wordNum] %in% stopNouns)
                  && !(grepl(regex("\\d+cm", ignore_case = TRUE), wordsTibble$word[indices[[z]][x] + wordNum], ignore.case = TRUE))) {
              
              #we want to be able to store a string of nouns, or nouns and an adjective, so collapse them into one string and then increment wordNum to check the next word
              wordsTibble$closestNoun[indices[[z]][x]] <<- str_c(wordsTibble$closestNoun[indices[[z]][x]], wordsTibble$word[indices[[z]][x] + wordNum], sep = " ")
              wordNum <- wordNum + 1
            }
          }
          #if no nouns were found, move out another word
          wordNum <- wordNum + 1
        }
      }
    }
  }
}

#Folder in which all files are located. Eventually, would like to make a GUI to allow the user to select this
inputFolder <- "C:\\Users\\bandg\\Documents\\flashdrivestuff\\Masters\\Thesis\\Software Repositories\\ArchLocateR\\Test_Files" #TODO: Populate

#Gather all .docx files from the folder. Eventually, should handle .txt and .pdf as well
files <-
  list.files(
    path = inputFolder,
    pattern = "*.docx|*.txt",
    full.names = TRUE,
    recursive = FALSE
  )

#Loop through all files to read their lines into textTibble. This will then be a tibble of individual lines
for (z in 1:length(files)) {
  if (z == 1) {
    textTibble <- tibble(doc = files[z], text = read_in_file(files[z]))
  } else {
     textTibble <- add_row(textTibble, doc = files[z], text = read_in_file(files[z]))
  }
}

#fullTextTibble will be one tibble with a row for the full text of each document. Loop through textTibble to condense texts
fullTextTibble <- tibble(document = textTibble$doc[1], text = "")
fttIndex <- 1
for (z in 1:nrow(textTibble)) {
  if (textTibble$doc[z] == fullTextTibble$document[fttIndex]) {
    fullTextTibble$text[fttIndex] <-
      str_c(fullTextTibble$text[fttIndex], textTibble$text[z])
  } else {
    fullTextTibble <-
      add_row(fullTextTibble,
              document = textTibble$doc[z],
              text = textTibble$text[z])
    fttIndex <- fttIndex + 1
  }
}

# #all functions I've found to read .docx files are confused by the spacing in mine so I have to add spaces
# for (z in 1:nrow(fullTextTibble)) {
#   #find locations where a capital letter is not preceded by a space
#   issues <- str_locate_all(fullTextTibble$text[z], "\\S[A-Z]+")
#   
#   #if there are some, replace with a substring including a space before the capital letter
#   if (length(issues[[1]]) > 0) {
#     #issues contains a beginning and end location for each entry, so we only need half of its length
#     for (x in 1:(length(issues[[1]]) / 2)) {
#       fullTextTibble$text[z] <-
#         str_c(
#           substr(fullTextTibble$text[z], 0, issues[[1]][x]),
#           " ",
#           substr(
#             fullTextTibble$text[z],
#             issues[[1]][x] + 1,
#             str_length(fullTextTibble$text[z])
#           )
#         )
#       issues[[1]][x:length(issues[[1]])] = issues[[1]][x:length(issues[[1]])] + 1 #fixing issues makes the indices for the rest of them incorrect, so increment them to reflect the new space
#     }
#   }
# }

#locate coordinates, structured like 'N' or 'S' and then a number, followed by 'E' or 'W' and then a number
fullTextTibble <-
  mutate(
    fullTextTibble,
    coords = str_locate_all(text, pattern = "([NnSs]\\d+\\.?\\d*\\s*(-\\s*\\d+\\.?\\d*\\s*)?[EeWw]\\d+\\.?\\d*\\s*(-\\s*\\d+\\.?\\d*\\s*)?)")
  )

#locate coordinates, structured "backwards" (direction comes before the magnitude)
fullTextTibble <-
  mutate(
    fullTextTibble,
    bkwrds_coords = str_locate_all(text, pattern = "(\\d+\\.?\\d*\\s*(-\\s*\\d+\\.?\\d*\\s*)?[NnSs]\\,?\\s*\\d+\\.?\\d*\\s*(-\\s*\\d+\\.?\\d*\\s*)?[EeWw]\\b)")
  )

#Clean up bkwrds_coords to remove coords which could be backwards or forwards. Prioritizing forwards
for (z in 1:nrow(fullTextTibble)) {
  
}

validWordExists <- FALSE #indicator of if word has been encountered

#Loop through all documents
for (z in 1:nrow(fullTextTibble)) {
  
  #annotate text with parts of speech
  textAnnotation <-
    annotate(
      fullTextTibble$text[z],
      list(
        Maxent_Sent_Token_Annotator(),
        Maxent_Word_Token_Annotator()
      )
    )
  wordAnnotation <-
    annotate(fullTextTibble$text[z],
             Maxent_POS_Tag_Annotator(),
             textAnnotation)
  
  #will now loop through annotations to extract words, the character index where they start, and their part of speech
  for (x in 1:(length(wordAnnotation))) {
    if (wordAnnotation$type[x] == "word") { #wordAnnotation can also contain sentence annotations and we don't want those
      if (!validWordExists) { #if no words have been encountered yet, create a new wordsTibble
        wordsTibble <- tibble(
          doc = fullTextTibble$document[z],
          start = wordAnnotation$start[x],
          word = substr(
            fullTextTibble$text[z],
            wordAnnotation$start[x],
            wordAnnotation$end[x]
          ),
          POS = wordAnnotation$features[[x]],
          isCoord = (start %in% fullTextTibble$coords[z][[1]][1:(length(fullTextTibble$coords[z][[1]]) /
                                                                   2)]),
          isBkwrdsCoord = (start %in% fullTextTibble$bkwrds_coords[z][[1]][1:(length(fullTextTibble$bkwrds_coords[z][[1]]) /
                                                                         2)])#if the index of this word is an index of a coordinate
        )
        validWordExists <- TRUE
      } else { #wordsTibble has already been created, so just add to it
        wordsTibble <- add_row(
          wordsTibble,
          doc = fullTextTibble$document[z],
          start = wordAnnotation$start[x],
          word = substr(
            fullTextTibble$text[z],
            wordAnnotation$start[x],
            wordAnnotation$end[x]
          ),
          POS = wordAnnotation$features[[x]],
          isCoord = (start %in% fullTextTibble$coords[z][[1]][1:(length(fullTextTibble$coords[z][[1]]) /
                                                                   2)]), #if the index of this word is an index of a coordinate
          isBkwrdsCoord = (start %in% fullTextTibble$bkwrds_coords[z][[1]][1:(length(fullTextTibble$bkwrds_coords[z][[1]]) /
                                                                                   2)]) #if the index of this word is an index of a coordinate
        )
      }
    }
  }
}


#Isolate just the forward coordinate words to get their character indices
#IMPORTANT: the way the words were parsed, only the first half of the coordinate is marked as one. This will be resolved later


#Add temp to fullTextTibble so every document is accompanied by a list of indices of coordinates it contains
fullTextTibble <-
  mutate(fullTextTibble, coordIndicesInWordCorpus = locate_coords_in_doc(fullTextTibble), 
         bkwrdsCoordIndicesInWordCorpus = locate_bkwrds_coords_in_doc(fullTextTibble))


#Collapse coordinates into just one token (if they were separated by spaces)
#Clean up to fix coordinates broken up during tokenizing
for (z in 1:nrow(fullTextTibble)) { #loop through each document
  collapse_coords(fullTextTibble$coords[z], FALSE, z, fullTextTibble$document[z])
}

#TODO: Handle backwards coordinates
#Clean up to fix coordinates broken up during tokenizing
for (z in 1:nrow(fullTextTibble)) { #loop through each document
  collapse_coords(fullTextTibble$bkwrds_coords[z], TRUE, z, fullTextTibble$document[z])
}

#filter to only include words which contain at least two letters. Also remove stopwords and fix some common archaeology-based issues
wordsTibble <- filter(wordsTibble, grepl(".*[A-Za-z]+.*[A-Za-z]+.*", word, ignore.case = TRUE))
wordsTibble <- anti_join(wordsTibble, tibble(word = c("em", "cm", "cmbd")))
wordsTibble <- mutate(wordsTibble, POS = (ifelse(grepl("cmbd", word), ".", (ifelse(grepl("screening", word), "VB", (ifelse(grepl("charred", word), "JJ", POS)))))))

#relocate the rest of the coordinates
#Add temp to fullTextTibble so every document is accompanied by a list of indices of coordinates it contains
fullTextTibble <-
  mutate(fullTextTibble, coordIndicesInWordCorpus = locate_coords_in_doc(fullTextTibble),
         bkwrdsCoordIndicesInWordCorpus = locate_bkwrds_coords_in_doc(fullTextTibble))


#add column for closest noun to wordsTibble to analyze coordinates
wordsTibble <- mutate(wordsTibble, closestNoun = "")

#list of common nouns which are not likely to be desired in end results

#Folder in which all files are located. Eventually, would like to make a GUI to allow the user to select this
stopWordFolder <- "C:\\Users\\bandg\\Documents\\flashdrivestuff\\Masters\\Thesis\\Software Repositories\\ArchLocateR\\Test_Files\\stop words" #TODO: Populate

#Gather all .docx files from the folder. Eventually, should handle .txt and .pdf as well
files1 <-
  list.files(
    path = stopWordFolder,
    pattern = "*.docx|*.txt",
    full.names = TRUE,
    recursive = FALSE
  )

#Loop through all files to read their lines into textTibble. This will then be a tibble of individual lines
stopNouns <- NULL
for (z in 1:length(files1)) {
  stopNouns <- c(stopNouns, scan(files1[z], ""))
}

stopNouns <- c(stopNouns, filter(wordsTibble, isCoord == TRUE | isBkwrdsCoord == TRUE)$word)


#loop through documents
find_nouns(FALSE, stopNouns)
find_nouns(TRUE, stopNouns)

#create a tibble of all of the coordinates recovered
AllCoords <- filter(wordsTibble, isCoord == TRUE | isBkwrdsCoord == TRUE)
AllCoords <- mutate(AllCoords, word = str_replace_all(word, "[^.\\-[:^punct:]]", "")) #remove punctuation except decimals and ranges
AllCoords <- mutate(AllCoords, word = str_replace_all(word, "[^EeWw0123456789]*$", "")) #remove anything that isn't a digit or direction (E or W) at the end of the string
AllCoords <- mutate(AllCoords, closestNoun = str_replace_all(closestNoun, "[ \t]+$","")) #clean up trailing and beginning spaces
AllCoords <- mutate(AllCoords, closestNoun = str_replace_all(closestNoun, "^[ \t]+",""))
AllCoords <- select(AllCoords, c('doc', 'word', 'closestNoun')) #select only document, coordinate, and closestNoun columns
AllCoords <- rename(AllCoords, coordinate = word)

#create a comparison tibble
compTable <- read_csv("C:\\Users\\bandg\\Documents\\flashdrivestuff\\Masters\\Thesis\\Software Repositories\\ArchLocateR\\Test_Files\\Comparison Table\\Feature40CompTable.csv") #a csv of expected results TODO: populate

#not all coordinates are pulled, so have to loop through to identify those which were not recovered
testResults <- cbind(AllCoords)
testCoords <- cbind(AllCoords)
testDesc <- cbind(AllCoords)
compTable <- mutate(compTable, exact = FALSE)
compTable <- mutate(compTable, actualResults = "")
compTable <- mutate(compTable, contains = FALSE)
compTable <- mutate(compTable, exactCoord = FALSE)
compTable <- mutate(compTable, exactDesc = FALSE)
compTable <- mutate(compTable, containsDesc = FALSE)
for(z in 1:nrow(compTable)) {
  tempCoord <- filter(testCoords, coordinate == compTable$Coordinate[z])
  if(nrow(tempCoord)>=1){
    compTable$exactCoord[z] <- TRUE
    anti_join(testCoords, tempCoord[1])
  }
  
  tempDesc <- filter(testCoords, tolower(closestNoun) == tolower(compTable$Description[z]))
  if(nrow(tempDesc)>=1){
    compTable$exactDesc[z] <- TRUE
    compTable$containsDesc[z] <- TRUE
    anti_join(testDesc, tempDesc[1])
  } else {
    for(j in 1:nrow(testDesc)) {
      if(grepl(testDesc$closestNoun[j], compTable$Description[z], ignore.case = TRUE)) {
        compTable$containsDesc[j] <- TRUE
        testDesc <- testDesc[-c(j),]
        break
      }
      
      if(grepl(compTable$Description[z], testDesc$closestNoun[j], ignore.case = TRUE)) {
        compTable$containsDesc[j] <- TRUE
        testDesc <- testDesc[-c(j),]
        break
      }
    }
  }
  
  temp <- filter(testResults, coordinate == compTable$Coordinate[z] & tolower(closestNoun) == tolower(compTable$Description[z]))
  print(temp)
  if(nrow(temp)>=1){
    compTable$exact[z] <- TRUE
    compTable$contains[z] <- TRUE
    compTable$actualResults[z] <- temp$closestNoun[1]
    anti_join(testResults, temp[1])
    print("TRUE")
  } else {
    for(j in 1:nrow(testResults)) {
      if(grepl(testResults$closestNoun[j], compTable$Description[z], ignore.case = TRUE)) {
        compTable$contains[z] <- TRUE
        compTable$actualResults[z] <- testResults$closestNoun[j]
        testResults <- testResults[-c(j),]
        break
      }
      
      if(grepl(compTable$Description[z], testResults$closestNoun[j], ignore.case = TRUE)) {
        compTable$contains[z] <- TRUE
        compTable$actualResults[z] <- testResults$closestNoun[j]
        testResults <- testResults[-c(j),]
        break
      }
    }
  }
}


#calculate success rates
containsRateTotal <- nrow(filter(compTable, contains == TRUE))/nrow(compTable)
containsDescriptionRateTotal <- nrow(filter(compTable, containsDesc == TRUE))/nrow(compTable)

#some coordinates do not actually have accompanying nouns, so we can take those out of calculations
containsRateTotalIgnoreInvalid <- nrow(filter(compTable, contains == TRUE))/
  (nrow(compTable) - nrow(filter(compTable, Description == "-1")))

exactRateTotal <- nrow(filter(compTable, exact == TRUE))/nrow(compTable)
exactCoordinateRateTotal <- nrow(filter(compTable, exactCoord == TRUE))/nrow(compTable)
exactDescriptionRateTotal <- nrow(filter(compTable, exactDesc == TRUE))/nrow(compTable)

#some coordinates do not actually have accompanying nouns, so we can take those out of calculations
exactRateTotalIgnoreInvalid <- nrow(filter(compTable, exact == TRUE))/
  (nrow(compTable) -  nrow(filter(compTable, Description == "-1")))

#calculate success for different types of objects
exactRateSiteIgnoreInvalid <- nrow(filter(compTable, exact == TRUE, Structure == "site"))/
  (nrow(filter(compTable, Structure == "site")) - nrow(filter(compTable, Structure == "site", Description == "-1")))
exactRateArtifactIgnoreInvalid <- nrow(filter(compTable, exact == TRUE, Structure == "artifact"))/
  (nrow(filter(compTable, Structure == "artifact")) - nrow(filter(compTable, Structure == "artifact", Description == "-1")))

containsRateSiteIgnoreInvalid <- nrow(filter(compTable, contains == TRUE, Structure == "site"))/
  (nrow(filter(compTable, Structure == "site")) - nrow(filter(compTable, Structure == "site", Description == "-1")))
containsRateArtifactIgnoreInvalid <- nrow(filter(compTable, contains == TRUE, Structure == "artifact"))/
  (nrow(filter(compTable, Structure == "artifact")) - nrow(filter(compTable, Structure == "artifact", Description == "-1")))

#write a csv of all points for use in other software
write_csv(AllCoords, "") #TODO: populate

#coordinates contain many duplicate entries, as many documents have the same formatting. Remove these
coords <- distinct(AllCoords, coordinate, closestNoun, .keep_all = TRUE)

#write a csv of distinct points to be mapped by other software
write_csv(coords, "") #TODO: populate


#Plot accuracy--create a dataset first
#group by corpus section ("Structure")
compTable <- group_by(compTable, Structure)
Section <- c(rep("site", 2), rep("artifact", 2), rep("Total", 2))

#Display accuracy as exact stacked with the extras which only contain
Accuracy <- rep(c("Exact", "Contains"), 3)
Percentage <- c(c(exactRateSiteIgnoreInvalid, containsRateSiteIgnoreInvalid - exactRateSiteIgnoreInvalid),
           c(exactRateArtifactIgnoreInvalid, containsRateArtifactIgnoreInvalid - exactRateArtifactIgnoreInvalid),
           c(exactRateTotalIgnoreInvalid, containsRateTotalIgnoreInvalid - exactRateTotalIgnoreInvalid))
data <- data.frame(Section,Accuracy,Percentage)

# Plot a stacked bar plot with more descriptive labels
plot <- ggplot(data, aes(fill=Accuracy, y=Percentage, x=Section)) + 
  geom_bar(position="stack", stat="identity")

plot <- plot + scale_y_continuous(labels = percent) 
plot + labs(x = "Corpus Section", 
            y = "Accuracy Rate of Valid Instances",
            fill = "Accuracy Level")