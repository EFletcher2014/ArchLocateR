library(tidyverse)
library(tidytext)
library(tidyselect)
library(openNLP)
library(tidyr)
library(NLP)
library(dplyr)
library(ggplot2)
library(scales)

#Folder in which all files are located. Eventually, would like to make a GUI to allow the user to select this
inputFolder <-
  "C:/Users/bandg/OneDrive/Documents/flashdrivestuff/Masters/ILS 695 CTA/N24_W6/Digitized Reports"

#Gather all .docx files from the folder. Eventually, should handle .txt and .pdf as well
files <-
  list.files(
    path = inputFolder,
    pattern = "*.docx",
    full.names = TRUE,
    recursive = FALSE
  )

#Loop through all files to read their lines into textTibble. This will then be a tibble of individual lines
for (z in 1:length(files)) {
  if (z == 1) {
    textTibble <- tibble(doc = files[z], text = textreadr::read_docx(files[z]))
  } else {
     textTibble <- add_row(textTibble, doc = files[z], text = textreadr::read_docx(files[z]))
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

#all functions I've found to read .docx files are confused by the spacing in mine so I have to add spaces
for (z in 1:nrow(fullTextTibble)) {
  #find locations where a capital letter is not preceded by a space
  issues <- str_locate_all(fullTextTibble$text[z], "\\S[A-Z]+")
  
  #if there are some, replace with a substring including a space before the capital letter
  if (length(issues[[1]]) > 0) {
    #issues contains a beginning and end location for each entry, so we only need half of its length
    for (x in 1:(length(issues[[1]]) / 2)) {
      fullTextTibble$text[z] <-
        str_c(
          substr(fullTextTibble$text[z], 0, issues[[1]][x]),
          " ",
          substr(
            fullTextTibble$text[z],
            issues[[1]][x] + 1,
            str_length(fullTextTibble$text[z])
          )
        )
      issues[[1]][x:length(issues[[1]])] = issues[[1]][x:length(issues[[1]])] + 1 #fixing issues makes the indices for the rest of them incorrect, so increment them to reflect the new space
    }
  }
}

#locate coordinates, which are always 'N' or 'S' and then a number, followed by 'E' or 'W' and then a number
fullTextTibble <-
  mutate(
    fullTextTibble,
    coords = str_locate_all(text, pattern = "[NnSs]\\d+\\.?\\d*\\s*[EeWw]\\d+\\.?\\d*\\s*")
  )



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
                                                                   2)]) #if the index of this word is an index of a coordinate
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
                                                                   2)]) #if the index of this word is an index of a coordinate
        )
      }
    }
  }
}


#Isolate just the coordinate words to get their character indices
#IMPORTANT: the way the words were parsed, only the first half of the coordinate is marked as one. This will be resolved later
temp <- NULL #will become a list of coordinates for each document
for (z in 1:nrow(fullTextTibble)) {
  temp[z] <-
    list(which(
      wordsTibble$isCoord == TRUE &
        wordsTibble$doc == fullTextTibble$document[z]
    ))
}

#Add temp to fullTextTibble so every document is accompanied by a list of indices of coordinates it contains
fullTextTibble <-
  mutate(fullTextTibble, coordIndicesInWordCorpus = temp)


#Clean up to fix coordinates broken up during tokenizing
for (z in 1:nrow(fullTextTibble)) { #loop through each document
  for (i in 1:(length(fullTextTibble$coords[z][[1]])/2)) { #loop through coords for each document. coords contains a start and end, so we only need half of its length
    
    #Gather chunks of the coordinate--words which have starts between the coordinate start and end
    coordsTokens <- filter(
        wordsTibble,
        wordsTibble$doc == fullTextTibble$document[z] 
          & wordsTibble$start >= fullTextTibble$coords[z][[1]][i, 1][[1]] 
          & wordsTibble$start <= fullTextTibble$coords[z][[1]][i, 2][[1]], 
        .preserve = TRUE
      )
    
    #collapse those chunks into one and store it in wordsTibble in place of the first chunk, then remove the extra chunks
    fullCoord <- str_c(coordsTokens$word, collapse = "")
    wordsTibble$word[fullTextTibble$coordIndicesInWordCorpus[[z]][i]] <- fullCoord
    wordsTibble <- anti_join(wordsTibble, slice(coordsTokens, 2:n()), by = c('start', 'doc'))
    
    #relocate the rest of the coordinates because collapsing the coordinates changed the setup of wordsTibble
    temp <- NULL #will become a list of coordinates for each document
    for (x in 1:nrow(fullTextTibble)) {
      temp[x] <-
        list(which(
          wordsTibble$isCoord == TRUE &
            wordsTibble$doc == fullTextTibble$document[x]
        ))
    }
    
    #Add temp to fullTextTibble so every document is accompanied by a list of indices of coordinates it contains
    fullTextTibble <-
      mutate(fullTextTibble, coordIndicesInWordCorpus = temp)
  }
}

#filter to only include words which contain at least two letters. Also remove stopwords and fix some common archaeology-based issues
wordsTibble <- filter(wordsTibble, grepl(".*[A-Za-z]+.*[A-Za-z]+.*", word, ignore.case = TRUE))
wordsTibble <- anti_join(wordsTibble, tibble(word = c("em", "cm", "cmbd")))
wordsTibble <- mutate(wordsTibble, POS = (ifelse(grepl("cmbd", word), ".", (ifelse(grepl("screening", word), "VB", POS)))))

#relocate the rest of the coordinates
temp <- NULL #will become a list of coordinates for each document
for (z in 1:nrow(fullTextTibble)) {
  temp[z] <-
    list(which(
      wordsTibble$isCoord == TRUE &
        wordsTibble$doc == fullTextTibble$document[z]
    ))
}

#Add temp to fullTextTibble so every document is accompanied by a list of indices of coordinates it contains
fullTextTibble <-
  mutate(fullTextTibble, coordIndicesInWordCorpus = temp)


#add column for closest noun to wordsTibble to analyze coordinates
wordsTibble <- mutate(wordsTibble, closestNoun = "")

#list of common nouns which are not likely to be desired in end results
stopNouns <- c("Western", 
               "Michigan", 
               "University", 
               "Anthropology", 
               "Project", 
               "Fort", 
               "St.", 
               "Joseph", 
               "Today", 
               "soil", 
               "something", 
               "Photograph", 
               "information", 
               "vicinity", 
               "season")

#loop through documents 
for(z in 1:nrow(fullTextTibble)) {
  #loop through coordinates to find the closest noun to each
  for(x in 1:length(fullTextTibble$coordIndicesInWordCorpus[[z]])) {
    
    #Index for which word to check, starting at the coordinate and moving out until the end of the text is reached
    #Tie breaks by choosing the word which occurs before the coordinate because it is assumed that most sentences will work that way
    wordNum <- 1
    
    #while this coordinate still has no associated noun and we have not finished the document, continue looking for a noun
    while(wordsTibble$closestNoun[fullTextTibble$coordIndicesInWordCorpus[[z]][x]] == "" && 
          ((fullTextTibble$coordIndicesInWordCorpus[[z]][x] - wordNum > 0 
              && wordsTibble$doc[fullTextTibble$coordIndicesInWordCorpus[[z]][x] - wordNum] == fullTextTibble$document[z]) 
           || (fullTextTibble$coordIndicesInWordCorpus[[z]][x] + wordNum <= nrow(wordsTibble) 
              && wordsTibble$doc[fullTextTibble$coordIndicesInWordCorpus[[z]][x] + wordNum] == fullTextTibble$document[z]))) {
      
      #Check preceding words first
      #if we have not reached the beginning of the document and the current word is a noun (or we already have a noun but it is preceded by an adjective), save it 
      while(fullTextTibble$coordIndicesInWordCorpus[[z]][x]-wordNum > 0 
            && wordsTibble$doc[fullTextTibble$coordIndicesInWordCorpus[[z]][x] - wordNum] == fullTextTibble$document[z]
            && (wordsTibble$POS[fullTextTibble$coordIndicesInWordCorpus[[z]][x] - wordNum] %in% c("NN", "NNS", "NNP", "NNPS")
                || (wordsTibble$closestNoun[fullTextTibble$coordIndicesInWordCorpus[[z]][x]] != "" 
                    && wordsTibble$POS[fullTextTibble$coordIndicesInWordCorpus[[z]][x] - wordNum] %in% c("JJ")))
            && !(wordsTibble$word[fullTextTibble$coordIndicesInWordCorpus[[z]][x] - wordNum] %in% stopNouns)) {
        
        #we want to be able to store a string of nouns, or nouns and an adjective, so collapse them into one string and then increment wordNum to check the next (preceding) word
        wordsTibble$closestNoun[fullTextTibble$coordIndicesInWordCorpus[[z]][x]] <- str_c(wordsTibble$word[fullTextTibble$coordIndicesInWordCorpus[[z]][x]-wordNum], wordsTibble$closestNoun[fullTextTibble$coordIndicesInWordCorpus[[z]][x]], sep = " ")
        wordNum <- wordNum + 1
      }
      
      #if the word before the coordinate did not work, try the word after it.
      if(wordsTibble$closestNoun[fullTextTibble$coordIndicesInWordCorpus[[z]][x]] == "") {
        #if we have not reached the end of the document and the current word is a noun not in stopNouns (or an adjective followed by a noun which isn't a stop noun), save it 
        while(fullTextTibble$coordIndicesInWordCorpus[[z]][x] + wordNum <= nrow(wordsTibble) 
              && wordsTibble$doc[fullTextTibble$coordIndicesInWordCorpus[[z]][x] + wordNum] == fullTextTibble$document[z]
              && (wordsTibble$POS[fullTextTibble$coordIndicesInWordCorpus[[z]][x] + wordNum] %in% c("NN", "NNS", "NNP", "NNPS")
                  || (wordsTibble$closestNoun[fullTextTibble$coordIndicesInWordCorpus[[z]][x]] != "" 
                      && wordsTibble$POS[fullTextTibble$coordIndicesInWordCorpus[[z]][x] - wordNum] %in% c("JJ"))
                  || (wordsTibble$POS[fullTextTibble$coordIndicesInWordCorpus[[z]][x] + wordNum] %in% c("JJ") 
                      && wordsTibble$POS[fullTextTibble$coordIndicesInWordCorpus[[z]][x] + wordNum + 1] %in% c("NN", "NNS", "NNP", "NNPS")
                      && !(wordsTibble$word[fullTextTibble$coordIndicesInWordCorpus[[z]][x] + wordNum + 1] %in% stopNouns)))
              && !(wordsTibble$word[fullTextTibble$coordIndicesInWordCorpus[[z]][x] + wordNum] %in% stopNouns)) {
          
          #we want to be able to store a string of nouns, or nouns and an adjective, so collapse them into one string and then increment wordNum to check the next word
          wordsTibble$closestNoun[fullTextTibble$coordIndicesInWordCorpus[[z]][x]] <- str_c(wordsTibble$closestNoun[fullTextTibble$coordIndicesInWordCorpus[[z]][x]], wordsTibble$word[fullTextTibble$coordIndicesInWordCorpus[[z]][x] + wordNum], sep = " ")
          wordNum <- wordNum + 1
        }
      }
      #if no nouns were found, move out another word
      wordNum <- wordNum + 1
    }
  }
}

#create a tibble of all of the coordinates recovered
AllCoords <- filter(wordsTibble, isCoord == TRUE)
AllCoords <- mutate(AllCoords, word = str_replace_all(word, "[^.[:^punct:]]", "")) #remove punctuation and trailing non-numbers from coordinates
AllCoords <- mutate(AllCoords, word = str_replace_all(word, "\\D$", ""))
AllCoords <- mutate(AllCoords, closestNoun = str_replace_all(closestNoun, "[ \t]+$","")) #clean up trailing and beginning spaces
AllCoords <- mutate(AllCoords, closestNoun = str_replace_all(closestNoun, "^[ \t]+",""))
AllCoords <- select(AllCoords, c('doc', 'word', 'closestNoun')) #select only document, coordinate, and closestNoun columns
AllCoords <- rename(AllCoords, coordinate = word)

#create a comparison tibble
compTable <- read_csv("C:/Users/bandg/OneDrive/Documents/flashdrivestuff/Masters/ILS 695 CTA/FinalProjectExpectedResults.csv") #a csv of expected results
compTable <- mutate(compTable, actualCoords = AllCoords$coordinate)
compTable <- mutate(compTable, actualResults = AllCoords$closestNoun)
compTable <- mutate(compTable, contains = FALSE)
compTable <- mutate(compTable, exact  = FALSE)

#loop through all coordinates to compare expected and actual results
for(z in 1:nrow(compTable)) {
  
  #flag if results contain expected results
  compTable$contains[z] <- (grepl(compTable$Coordinate[z], compTable$actualCoords[z]) 
                            && grepl(compTable$Association[z], compTable$actualResults[z]))
  
  #flag if results are an exact match to expected results
  compTable$exact[z] <- compTable$Association[z] == compTable$actualResults[z]
}

#calculate success rates
containsRateTotal <- nrow(filter(compTable, contains == TRUE))/nrow(AllCoords)

#some coordinates do not actually have accompanying nouns, so we can take those out of calculations
containsRateTotalIgnoreInvalid <- nrow(filter(compTable, contains == TRUE))/
  (nrow(AllCoords) - nrow(filter(compTable, Association == "-1")))

exactRateTotal <- nrow(filter(compTable, exact == TRUE))/nrow(AllCoords)

#some coordinates do not actually have accompanying nouns, so we can take those out of calculations
exactRateTotalIgnoreInvalid <- nrow(filter(compTable, exact == TRUE))/
  (nrow(AllCoords) -  nrow(filter(compTable, Association == "-1")))

#filter coordinates by the location on the document at which they were found. Notes are more complex text (full sentences), while documents also contain headings (formatting)
coordsInNotes <- filter(compTable, Structure == "Notes")
coordsInFormatting <- filter(compTable, Structure == "Formatting")

#calculate success rates based on section of document
exactRateNotes <- nrow(filter(coordsInNotes, exact == TRUE))/nrow(coordsInNotes)
containsRateNotes <- nrow(filter(coordsInNotes, contains == TRUE))/nrow(coordsInNotes)

exactRateFormatting <- nrow(filter(coordsInFormatting, exact == TRUE))/nrow(coordsInFormatting)
containsRateFormatting <- nrow(filter(coordsInFormatting, contains == TRUE))/nrow(coordsInFormatting)

exactRateFormattingIgnoreInvalid <- nrow(filter(coordsInFormatting, exact == TRUE))/(nrow(coordsInFormatting) - nrow(filter(coordsInFormatting, Association == -1)))
containsRateFormattingIgnoreInvalid <-nrow(filter(coordsInFormatting, contains == TRUE))/(nrow(coordsInFormatting) - nrow(filter(coordsInFormatting, Association == -1)))


#write a csv of all points for use in other software
write_csv(AllCoords, "C:/Users/bandg/OneDrive/Documents/flashdrivestuff/Masters/ILS 695 CTA/N24_W6/ILS695PointsAll.csv")

#coordinates contain many duplicate entries, as many documents have the same formatting. Remove these
coords <- distinct(AllCoords, coordinate, closestNoun, .keep_all = TRUE)

#write a csv of distinct points to be mapped by other software
write_csv(coords, "C:/Users/bandg/OneDrive/Documents/flashdrivestuff/Masters/ILS 695 CTA/N24_W6/ILS695Points.csv")


#Plot accuracy--create a dataset first
#group by corpus section ("Structure")
compTable <- group_by(compTable, Structure)
Section <- c(rep("Notes", 2), rep("Tabular", 2), rep("Total", 2))

#Display accuracy as exact stacked with the extras which only contain
Accuracy <- rep(c("Exact", "Contains"), 3)
Percentage <- c(c(exactRateNotes, containsRateNotes - exactRateNotes),
           c(exactRateFormattingIgnoreInvalid, containsRateFormattingIgnoreInvalid - exactRateFormattingIgnoreInvalid),
           c(exactRateTotalIgnoreInvalid, containsRateTotalIgnoreInvalid - exactRateTotalIgnoreInvalid))
data <- data.frame(Section,Accuracy,Percentage)

# Plot a stacked bar plot with more descriptive labels
plot <- ggplot(data, aes(fill=Accuracy, y=Percentage, x=Section)) + 
  geom_bar(position="stack", stat="identity")

plot <- plot + scale_y_continuous(labels = percent) 
plot + labs(x = "Corpus Section", 
            y = "Accuracy Rate of Valid Instances",
            fill = "Accuracy Level")