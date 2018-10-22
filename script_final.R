library(XML)
library(purrr)
library(lubridate)
library(dplyr)
library(rlist)
library(tidyverse)
library(gridExtra)

TestIfNodeExists <- function(input.result, node.to.test){
  test <- input.result %>% 
    with(result) %>%
    xmlChildren(.) %>%
    names(.) 
  test <- any(test == node.to.test)
  return(test)
}


GetNumberOfWordsPerSubject <- function(data.input) {
  #data.input is a string |-separated 
  #data.out is a numeric which is the number of words of the input
  data.out <- data.input %>%
    as.character %>%
    strsplit(x = ., split = "\\|") %>%
    unlist() %>%
    map_int(., length) %>%
    sum()
  return(data.out)
}

GetNumberOfWordsPerFulltext <- function(data.input) {
  # data.input is a list, which is the output of xmlElementsByTagName getting the result node
  # data.out is a numeric vector with the number of words for each fulltext
  if (TestIfNodeExists(input.result = data.input, node.to.test = "fulltext") == FALSE) {
    data.out <- 0
    return(data.out)
  } else {
    data.out <- data.input %>%
      with(result) %>%
      xmlElementsByTagName(., "fulltext") %>%
      with(fulltext) %>%
      xmlValue() %>%
      trimws() %>%
      strsplit(x = ., split = " ") %>%
      unlist() %>%
      map_int(., length) %>%
      sum() 
  }
  return(data.out)
}

ExtractDailyRecordsTags <- function (filenames) {
  #path.to.files is a string 
  filenames <- list.files("S:\\Text Mining Project\\congressional_xml_files\\congressional records", pattern="^.*\\.xml$", recursive = TRUE, full.names = TRUE)
  date.list <- list()
  subject.list <- list()
  words.per.fulltext <- list()
  words.per.subject <- list()
  counter <- 1
  for (each.xml.file in filenames) {
    if (counter %% 100 == 0 ) {
    print(paste(counter, "/", length(filenames), sep = ""))
    }
    counter <- counter+1
    #parse XML
    doc <- xmlTreeParse(each.xml.file, useInternalNodes = T)
    #get the result xml tag
    result <- xmlElementsByTagName(doc,"result")
    #count the words in the tag fulltext
    # TestIfNodeExists(result, "fulltext")
    words.per.fulltext <- list.append(words.per.fulltext, GetNumberOfWordsPerFulltext(data.input = result))
    # extract the date, formatted with lubridate
    date <- result$result %>% 
      xmlElementsByTagName(., "date") %>% 
      with(date) %>% 
      xmlValue() %>%
      as.date() 
    date.list <- list.append(date.list, date)
    # Reads the subject tag, each one separated by |
    subject.tags <- result %>%
      with(result) %>%
      xmlElementsByTagName(., "subject") %>% 
      map(., xmlValue) %>%
      paste(., sep= ",", collapse = "|") %>%
      ifelse(test = length(.)>0, yes = ., no = NA)
    # count the words in each subject tag
    words.per.subject <- list.append(words.per.subject, GetNumberOfWordsPerSubject(subject.tags)) 
    subject.list <- list.append(subject.list, subject.tags)
  }
  # Build a dataframe with the results and return
  return(data.frame(date= as.date(unlist(date.list)), 
                    subject= unlist(subject.list), 
                    words_per_fulltext = unlist(words.per.fulltext), 
                    words_per_subject = unlist(words.per.subject)))
}


data.frame.result.final <- ExtractDailyRecordsTags("S:\\Text Mining Project\\congressional_xml_files\\congressional records")  
# Plotting the results ----------------------------------------------------

plot.1 <- ggplot(data= data.frame.result.final, aes(x=date, y=words_per_fulltext)) +
  geom_line() + 
  xlab("Time (months)") + ylab("Counts of words per fulltext per day")

plot.2 <- ggplot(data=data.frame.result.final, aes(x=date, y=words_per_subject)) + 
  geom_line() + 
  xlab("Time (months)") + ylab("Counts of subjects per day")
png(filename = "faceted_plot.png", width = 24, height = 22, units = "cm", res = 300)
grid.arrange(plot.1, plot.2, nrow= 2)

