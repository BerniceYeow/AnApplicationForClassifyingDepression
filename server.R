

setwd("C:\\Users") #enter source file destination
library(shiny)

# Installing package if not already installed
EnsurePackage <- function(x)
{
  x <- as.character(x)
  if (!require(x, character.only = TRUE))
  {
    install.packages(pkgs = x, repos = "https://mran.microsoft.com/")
    require(x, character.only = TRUE)
  }
}

#Identifying packages required
PrepareTwitter <- function()
{
  EnsurePackage("twitteR")
  EnsurePackage("stringr")
  EnsurePackage("ROAuth")
  EnsurePackage("RCurl")
  EnsurePackage("ggplot2")
  EnsurePackage("reshape")
  EnsurePackage("tm")
  EnsurePackage("RJSONIO")
  EnsurePackage("wordcloud")
  EnsurePackage("gridExtra")
  EnsurePackage("plyr")
  EnsurePackage("e1071")
  EnsurePackage("RColorBrewer")
  EnsurePackage("wordcloud2")
}

PrepareTwitter()

shinyServer(function(input, output, session) {
  consumer_api_key <-
    " " # Enter your consumer key
  consumer_api_secret <-
    " " # Enter your consumer secret
  access_token <-
    " " # Enter your access token
  access_token_secret <-
    " " # Enter your access secret
  setup_twitter_oauth(consumer_api_key,
                      consumer_api_secret,
                      access_token,
                      access_token_secret) # Sets up the OAuth credentials for a twitteR session
  
  #Search tweets and create a data frame -Stanton (2013)
  # Clean the tweets
  TweetFrame <- function(twtList)
  {
    df <- do.call("rbind", lapply(twtList, as.data.frame)) #apply list to dataframe
    #removal of emoticons
    df$text <-
      sapply(df$text, function(row)
        iconv(row, "latin1", "ASCII", sub = "")) #remove emojis, convert from latin to ascii and substitute with blank = removed
    df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text) #removing URLs
    return (df$text)
  }
  
  
  # Function to create a data frame from tweets
  
  pos.words = scan('positive_words2.txt',
                   what = 'character',
                   comment.char = ';') #Provide your path to "positive_words.txt" file
  neg.words = scan('negative_words2.txt',
                   what = 'character',
                   comment.char = ';') #Provide your path to "negative_words.txt" file
  
  wordDatabase <- function()
  {
    pos.words <<- c(pos.words)
    neg.words <<- c(neg.words)
  }
  
  score.sentiment <-
    function(sentences,
             pos.words,
             neg.words,
             .progress = 'none') #by default
    {
      require(plyr)
      require(stringr)
      list = lapply(sentences, function(sentence, pos.words, neg.words)
      {
        sentence = gsub('[[:punct:]]', ' ', sentence) #punctuations replace by blank spaces
        sentence = gsub('[[:cntrl:]]', '', sentence) #control words repaced by blank
        sentence = gsub('\\d+', '', sentence) #decimal replace with nothing
        sentence = gsub('\n', '', sentence) #next line replace with nothing
        sentence = tolower(sentence)
        sentence = gsub("not ","not not_",gsub("n't ","n't not_",sentence))
        sentence = gsub("never ","never never_",sentence)
        sentence = gsub("no ","no no_",sentence)
        
        word.list = str_split(sentence, '\\s+') #Sentence is splitted
        words = unlist(word.list) #convert into chracter vector using unlist
        pos.matches = match(words, pos.words) #compare character vector taking one word at a time match each of them with positive words in the lexicon, will return true or NA 
        neg.matches = match(words, neg.words)
        pos.matches = !is.na(pos.matches)#store the one dont have NA
        neg.matches = !is.na(neg.matches)
        pp = sum(pos.matches) #sum those not NA, get the total positive matches
        nn = sum(neg.matches)
        score = ((sum(pos.matches) - sum(neg.matches)) / (sum(pos.matches) + sum(neg.matches))) #obtain overall score
        list1 = c(score, pp, nn) #create variable list1 and append positive and negative score
        return (list1) #return that list
      }, pos.words, neg.words)
      score_new = lapply(list, `[[`, 1) #extract the list first column as score
      pp1 = score = lapply(list, `[[`, 2) #store positive score
      nn1 = score = lapply(list, `[[`, 3) #store negative score
      
      scores.df = data.frame(score = score_new, text = sentences) #change the list to dataframe
      positive.df = data.frame(Positive = pp1, text = sentences)
      negative.df = data.frame(Negative = nn1, text = sentences)
      
      list_df = list(scores.df, positive.df, negative.df) #merge those dataframes into 1
      return(list_df) #return the merged dataframe
    }
  
  #TABLE DATA
  
  library(reshape)
  sentimentAnalyser <- function(result)
  {
    #Creating a copy of result data frame, get from the data from previous function - create copy of them
    test1 = result[[1]]
    test2 = result[[2]]
    test3 = result[[3]]
    
    #Creating three different data frames for Score, Positive and Negative
    #Removing text column from data frame, text is redundant remove
    test1$text = NULL
    test2$text = NULL
    test3$text = NULL
    #Storing the first row(Containing the sentiment scores) in variable q
    q1 = test1[1, ] #first row contain sentiment score in variable q , test 1 first row 
    q2 = test2[1, ] 
    q3 = test3[1, ]
    qq1 = melt(q1, var = 'Score') #melt and name it as score, merge all the random thing into score, by default categories by ID
    qq2 = melt(q2, var = 'Positive')
    qq3 = melt(q3, var = 'Negative')
    qq1['Score'] = NULL #remove all those random values in one column
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    #Creating data frame
    table1 = data.frame(Text = result[[1]]$text, Score = qq1) #store the text repreated in result 1 result 2 result 3 as text
    table2 = data.frame(Text = result[[2]]$text, Score = qq2)
    table3 = data.frame(Text = result[[3]]$text, Score = qq3)
    
    #Merging three data frames into one
    table_final = data.frame(
      Text = table1$Text,
      Non_Depressed_Words = table2$value,
      Depressed_Words = table3$value,
      Score = table1$value
    ) #store text, the overall score, positive score and negative score
    
    return(table_final)
  }
  
  #calculate percentage
  percentage <- function(table_final)
  {
    #Positive Percentage
    
    #Renaming
    posSc = table_final$Non_Depressed_Words
    negSc = table_final$Depressed_Words
    
    #Adding column
    table_final$NonDepressedWordsPercentage = posSc / (posSc + negSc) #if divided by 0 wil get NA
    
    #Replacing Nan with zero
    pp = table_final$NonDepressedWordsPercentage
    pp[is.nan(pp)] <- 0 #replace NAN with 0
    table_final$NonDepressedWordsPercentage = pp * 100
    
    #Negative Percentage
    
    #Adding column
    table_final$DepressedWordsPercentage = negSc / (posSc + negSc)
    
    #Replacing Nan with zero
    nn = table_final$DepressedWordsPercentage
    nn[is.nan(nn)] <- 0
    table_final$DepressedWordsPercentage = nn * 100
    return(table_final)
  }
  
  wordDatabase()
  
  twtList <-
    reactive({
      twtList <- userTimeline(input$searchTerm, n = input$maxTweets) #extract tweets, this is a list
    })
  tweets <- reactive({
    tweets <- TweetFrame(twtList()) #convert those tweets into dataframe
  })
  
  result <-
    reactive({
      result <-
        score.sentiment(tweets(), pos.words, neg.words, .progress = 'none')
    })
  
  table_final <-
    reactive({
      table_final <- sentimentAnalyser(result())
    })
  table_final_percentage <-
    reactive({
      table_final_percentage <- percentage(table_final())
    })
  
  output$tabledata <- renderTable(table_final_percentage())
  
  
  #WORDCLOUD
  wordclouds <- function(text)
  {
    library(tm)
    library(wordcloud)
    corpus <-
      VCorpus(VectorSource(text)) #Fixed Corpus Transformation issue, GET TEXT CREATE A CORPUS,one text one doc
    #clean text before doing word cloud
    clean_text <- tm_map(corpus, removePunctuation)
    #clean_text <- tm_map(clean_text, content_transformation)
    clean_text <- tm_map(clean_text, content_transformer(tolower))
    clean_text <-
      tm_map(clean_text, removeWords, stopwords("english"))
    clean_text <- tm_map(clean_text, removeNumbers)
    clean_text <- tm_map(clean_text, stripWhitespace)
    clean_text <- tm_map(clean_text, stemDocument)
    
    return (clean_text)
  }
  
  text_word <- reactive({
    text_word <- wordclouds(tweets())
  })
  set.seed(1234)
  
  output$word <-
    renderPlot({
      wordcloud(
        words = text_word(),
        min.freq = 1,
        max.words = 200,
        random.order = FALSE,
        rot.per = 0.35, #fraction of words plotted vertically
        colors = brewer.pal(8, "Dark2")
      )
    })
  
  

  output$plot <- renderPlot({
    #access tweets and create cumulative file
    
    searchterm <- input$searchTerm
    num <- input$maxTweets
    
    
    
    list <-
      userTimeline(
        searchterm,
        n = num,
        since = NULL,
        retryOnRateLimit = 10 #Logical indicating whether to wait and retry when rate limited. This argument is only relevant if the desired return (n) exceeds the remaining limit of available requests (assuming no other searches have been conducted in the past 15 minutes, this limit is 18,000 tweets).
      )
    df <- twListToDF(list)
    df <- df[, order(names(df))] #arrange tweets by name
    df$created <- strftime(df$created, '%Y-%m-%d')
    if (file.exists(paste(searchterm, '_stack.csv')) == FALSE)
      write.csv(df,
                file = paste(searchterm, '_stack.csv'),
                row.names = F)
    #merge last access with cumulative file and remove duplicates
    stack <- read.csv(file = paste(searchterm, '_stack.csv'))
    stack <- rbind(stack, df)
    stack <- subset(stack,!duplicated(stack$text)) #do not want duplicated ones
    write.csv(stack,
              file = paste(searchterm, '_stack.csv'),
              row.names = F)
    
    
    #evaluation tweets function
    score.sentiment <-
      function(sentences,
               pos.words,
               neg.words,
               .progress = 'none')
      {
        library("plyr")
        library("stringr")
        scores <-
          laply(sentences, function(sentence, pos.words, neg.words) {
            sentence <- iconv(sentence, "latin1", "ASCII//TRANSLIT")
            sentence <- iconv(sentence, to = 'ASCII//TRANSLIT')
            sentence <- gsub('[[:punct:]]', "", sentence)
            sentence <- gsub('[[:cntrl:]]', "", sentence)
            sentence <- gsub('\\d+', "", sentence)
            sentence <- tolower(sentence)
            sentence = gsub("not ","not not_",gsub("n't ","n't not_",sentence))
            sentence = gsub("never ","never never_",sentence)
            sentence = gsub("no ","no no_",sentence)
            word.list <- str_split(sentence, '\\s+')
            words <- unlist(word.list)
            pos.matches <- match(words, pos.words)
            neg.matches <- match(words, neg.words)
            pos.matches <- !is.na(pos.matches)
            neg.matches <- !is.na(neg.matches)
            score <- (sum(pos.matches) - sum(neg.matches)) / (sum(pos.matches) + sum(neg.matches))
            return(score)
          }, pos.words, neg.words, .progress = .progress)
        scores.df <- data.frame(score = scores, text = sentences)
        return(scores.df)
        detach("package:plyr", unload = TRUE)
      }
    library("ggplot2")
    pos <-
      scan('positive_words2.txt',
           what = 'character',
           comment.char = ';') #folder with positive dictionary
    neg <-
      scan('negative_words2.txt',
           what = 'character',
           comment.char = ';') #folder with negative dictionary
    
    
    Dataset <- stack
    Dataset$text <- as.factor(Dataset$text)
    scores <-
      score.sentiment(Dataset$text, pos.words, neg.words, .progress = 'text')
    write.csv(scores,
              file = paste(searchterm, '_scores.csv'),
              row.names = TRUE) #save evaluation results into the file
    #total evaluation: positive / negative / neutral
    stat <- scores
    stat$created <- stack$created
    stat$created <- as.Date(stat$created)
    stat[is.na(stat)] <- 0
    stat <-
      mutate(stat, tweet = ifelse(stat$score >= 0, 'Not Depressed', 'Depressed')) #create score variable
    require("dplyr")
    by.tweet <- dplyr::group_by(stat, tweet, created) #group by similar records, the datframe got all tweets is stat, group by each day to its depressed and non depressed tweets
    by.tweet <- dplyr::summarise(by.tweet, number = dplyr::n()) #after group then count the number of depressed and non depressed tweets on a day
    detach("package:dplyr", unload = TRUE)
    write.csv(by.tweet,
              file = paste(searchterm, '_opin.csv'),
              row.names = TRUE)
    #create chart
    
    
    (
      ggplot(by.tweet, aes(created, number)) + geom_line(aes(group = tweet, color =
                                                               tweet), size = 2) +
        geom_point(aes(group = tweet, color = tweet), size = 4) +
        theme(
          text = element_text(size = 18),
          axis.text.x = element_text(angle = 90, vjust = 1) #color depending on type of tweets, depressed or not depressed, got line and point
        ) +
        #stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') +
        ggtitle(searchterm) +
        xlab("Date of Tweet") +
        ylab("Depression state")
    )
  })
  
}) #shiny server
