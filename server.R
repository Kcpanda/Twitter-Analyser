#make sure to run the twitter.R script before running the app
library(shiny)
library(twitteR)
library(wordcloud)
library(tm)
library(sentiment)#if you dont have sentiment package download it from here 
#install_url('http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz')
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(plotrix)
shinyServer(function (input, output) {
  rawData <- reactive(
    { tweets <- searchTwitter(input$term, n=input$cant,lang=input$lang)
    return(twListToDF(tweets))
    })
  
  output$tablel <- renderTable( {
    rawData()
    
  })
 
  output$PieC<- renderPlot(
    {
      tw.text <- rawData()$text
      tw.text <- enc2native(rawData()$text)
      tw.text <- tolower(tw.text)
      #remove retweets
      tw.text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tw.text)
      # remove at people
      tw.text = gsub("@\\w+", "", tw.text)
      # remove punctuation
      tw.text = gsub("[[:punct:]]", "", tw.text)
      # remove numbers
      tw.text = gsub("[[:digit:]]", "", tw.text)
      # remove html links
      tw.text = gsub("http\\w+", "", tw.text)
      # remove unnecessary spaces
      tw.text = gsub("[ \t]{2,}", "", tw.text)
      tw.text = gsub("^\\s+|\\s+$", "", tw.text)
      
      class_emo = classify_emotion(tw.text, algorithm="bayes", prior=1.0)
      # get emotion best fit
      emotion = class_emo[,7]
      # substitute NA's by "unknown"
      emotion[is.na(emotion)] = "unknown"
      
      # classify polarity
      class_pol = classify_polarity(tw.text, algorithm="bayes")
      # get polarity best fit
      polarity = class_pol[,4]
      
      # data frame with results
      sent_df = data.frame(text=tw.text, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)
                           
      
      # sort data frame
      sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
      # plot distribution of emotions
      ggplot(sent_df,aes(x=polarity))+geom_bar(stat = "count")
      slices<-c(sum(sent_df$polarity=="positive"),sum(sent_df$polarity=="neutral"),
                sum(sent_df$polarity=="negative"))
      name<-c("positive","neutral","negative")
     total<-sum(sent_df$polarity=="positive")+sum(sent_df$polarity=="neutral")+sum(sent_df$polarity=="negative")
    percentages<-c((sum(sent_df$polarity=="positive")/total)*100,(sum(sent_df$polarity=="neutral")/total)*100,
                   (sum(sent_df$polarity=="negative")/total)*100)
    label<-paste(name,'/',percentages)
      #pie(slices,labels = labels)
      pie3D(slices,labels = label,col = c("brown","#ddaa00","pink"),main="Sentiment Analysis",theta = 0.9)
    }
  )
})