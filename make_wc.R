


make_wc <- function ( infile, freq_col=NULL, outdir=NULL, remwords=NULL, maxword=NULL, 
                      colors=c(input$color4, input$color3, input$color2, input$color1), preview )   { 
  
  dat <- infile
  
  print(head(dat))
  
  if (!is.null(freq_col)) { not.w.cols <- colnames(dat)[ which(colnames(dat)!=freq_col) ] }
  else { not.w.cols <- colnames(dat) }
  
  if (preview==TRUE) { use_cols <- not.w.cols[1]} 
  else {  use_cols <- not.w.cols }
  
  
  for (this_col in (use_cols)) {
    
    if (length(dat[,this_col])>0) {    
      
      print(this_col)
      print(length(dat[,this_col]))
      
      # if the user specified a freq column, make a vector 
      # with the words as name and the freqs as the vector values 
      # otherwise use a corpus
      
      if (!is.null(freq_col)) { 
        
        this_df <- dplyr::select_ (dat, this_col, freq_col )
        
        print(this_df)
        print ( this_df[freq_col])
        
        w.vec <- this_df[,2]
        names(w.vec) <- this_df[,1]
        
        names(w.vec) <- tolower ( names(w.vec))
        
        
        return(  wordcloud(names(w.vec), w.vec, random.order=FALSE,
                           ordered.colors=FALSE, scale=c(3,.3)) ) 
        
        
      }
      
      else {
        
        this_df <- dplyr::select_ (dat, this_col)
        this_df <- this_df[!this_df[this_col] == '',]

        corpus <- Corpus(VectorSource(tolower(as.character(this_df) )))
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, PlainTextDocument)
        
        corpus <- tm_map(corpus, removeWords, c(remwords, stopwords('english')))
        corpus <- tm_map(corpus, removePunctuation)

        
      
        return(wordcloud(corpus, colors=colors, scale=c(7,.2), max.words=input$max, 
                         random.order=FALSE,
                         ordered.colors=FALSE ) )
        # 
        
      }
    }
  }
}
