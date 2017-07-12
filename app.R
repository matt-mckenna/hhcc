library(tm)
library(wordcloud)
library(memoise)
library(ggplot2)
library("colourpicker")


server <- 
function(input, output, session) {
  
  
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
  
  #observeEvent (input$go, {
  plotwc <- function () {
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    stops <- as.vector( strsplit ( gsub(" ", "", input$stopwords, fixed = TRUE) , ',')[[1]])
    #stops <- stops[length(stops)+1] <- ','
    
    print (stops)
    #print (head(read.csv(inFile$datapath)))
    wc <- make_wc ( infile=read.csv(inFile$datapath, stringsAsFactors = F), 
              remwords=stops, maxword=input$max, preview=TRUE)
    
  }
  
  output$plot <- renderPlot({
    print(plotwc())
  })
  
  observeEvent (input$reset, {
    updateColourInput (session, "color1", value="black" ) 
    updateColourInput (session, "color2", value="black" ) 
    updateColourInput (session, "color3", value="black" ) 
    updateColourInput (session, "color4", value="black" ) 
    
  })
    
  wcs_downloaded <- reactiveValues(renderd=c(1))
  
  output$downloadPlot <- downloadHandler(

    filename = function() { paste('word', wcs_downloaded$renderd, '.zip', sep='') },
    content  =  "application/zip"
    # content = function(file) {
    #   ggsave(as.name(file), plot = plotwc(), device = "png")
    # }
   
  )
  
  observeEvent(input$downloadPlot, 
 
               { wcs_downloaded$renderd <- isolate(wcs_downloaded$renderd)+1
                 print ('dls is ')
                 print ( isolate(wcs_downloaded$renderd) ) } )
  
  
}



ui <- fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      fileInput('file1', 'Upload CSV File',
               accept=c('text/csv', 
                        'text/comma-separated-values,text/plain', 
                        '.csv')),
      hr(),
      textInput ( 'stopwords', 'Stop Words (words to exclude, separated by commas)', value=""),
      textOutput("test"),
      sliderInput("max",
                  "Maximum Number of Words in Wordcloud:",
                  min = 1,  max = 200,  value = 100),
      colourInput('color1', "Select colour 1", value = "black"), 
      colourInput('color2', "Select colour 2", value = "black"),
      colourInput('color3', "Select colour 3", value = "black"),
      colourInput('color4', "Select colour 4", value = "black"),
      actionButton("reset", "Reset Colors")
    ),
    
    # Show Word Cloud
    mainPanel(
      wellPanel(h4('Live Preview')),
      plotOutput("plot",  width = "100%", height=500), 
      downloadButton("downloadPlot", "Download wordclouds")
    )
  )
)

## run app 
shinyApp(ui = ui, server = server)




