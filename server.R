library(shiny)


shinyServer(function(input, output) {
  
  ####Pre Load####  
  #we have to wrap this in a reactive expression to make this work, instead of recalling and processing the text file each and every time.
  
  #This gives us the string for the file location. You ideally need not use this.
  fileLocation <- reactive({FileLoadFunction(input)})
  
  #Getting and converting CSV from list to dataframe
  listCSV <- reactive({FileProcFunction(fileLocation(),input$animalID,input$finalID)})
  
  #Use dataInput() to point to your CSV file, processed by the FileProcFunction
  dataInput <- reactive({ as.data.frame(do.call(cbind,listCSV()))})
  

})

FileLoadFunction <- function(inputArg){
  
  #HTML can't access local file system due to security reasons. Since we are running a local server, we
  #have to modify the string path to reflect that, as the input string contains initial garbage
  #apparently in ubuntu, the 4th argument is the tmp filepath... will this change in windows?
  
  v=strsplit(toString(inputArg$fileInput),split=",",fixed=TRUE)
  
  #we have to perform a substr to trim initial whitespace  
  fileInput <-  substr(v[[1]][4],start=2,stop=nchar(v[[1]][4]))  
  
  return(fileInput)
  
  
}

FileProcFunction <- function(csvImport,animalID_i,cumuStrain_i)
{
  #Data Input  
  Data <- read.csv(csvImport)
  Data <- within(Data, {
    FinalID <-  factor(as.character(Data[[cumuStrain_i]])) 
    AnimalID <- factor(as.character(Data[[animalID_i]]))    
  })
  
  return(Data)  
  
}



