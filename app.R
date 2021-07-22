#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(bat2inat)
library(shinythemes)
library(reticulate)

post <- FALSE

# file <- "C:\\Users\\t_a_a\\OneDrive - UKCEH\\Bat audio - EM touch\\Cholsey marsh - 19_07_21//Session_20210718_215459/NYCNOC_20210718_220157.wav"

# load the token
load('../../../t_a_a/OneDrive - UKCEH/bat2inat/token.rdata')

# Import pyinaturalist
pynat <- import('pyinaturalist')

# this can be used to test login
upload_token <- pynat$get_access_token(token[[1]],
                                       token[[2]],
                                       token[[3]],
                                       token[[4]])

# Set up log
log <- data.frame(sp = NULL,
                  lat = NULL,
                  long = NULL,
                  date = NULL)

# Define UI 
ui <- fluidPage(
    
    theme = shinytheme("darkly"),

    # Application title
    titlePanel("Bat 2 iNat"),

    sidebarLayout(
        sidebarPanel(
            fileInput(inputId = 'files',
                      multiple = TRUE, 
                      label = 'Choose a file')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            pre(id = "console")
        )
    )
)

# Define server logic 
server <- function(input, output) {

    observe({
        
        files <- input$files
        str(files)
        nrow(files)
        
        if(!is.null(files)){    
        
            for(i in 1:nrow(files)){
            
            name <- files$name[i]
            print(name)
            file <- files$datapath[i]
            print(file)
            
            withProgress(message = paste('Processing', name), value = 0, {
                
                # get metadata
                incProgress(0.2, detail = paste("Extracting metadata"))
                md <- call_metadata(file, verbose = FALSE)
                print(md)
                
                if(is.null(md)){
                    
                    print('no metadata')
                    incProgress(1, detail = paste("No metadata"))
                    insertUI(
                        selector = "#console",
                        where = "afterEnd",
                        ui = div(paste('No metadata', name)), 
                        immediate = TRUE
                    )
                    next
                    
                } else {
                    
                    # check against log
                    log_check <- log[log$sp == md$sp &
                                     log$lat == md$lat &
                                     log$long == md$long &
                                     log$date == md$date, ]
                    
                    if(nrow(log_check) > 0){
                        
                        print('duplicate in log')
                        incProgress(1, detail = paste("Duplicate in this batch - skipping"))
                        insertUI(
                            selector = "#console",
                            where = "afterEnd",
                            ui = div(paste('Duplicate in batch', name)), 
                            immediate = TRUE
                        )
                        next
                        
                    }
                    
                } 
                
                # Check we don't have a duplicate observation already
                incProgress(0.2, detail = paste("Searching for duplicates online"))
                print('is_duplicate')
                print(md)
                print(token$username)
                dupe <- is_duplicate(md = md,
                                     radius = 10,
                                     username = token$username,
                                     verbose = FALSE)
                print('HERE')

                if(dupe){
                    
                    print('duplicate online')
                    incProgress(1, detail = paste("Duplicate online"))
                    insertUI(
                        selector = "#console",
                        where = "afterEnd",
                        ui = div(paste('Duplicate online', name)), 
                        immediate = TRUE
                    )
                    next

                }
                
                # filter calls
                incProgress(0.1, detail = paste("Locating calls in sequence"))
                print('filter')
                TD <- filter_calls(file, verbose = FALSE)
                
                # create spectrogram
                incProgress(0.1, detail = paste("Creating spectrograms"))
                print('spectrograms')
                png <- write_spectro(file, TD, samp_freq = md$sampling,
                                     verbose = FALSE)
                
                # browseURL(dirname(TD$filtered_calls_image))
                
                # load token
                incProgress(0.2, detail = paste("Uploading observation data"))
                print('uploading')
                
                
                if(is.null(TD$freq_peak)){
                    
                    desc <- paste('Recorded on', md$model, md$firmware, '\n',
                                  'Call parameters could not automatically be extracted\n',
                                  'Recorder settings\n',
                                  md$settings)
                    
                } else {
                    
                    desc <- paste('Recorded on', md$model, md$firmware, '\n',
                                  'Number of calls in sequence:', length(TD$freq_peak), '\n',
                                  'Peak frequencies (kHz):', paste(round(TD$freq_peak/1000), collapse = ', '), '\n',
                                  'Max frequencies (kHz):', paste(round(TD$freq_max/1000), collapse = ', '), '\n',
                                  'Min frequencies (kHz):', paste(round(TD$freq_min/1000), collapse = ', '), '\n',
                                  'Call durations (ms):', paste(round(TD$call_duration, digits = 1), collapse = ', '), '\n',
                                  'Recorder settings\n',
                                  md$settings)
                    
                }
                
                # Set up observation fields
                of <- list('567' = paste(md$model, md$firmware), #model
                           '4936' = md$sampling/1000,
                           '12583' = md$time)
                
                # Add average frequency if its there
                if(!is.null(TD$freq_peak)){
                    
                    of <- c(of, '308' = round(mean(TD$freq_peak/1000)))
                    
                }
                
                if(post){
                    
                    resp <- pynat$create_observation(
                        species_guess = md$sp,
                        observed_on = paste(md$date, md$time),
                        description = desc,
                        latitude = md$lat, 
                        longitude = md$long,
                        photos = png,
                        sounds = file,
                        access_token = upload_token,
                        observation_fields = of
                    )
                    
                    log <- rbind(log, 
                                 data.frame(sp = resp$md$sp,
                                            lat = resp$md$lat,
                                            long = resp$md$long,
                                            date = resp$md$date))
                    
                    incProgress(1, detail = paste("Uploaded"))
                    insertUI(
                        selector = "#console",
                        where = "afterEnd",
                        ui = div(paste('Skipped', name)), 
                        immediate = TRUE
                    )
                    
                    
                } else {
                    
                    incProgress(1, detail = paste("Skipped"))
                    insertUI(
                        selector = "#console",
                        where = "afterEnd",
                        ui = div(paste('Skipped', name)), 
                        immediate = TRUE
                    )
                    
                }
            })
        }
            
        }
        
        # withConsoleRedirect("console", {
        #     
        #     if(!is.null(input$files)){
        #         send_observations(files = input$files$datapath,
        #                           post = FALSE, 
        #                           token = token)
        #         # str(cars)
        #     }
        #     
        # })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
