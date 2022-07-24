# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# devtools::install_github('Augustt/bat2inat')
require(shiny)
require(bat2inat)
require(shinythemes)
require(reticulate)
require(OpenStreetMap)
require(ggplot2)
require(bioacoustics)
require(av)
require(geosphere)
require(soundgen)
  
## Fix deploy, module pyinaturalist not found
## Add observation to project feature functionality
## Modal asking if manual ID has been done
## Limit the number of upload files (if not me)


if(!Sys.info()['user'] %in% c('t_a_a', 'tomaug')){
    reticulate::virtualenv_create(envname = 'python3_env', 
                                  python = '/usr/bin/python3')
    reticulate::virtualenv_install('python3_env', 
                                   packages = c('pyinaturalist'))
    reticulate::use_virtualenv("python3_env", required = TRUE)
}


# Create a virtual environment selecting your desired python version

# Import pyinaturalist
pynat <- reticulate::import('pyinaturalist')

# 10MB max size
options(shiny.maxRequestSize = 10 * 1024^2)

# Set FALSE for testing
post <- TRUE

radius <- 30

# Create the folder where we will put figures
# This folder is deleted when the session ends
figDir <- file.path('www', basename(tempfile()))
dir.create(figDir, recursive = TRUE)

# load the token
load('token.rdata')

# Define UI 
ui <- fluidPage(
    
    theme = shinytheme("darkly"),

    # Application title
    titlePanel("Bat 2 iNat"),

    fluidRow(
        column(12,
            fileInput(inputId = 'files', 
                      multiple = TRUE, 
                      label = 'Choose files (max 10MB each)',
                      accept = 'audio/*'),
            div(id = 'console')
        )
    )
)

# Define server logic 
server <- function(input, output, session) {

    vals <- reactiveValues(upload_token = NULL,
                           log = data.frame(sp = NULL,
                                            lat = NULL,
                                            long = NULL,
                                            date = NULL))
    
    loginModal <- function(failed = FALSE) {
        modalDialog(
            size = 's',
            textInput("username", "Username",
                      placeholder = 'Enter your iNaturalist username'
            ),
            passwordInput("password", label = "Password", 
                          placeholder = 'Enter your iNaturalist password'),
            if (failed)
                div(tags$b("Username or password incorrect", style = "color: red;")),
            
            footer = tagList(
                actionButton("login", "Login")
            )
        )
    }
    
    if(!Sys.info()['user'] %in% c('t_a_a', 'tomaug')){
      showModal(loginModal())
    } else {
      load('pwd.rdata')
      vals$upload_token <-  pynat$get_access_token(pwd$username,
                                                   pwd$pwd,
                                                   token[[3]],
                                                   token[[4]])
    }
    
    observeEvent(input$login, {
        
        # this can be used to test login
        upload_token <- try({
            pynat$get_access_token(input$username,
                                   input$password,
                                   token[[3]],
                                   token[[4]])
        }, silent = TRUE)
        
        vals$upload_token <- upload_token

        if(length(upload_token) == 1 &
            class(upload_token) == 'character'){
            removeModal()
        } else {
            showModal(loginModal(failed = TRUE))
        }
    })

    # Delete the figure folder on session close    
    session$onSessionEnded(function() {
        unlink(x = figDir, recursive = TRUE)
    })
    
    observeEvent(input$files, {
        
        files <- input$files
        str(files)
        nrow(files)
        
        if(!is.null(files)){    
        
            local({
                
                withProgress(message = paste('Processing'), value = 0, {
                    
                for(i in 1:nrow(files)){
                    
                    name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", files$name[i])
                    print(paste('File name:', name))
                    file <- normalizePath(files$datapath[i])
                    print(paste('File path:', file))
                    print(paste('Exists?', file.exists(file)))
                    
                    # get metadata
                    incProgress(0.2 * (1/nrow(files)), detail = paste('File', i, "- Extracting metadata"))
                    md <- bat2inat::call_metadata(file, name = name, verbose = FALSE)
                    # print('HERE')
                    print(md)
                    
                    if(is.null(md)){
                        
                        # print('no metadata')
                        incProgress(0.8 * (1/nrow(files)), detail = paste('File', i, "- No metadata"))
                        insertUI(
                            selector = "#console",
                            where = "afterEnd",
                            ui = div(h3(name),
                                     shiny::span('Skipped - No metadata'),
                                     style = "border-radius: 25px;
                                      border: 2px solid rgba(255, 102, 0, 0.8);
                                      padding: 0px 20px 20px 20px;
                                      margin: 10px;
                                      width: fit-content;"), 
                            immediate = TRUE
                        )
                        next
                        
                    } else {
                        
                        # Get species image
                        image_url <- pynat$get_taxa(md$sp)$results[[1]]$default_photo$square_url
                        
                        # create map
                        buf <- 0.005
                        suppressWarnings({
                            mp <- openmap(c(md$lat + (buf*0.7), md$long - buf),
                                          c(md$lat - (buf*0.7), md$long + buf),
                                          zoom = 15,
                                          type = 'osm')
                        })
                        
                        mapFile <- file.path(figDir, paste0(name, 'map.png'))
                        # print(mapFile)
                        png(filename = mapFile,
                            width = floor(150 / (mp$tiles[[1]]$xres/mp$tiles[[1]]$yres)),
                            height = 150, units = 'px')
                            
                            op <- par(mar = rep(0, 4))
                            plot(mp)
                            par(op)
                        dev.off()
                            
                        # check against log
                        # print(md)
                        # print(vals$log)
                        
                        log_check <- vals$log[vals$log$sp == md$sp &
                                              # vals$log$lat == md$lat &
                                              # vals$log$long == md$long &
                                              vals$log$date == md$date, ]
                        
                        if(nrow(log_check) > 0){
                            
                            dists <- NULL
                            
                            # Measure distances
                            for(iL in 1:nrow(log_check)){
                                
                                dists <- c(dists,
                                           distm(c(md$long, md$lat),
                                                 c(log_check$long[iL], log_check$lat[iL]),
                                                 fun = distHaversine))
                            }
                            
                            if(any(dists < radius)){
                                
                                incProgress(0.8 * (1/nrow(files)), 
                                            detail = paste('File', i, 
                                                           'Duplicate in this batch',
                                                           paste0('(', round(min(dists)), 'm)'),
                                                           '- skipping'))
                                insertUI(
                                    selector = "#console",
                                    where = "afterEnd",
                                    ui = div(h3(name),
                                             shiny::span('Skipped - Duplicate in batch',
                                                  paste0('(', round(min(dists)), 'm)')),
                                             style = "border: 2px solid rgba(255, 183, 0, 0.8);
                                          border-radius: 25px;
                                          padding: 0px 20px 20px 20px;
                                          margin: 10px;
                                          width: fit-content;"), 
                                    immediate = TRUE
                                )
                                next
                                
                            }
                        }
                    } 
                    
                    # Check we don't have a duplicate observation already
                    incProgress(0.2 * (1/nrow(files)), detail = paste('File', i, "- Searching for duplicates online"))
                    # print('is_duplicate')
                    # print(md)
                    # print(token$username)
                    dupe <- is_duplicate(md = md,
                                         radius = radius,
                                         username = token$username,
                                         verbose = FALSE)

                    if(dupe){
                        
                        # print('duplicate online')
                        incProgress(0.6 * (1/nrow(files)), detail = paste('File', i, "- Duplicate online"))
                        insertUI(
                            selector = "#console",
                            where = "afterEnd",
                            ui = div(h3(name),
                                     shiny::span('Skipped - Duplicate online'),
                                     style = "border: 2px solid rgba(255, 183, 0, 0.8);
                                      border-radius: 25px;
                                      padding: 0px 20px 20px 20px;
                                      margin: 10px;
                                      width: fit-content;"), 
                            immediate = TRUE
                        )
                        next
                        
                    }
                    
                    # filter calls
                    incProgress(0.1 * (1/nrow(files)), detail = paste('File', i, "- Locating calls in sequence"))
                    # print('filter')
                    TD <- filter_calls(file,
                                       plot = FALSE, 
                                       verbose = FALSE)
                    
                    # create spectrogram
                    incProgress(0.1 * (1/nrow(files)), detail = paste('File', i, "- Creating spectrograms"))
                    # print('spectrograms')
                    pngs <- write_spectro(file, TD,
                                          samp_freq = md$sampling,
                                          tempDir = figDir,
                                          verbose = FALSE)

                    # load token
                    incProgress(0.2 * (1/nrow(files)), detail = paste('File', i, "- Uploading observation data"))

                    
                    if(is.null(TD$freq_peak)){
                        
                        desc <- paste('Recorded on', md$model, md$firmware, '\n',
                                      'Call parameters could not automatically be extracted\n',
                                      'Recorder settings\n',
                                      md$settings,
                                      '\nUploaded using Bat2iNat')
                        cat('description created\n')
                        
                    } else {
                        
                        desc <- paste('Recorded on', md$model, md$firmware, '\n',
                                      'Number of good quality calls:', length(TD$freq_peak), '\n',
                                      'Av. peak frequency (kHz):', round(median(TD$freq_peak/1000)), '\n',
                                      'Av. max frequency (kHz):', round(median(TD$freq_max/1000)), '\n',
                                      'Av. min frequency (kHz):', round(median(TD$freq_min/1000)), '\n',
                                      'Call durations (ms):', round(median(TD$call_duration), digits = 1), '\n',
                                      'Recorder settings\n',
                                      md$settings,
                                      '\nUploaded using Bat2iNat')
                        
                        cat('description created\n')
                        
                    }
                    
                    # Set up observation fields
                    of <- list('567' = paste(md$model, md$firmware), #model
                               '4936' = md$sampling/1000,
                               '12583' = md$time)
                    
                    # Add average frequency if its there
                    if(!is.null(TD$freq_peak)){
                        
                        of <- c(of, '308' = round(median(TD$freq_peak/1000)))
                        
                    }
                    
                    ## Posting the data ##
                    if(post){
                        
                        cat('Posting data...')
                        resp <- pynat$create_observation(
                            species_guess = md$sp,
                            observed_on = paste(md$date, md$time),
                            description = desc,
                            latitude = md$lat, 
                            longitude = md$long,
                            photos = pngs,
                            sounds = file,
                            access_token = vals$upload_token,
                            observation_fields = of
                        )
                        cat('Done')
                        
                        vals$log <- rbind(vals$log, 
                                          data.frame(sp = md$sp,
                                                     lat = md$lat,
                                                     long = md$long,
                                                     date = md$date))
                        
                        if(length(resp) == 1) resp <- resp[[1]]
                        
                        incProgress(0.2 * (1/nrow(files)), detail = paste("Uploaded"))
                        insertUI(
                            selector = "#console",
                            where = "afterEnd",
                            ui = div(h3(name),
                                     div(id = 'sp',
                                         img(src = image_url, height = '150px'),
                                         style = 'float: left; padding-right: 20px;'),
                                     div(id = 'metadata',
                                         shiny::span(strong('Species: '), em(md$sp)), br(),
                                         shiny::span(strong('Date & time: '), paste(md$date,md$time)), br(),
                                         shiny::span(strong('Number of good quality calls: '), length(TD$freq_peak)), br(),
                                         shiny::span(strong('Av. Peak Frequency (kHz):'), ifelse(!is.null(TD$freq_peak),
                                                                                          round(median(TD$freq_peak)/1000),
                                                                                          'NA')), br(),
                                         shiny::span(strong('Av. Call Duration (ms):'), ifelse(!is.null(TD$freq_peak),
                                                                                        round(median(TD$call_duration), digits = 1),
                                                                                        'NA')),
                                         style = 'float: left; padding-right: 20px; font-size: large;'),
                                     div(id = paste0(name, 'spectro'),
                                         # imageOutput(paste0(name, 'spectro'), height = '150px'),
                                         img(src = gsub('^www/', '', pngs[1]), height = '150px'),
                                         style = 'float: left; padding-right: 20px;'),
                                     div(id = paste0(name, 'map'),
                                         img(src = gsub('^www/', '', mapFile), height = '150px'),
                                         # plotOutput(paste0(name, 'map'), height = '150px'),
                                         style = 'float: left; padding-right: 20px;'),
                                     div(id = 'link',
                                         actionButton(inputId = paste0(name, 'link'),
                                                      label = "View on iNat", 
                                                      onclick = paste0("window.open('https://www.inaturalist.org/observations/",
                                                                       resp$id,
                                                                       "',
                                                                       '_blank')")),
                                         style = 'float: left; padding-right: 20px;'),
                                     div(style = "clear: both;"),
                                     style = "border: 2px solid rgba(0, 255, 166, 0.4);
                                      border-radius: 25px;
                                      padding: 0px 20px 20px 20px;
                                      margin: 10px;
                                      width: fit-content;"), 
                            immediate = TRUE
                        )
                        
                        
                    } else {
                        
                        incProgress(0.2 * (1/nrow(files)), detail = paste('File', i, "- Skipped"))
                        insertUI(
                            selector = "#console",
                            where = "afterEnd",
                            ui = div(h3(name),
                                     div(id = 'sp',
                                         img(src = image_url, height = '150px'),
                                         style = 'float: left; padding-right: 20px;'),
                                     div(id = 'metadata',
                                         shiny::span(strong('Species: '), em(md$sp)), br(),
                                         shiny::span(strong('Date & time: '), paste(md$date,md$time)), br(),
                                         shiny::span(strong('Number of good quality calls: '), length(TD$freq_peak)), br(),
                                         shiny::span(strong('Av. Peak Frequency (kHz):'), ifelse(!is.null(TD$freq_peak),
                                                                                          round(median(TD$freq_peak)/1000),
                                                                                          'NA')), br(),
                                         shiny::span(strong('Av. Call Duration (ms):'), ifelse(!is.null(TD$freq_peak),
                                                                                        round(median(TD$call_duration), digits = 1),
                                                                                        'NA')),
                                         style = 'float: left; padding-right: 20px; font-size: large;'),
                                     div(id = paste0(name, 'spectro'),
                                         # imageOutput(paste0(name, 'spectro'), height = '150px'),
                                         img(src = gsub('^www/', '', pngs[1]), height = '150px'),
                                         style = 'float: left; padding-right: 20px;'),
                                     div(id = paste0(name, 'map'),
                                         img(src = gsub('^www/', '', mapFile), height = '150px'),
                                         # plotOutput(paste0(name, 'map'), height = '150px'),
                                         style = 'float: left; padding-right: 20px;'),

                                     div(style = "clear: both;"),
                                     style = "border: 2px solid rgba(0, 255, 166, 0.4);
                                      border-radius: 25px;
                                      padding: 0px 20px 20px 20px;
                                      margin: 10px;
                                      width: fit-content;"),
                            immediate = TRUE
                        )
                        vals$log <- rbind(vals$log, 
                                          data.frame(sp = md$sp,
                                                     lat = md$lat,
                                                     long = md$long,
                                                     date = md$date))
                        }
                    }
                })
            })
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
