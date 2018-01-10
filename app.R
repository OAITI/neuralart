library(shiny)
library(shinythemes)
library(jpeg)
library(png)
library(shinycssloaders)
library(shinyBS)
library(shinyjs)
library(magick)

tensorflow_activate_path <- "~/Documents/Python/tensorflow/bin/activate"

## Set images resource path
addResourcePath("images", "images")

style <- file.path("images", "style", dir("images/style"))
names(style) <- tools::toTitleCase(gsub(".jpg|.png", "", basename(style)))

gallery <- image_read("images/gallery.png")

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                includeCSS("css/styles.css"),
                
                useShinyjs(),
                
                titlePanel("Neural Art Image Creator"),
                
                sidebarLayout(
                    sidebarPanel(
                        a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
                        h4("About"),
                        HTML("This application uses a neural art algorithm by Anish Athalye called <a href='https://github.com/anishathalye/neural-style' target='_blank'>Neural Style</a>. This algorithm slowly blends a content image and a style image into a resulting output image. Currently we support <b>PNG</b> or <b>JPEG</b> files only. Images are downsampled to a maximum width of 500px to save computational resources."),
                        
                        hr(),
                        
                        h4("Configuration"),
                        
                        fileInput("content_upload", "Upload Content Image"),
                        selectInput("style", "Choose Style Image", choices = c("Upload Your Own", style)),
                        conditionalPanel(condition = "input.style == 'Upload Your Own'",
                                         fileInput("style_upload", "Upload Style Image")
                        ),
                        sliderInput("iterations", "Iterations", min = 100, max = 2000, step = 100, value = 1000),
                        
                        hr(),
                        
                        shinyjs::disabled(actionButton("begin", "Begin Algorithm", icon = icon("picture-o"))),
                        
                        hr(),
                        
                        shinyjs::disabled(downloadButton("download", "Download Image")),
                        shinyjs::disabled(downloadButton("download_gif", "Download GIF"))
                    ),
                    
                    mainPanel(
                        bsModal(id = 'beginModal', title = 'Neural Art Algorithm Started', trigger = '',
                                size = 'medium', 'The neural art algorithm has started. The results will be displayed every 10 iterations below, along with a progress bar.'),
                        bsModal(id = 'endModal', title = 'Neural Art Algorithm Completed', trigger = '',
                                size = 'medium', 'The neural art algorithm has completed. If you are satisfied, you can download the result. Otherwise, you can try a new style image or specify a new iterations value and try again.'),
                        bsModal(id = 'termModal', title = 'Neural Art Algorithm Terminated', trigger = '',
                                size = 'medium', 'The neural art algorithm has been terminated early. If you are satisfied, you can download the result. Otherwise, you can try a new style image or specify a new iterations value and try again.'),
                        bsModal(id = 'waitModal', title = 'Neural Art Algorithm Cannot Be Started', trigger = '',
                                size = 'medium', 'Sorry, but there is a current neural art process computing. Please wait a minute and then try again.'),
                        
                        tabsetPanel(
                            tabPanel("Neural Art",
                                     fluidRow(
                                         column(6,
                                                h4("Content Image"),
                                                imageOutput("content_img", height = "250px")
                                         ),
                                         column(6,
                                                h4("Style Image"),
                                                withSpinner(imageOutput("style_img", width = "250px"))
                                         )
                                     ),
                                     
                                     hr(),
                                     
                                     h3("Neural Art Image"),
                                     conditionalPanel(condition = "input.begin", 
                                                      h4(uiOutput("iteration")),
                                                      imageOutput("result_img", width = "350px")
                                     )
                            ),
                            tabPanel("Style Gallery",
                                     img(src = "images/gallery.png", width = "800px")
                            )
                        )
                        
                    )
                )
)

server <- function(input, output, session) {
    
    result_dir <- tempdir()
    result_file <- tempfile(fileext = ".png", tmpdir = result_dir)
    gif_file <- tempfile(fileext = ".gif", tmpdir = result_dir)
    
    file_copy_res <- file.copy("images/white.png", result_file, overwrite = TRUE)
    
    values <- reactiveValues(content_ext = "png", content_file = "", style_ext = "png", style_file = "", iteration = -1, total_iterations = 1000, begin_flag = TRUE)
    
    observe({
        invalidateLater(100, session)
        
        myfiles <- dir(result_dir)[grep(paste0(basename(values$content_file), "_", basename(values$style_file), "_checkpoint__"), dir(result_dir))]
        mysplit <- as.numeric(sapply(strsplit(gsub(".png", "", myfiles), "__"), `[`, 2))
        
        newest <- myfiles[which.max(mysplit)]
        Sys.sleep(.1)
        if (values$iteration == values$total_iterations - 10) {
            final_file <- file.path(result_dir, paste0(basename(values$content_file), "_", basename(values$style_file), "_final.png"))
            if (file.exists(final_file)) {
                Sys.sleep(.1)
                
                final_checkpoint <- paste0(basename(values$content_file), "_", basename(values$style_file), "_checkpoint__", values$total_iterations, ".png")
                
                file.copy(final_file, result_file, overwrite = TRUE)
                file.copy(final_file, file.path(result_dir, final_checkpoint), overwrite = TRUE)
                
                values$iteration <- values$total_iterations
                
                myfiles <- dir(result_dir)[grep(paste0(basename(values$content_file), "_", basename(values$style_file), "_checkpoint__"), dir(result_dir))]
                myfiles <- c(myfiles, final_checkpoint)
                mysplit <- as.numeric(sapply(strsplit(gsub(".png", "", myfiles), "__"), `[`, 2))
                
                files_checkpoint <- file.path(result_dir, myfiles)
                
                myseq <- seq(min(mysplit), max(mysplit), length.out = min(11, length(mysplit)))
                myvec <- sapply(myseq, function(x) { which.min(abs(mysplit - x)) })
                
                gif_path <- file.path(result_dir, paste0(basename(values$content_file), "_", basename(values$style_file), "_final.gif"))
                system(paste0("convert -delay 20 -loop 0 ", paste(files_checkpoint[myvec], collapse = " "), " ", gif_path))
                sapply(files_checkpoint, file.remove)
                
                file.copy(gif_path, gif_file, overwrite = TRUE)
                shinyjs::enable("download_gif")
            }
        } else if (length(mysplit) > 0 && !is.na(max(mysplit)) && values$iteration < max(mysplit)) {
            values$iteration <- max(mysplit)
            
            checkpoint_img <- file.path(result_dir, newest)
            
            file.copy(checkpoint_img, result_file, overwrite = TRUE)
        }
    })
    
    observe({
        if (values$iteration == values$total_iterations) {
            toggleModal(session, "endModal", toggle = "open")
            updateActionButton(session, "begin", label = "Begin Algorithm")
            values$begin_flag <- TRUE
        }
    })
    
    neural_result <- reactiveFileReader(100, session, result_file, image_read)
    gif_result <- reactiveFileReader(100, session, gif_file, image_read)
    
    observeEvent(input$begin, { 
        if (values$begin_flag) {
            other_processes <- system("ps -u shiny | grep python", intern = TRUE)
            if (length(other_processes) > 0) {
                toggleModal(session, "waitModal", toggle = "open")
            } else if (!is.null(content_image()) && !is.null(style_image())) {
                values$begin_flag <- FALSE
                updateActionButton(session, "begin", label = "End Algorithm")
                values$iteration <- -1
                values$total_iterations <- input$iterations
                
                toggleModal(session, "beginModal", toggle = "open")
                
                system(paste0("source ", tensorflow_activate_path, " && python neural_style.py --iterations ", values$total_iterations, " --checkpoint-output '", result_dir, "/", basename(values$content_file), "_", basename(values$style_file), "_checkpoint__%s.png' --checkpoint-iterations 10 --content ", content_path(), " --styles ", style_path(), " --output ", result_dir, "/", basename(values$content_file), "_", basename(values$style_file), "_final.png"), wait = FALSE)
            }
        } else {
            values$begin_flag <- TRUE
            updateActionButton(session, "begin", label = "Begin Algorithm")
            
            toggleModal(session, "termModal", toggle = "open")
            
            system("killall python")
            
            myfiles <- dir(result_dir)[grep(paste0(basename(values$content_file), "_", basename(values$style_file), "_checkpoint__"), dir(result_dir))]
            mysplit <- as.numeric(sapply(strsplit(gsub(".png", "", myfiles), "__"), `[`, 2))
            
            gif_path <- file.path(result_dir, paste0(basename(values$content_file), "_", basename(values$style_file), "_final.gif"))
            
            myseq <- seq(min(mysplit), max(mysplit), length.out = min(11, length(mysplit)))
            myvec <- sapply(myseq, function(x) { which.min(abs(mysplit - x)) })
            
            files_checkpoint <- file.path(result_dir, myfiles)
            system(paste0("convert -delay 20 -loop 0 ", paste(files_checkpoint[myvec], collapse = " "), " ", file.path(result_dir, paste0(basename(values$content_file), "_", basename(values$style_file), "_final.gif"))))
            
            file.copy(gif_path, gif_file, overwrite = TRUE)
            sapply(files_checkpoint, file.remove)
            
            shinyjs::enable("download_gif")
        }
    })
    
    observeEvent(input$content_upload, {
        values$content_file <- tempfile(tmpdir = result_dir)
    }, ignoreInit = TRUE)
    
    observeEvent(input$style_upload, {
        values$style_file <- tempfile(tmpdir = result_dir)
    }, ignoreInit = TRUE)
    
    observeEvent(input$style, {
        values$style_file <- tempfile(tmpdir = result_dir)
    }, ignoreInit = TRUE)
    
    observe({
        if (values$content_file != "" && values$style_file != "") {
            shinyjs::enable("begin")
        }
    })
    
    content_path <- reactive({
        if (is.null(input$content_upload)) return(NULL)
        
        myimg_path <- input$content_upload$datapath
        
        return(myimg_path)
    })
    
    content_image <- reactive({
        if (is.null(content_path())) return(NULL)
        
        myimg_path <- content_path()
        
        fileext <- tools::file_ext(myimg_path)
        values$content_ext <- fileext
        
        myimg <- image_read(myimg_path)
        new_img <- image_scale(myimg, min(image_info(myimg)$width, 500))
        image_write(new_img, path = myimg_path)
        
        file.copy(myimg_path, values$content_file, overwrite = TRUE)
        
        return(new_img)
    })
    
    output$content_img <- renderImage({
        myimg <- content_image()
        
        if (is.null(myimg)) {
            return(list(
                src = "images/white.png",
                contentType = "image/png",
                width = "0px",
                height = "0px"
            ))
        }
        
        myinfo <- image_info(myimg)
        myratio <- myinfo$width / myinfo$height
        height <- min(250, myinfo$height)
        width <- min(round(height * myratio), myinfo$width)
        
        list(
            src = values$content_file,
            contentType = paste0("image/", values$content_ext),
            width = paste0(width, "px"),
            height = paste0(height, "px")
        )
    }, deleteFile = FALSE)
    
    style_path <- reactive({
        if (input$style == "Upload Your Own") {
            if (is.null(input$style_upload)) return(NULL)
            
            myimg_path <- input$style_upload$datapath
        } else {
            myimg_path <- input$style
        }
        
        return(myimg_path)
    })
    
    style_image <- reactive({
        if (is.null(style_path())) return(NULL)
        
        myimg_path <- style_path()
        
        file.copy(myimg_path, values$style_file, overwrite = TRUE)
        
        fileext <- tools::file_ext(myimg_path)
        values$style_ext <- fileext
        
        return(image_read(myimg_path))
    })
    
    output$style_img <- renderImage({
        myimg <- style_image()
        
        if (is.null(myimg)) {
            return(list(
                src = "images/white.png",
                contentType = "image/png",
                width = "0px",
                height = "0px"
            ))
        }
        
        myinfo <- image_info(myimg)
        myratio <- myinfo$width / myinfo$height
        height <- min(250, myinfo$height)
        width <- min(round(height * myratio), myinfo$width)
        
        list(
            src = values$style_file,
            contentType = paste0("image/", values$style_ext),
            width = paste0(width, "px"),
            height = paste0(height, "px")
        )
    }, deleteFile = FALSE)
    
    output$result_img <- renderImage({
        if (values$iteration < 0) {
            shinyjs::disable("download")
            shinyjs::disable("download_gif")
            
            return(list(
                src = "images/white.png",
                contentType = "image/png",
                width = "0px",
                height = "0px"
            ))
        }
        
        shinyjs::enable("download")
        
        myimg <- neural_result()
        
        myinfo <- image_info(myimg)
        myratio <- myinfo$width / myinfo$height
        height <- min(350, myinfo$height)
        width <- min(round(height * myratio), myinfo$width)
        
        list(
            src = result_file,
            contentType = paste0("image/png"),
            width = paste0(width, "px"),
            height = paste0(height, "px")
        )
    }, deleteFile = FALSE)
    
    output$iteration <- renderUI({
        myit <- values$iteration
        if (myit == -1) myit <- "Process starting, please wait..."
        
        return(HTML(paste0("Iteration: ", myit, "<br>Progress Complete: ", round(100 * values$iteration / values$total_iterations), "%")))
    })
    
    output$download <- downloadHandler(
        filename = function() { paste0("neural_", input$content_upload$name) },
        content = function(file) {
            image_write(im = neural_result(), path = file)
        }
    )
    
    output$download_gif <- downloadHandler(
        filename = function() { paste0("neural_", input$content_upload$name, ".gif") },
        content = function(file) {
            image_write(im = gif_result(), path = file)
        }
    )
}

shinyApp(ui = ui, server = server)
