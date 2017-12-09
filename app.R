library(shiny)
library(shinythemes)
library(jpeg)
library(png)
library(shinycssloaders)
library(shinyBS)
library(imager)

tensorflow_activate_path <- "~/Documents/Python/tensorflow/bin/activate"

## Set images resource path
addResourcePath("images", "images")

style <- file.path("images", "style", dir("images/style"))
names(style) <- tools::toTitleCase(gsub(".jpg|.png", "", basename(style)))

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                titlePanel("Neural Art Image Creator"),
                
                sidebarLayout(
                    sidebarPanel(
                        a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
                        h4("About"),
                        HTML("This application uses a neural art algorithm by Anish Athalye called <a href='https://github.com/anishathalye/neural-style' target='_blank'>Neural Style</a>. This algorithm slowly blends a content image and a style image into a resulting output image. Currently we support <b>PNG</b> or <b>JPEG</b> files only."),
                        
                        hr(),
                        
                        h4("Configuration"),
                        
                        fileInput("content_upload", "Upload Content Image"),
                        selectInput("style", "Choose Style Image", choices = c("Upload Your Own", style)),
                        conditionalPanel(condition = "input.style == 'Upload Your Own'",
                                         fileInput("style_upload", "Upload Style Image")
                        ),
                        sliderInput("iterations", "Iterations", min = 100, max = 2000, step = 100, value = 1000),
                        actionButton("begin", "Begin Algorithm")
                    ),
                    
                    mainPanel(
                        bsModal(id = 'beginModal', title = 'Neural Art Algorithm Started', trigger = '',
                                size = 'medium', 'The neural art algorithm has started. The results will be displayed every 50 iterations below, along with a progress bar.'),
                        bsModal(id = 'endModal', title = 'Neural Art Algorithm Completed', trigger = '',
                                size = 'medium', 'The neural art algorithm has completed. If you are satisfied, you can download the result. Otherwise, you can try a new style image or specify a new iterations value and try again.'),
                        
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
                        h4(uiOutput("iteration")),
                        imageOutput("result_img", width = "350px")
                    )
                )
)

server <- function(input, output, session) {
    
    result_dir <- tempdir()
    result_file <- tempfile(fileext = ".png", tmpdir = result_dir)
    
    file.copy("images/white.png", result_file)
    
    values <- reactiveValues(content_ext = "png", content_file = tempfile(tmpdir = result_dir), style_ext = "png", style_file = tempfile(tmpdir = result_dir), iteration = -1, total_iterations = 1000)
    
    observe({
        invalidateLater(500, session)
        
        myfiles <- dir(result_dir)[grep(paste0(basename(values$content_file), "_", basename(values$style_file), "_checkpoint__"), dir(result_dir))]
        mysplit <- as.numeric(sapply(strsplit(gsub(".png", "", myfiles), "__"), `[`, 2))
        
        newest <- myfiles[which.max(mysplit)]
        
        if (length(mysplit) > 0 && values$iteration < max(mysplit)) {
            values$iteration <- max(mysplit)
            
            test <- file.path(result_dir, newest)
            
            file.copy(test, result_file, overwrite = TRUE)
        }
    })
    
    observe({
        if (values$iteration == values$total_iterations) {
            toggleModal(session, "endModal", toggle = "open")
        }
    })
    
    neural_result <- reactiveFileReader(500, session, result_file, load.image)
    
    observeEvent(input$begin, { 
        values$iteration <- -1
        values$total_iterations <- input$iterations
        
        if (!is.null(content_image()) && !is.null(style_image())) {
            toggleModal(session, "beginModal", toggle = "open")
            
            system(paste0("source ", tensorflow_activate_path, " && python neural_style.py --iterations ", values$total_iterations + 50, " --checkpoint-output '", result_dir, "/", basename(values$content_file), "_", basename(values$style_file), "_checkpoint__%s.png' --checkpoint-iterations 50 --content ", content_path(), " --styles ", style_path(), " --output ", file.path(result_dir, "final.png")), wait = FALSE)
        }
    })
    
    observeEvent(input$content_upload, {
        values$content_file <- tempfile(tmpdir = result_dir)
    })
    
    observeEvent(input$style_upload, {
        values$style_file <- tempfile(tmpdir = result_dir)
    })
    
    observeEvent(input$style, {
        values$style_file <- tempfile(tmpdir = result_dir)
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
        
        myimg <- load.image(myimg_path)
        width_fac <- max(1, width(myimg) / 500)
        
        new_img <- resize(myimg, round(width(myimg) / width_fac), round(height(myimg) / width_fac))
        save.image(new_img, file = myimg_path)
        
        file.copy(myimg_path, values$content_file)
        
        return(new_img)
    })
    
    output$content_img <- renderImage({
        myimg <- content_image()
        
        if (is.null(myimg)) {
            list(
                src = "images/white.png",
                contentType = "image/png",
                width = "0px",
                height = "0px"
            )
        }
        
        myratio <- dim(myimg)[1] / dim(myimg)[2]
        height <- min(250, height(myimg))
        width <- min(round(height * myratio), width(myimg))
        
        list(
            src = values$content_file,
            contentType = paste0("image/", values$content_ext),
            width = paste0(width, "px"),
            height = paste0(height, "px")
        )
    })
    
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
        
        file.copy(myimg_path, values$style_file)
        
        fileext <- tools::file_ext(myimg_path)
        values$style_ext <- fileext
        
        return(load.image(myimg_path))
    })
    
    output$style_img <- renderImage({
        myimg <- style_image()
        
        if (is.null(myimg)) {
            list(
                src = "images/white.png",
                contentType = "image/png",
                width = "0px",
                height = "0px"
            )
        }
        
        myratio <- dim(myimg)[1] / dim(myimg)[2]
        height <- min(250, height(myimg))
        width <- min(round(height * myratio), width(myimg))
        
        list(
            src = values$style_file,
            contentType = paste0("image/", values$style_ext),
            width = paste0(width, "px"),
            height = paste0(height, "px")
        )
    })
    
    output$result_img <- renderImage({
        if (values$iteration < 0) {
            return(list(
                src = "images/white.png",
                contentType = "image/png",
                width = "0px",
                height = "0px"
            ))
        }
        
        myimg <- neural_result()
        
        myratio <- dim(myimg)[1] / dim(myimg)[2]
        height <- min(350, height(myimg))
        width <- min(round(height * myratio), width(myimg))
        
        list(
            src = result_file,
            contentType = paste0("image/png"),
            width = paste0(width, "px"),
            height = paste0(height, "px")
        )
    })
    
    output$iteration <- renderUI({
        return(HTML(paste0("Iteration: ", values$iteration, "<br>Progress Complete: ", round(100 * values$iteration / values$total_iterations), "%")))
    })
    
}

shinyApp(ui = ui, server = server)
