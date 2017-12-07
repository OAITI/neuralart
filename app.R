library(shiny)
library(shinythemes)
library(jpeg)
library(png)
library(shinycssloaders)
library(shinyjs)

tensorflow_activate_path <- "~/Documents/Python/tensorflow/bin/activate"

## Set images resource path
addResourcePath("images", "images")

style <- file.path("images", "style", dir("images/style"))
names(style) <- tools::toTitleCase(gsub(".jpg|.png", "", basename(style)))

ui <- fluidPage(theme = shinytheme("cerulean"),
   
   titlePanel("Neural Art Image Creator"),
   
   useShinyjs(),
   
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
          )
      ),
      
      mainPanel(
          fluidRow(
              column(6,
                      h4("Content Image"),
                      imageOutput("content_img", height = "250px")
              ),
              column(6,
                      h4("Style Image"),
                      withSpinner(imageOutput("style_img", height = "250px"))
              )
          ),
          
          hr(),
          
          h3("Neural Art Image"),
          textOutput("iteration"),
          imageOutput("result_img", height = "300px")
      )
   )
)

server <- function(input, output, session) {
    
    result_dir <- tempdir()
    result_file <- paste0(result_dir, "/test.png")
    
    sapply(file.path(result_dir, dir(result_dir)), file.remove)
    file.copy("images/white.png", result_file)
    
    values <- reactiveValues(content_ext = "png", content_file = "", style_ext = "png", style_file = "", iteration = -1)
    
    observe({
        invalidateLater(500, session)
        
        myfiles <- dir(result_dir)[grep("checkpoint__", dir(result_dir))]
        mysplit <- as.numeric(sapply(strsplit(gsub(".png", "", myfiles), "__"), `[`, 2))
        
        newest <- myfiles[which.max(mysplit)]
        
        if (length(mysplit) > 0 && values$iteration < max(mysplit)) {
            values$iteration <- max(mysplit)
            
            test <- file.path(result_dir, newest)

            file.copy(test, result_file, overwrite = TRUE)
        }
    })

    neural_result <- reactiveFileReader(500, session, result_file, readPNG)
    
    content_path <- reactive({
        if (is.null(input$content_upload)) return(NULL)
        
        myimg_path <- input$content_upload$datapath
        
        sapply(file.path(result_dir, dir(result_dir)), file.remove)
        values$iteration <- -1
        
        return(myimg_path)
    })
    
    content_image <- reactive({
        if (is.null(content_path())) return(NULL)
        
        myimg_path <- content_path()
        
        mytempfile <- tempfile()
        file.copy(myimg_path, mytempfile)
        values$content_file <- mytempfile
        
        fileext <- tools::file_ext(myimg_path)
        values$content_ext <- fileext
        myfunc <- ifelse(fileext == "jpg", readJPEG, readPNG)
        
        return(myfunc(myimg_path))
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
        width <- 400
        height <- min(round(width * myratio), 250)
        
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
        
        sapply(file.path(result_dir, dir(result_dir)), file.remove)
        values$iteration <- -1
        
        return(myimg_path)
    })
    
    style_image <- reactive({
        if (is.null(style_path())) return(NULL)
        
        myimg_path <- style_path()
        
        mytempfile <- tempfile()
        file.copy(myimg_path, mytempfile)
        values$style_file <- mytempfile
        
        fileext <- tools::file_ext(myimg_path)
        values$style_ext <- fileext
        myfunc <- ifelse(fileext == "jpg", readJPEG, readPNG)
        
        return(myfunc(myimg_path))
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
        width <- 400
        height <- min(round(width * myratio), 250)
        
        list(
            src = values$style_file,
            contentType = paste0("image/", values$style_ext),
            width = paste0(width, "px"),
            height = paste0(height, "px")
        )
    })
    
    observe({
        if (!is.null(content_image()) && !is.null(style_image())) {
            shinyjs::alert("Neural art algorithm started! Please wait and your results will begin to appear...")
            
            system(paste0("source ", tensorflow_activate_path, " && python neural_style.py --iterations 1050 --checkpoint-output '", result_dir, "/checkpoint__%s.png' --checkpoint-iterations 50 --content ", content_path(), " --styles ", style_path(), " --output ", file.path(result_dir, "final.png")), wait = FALSE)
        }
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
        width <- 800
        height <- min(round(width * myratio), 300)
        
        list(
            src = result_file,
            contentType = paste0("image/png"),
            width = paste0(width, "px"),
            height = paste0(height, "px")
        )
    })
    
    output$iteration <- renderText({
        return(paste("Iteration:", values$iteration))
    })

}

shinyApp(ui = ui, server = server)
