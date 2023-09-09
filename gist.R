
library(shiny)
library(imager)
mpath = "https://mghp.osn.xsede.org/bir190004-bucket01/BiocGeneralData/mast.jpg"
happ = function () {
  ui = fluidPage(
   sidebarLayout(
   sidebarPanel(
    helpText("slider for cannyEdges alpha"), 
    sliderInput("alpha", "alpha", min = 0, max = 1, step = 0.05, 
                                                        value = 1), 
    helpText("slider for cannyEdges isoblur"), 
    sliderInput("blur", "blur SD", min = .25, max = 5, step = 0.25, 
                                                        value = .25), 
    helpText("slider for cannyEdges threshold"), 
    sliderInput("threshold", "threshold", min = 0, max = 100, step = 2, 
                                                        value = 60)),              
    mainPanel(
     tabsetPanel(
      tabPanel("plot", 
       plotOutput("basic"), 
       plotOutput("blurred")),
      tabPanel("about", helpText("This app presents basic aspects of image analysis
applied to an image of an activated mast cell found at cellimagelibrary.org (http://www.cellimagelibrary.org/images/222)."),
 helpText("There are 5 displays.  Depending on settings of sliders, some may disappear.  The upper two
images are the base image and its direct thresholding.  The bottom three show the blurred main image
and then results of canny edge detection with thresholding preceded by or followed by blurring."),
 helpText("The alpha setting is a thresholding tuner for canny image detection in the
imager package.  The blur and threshold sliders operate the isoblur and threshold functions
in imager.  With alpha at 0.75, blur SD at 1.75 and threshold at 84 (%), a small number of discrete
objects are identified."))
      )
     )
    )
   )
  server = function(input, output) {
    
    im <- imager::load.image(mpath)
    output$basic = renderPlot({
      par(mfrow=c(1,2))
      plot(im)
      plot(im %>% threshold(paste0(sprintf("%d", input$threshold), "%"))) # %>% imager::cannyEdges(alpha=input$alpha))
      })
    output$blurred = renderPlot({
      eimp = im %>% imager::isoblur(input$blur)
      eim = im %>% imager::isoblur(input$blur) %>%
                 imager::threshold(paste0(sprintf("%d", input$threshold), "%")) %>% imager::cannyEdges(alpha=input$alpha)
      eim2 = im %>% imager::threshold(paste0(sprintf("%d", input$threshold), "%")) %>% 
                 imager::isoblur(input$blur)  %>% imager::cannyEdges(alpha=input$alpha)
      par(mfrow=c(1,3))
      plot(eimp, main="blur")
      plot(eim, main="blur:thr")
      plot(eim2, main="thr:blur")
    })
   }
 runApp(list(ui=ui, server=server))
}
