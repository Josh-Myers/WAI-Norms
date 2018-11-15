library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# load data frames
# median by age for each wai measure
wai.24 = readRDS("wai.24.median.rds")
wai.12 = readRDS("wai.12.median.rds")
wai.6 = readRDS("wai.6.median.rds")
wai.3 = readRDS("wai.3.median.rds")
wai.2 = readRDS("wai.2.median.rds")

wai.list = list(wai.24, wai.12, wai.6, wai.3, wai.2)

# ears that were seen at all ages
abs.all.3 = readRDS("abs.all.3.rds")
mag.all.3 = readRDS("mag.all.3.rds")
pha.all.3 = readRDS("pha.all.3.rds")

names.all = c("age", "frequency", "S1L", "S2L", "S3R", "S4R", "S5R", "S6L", "S7R", "S8L")

names(abs.all.3) = names.all
names(mag.all.3) = names.all
names(pha.all.3) = names.all

# normative tables
norms.24 = readRDS("norms.list.24.rds")
norms.12 = readRDS("norms.list.12.rds")
norms.6 = readRDS("norms.list.6.rds")
norms.3 = readRDS("norms.list.3.rds")
norms.2 = readRDS("norms.list.2.rds")

theme_set(theme_bw(base_size = 16))

# ui ----
ui <- fluidPage(
                headerPanel("WAI Norms"),
                    tabsetPanel(
                       tabPanel("Group Averages", fluid = TRUE,
                          sidebarLayout(
                              sidebarPanel(
                                 
                                 selectInput(inputId = "wai", label = "WAI measure", 
                                              choices = c("A", "|Y|", "Y phase", "g", "b", "|Z|", "Z phase", "r", "x", "normalized |Y|", "normalized |Z|")),
                                 selectInput(inputId = "freq.res", label = "Frequency resolution", 
                                             choices = c("1/2 octave", "1/3 octave", "1/6 octave", "1/12 octave", "1/24 octave" 
                                             ))
                                 ),
                              mainPanel(br(), plotOutput("group.plot"), br())
                                            )),
                       tabPanel("Individual Results", fluid = TRUE,
                                sidebarLayout(
                                   sidebarPanel(
                                     
                                     selectInput(inputId = "sub", label = "Subject", choices = c(1:8)),
                                     selectInput(inputId = "measure", label = "WAI measure", 
                                               choices = c("A", "|Y|", "Y phase"))
                                     
                                     
                                     ),
                                mainPanel(br(), plotOutput("ind.plot"), br())
                                           )
                                   ),
                       tabPanel("Normative Data", fluid = TRUE,
                                sidebarLayout(
                                   sidebarPanel(selectInput(inputId = "f.res", label = "Frequency resolution", 
                                                            choices = c("1/2 octave", "1/3 octave", "1/6 octave", "1/12 octave", "1/24 octave")),
                                                selectInput(inputId = "ethnicity", label = "Ethnic Specific", choices = c("No", 
                                                                                                                        "Yes")),
                                                   downloadButton("download", label = "Download data")),
                                mainPanel(br(), dataTableOutput("table"), br())
                                      )
                                 ),
                       tabPanel("About", fluid = TRUE,
                                br(),
                                p("This interactive web application accompanies the article 'Longitudinal Development of Wideband 
                                    Absorbance and Admittance Through Infancy' (Myers et al., 2018). The study longitudinally measured wideband acoustic immittance (WAI) on
                                  218 infants at birth, 6 months, 12 months, and 18 months of age. Resutls from ears with normal middle ear function were included in the study, 
                                  assessed using distortion product otoacoustic emissions and tympanometry. High-frequency (1000 Hz) tympanometry was used at birth and
                                  6 months, and low-frequency (226 Hz) tympanometry at 12 and 18 months. Refer to the article for further information about the study"),
                                p("The 'Group Averages' tab shows median WAI results by age.
                                    You can choose the the frequency resolution and the WAI measure to diplay from the drop down menus."),
                                p("The 'Individual Results' tab presents results from individual infants who attended all four of the follow up appointments. 
                                    Select the subject and WAI measure from the menus. These results are presented at 1/3 octave frequency resolution."),
                                p("The 'Normative Data' tab provides the group 2.5, 5, 25, 50, 75, 95, and 97.5 percentile results at each frequency for the various WAI 
                                  measures. You can choose the frequency resolution and whether or not you want the results to be ethnic specific (Caucasian and non-Caucasian).  
                                  The selected results can be downloaded as a spreadsheet with the 'Download data' button."),
                                p("If you have any questions or experience issues using the app please email myers.josh@gmail.com."),
                                br(),
                                h4("References"),
                                p("Myers, J., Kei, J., Aithal, S., Aithal, V., Driscoll, C., Khan, A., Manuel, A., Joseph, A., Malicka, A. N. (2018). 
                                  Longitudinal development of wideband absorbance and admittance through infancy. Manuscript submitted for publication."),
                                br(),
                                h4("Abbreviations"),
                                p("A = absorbance"),
                                p("b = susceptance"),
                                p("g = conductance"),
                                p("r = resistance"),
                                p("WAI = wideband acoustic immittance"),
                                p("x = reactance"),
                                p("|Y| = admittance magnitude"),
                                p("Y phase = admittance phase angle"),
                                p("|Z| = impedance magnitude"),
                                p("Z phase = impedance phase angle"),
                                br()
                                 )
                             )
                         )
                       
                    
server <- function(input, output) {
  
  df = reactive({
        switch(input$freq.res,
           "1/24 octave" = as.data.frame(wai.list[1]),
           "1/12 octave" = as.data.frame(wai.list[2]),
           "1/6 octave" = as.data.frame(wai.list[3]),
           "1/3 octave" = as.data.frame(wai.list[4]),
           "1/2 octave" = as.data.frame(wai.list[5])
           )
  })
  
  wai = reactive({
    wai = 
    switch(input$wai,
              "A" = df()$A,
              "|Y|" = df()$Y,
              "Y phase" = df()$Ypha,
              "g" = df()$G,
              "b" = df()$B,
              "|Z|" = df()$Z,
              "Z phase" = df()$Zpha,
              "r" = df()$R,
              "x" = df()$X,
              "normalized |Y|" = df()$Ynorm,
              "normalized |Z|" = df()$Znorm
           )
    frequency = df()$Frequency
    age = df()$age
    cbind.data.frame(wai, frequency, age)
     })
  
  wai.names = reactive({
      switch(input$wai,
             "A" = "A",
             "|Y|" = "|Y|, millimho",
             "Y phase" = "Y phase, degrees",
             "g" = "g, millimho",
             "b" = "b, millimho",
             "|Z|" = "|Z|, milliohm",
             "Z phase" = "Z phase, degrees",
             "r" = "r, milliohm",
             "x" = "x, milliohm",
             "normalized |Y|" = "Normalized |Y|",
             "normalized |Z|" = "Normalized |Z|"
      )
  })
  
  output$group.plot <- renderPlot({
      ggplot(wai(), aes(x=frequency, y=wai, group=age, colour=age)) +
      geom_line()  +
      xlab("Frequency, Hz") +
      ylab(wai.names()) +
      scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
      theme(aspect.ratio=1/2) +
      theme(legend.title=element_blank())
  })
  
 abs.ind = reactive({
   wai = 
     switch(input$sub,
            "1" = abs.all.3$S1L,
            "2" = abs.all.3$S2L,
            "3" = abs.all.3$S3R,
            "4" = abs.all.3$S4R,
            "5" = abs.all.3$S5R,
            "6" = abs.all.3$S6L,
            "7" = abs.all.3$S7R,
            "8" = abs.all.3$S8L
             )
   
   frequency = abs.all.3$frequency
   age = abs.all.3$age
   cbind.data.frame(wai, frequency, age)
 })
  
 mag.ind = reactive({
   wai = 
     switch(input$sub,
            "1" = mag.all.3$S1L,
            "2" = mag.all.3$S2L,
            "3" = mag.all.3$S3R,
            "4" = mag.all.3$S4R,
            "5" = mag.all.3$S5R,
            "6" = mag.all.3$S6L,
            "7" = mag.all.3$S7R,
            "8" = mag.all.3$S8L
     )
   frequency = mag.all.3$frequency
   age = mag.all.3$age
   cbind.data.frame(wai, frequency, age)
 })
 
 pha.ind = reactive({
   wai = 
     switch(input$sub,
            "1" = pha.all.3$S1L,
            "2" = pha.all.3$S2L,
            "3" = pha.all.3$S3R,
            "4" = pha.all.3$S4R,
            "5" = pha.all.3$S5R,
            "6" = pha.all.3$S6L,
            "7" = pha.all.3$S7R,
            "8" = pha.all.3$S8L
     )
   frequency = pha.all.3$frequency
   age = pha.all.3$age
   cbind.data.frame(wai, frequency, age)
 })
 
 measure = reactive({
   switch(input$measure,
          "A" = abs.ind(),
          "|Y|" = mag.ind(),
          "Y phase" = pha.ind()
   )
 })
 
 wai.names.ind = reactive({
   switch(input$measure,
          "A" = "A",
          "|Y|" = "|Y|, millimho",
          "Y phase" = "Y phase, degrees")
 })
 
 output$ind.plot <- renderPlot({
   ggplot(measure(), aes(x=frequency, y=wai, group=age, colour=age)) +
     geom_line()  +
     xlab("Frequency, Hz") +
     ylab(wai.names.ind()) +
     scale_x_log10(expand=c(0, 0), breaks=c(250, 500, 1000, 2000, 4000, 8000))  +
     theme(aspect.ratio=1/2) +
     theme(legend.title=element_blank())
 })
 
 norms.list = reactive({
   switch(input$f.res,
          "1/24 octave" = norms.24,
          "1/12 octave" = norms.12,
          "1/6 octave" = norms.6,
          "1/3 octave" = norms.3,
          "1/2 octave" = norms.2
          )
 })
 
 norms.df = reactive({
   switch(input$ethnicity,
          "No" = as.data.frame(norms.list()[1]),
          "Yes" = as.data.frame(norms.list()[2])
   )
 })
 
 output$table = renderDataTable({
   datatable(norms.df(), options = list(lengthMenu = list(c(50, 100, 200, -1), c('50', '100', '200', 'All')), pageLength = 100), rownames = FALSE, 
             class = 'white-space: nowrap stripe hover') %>% formatSignif("Percentile", 3) %>% formatRound( -c(1:2), 2) 
   }) 
  
 output$download <- downloadHandler(
   filename = function(){"WAI_Norms.csv"}, 
   content = function(fname){
     write.csv(norms.df(), fname, row.names = F)
   }
 ) 
}

# Run the application 
shinyApp(ui = ui, server = server)

