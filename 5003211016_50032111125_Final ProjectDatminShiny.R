library(tidyverse)
library(patchwork)
library(plotly)
library(shiny)
library(rsconnect)
library(shinythemes)
library(markdown)
library(reshape2)
library(ggplot2)
library(shinydashboard) 
library(shinydashboardPlus)
library(tidyverse) 
library(ggplot2) 
library(summarytools) 
library(DT)
library(dplyr)
library(dashboardthemes)
library(bslib)
library(fmsb)
library(plotly)
library (scales)
library(echarts4r)
library(leaflet)
library(e1071)

# Setwd
#setwd("D:/Semester 6/Data Mining/EAS Data Mining/Final Project")

# IMPORT DATA

heart_data <- read_csv('heart1.csv')

heart_data <- as.data.frame(heart_data)

#Renaming columns.
data_col_names <- c('Age', 'Sex', 'Chest Pain Type', 'Resting Blood Pressure', 'Cholesterol', 'Fasting Blood Sugar', 'Resting ECG', 'Max. Heart Rate',
                    'Exercise Induced Angina', 'Previous Peak', 'Slope', 'No. Major Blood Vessels', 'Thal Rate', 'Condition')
colnames(heart_data) <- data_col_names

# Select numerical and categorical data
Numerical <- heart_data %>% select('Age','Resting Blood Pressure','Cholesterol','Max. Heart Rate','Previous Peak')
Categorical <- heart_data %>% select('Sex','Chest Pain Type','Fasting Blood Sugar','Resting ECG','Exercise Induced Angina','Slope','No. Major Blood Vessels','Thal Rate')

# separate x and y of the dataset
Samples <- heart_data %>% select(!Condition)
Labels <- heart_data %>% select(Condition)

#plot the correlation_matrix

Correlation_matrix <- cor(Numerical) %>% round(3)


get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(Correlation_matrix)
melted_cormat <- melt(upper_tri,na.rm = TRUE)


Correlation_matrix_plot <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "black", high = "yellow", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0.5),
    legend.position = "right",
    legend.direction = "vertical"
  ) +
  guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10,
                               title.position = "right", title.hjust = 0.5))


##==============================================================================
# create categorical plots with condition
heart_data_copy <- data.frame(heart_data)
colnames(heart_data_copy) <- data_col_names



heart_data_copy$Slope <- as.factor(heart_data_copy$Slope)
heart_data_copy$`No. Major Blood Vessels`<- as.factor(heart_data_copy$`No. Major Blood Vessels`)
heart_data_copy$`Thal Rate`<- as.factor(heart_data_copy$`Thal Rate`)


heart_data_copy$Condition <-factor(heart_data_copy$Condition,
                                   levels = c(0,1),
                                   labels = c("less chance of heart attack","more chance of heart attack"))

heart_data_copy$Sex <- factor(heart_data_copy$Sex,
                              levels = c(0,1),
                              labels = c("female","male"))
heart_data_copy$`Chest Pain Type` <- factor(heart_data_copy$`Chest Pain Type`,
                                            levels =c(0,1,2,3),
                                            labels = c("typical angina","atypical angina","non-anginal pain","asymptomatic"))
heart_data_copy$`Fasting Blood Sugar`<-factor(heart_data_copy$`Fasting Blood Sugar`,
                                              levels = c(0,1),
                                              labels = c("false","true"))
heart_data_copy$`Resting ECG`<- factor(heart_data_copy$`Resting ECG`,
                                       levels = c(0,1,2),
                                       labels = c("normal","having ST-T wave abnormality","showing probable or definite left ventricular hypertrophy"))

heart_data_copy$`Exercise Induced Angina`<-factor(heart_data_copy$`Exercise Induced Angina`,
                                                  levels = c(0,1),
                                                  labels = c("no","yes"))


Sex_plot <- ggplot(heart_data_copy, aes(x = Sex, fill = Condition)) +
  geom_bar(position = "dodge") +
  labs(title = "Barplot of Condition by Sex") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()

Chest_plot <- ggplot(heart_data_copy, aes(x = `Chest Pain Type`, fill = Condition)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  labs(title = "Barplot of Condition by Chest Pain Type") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()

Sugar_plot <- ggplot(heart_data_copy, aes(x = `Fasting Blood Sugar`, fill = Condition)) +
  geom_bar(position = "dodge") +
  labs(title = "Barplot of Condition by Fasting Blood Sugar") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()

ECG_plot <- ggplot(heart_data_copy, aes(x = `Resting ECG`, fill = Condition)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, size = 8, hjust = 1)) +
  labs(title = "Barplot of Condition by Resting ECG") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()

Exercise_plot <- ggplot(heart_data_copy, aes(x = `Exercise Induced Angina`, fill = Condition)) +
  geom_bar(position = "dodge") +
  labs(title = "Barplot of Condition by Exercise Induced Angina") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()

Slope_plot <- ggplot(heart_data_copy, aes(x = `Slope`, fill = Condition)) +
  geom_bar(position = "dodge") +
  labs(title = "Barplot of Condition by Slope") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()

Vessels_plot <- ggplot(heart_data_copy, aes(x = `No. Major Blood Vessels`, fill = Condition)) +
  geom_bar(position = "dodge") +
  labs(title = "Barplot of Condition by No. Major Blood Vessels") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()

Thal_plot <- ggplot(heart_data_copy, aes(x = `Thal Rate`, fill = Condition)) +
  geom_bar(position = "dodge") +
  labs(title = "Barplot of Condition by Thal Rate") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()


# create numerical plot with condition

heart_data_copy$Age <- as.numeric(heart_data_copy$Age)
heart_data_copy$`Resting Blood Pressure` <- as.numeric(heart_data_copy$`Resting Blood Pressure`)
heart_data_copy$Cholesterol <- as.numeric(heart_data$Cholesterol)
heart_data_copy$`Max. Heart Rate` <- as.numeric(heart_data_copy$`Max. Heart Rate`)
heart_data_copy$`Previous Peak` <- as.numeric(heart_data_copy$`Previous Peak`)

Age_plot <- ggplot(heart_data_copy, aes(x = Age, fill = Condition)) +
  geom_density(alpha = 0.3) +
  labs(title = "Age Density Plot by Condition", x = "Age", y = "Density") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(legend.position = "top")

Pressure_plot <- ggplot(heart_data_copy, aes(x = `Resting Blood Pressure`, fill = Condition)) +
  geom_density(alpha = 0.3) +
  labs(title = "Resting Blood Pressure Density Plot by Condition", x = "Resting Blood Pressure", y = "Density") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(legend.position = "top")

Cholesterol_plot <- ggplot(heart_data_copy, aes(x = Cholesterol, fill = Condition)) +
  geom_density(alpha = 0.3) +
  labs(title = "Cholesterol Density Plot by Condition", x = "Cholesterol", y = "Density") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(legend.position = "top")

HeartRate_plot <- ggplot(heart_data_copy, aes(x = `Max. Heart Rate`, fill = Condition)) +
  geom_density(alpha = 0.3) +
  labs(title = "Max. Heart Rate Density Plot by Condition", x = "Max. Heart Rate", y = "Density") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(legend.position = "top")

Peak_plot <- ggplot(heart_data_copy, aes(x = `Previous Peak`, fill = Condition)) +
  geom_density(alpha = 0.3) +
  labs(title = "Previous Peak Density Plot by Condition", x = "Previous Peak", y = "Density") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(legend.position = "top")


# Naive Bayes Prediction
heart_pred <- read_csv('heart1.csv')
x = c('caa','cp','exng','slp','sex','chol','trtbps','age')
y = ('output')

nb_model <- naiveBayes(output ~ caa + cp + exng + slp + sex + chol + trtbps + age, data = heart_pred)


## UI Element
Header <- dashboardHeader(
  title = tagList(
    span(class = "logo-lg", "Heart Attack Prediction"), 
    img(
      src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQhzsL6WBvrs5Y6eQmBwCkuE4Y6UcYT27GkHHAdGkKbqHvrB7todeJqSs52aQ&s",
      style = "width: 35px"
    ),
    titleWidth = 350),
  dropdownMenu(headerText = "Our Linkedin", type = 'message',
               icon = icon("envelope"),
               messageItem(
                 from = "Rahmannuaji Satuhu",
                 message = "LinkedIn",
                 icon = icon("envelope"),
                 href = "https://www.linkedin.com/in/rahmannuajisatuhu/"
               ),
               messageItem(
                 from = "Andrew Putra Hartanto",
                 message = "LinkedIn",
                 icon = icon("envelope"),
                 href = "https://www.linkedin.com/in/andrew-putra-hartanto/"
               )
  )
)

sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("home",lib="glyphicon")),
    menuItem("Feature Analysis", tabName = "Feature", icon = icon("stats",lib="glyphicon")),
    menuItem("About Data", tabName = "Data", icon = icon("list-alt",lib = "glyphicon")),
    menuItem("Heart Check-Up", tabName = "Checkup", icon = icon("paste",lib = "glyphicon")),
    menuItem("Author", tabName = "Author", icon = icon("user",lib="glyphicon"))
  ))


body <- dashboardBody(
  tags$head(tags$style(HTML('* {font-family: "Montserrat"};'))),
  tags$head(tags$style(HTML('
      /* logo */
      .skin-blue .main-header .logo {
        background-color: #2C3E50;
        color: #ecf0f1;
        font-weight: bold;
      }

      /* logo when hovered */
      .skin-blue .main-header .logo:hover {
        background-color: #34495E;
      }

      /* navbar (rest of the header) */
      .skin-blue .main-header .navbar {
        background-color: #1ABC9C;
        color: #ecf0f1;
      }

      /* main sidebar */
      .skin-blue .main-sidebar {
        background-color: #34495E;
      }

      /* active selected tab in the sidebarmenu */
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
        background-color: #1ABC9C;
        color: #ecf0f1;
      }

      /* other links in the sidebarmenu */
      .skin-blue .main-sidebar .sidebar .sidebar-menu a {
        background-color: #34495E;
        color: #ecf0f1;
      }

      /* other links in the sidebarmenu when hovered */
      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
        background-color: #16A085;
      }

      /* toggle button when hovered */
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        background-color: #16A085;
      }

      /* body */
      .content-wrapper, .right-side {
        background-color: #ecf0f1;
      }

      /* Box styling */
      .box {
        border-top: 3px solid #1ABC9C;
      }
  '))),
  tabItems(
    tabItem(
      tabName = "Home",
      fluidRow(
        div(strong("Welcome to Heart Attack Analysis"),
            style="text-align: center;font-size: 250%"),
        br(),
        carousel(width = 12,
                 id = "mycarousel",
                 carouselItem(
                   tags$img(src = "https://media.licdn.com/dms/image/C4E12AQHA9DbIRW1Epw/article-cover_image-shrink_600_2000/0/1520177543135?e=2147483647&v=beta&t=3eVEJVp7pSAVqX_irmpF2rCdPzrhOMpegQ_W2Q6tY9s",
                            style ="width: 100%; height: auto;")
                 ),
                 carouselItem(
                   tags$img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQJURBeKD-rVR-vNKNSXHpZ1O7gjMHV6WQXFuPSl23qgYhADCIao8qacdc4BCnUWZuHNqA&usqp=CAU",
                            style ="width: 100%; height: auto;")
                 )
                 ,
                 carouselItem(
                   tags$img(src = "https://media.rs-jih.co.id/media-rsjih/jogja/news/img_K51IoxC.jpg",
                            style ="width: 100%; height: auto;")
                 ),
                 br()     
        ),
        div(h3(strong("Latar Belakang"),
               style="text-align: center;")),
        box(title = NULL,
            width=12,
            p("Penyakit kardiovaskular, terutama serangan jantung, merupakan salah satu 
                                penyebab utama kematian di seluruh dunia, yang menimbulkan tantangan kesehatan 
                                masyarakat yang signifikan. Penelitian ini mengeksplorasi hubungan yang kompleks antara 
                                berbagai indikator kesehatan dan potensi kontribusinya terhadap penyakit jantung dengan 
                                tujuan mengembangkan model prediksi yang akurat untuk deteksi dini risiko serangan jantung. 
                                Dataset yang digunakan mencakup berbagai faktor seperti usia, jenis kelamin, jenis nyeri dada, 
                                tekanan darah istirahat, kolesterol serum, gula darah puasa, dan hasil elektrokardiografi istirahat. 
                                Masing-masing fitur ini memberikan perspektif yang unik tentang profil kesehatan individu, menawarkan 
                                wawasan penting tentang berbagai atribut perkembangan penyakit jantung. Dengan menyertakan variabel yang 
                                mengindikasikan diagnosis medis penyakit kardiovaskular, data ini menjadi sangat berguna untuk prediksi 
                                yang ditargetkan untuk deteksi dan pencegahan dini. Strategi tradisional untuk mengenali penyakit 
                                  kardiovaskuler terutama bergantung pada analisis statistik faktor risiko seperti usia, jenis kelamin, 
                                  kadar kolesterol, tekanan darah tinggi, diabetes melitus, dan kebiasaan merokok.",style="text-align:justify;")
        ))
    ),
    tabItem(
      tabName="Feature",
      div(h2(strong("Feature Analysis"),
             style="text-align: center;")),
      br(),
      div(h5("Pada halaman ini kami akan menampilkan visualisasi dataset kami dengan beberapa kondisi,
                                 diantaranya visualisasi kategorik, numerik, matrik korelasi, dan summary data",
             style="text-align: center;")),
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fluidRow(selectInput(
            inputId = "Categorical",
            label = "Choose one categorical feature to display:",
            choices = c(
              "Sex"= 1,
              "Chest Pain Type"= 2 ,
              "Fasting Blood Sugar" = 3,
              "Resting Electrocardiographic Results"= 4,
              "Exercise Induced Angina" = 5,
              "Slope" = 6,
              "Number of Major Blood Vessels" =7,
              "Thal Rate" = 8
              
            ),
            selected = 1)
          ),
          
          fluidRow(selectInput(
            inputId = "Numerical",
            label = "Choose one numerical feature to display:",
            choices = c("Age" = 1,
                        "Resting Blood Pressure" =2,
                        "Cholesterol" =3,
                        "Max. Heart Rate"=4,
                        "Previous Peak" =5),
            selected = 1
            
            
          )
          )
        ),
        
        # Display the Plotly plot in the main panel
        
        mainPanel(width =9,
                  tabsetPanel(
                    tabPanel("Feature Plots Categorical",
                             fluidRow(plotlyOutput("Categorical_plot",height="300px"))
                    ),
                    tabPanel("Feature Plots Numeric",
                             fluidRow(plotlyOutput("Numerical_plot",height = "300px"))
                    ),
                    tabPanel("Correlation Matrix",
                             fluidRow(plotlyOutput("Correlation_Matrix_plot",height = "600px"))
                    ),
                    tabPanel("Data Summary",
                             fluidRow(htmlOutput("data_summary")))
                  )         
        )
      )
    ),
    tabItem(
      tabName = "Data",
      fluidRow(
        div(strong("Source Data"),
            style="text-align: center"),
        br(),
        div("Data yang digunakan pada penelitian ini merupakan 
                            data sekunder yang bersumber dari situs Kaggle.
                                  Data yang digunakan adalah data analisis dan prediksi serangan jantung.",
            style="text-align: center;"),
        br(),
        div(strong("Variable Data"),
            style="text-align: center"),
        br(),
        div(imageOutput("logo"),
            style="text-align: center;"),
        br(),
        br(),
        br(),
        div(strong("Dataset"),
            style="text-align: center;"),
        br(),
        div("Dataset ini berisi informasi tentang pasien dengan penyakit jantung. 
                                       Dataset ini mencakup informasi seperti usia, jenis kelamin, jenis nyeri dada, tekanan darah istirahat, 
                                       kadar kolesterol, kadar gula darah puasa, hasil elektrokardiografi istirahat, detak jantung maksimum 
                                       yang dicapai, angina yang diinduksi olahraga, oldpeak (depresi ST yang diinduksi oleh olahraga relatif 
                                       terhadap istirahat), kemiringan segmen ST latihan puncak, jumlah pembuluh darah utama yang diwarnai dengan 
                                       fluoroskopi, talasemia (sejenis kelainan darah), dan ada tidaknya penyakit jantung.",
            style="text-align: justify;"),
        br(),
        div(strong("Download Here")),
        # Button
        downloadButton("downloadData", "Download",
                       style="text-align: center;"),
        br(),
        DTOutput("data")
      )
    ),
    tabItem(
      tabName = "Checkup",
      sidebarLayout(
        sidebarPanel(
          numericInput("age", "Usia:", value = NA, min = 0, max = 120),
          selectInput("sex", "Jenis Kelamin:", choices = c("Pria" = 1, "Wanita" = 0)),
          numericInput("cp", "Tipe Nyeri Dada:", value = NA, min = 0, max = 3),
          numericInput("trtbps", "Tekanan Darah Istirahat:", value = NA, min = 0, max = 300),
          numericInput("chol", "Kolesterol:", value = NA, min = 0, max = 600),
          numericInput("exng", "Angina yang Diinduksi Latihan:", value = NA, min = 0, max = 1),
          numericInput("slp", "Slope dari Puncak ST:", value = NA, min = 0, max = 2),
          numericInput("caa", "Jumlah Pembuluh Darah Utama:", value = NA, min = 0, max = 4),
          actionButton("submit", "Submit")
        ),
        
        mainPanel(
          h3("Hasil Pemeriksaan Kesehatan Jantung"),
          verbatimTextOutput("result"),
          downloadButton("downloadReport", "Download Report"),
          br(),
          br(),
          h4("Petunjuk Pengisian:"),
          tags$ul(
            tags$li("Usia: Masukkan usia Anda dalam tahun."),
            tags$li("Jenis Kelamin: Pilih 'Pria' atau 'Wanita'."),
            tags$li("Tipe Nyeri Dada: Masukkan angka antara 0 hingga 3."),
            tags$li("Tekanan Darah Istirahat: Masukkan tekanan darah Anda dalam mmHg."),
            tags$li("Kolesterol: Masukkan kadar kolesterol Anda dalam mg/dl."),
            tags$li("Angina yang Diinduksi Latihan: Masukkan 1 jika ya, dan 0 jika tidak."),
            tags$li("Slope dari Puncak ST: Masukkan 0 untuk Upsloping, 1 untuk Flat, atau 2 untuk Downsloping."),
            tags$li("Jumlah Pembuluh Darah Utama: Masukkan jumlah pembuluh darah utama yang terpengaruh (0-4)."),
          )
          
        )
      )
    ),
    tabItem(
      tabName = "Author",
      fluidRow(
        div(h2(strong("HEART ATTACK ANALYSIS")), 
            style = "text-align: center;"),
        br(),
        div(h2("Author"), 
            style = "text-align: center;"),
        br()),
      fluidRow(
        box(
          width = 6,
          status = NULL,
          div(imageOutput("andrew"), 
              style = "text-align:center;",
              style = "margin-bottom:-180px;"),
          br(),
          div(strong("Andrew Putra Hartanto"), 
              style = "text-align:center;font-family: 'Montserrat';"),
          div(strong("5003211016"), 
              style = "text-align: center;font-family: 'Montserrat';"),
          br(),
          div(strong("For More Information"),
              style = "=font-family: 'Montserrat', sans-serif"),
          div("Contact Us",style = "font-family: 'Montserrat', sans-serif"),
          br(),
          tags$a(href="https://www.linkedin.com/in/andrew-putra-hartanto/",
                 target="_blank",
                 tags$img(src="https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png", height="16", style="margin-right:5px;"),
                 "LinkedIn"),
          br(),
          tags$a(href="https://www.instagram.com/andrew_p_h_26/",
                 target="_blank",
                 tags$img(src="https://upload.wikimedia.org/wikipedia/commons/a/a5/Instagram_icon.png", height="16", style="margin-right:5px;"),
                 "Instagram")
        ),
        box(
          width = 6,
          status = NULL,
          div(imageOutput("aji"), 
              style = "text-align:center;",
              style = "margin-bottom:-180px;"),
          br(),
          div(strong("Rahmannuaji Satuhu"), 
              style = "text-align:center;font-family: 'Montserrat', sans-serif"),
          div(strong("5003211125"), 
              style = "text-align:center;font-family: 'Montserrat', sans-serif"),
          br(),
          div(strong("For More Information"),
              style = "font-family: 'Montserrat', sans-serif"),
          div("Contact Us",style = "font-family: 'Montserrat', sans-serif"),
          br(),
          tags$a(href="https://www.linkedin.com/in/rahmannuajisatuhu/",
                 target="_blank",
                 tags$img(src="https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png", height="16", style="margin-right:5px;"),
                 "LinkedIn"),
          br(),
          tags$a(href="https://www.instagram.com/rahmannuaji.s/",
                 target="_blank",
                 tags$img(src="https://upload.wikimedia.org/wikipedia/commons/a/a5/Instagram_icon.png", height="16", style="margin-right:5px;"),
                 "Instagram")
        )),
      fluidRow(br(),
               div(h3(strong("Data Mining dan Visualisasi")), 
                   style = "text-align: center;"),
               div(("Departemen Statistika"), 
                   style = "text-align: center;"),
               div(("Fakultas Sains dan Analitika Data"), 
                   style = "text-align: center;"),
               div(("Institut Teknologi Sepuluh Nopember"), 
                   style = "text-align: center;"),
               div(("2024"), 
                   style = "text-align: center;")
      )
    )
  )
)

ui=dashboardPage(header = Header,
                 sidebar = sidebar,
                 body = body)


server <- function(input, output) {
  #    Create reactive Plotly plot for app
  library(caret)
  
  output$Categorical_plot <- renderPlotly({
    if(input$Categorical==1){
      Target_plot=Sex_plot
    }else if(input$Categorical==2){
      Target_plot=Chest_plot
    }else if(input$Categorical==3){
      Target_plot=Sugar_plot
    }else if(input$Categorical==4){
      Target_plot=ECG_plot
    }else if(input$Categorical==5){
      Target_plot=Exercise_plot
    }else if(input$Categorical==6){
      Target_plot=Slope_plot
    }else if(input$Categorical==7){
      Target_plot=Vessels_plot
    }else if(input$Categorical==8){
      Target_plot=Thal_plot
    }
    plotly_build(Target_plot)
  })
  output$Numerical_plot <- renderPlotly({
    if(input$Numerical==1){
      Num_plot=Age_plot
    }else if(input$Numerical==2){
      Num_plot=Pressure_plot
    }else if(input$Numerical==3){
      Num_plot=Cholesterol_plot
    }else if(input$Numerical==4){
      Num_plot=HeartRate_plot
    }else if(input$Numerical==5){
      Num_plot=Peak_plot
    }
    plotly_build(Num_plot)
  })
  
  output$Correlation_Matrix_plot <- renderPlotly({
    plotly_build(Correlation_matrix_plot)
  })
  
  output$data_summary <- renderUI({
    summarytools::view(summarytools::dfSummary(Numerical), method = "render")
  })
  
  output$logo<-renderImage({
    list(src="www/Dataset.jpg",
         height = 440,
         width = 290,
         align="center")
  },deleteFile = F)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("heart.csv", sep = "")
    },
    content = function(file) {
      write.csv(heart_data, file, row.names = FALSE)
    }
  )
  output$data <- renderDT({datatable(heart_data)})
  
  calculate_risk <- reactive({
    input$submit
    isolate({
      # Mengambil input dari pengguna
      input_data <- data.frame(
        caa = input$caa,
        cp = input$cp,
        exng = input$exng,
        slp = input$slp,
        sex = input$sex,
        chol = input$chol,
        trtbps = input$trtbps,
        age = input$age
      )
      
      # Menggunakan model Naive Bayes untuk memprediksi risiko
      prediction <- predict(nb_model, input_data)
      
      if (prediction == 1) {
        result <- "Risiko tinggi untuk penyakit jantung."
      } else {
        result <- "Risiko rendah untuk penyakit jantung."
      }
      return(result)
    })
  })
  
  output$result <- renderText({
    calculate_risk()
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("Heart_Health_Report_", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      result <- calculate_risk()
      input_data <- paste(
        "=======================================\n",
        "         Data Pemeriksaan Kesehatan Jantung\n",
        "=======================================\n\n",
        "Data yang Dimasukkan:\n",
        "---------------------------------------\n",
        "Usia                : ", input$age, "\n",
        "Jenis Kelamin       : ", ifelse(input$sex == 1, "Pria", "Wanita"), "\n",
        "Tipe Nyeri Dada     : ", input$cp, "\n",
        "Tekanan Darah Istirahat: ", input$trtbps, "\n",
        "Kolesterol          : ", input$chol, "\n",
        "Angina yang Diinduksi Latihan: ", input$exng, "\n",
        "Slope dari Puncak ST: ", input$slp, "\n",
        "Jumlah Pembuluh Darah Utama: ", input$caa, "\n",
        "---------------------------------------\n\n"
      )
      
      result_data <- paste(
        "=======================================\n",
        "                Hasil Pemeriksaan\n",
        "=======================================\n\n",
        "Hasil: ", result, "\n",
        "=======================================\n"
      )
      
      writeLines(c(input_data, result_data), file)
    }
  )
  
  output$andrew <- renderImage({
    list(src="www/andrew.jpg",height=200,width=150)
  },deleteFile = F)
  output$aji <- renderImage({
    list(src="www/aji.png",height=200,width=150)
  },deleteFile = F)
  
}

# Run the Code
shinyApp(ui = ui, server = server)



# Publish
#library(rsconnect)

#rsconnect::setAccountInfo(name='rahmannuaji', 
#                          token='', 
#                          secret='')
#rsconnect::deployApp("D:/Semester 6/Data Mining/EAS Data Mining/Final Project")
