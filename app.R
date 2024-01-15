library(shiny)
library(leaflet)
library(plotly)
library(tidycensus)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(gplots)
library(sf) 
devtools::install_github("ropensci/USAboundariesData")

library(USAboundaries)

#Read csv's for news data
nyt_data<- read_csv("nyt_teacher_mn_data.csv")
nyt_student<- read_csv("nyt_student_mn_data.csv")

mpr_student<- read_csv("mpr_student_mn_data.csv")
mpr_data_teacher<- read_csv("mpr_teacher_data.csv")

fox_teacher <- read_csv("fox_teacher.csv")
fox_student <- read_csv("fox_student.csv")

#Words being looked for in the articles
words_to_count <- c("union", "struggle", "tired", "money", "raise", "new", "poor", "lack", "happy", "joy", "fun", "want", "need")
student_words_to_count <- c("struggle", "tired", "poor", "lack", "shooting", "joy", "fun", "need")

# Function to count occurrences of multiple words in a given string
count_words <- function(text, words) {
  sapply(words, function(word) sum(grepl(word, tolower(text))))
}

# Apply the function to each abstract in the 'abstract' column for nyt articles
word_counts_summary <- nyt_data %>%
  rowwise() %>%
  mutate(word_counts = list(sapply(abstract, count_words, words = words_to_count))) %>%
  unnest_wider(word_counts) %>%
  summarise(across(where(is.numeric), sum)) %>%
  pivot_longer(everything(), names_to = "word", values_to = "total_count") %>%
  arrange(total_count) %>%
  mutate(word = factor(word, levels = word))

word_counts_summary <-word_counts_summary %>%
  filter(word != c("word_count") ) %>%
  filter(word != c("print_page") )

#student data nyt
student_word_counts_summary <- nyt_student %>%
  rowwise() %>%
  mutate(word_counts = list(sapply(abstract, count_words, words = student_words_to_count))) %>%
  unnest_wider(word_counts) %>%
  summarise(across(where(is.numeric), sum)) %>%
  pivot_longer(everything(), names_to = "word", values_to = "total_count") %>%
  arrange(total_count) %>%
  mutate(word = factor(word, levels = word))

student_word_counts_summary <-student_word_counts_summary %>%
  filter(word != c("word_count") ) %>%
  filter(word != c("print_page") )

# Apply the function to each abstract in the 'abstract' column for mpr articles
mpr_student_word_counts_summary <- mpr_student %>%
  rowwise() %>%
  mutate(word_counts = list(sapply(Headline, count_words, words = student_words_to_count))) %>%
  unnest_wider(word_counts) %>%
  summarise(across(where(is.numeric), sum)) %>%
  pivot_longer(everything(), names_to = "word", values_to = "total_count") %>%
  arrange(total_count) %>%
  mutate(word = factor(word, levels = word))

mpr_student_word_counts_summary <- mpr_student_word_counts_summary %>%
  filter(word != c("word_count")) %>%
  filter(word != c("print_page"))

##Student data
mpr_word_counts_summary <- mpr_data_teacher %>%
  rowwise() %>%
  mutate(word_counts = list(sapply(Headline, count_words, words = words_to_count))) %>%
  unnest_wider(word_counts) %>%
  summarise(across(where(is.numeric), sum)) %>%
  pivot_longer(everything(), names_to = "word", values_to = "total_count") %>%
  arrange(total_count) %>%
  mutate(word = factor(word, levels = word))

mpr_word_counts_summary <-mpr_word_counts_summary %>%
  filter(word != c("word_count") ) %>%
  filter(word != c("print_page") )

##fox data
fox_teacher <- fox_teacher %>%
  filter(Headline != "n/a")

fox_student <- fox_student %>%
  filter(Headline != "n/a")

word_counts_foxT<- fox_teacher %>%
  rowwise() %>%
  mutate(word_counts = list(sapply(Headline, count_words, words = words_to_count))) %>%
  unnest_wider(word_counts) %>%
  summarise(across(where(is.numeric), sum)) %>%
  pivot_longer(everything(), names_to = "word", values_to = "total_count") %>%
  arrange(total_count) %>%
  mutate(word = factor(word, levels = word))

word_counts_foxT <-word_counts_foxT %>%
  filter(word != c("word_count") ) %>%
  filter(word != c("print_page") )

student_word_counts_foxS <- fox_student %>%
  rowwise() %>%
  mutate(word_counts = list(sapply(Headline, count_words, words = student_words_to_count))) %>%
  unnest_wider(word_counts) %>%
  summarise(across(where(is.numeric), sum)) %>%
  pivot_longer(everything(), names_to = "word", values_to = "total_count") %>%
  arrange(total_count) %>%
  mutate(word = factor(word, levels = word))

student_word_counts_foxS <-student_word_counts_foxS %>%
  filter(word != c("word_count")) %>%
  filter(word != c("print_page"))



#Ternary Graph 
##Teachers data

word_counts_foxT$news_org <- 'Fox News'
mpr_word_counts_summary$news_org <- 'MPR'
word_counts_summary$news_org <- 'New York Times'

teacher_news <- bind_rows(word_counts_foxT, mpr_word_counts_summary, word_counts_summary)

df_wider_teacher <- teacher_news%>%
  pivot_wider(names_from = news_org, values_from = total_count)

df_wider_teacher_prop<- df_wider_teacher %>%
  mutate(Fox_news_prop = df_wider_teacher$`Fox News` / 350) %>%
  mutate(MPR_prop = df_wider_teacher$MPR / 329) %>%
  mutate(NYT_prop = df_wider_teacher$`New York Times` / 220) 


df_wider_teacher_prop$total_count <- rowSums(df_wider_teacher_prop[, c("Fox News", "MPR", "New York Times")])


# axis layout
axis <- function(title) {
  list(
    title = title,
    titlefont = list(
      size = 12
    ),
    tickfont = list(
      size = 10
    ),
    tickcolor = 'rgba(0,0,0,0)',
    ticklen = 2.5
  )
}


## students 

student_word_counts_foxS$news_org <- 'Fox News'
mpr_student_word_counts_summary$news_org <- 'MPR'
student_word_counts_summary$news_org <- 'New York Times'

student_news <- bind_rows(student_word_counts_foxS, mpr_student_word_counts_summary, student_word_counts_summary)

df_wider_student <- student_news%>%
  pivot_wider(names_from = news_org, values_from = total_count)

df_wider_student_prop<- df_wider_student %>%
  mutate(Fox_news_prop = df_wider_student$`Fox News` / 350) %>%
  mutate(MPR_prop = df_wider_student$MPR / 330) %>%
  mutate(NYT_prop = df_wider_student$`New York Times` / 220)


df_wider_student_prop$total_count <- rowSums(df_wider_student_prop[, c("Fox News", "MPR", "New York Times")])

#Mn data

df_wider_student_prop$total_count <- rowSums(df_wider_student_prop[, c("Fox News", "MPR", "New York Times")])


#Mn data
mn_cities <- sf::read_sf("Data/shp_loc_pop_centers")

#mn_cities <- sf::read_sf("../Data/shp_loc_pop_centers") #shp file/folder -> had to change this to publish
mn_cities <- sf::read_sf("Data/shp_loc_pop_centers")

mn_counties <- us_counties(resolution = "high", states = "Minnesota")
names_counties <- names(mn_counties)
names(mn_counties)[names_counties == 'state_name'] <- c("state_name1", "state_name2")

#race data from census
race_variables <- c(
  "B02001_002",  # White alone
  "B02001_003",  # Black or African American alone
  "B02001_004",  # American Indian and Alaska Native alone
  "B02001_005",  # Asian alone
  "B03002_012"  # Hispanic or Latino
)

race_data_county <- get_acs(
  year = 2020,
  state = "MN",
  geography = "county",
  variables = race_variables,
  output = 'wide',
  geometry = TRUE
) %>%
  rename(
    "White" = "B02001_002E",
    "Black" = "B02001_003E",
    "Indigenous" = "B02001_004E",
    "Asian" = "B02001_005E",
    "Latino" = "B03002_012E"
  ) %>%
  select(-contains("_"))

race_per_county <- race_data_county %>%
  pivot_longer(cols = c(`White`, `Black`, `Indigenous`, `Asian`, `Latino`), names_to = "race", values_to = "total") 

race_per_county$NAME <- sub(" County, Minnesota", "", race_per_county$NAME)

#overall population
population_variable <- "B01003_001"

population_data_county <- get_acs(
  year = 2020,
  state = "MN",
  geography = "county",
  variables = population_variable,
  output = 'wide',
  geometry = TRUE
) %>%
  rename(
    "Total_Population" = "B01003_001E"
  )

population_data_county <- population_data_county %>%
  select(GEOID, NAME, Total_Population, geometry)


MN_SCHOOL_PROGRAMS<-read_csv("school_program_locations.csv")
states_map <- map_data("state")

MN_SCHOOL_PROGRAMS <- MN_SCHOOL_PROGRAMS %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% #changes from a df to sf(spatial  feature)
  st_transform(crs = st_crs(mn_counties)) #changing crs



filtered_df <- MN_SCHOOL_PROGRAMS[
  !grepl('library', MN_SCHOOL_PROGRAMS$GISNAME, ignore.case = TRUE) &
    !grepl('District', MN_SCHOOL_PROGRAMS$GISNAME, ignore.case = TRUE) &
    !grepl('Program', MN_SCHOOL_PROGRAMS$GISNAME, ignore.case = TRUE),
]

school_count_by_county <- filtered_df %>%
  group_by(COUNTYNAME) %>%
  summarize(School_Count = n())

county_names <- race_per_county %>%
  pull(NAME)%>%
  unique()

population_data_county <- st_transform(population_data_county, crs = st_crs(school_count_by_county))

result <- st_join(population_data_county, school_count_by_county)

result<-result%>%
  mutate(school_per_population = floor(Total_Population / School_Count))


budget <- read_csv("budget.csv")
full_budget <- read_csv("full_budget.csv")

budget <- budget%>%
  mutate(`District` = str_replace_all(`School District`, 'District', ''))%>%
  mutate(`District` = str_replace_all(`District`, 'Dist', ''))%>%
  mutate(`District` = str_replace_all(`District`, 'Dist.', ''))%>%
  mutate(`District` = str_replace_all(`District`, 'Schools', 'School'))%>%
  mutate(`District` = trimws(District))%>%
  mutate(`District` = str_to_upper(`District`))

budget <- budget%>%
  mutate(strip_name = str_replace_all(`District`, 'SCHOOL', ''))%>%
  mutate(strip_name = str_replace_all(`strip_name`, 'PUBLIC', ''))%>%
  mutate(strip_name = str_replace_all(`strip_name`, 'AREA', ''))%>%
  mutate(strip_name = str_replace_all(`strip_name`, 'ST.', 'ST'))%>%
  mutate(strip_name = trimws(strip_name))

full_budget <- full_budget%>%
  mutate(`District` = str_replace_all(`District`, 'DISTRICT', ''))%>%
  mutate(`District` = str_replace_all(`District`, 'DIST', ''))%>%
  mutate(`District` = str_replace_all(`District`, 'DIST.', ''))%>%
  mutate(`District` = str_replace_all(`District`, 'DIS', ''))%>%
  mutate(`District` = str_replace_all(`District`, 'SCHOOL .', 'SCHOOL'))%>%
  mutate(`District` = str_replace_all(`District`, 'SCHOOLCADEMY', 'SCHOOL'))%>%
  mutate(`District` = trimws(District))

full_budget<- full_budget%>%
  mutate(strip_name = str_replace_all(District, 'SCHOOL', ''))%>%
  mutate(strip_name = str_replace_all(`strip_name`, 'PUBLIC', ''))%>%
  mutate(strip_name = str_replace_all(`strip_name`, 'ACADEMY', ''))%>%
  mutate(strip_name = str_replace_all(`strip_name`, 'AREA', ''))%>%
  mutate(strip_name = str_replace_all(`strip_name`, 'ST.', 'ST'))%>%
  mutate(strip_name = trimws(strip_name))

total_budget <- left_join(full_budget, budget, by = "strip_name")

total_budget <- total_budget%>%
  subset(select = c("District.y", "School", "City", "Grade Level", "Expenditures Per Pupil.x", "Income", "strip_name"))

total_budget <- total_budget%>%
  filter(!is.na(Income))

mn_cities_new <- mn_cities%>%
  mutate(`City` = str_to_upper(`Name`))

county_data <- left_join(total_budget, mn_cities_new, by = "City")

county_data%>%
  subset(select = c("District.y", "School", "City", "Grade Level", "Expenditures Per Pupil.x", "Income", "County", "Population", "strip_name", "GNIS", "geometry"))

county_data <- county_data%>%
  filter(!is.na(School))

new_data <- county_data%>%
  group_by(County)%>%
  filter(!is.na(County))%>%
  summarize(TotalIncome = sum(`Income`),
            AverageIncome = mean(`Income`),
            Total_Expenditures = sum(`Expenditures Per Pupil.x`),
            Average_Expenditures = mean(`Expenditures Per Pupil.x`))

full_budget <- full_budget%>%
  filter(!is.na(School))

HS_budget <- full_budget%>%
  filter(`Grade Level` == "High")%>%
  filter(strip_name != "LAKE BENTON")

M_budget <- full_budget%>%
  filter(`Grade Level` == "Middle")

E_budget <- full_budget%>%
  filter(`Grade Level` == "Elementary")

PK_budget <- full_budget%>%
  filter(`Grade Level` == "Prekindergarten")%>%
  filter(strip_name != "RED LAKE COUNTY CENTRAL")

O_budget <- full_budget%>%
  filter(`Grade Level` == "Other")

HS_total = sum(HS_budget$`Expenditures Per Pupil`)
M_total = sum(M_budget$`Expenditures Per Pupil`)
E_total = sum(E_budget$`Expenditures Per Pupil`)
PK_total = sum(PK_budget$`Expenditures Per Pupil`)
O_total = sum(O_budget$`Expenditures Per Pupil`)
HS_avg = HS_total/746
M_avg = M_total/292
E_avg = E_total/946
PK_avg = PK_total/121
O_avg = O_total/82

School_data <- data.frame(
  schooling_level = c("High", "Middle", "Elementary", "Prekindergarten", "Other"),
  expenditure_per_pupil = c(16026.91, 12272.47, 13515.13, 17583.4, 31201.72)
)

ui <- fluidPage(
   tags$head(
    tags$style(HTML("
      body {
        background-color: #f0f0f0;
         color: white;
          font-family: 'Times New Roman', Times, serif;

      }
      .section {
        padding: 20px;
        margin: 20px;
        background-color: white;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
        font-family: 'Times New Roman', Times, serif;

      }
    "))
  ),
  navbarPage(
    title="What does a classroom look like in the state of Minnesota?",
    
    tabPanel("Diversity and population in schools",
             sidebarLayout(
               sidebarPanel(
                 width = 3,  # Adjust the width of the sidebar panel
                 selectInput(inputId = "county_choice",
                             label = "Select a County: ",
                             choices = county_names),
                 div(
                   p(HTML("<strong>Purpose:</strong>")),
                   
                   p("We want to explore learning environments for students in the state of Minnesota. 
                   We consider learning environments and schools to be beyond the common public, charter, 
                   and private schools, but also online schooling and learning centers. Our site is separated into three sections. The first is 
                   what a classroom in general looks like from diversity to quantity of schools in a county. Our second section is looking into the news
                   and what is being discussed. Our third section is looking into funding in schools. We hope that parents looking into different schools
                    can use this as a guide of where their children should go to school, but also for school administrators to see where they could focus
                     their power in their community. "),
                   
                   p(HTML("<strong>---------------</strong>")),
                   p("This tab allows one to explore basic parts of classrooms, such as diversity and grade ranges. 
                     Change the county with the dropdown box to explore different demographics in a different county. "),

                   p(HTML("<strong>Application authors:</strong>")),
                   
                   p("Alicia Severiano Perez"),
                   p("Michael Nadeau"),

                   p(HTML("<strong>Data sources:</strong>")),
                   p("Census"),
                   p("Minnesota Department of Education")
                 ),
                 style = "background-color: #4F5175  ;"  
               ),
               
               mainPanel(
                 fluidRow(
                   column(10, leafletOutput("countyMap")),
                   column(2,
                          HTML(paste(
                            "<div style='background-color: #f0f0f0; color:  #6D6F8F; padding: 10px; border-radius: 0px;font-size: 15px;'>",
                            "<strong>County Map:</strong><br>",
                            "The State of Minnesota Map shows the number of schools per county and estimated overall population per school. 
                            We qualify a school to either be private, public, and charter, as well as online school Pre-K centers and such.",
                            "</div>"
                          )))
                   ),
                fluidRow(
                   column(6, plotlyOutput("gradeRangeBarPlot")),
                   column(6, plotlyOutput("barPlot"))
                 ), 
                 
                 fluidRow(
                   column(7,
                          HTML(paste(
                            "<div style='background-color: #f0f0f0; color:  #6D6F8F; padding: 10px; border-radius: 5px;font-size: 16px; text-align: center;'>",
                            "<strong>Grade Ranges</strong><br>",
                            "Pre-k = Pre School | EC = Early Childhood<br>",
                            "The numbers symbolize grade | Ex: 1 = 1st grade <br>",
                            "</div>"
                          ))),
                   column(5, HTML(paste(
                     "<div style='background-color: #f0f0f0; color: #545679; padding: 10px; border-radius: 5px; font-size: 16px; text-align: center;'>",
                     "<strong>Race & Ethnicity</strong><br>",
                     "The race divesity is specfic for the community/county overall population, not specfic to studentsl<br>",
                     "</div>"
                   )))
                 ),
                 
                 fluidRow(
                   column(6, plotlyOutput("magnentBarPlot")),
                   column(6, plotlyOutput("privOrPubBarPlot"))
                 ),
                 fluidRow(
                   column(6,
                          HTML(paste(
                            "<div style='background-color: #f0f0f0; color: #545679; padding: 10px; border-radius: 5px; font-size: 16px; text-align: center;'>",
                            "<strong>Magnent is defined by the U.S dep of Education as</strong><br>",
                            "a public elementary school, public secondary school, public elementary education center, or public secondary education center that offers a special
                            curriculum capable of attracting substantial numbers of students of different racial backgrounds<br>",
                            "</div>"
                          ))),
                   
                   column(2,
                          HTML(paste(
                            "<div style='background-color: #f0f0f0; color: #f0f0f0; padding: 10px; border-radius: 5px; font-size: 14px ;text-align: center;'>",
                            " ",
                            "</div>"
                          ))),
                   column(4, HTML(paste(
                     "<div style='background-color: #f0f0f0; color: #3D3F65; padding: 10px; border-radius: 5px; font-size: 16px;text-align: center;'>",
                     "<strong>Types of Schools</strong><br>",
                     "We consider beyond public and private, as we note charter and online school<br>",
                     " ",
                     
                     "</div>"
                   )))
                 )
               )
             )
          ),
    tabPanel("Student and Teachers in articles",
             sidebarLayout(
               sidebarPanel(
                 width = 3,  # Adjust the width of the sidebar panel
                 
                 div(
                   p(HTML("<strong>Explanation:</strong>")),
                   
                   p("For this section of our website, we decided to look into three different news organizations
                   (Fox News, Minnesota Public Radio, and New York Times) and explore the articles associated with Teachers Minnesota and Students Minnesota. We hope this
                   will give us insights into what is needed and being advocated for. Thus our question throughout this section in terms of 
                   teachers and students in Minnesota, what is being continuously asked for in the media?
                   Click on the Ternary graphs to explore what words are used most by different news organizations"),
                  
                    p(HTML("<strong>-----------</strong>")),
                   p("The articles reviewed for this section have no time range, so included in the data
                     is articles from back then like 1980 to as of right now (Dec 2023) "),
                   

                   p(HTML("<strong>Data sources: </strong>")),
                   
                   p("New York Times API"),
                   p("Fox 9"),
                   p("Minnesota Public Radio")
                   
                 ),
                  style = "background-color: #4F5175  ;"  
               ),
               
               mainPanel(
                 h2("Word Occurrences related to 'Minnesota Teachers'",
                    align = "center",
                    style = "color: #6D6F8F; font-family: 'Times New Roman';"
                 ) , 
                 fluidRow(
                   column(12, plotlyOutput("teacherTernary"))
                 ),
                 h5("The size of the circles represents the sum of times a word has been used across all articles.",
                    align = "center",
                    style = "color: #6D6F8F; font-family: 'Times New Roman';"
                 ) , 
                 h5("The location on the triangle represents who used the word more across all three news organizations",
                    align = "center",
                    style = "color: #6D6F8F; font-family: 'Times New Roman';"
                 ) , 
                 
                 fluidRow(
                   column(4, plotlyOutput("teacherNYTBargraph")),
                   column(4, plotlyOutput("teacherMPRBargraph")),
                   column(4, plotlyOutput("teacherFOXBargraph"))
                 ),
                 h2("Word Occurrences related to 'Minnesota Students'",
                    align = "center",
                    style = "color: #6D6F8F; font-family: 'Times New Roman';"
                 ) , 
                 fluidRow(
                   column(12, plotlyOutput("studentTernary"))
                 ),
                 h5("The size of the circles represents the sum of times a word has been used across all articles.",
                    align = "center",
                    style = "color: #6D6F8F; font-family: 'Times New Roman';"
                 ) , 
                 h5("The location on the triangle represents who used the word more across all three news organizations",
                    align = "center",
                    style = "color: #6D6F8F; font-family: 'Times New Roman';"
                 ) , 

                 fluidRow(
                   column(4, plotlyOutput("studentNYTBargraph")),
                   column(4, plotlyOutput("studentMPRBargraph")),
                   column(4, plotlyOutput("studentFOXBargraph"))
                 )
               )
             )
          ),    
    tabPanel("Funding and impacts",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,  # Adjust the width of the sidebar panel
                       div(
                       p(HTML("<strong>Purpose:</strong>")),
                       p("In this aspect of our project we wanted to look at the spending of each school district 
                   and factors that may affect it."),
                       
                       p(HTML("<strong>Data Sources:</strong>")),
                       
                       p("Office of Elementary & Secondary Education"),
                       p("WalletHub")
                     ),
                 style = "background-color: #4F5175;"  
                 ),
                 mainPanel(
                   fluidRow(
                     column(12, plotlyOutput("spending_scatter"))),
                   h5("A look at the comparison of spending per student with the average income of a given school district.",
                      align = "center",
                      style = "color: #6D6F8F; font-family: 'Times New Roman';"
                   ) , 
                   h5("We can see that lower income school districts do a good job of providing more help",
                      align = "center",
                      style = "color: #6D6F8F; font-family: 'Times New Roman';"
                   ) , 
                   h5(" and money for students who may need a bit more help.",
                      align = "center",
                      style = "color: #6D6F8F; font-family: 'Times New Roman';"
                   ) , 
                   fluidRow(
                     column(12, plotlyOutput("grade_bar"))),
                   h5("This graph looks at how much each school type spends per student.",
                      align = "center",
                      style = "color: #6D6F8F; font-family: 'Times New Roman';"
                   ) , 
                   h5("The “Other” category consists of special education programs, learning centers for disabled children,",
                      align = "center",
                      style = "color: #6D6F8F; font-family: 'Times New Roman';"
                   ) , 
                   h5("after school programs, and other specialized academies or programs.",
                      align = "center",
                      style = "color: #6D6F8F; font-family: 'Times New Roman';"
                   ) 
        )
      )
    )
  )
)

# Define a custom CSS style to make the legend smaller
new_palette <- colorBin("Purples", domain = result$school_per_population)

server <- function(input, output) {
  
  output$countyMap <- renderLeaflet({
    leaflet() %>%
      addPolygons(
        data = mn_counties,
        fill = TRUE,
        fillColor = ~new_palette(result$school_per_population),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        layerId = ~name,
        popup = ~paste(name, "<br> School Count: ", result$School_Count,"<br> Population per School: ", result$school_per_population,  sep = "")
      ) %>%
      addLegend(pal = new_palette, 
                values = result$school_per_population, title = 'Population per school')
  })

  output$gradeRangeBarPlot <- renderPlotly({
    p1<- filtered_df %>%
      filter(COUNTYNAME == input$county_choice) %>%
      count(GRADERANGE) %>%
      filter(!is.na(GRADERANGE))%>%
      mutate(row_rank = row_number()) %>%
      mutate(show_top = ifelse(input$county_choice %in% c("Ramsey", "Hennepin", "Olmsted", "Anoka", "Scott", "Washington", "Dakota", "Wright", "St Louis"), TRUE, FALSE)) %>%
      filter((show_top & row_rank <= 5) | (!show_top & row_rank <= 10)) %>%
      ggplot(aes(y = GRADERANGE, x = n))+
      geom_col(fill= "#6D6F8F", colour="black")+
      theme_classic()+
      theme(text = element_text(family = "Times New Roman"),
            panel.background = element_rect(fill = "#f0f0f0"),
            plot.background = element_rect(fill = "#f0f0f0")) +
      labs(x = "Grade Range", y="Total", title="Grade Range")
    
    ggplotly(p1)
  })
  
  output$barPlot <- renderPlotly({
    p2<- race_per_county %>%
      filter(NAME == input$county_choice) %>%
      ggplot(aes(x=race,y = total))+
      theme_classic()+
      labs(title = "Race/ethnicity", y= "Population", x="Race/ethnicity")+
      theme(text = element_text(family = "Times New Roman"),
            panel.background = element_rect(fill = "#f0f0f0"),
            plot.background = element_rect(fill = "#f0f0f0")) +
      
      geom_col(fill="#545679", colour="black")
    
    ggplotly(p2)
    })
  
  output$privOrPubBarPlot <- renderPlotly({
    
    p3<- filtered_df %>%
      filter(COUNTYNAME == input$county_choice) %>%
      count(PUBPRIV) %>%
      ggplot(aes(y = PUBPRIV, x = n))+
      geom_col(fill="#3D3F65", colour="black")+
      theme_classic()+
      theme(text = element_text(family = "Times New Roman"),
            panel.background = element_rect(fill = "#f0f0f0"),
            plot.background = element_rect(fill = "#f0f0f0")) +
      labs(y = " ", x="Total", title="Type of school")
    
    ggplotly(p3)
  })
  
  output$magnentBarPlot <- renderPlotly({
    
    p4<- filtered_df %>%
      filter(COUNTYNAME == input$county_choice) %>%
      count(MAGNET) %>%
      ggplot(aes(y = MAGNET, x = n))+
      geom_col(fill="#545679", colour="black")+
      theme_classic()+
      theme(text = element_text(family = "Times New Roman"),
            panel.background = element_rect(fill = "#f0f0f0"),
            plot.background = element_rect(fill = "#f0f0f0")) +
      labs(y = " ", x="Total", title="Magnent School?")
    
    ggplotly(p4)
  })
  
  output$teacherTernary <- renderPlotly({
    fig <- df_wider_teacher_prop %>% plot_ly()
    fig <- fig %>% add_trace(
      type = 'scatterternary',
      mode = 'markers',
      a = ~Fox_news_prop,
      b = ~MPR_prop,
      c = ~NYT_prop,
      text = ~word,
      marker = list(
        symbol = 100,
        color = '#6D6F8F',
        size = ~total_count,
        line = list('width' = 1)
      )
    )
    fig <- fig %>% layout(
      ternary = list(
        sum = 1,
        aaxis = axis('Fox 9'),  
        baxis = axis('MPR'),   
        caxis = axis('NYT')     
      ),
      paper_bgcolor = '#f0f0f0'  # Set the background color
      
    )
    fig
  })
  
  output$teacherNYTBargraph <- renderPlotly({
    
    p5<- word_counts_summary %>%
      ggplot(aes(y = word, x = total_count)) +
      geom_bar(stat = "identity", fill = "#3D3F65", colour="darkgrey") +
      labs(title = "New York Times", y = "Words", x = "Total Count")+
      theme_classic()+
      theme(plot.title = element_text(face = "bold", vjust = 0.01, size=10),
            axis.text.x=element_blank(),
            axis.title=element_blank(),
            axis.line.y = element_line(NULL),
            axis.line.x = element_line(NULL),
            axis.ticks.x=element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#f0f0f0"),
            plot.background = element_rect(fill = "#f0f0f0"),
            text = element_text(family = "Times New Roman")) 
    ggplotly(p5)
  })
  
    output$studentNYTBargraph <- renderPlotly({
      
    p6<- student_word_counts_summary %>%
      ggplot(aes(y = word, x = total_count)) +
      geom_bar(stat = "identity", fill = "#6D6F8F", colour="darkgrey") +
      labs(title = "New York Times", y = "Words", x = "Total Count")+
      theme_classic()+
      theme(plot.title = element_text(face = "bold", vjust = 0.01, size=10),
            plot.subtitle =  element_text(face = "bold", vjust = 0.01, size=10),
            axis.text.x=element_blank(),
            axis.title=element_blank(),
            axis.line.y = element_line(size = 0.0),
            axis.line.x = element_line(size = 0.0),
            axis.ticks.x=element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#f0f0f0"),
            plot.background = element_rect(fill = "#f0f0f0"),
            text = element_text(family = "Times New Roman")) 
    ggplotly(p6)
  })
  
  output$teacherMPRBargraph <- renderPlotly({
    
    p7<- mpr_word_counts_summary %>%
      ggplot(aes(y = word, x = total_count)) +
      geom_bar(stat = "identity", fill = "#2D2F53", colour="darkgrey") +
      labs(title = "Minnesota Public Radio", y = "Words", x = "Total Count")+
      theme_classic()+
      theme(plot.title = element_text(face = "bold", vjust = 0.01, size=10),
            axis.text.x=element_blank(),
            axis.title=element_blank(),
            axis.line.y = element_line(NULL),
            axis.line.x = element_line(NULL),
            axis.ticks.x=element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#f0f0f0"),
            plot.background = element_rect(fill = "#f0f0f0"),
            text = element_text(family = "Times New Roman")) 
    ggplotly(p7)
  })
  
  output$studentTernary <- renderPlotly({
    fig <- df_wider_student_prop %>% plot_ly()
    fig <- fig %>% add_trace(
      type = 'scatterternary',
      mode = 'markers',
      a = ~Fox_news_prop,
      b = ~MPR_prop,
      c = ~NYT_prop,
      text = ~word,
      marker = list(
        symbol = 100,
        color = '#6D6F8F',
        size = ~total_count,
        line = list('width' = 1)
      )
    )
    fig <- fig %>% layout(
      ternary = list(
        sum = 1,
        aaxis = axis('Fox 9'),  
        baxis = axis('MPR'),   
        caxis = axis('NYT')     
      ),
      paper_bgcolor = '#f0f0f0'  # Set the background color
      
    )
    fig
  })
  
  
  output$studentMPRBargraph <- renderPlotly({
    
    p8<- mpr_student_word_counts_summary %>%
      ggplot(aes(y = word, x = total_count)) +
      geom_bar(stat = "identity", fill = "#6D6F8F", colour="darkgrey") +
      labs(title = "Minnesota Public Radio", y = "Words", x = "Total Count")+
      theme_classic()+
      theme(plot.title = element_text(face = "bold", vjust = 0.01, size=10),
            axis.text.x=element_blank(),
            axis.title=element_blank(),
            axis.line.y = element_line(NULL),
            axis.line.x = element_line(NULL),
            axis.ticks.x=element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#f0f0f0"),
            plot.background = element_rect(fill = "#f0f0f0"),
            text = element_text(family = "Times New Roman")) 
    ggplotly(p8)
  })
  
  output$teacherFOXBargraph <- renderPlotly({
    
    p9<- word_counts_foxT %>%
      ggplot(aes(y = word, x = total_count)) +
      geom_bar(stat = "identity", fill = "#2D2F53", colour="darkgrey") +
      labs(title = "Fox News", y = "Words", x = "Total Count")+
      theme_classic()+
      theme(plot.title = element_text(face = "bold", vjust = 0.01, size=10),
            axis.title=element_blank(),
            axis.line.y = element_line(NULL),
            axis.line.x = element_line(NULL),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#f0f0f0"),
            plot.background = element_rect(fill = "#f0f0f0"),
            text = element_text(family = "Times New Roman")) 
    ggplotly(p9)
  })
  
  output$studentFOXBargraph <- renderPlotly({
    
    p10<- student_word_counts_foxS %>%
      ggplot(aes(y = word, x = total_count)) +
      geom_bar(stat = "identity", fill = "#6D6F8F", colour="darkgrey") +
      labs(title = "Fox News", y = "Words", x = "Total Count")+
      theme_classic()+
      theme(plot.title = element_text(face = "bold", vjust = 0.01, size=10),
            axis.text.x=element_blank(),
            axis.title=element_blank(),
            axis.line.y = element_line(NULL),
            axis.ticks.x=element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#f0f0f0"),
            plot.background = element_rect(fill = "#f0f0f0"),
            text = element_text(family = "Times New Roman")) 
    ggplotly(p10)
  })
  
  output$spending_scatter <- renderPlotly({
    
    p11 <- budget %>%
      ggplot(aes(x = as.numeric(Income), y = `Expenditures Per Pupil`)) +
      geom_point(alpha = 0.7) +
      scale_x_continuous(limits = c(30000,145000), breaks = seq(30000,145000, by = 23000))+
      scale_y_continuous(limits = c(9000, 30000), breaks = seq(9000,30000, by = 2000))+ 
      geom_smooth(color = "#727498") +  # Set the line color to purple
      labs(title = "Expenditures Per Pupil vs. Income",
           x = "Income",
           y = "Expenditures Per Pupil")+
      theme(plot.title = element_text(face = "bold", vjust = 0.01, size=10),
            axis.line.y = element_line(NULL),
            axis.line.x = element_line(NULL),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "#f0f0f0"),
            plot.background = element_rect(fill = "#f0f0f0"),
            text = element_text(family = "Times New Roman")) 
      
    ggplotly(p11)
  })
  
  output$grade_bar <- renderPlotly({
    
    p12 <- School_data %>%
      mutate(schooling_level = fct_relevel(schooling_level, "Prekindergarten", "Elementary", "Middle", "High", "Other")) %>%
      ggplot(aes(x = schooling_level, y = expenditure_per_pupil)) +
      geom_bar(stat = "identity", fill = "#6D6F8F") +
      labs(title = "Spending Per Student by School Level",
           x = "School Level",
           y = "Spending Per Student")+
      theme(plot.title = element_text(face = "bold", vjust = 0.01, size=10),
            panel.grid = element_blank(),
            axis.line.y = element_line(NULL),
            axis.line.x = element_line(NULL),
            panel.background = element_rect(fill = "#f0f0f0"),
            plot.background = element_rect(fill = "#f0f0f0"),
            text = element_text(family = "Times New Roman")) 
    ggplotly(p12)
  })
}

shinyApp(ui = ui, server = server)