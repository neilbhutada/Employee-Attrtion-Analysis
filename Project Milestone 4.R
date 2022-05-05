library(shiny)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(shinydashboard)
library(data.table)
library(mltools)
library(ggridges)
library(viridis)
library(DT)
library(dashboardthemes)
library(fresh)
library(tidyr)
library(magrittr)
library(plotly)
library(readr)
library(dplyr)
library(lmtest)
library(randomForest)
library(tree)
library(datasets)
library(caret)
library(rpart)
library(rpart.plot)
library(DALEX)
library(scales)
library(shinycssloaders)
library(stringr)

###Pre-processing###

job <- read_csv("https://github.com/hbnarayanan/stat479finalproject/raw/main/hr_analytics.csv")
categorical <- c("BusinessTravel", "Department", "EducationField", "Gender", "JobRole",
                 "MaritalStatus", "OverTime", "Education", "EnvironmentSatisfaction", 
                 "JobInvolvement", "JobLevel", "JobSatisfaction", "PerformanceRating",
                 "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance")      

col_names <- names(job[, categorical])
col_names <- c(col_names, "Attrition")
job[col_names] <- lapply(job[col_names], factor)
numeric <- c("Age", "DailyRate", "DistanceFromHome", "HourlyRate", "MonthlyIncome", "MonthlyRate", 
             "NumCompaniesWorked", "PercentSalaryHike", "TotalWorkingYears", "TrainingTimesLastYear",
             "YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager")


## PCA 

non_useful <- c("EmployeeCount", "StandardHours", "EmployeeNumber", "DailyRate", "DistanceFromHome",
                "Education", "JobInvolvement", "JobSatisfaction", "JobSatisfaction", "PerformanceRating",
                "EnvironmentSatisfaction", "JobLevel", "RelationshipSatisfaction", "StockOptionLevel",
                "WorkLifeBalance", "HourlyRate", "MonthlyRate", "PercentSalaryHike", "TrainingTimesLastYear",
                "NumCompaniesWorked")

non_useful <- setdiff(non_useful, categorical)
num_dat <- select_if(job, is.numeric) %>% 
  select(-non_useful)
numeric_cols <- colnames(num_dat)

pca_data <- num_dat %>%
  prcomp(scale = TRUE)

pc_df <- data.frame(pca_data$x[, c(1, 2)])
pca_data <- cbind(job, pc_df)
hclust_data <- pca_data %>% 
  select(PC1, PC2) %>%
  dist()
fit <- hclust(hclust_data)
pca_data <- pca_data %>% 
  mutate(cluster = cutree(fit, k = 4))

## Plotting PCA
pca_plot <- function() {
  
  ggplot(pca_data, aes(PC1, PC2, col = Attrition, alpha = Attrition, shape = as.factor(cluster))) +
    scale_alpha_discrete(range = c(0.3, 1)) +
    geom_point(size = 5) + 
    labs(x = "PC1", y = "PC2") +
    theme_bw() +
    theme(legend.position = "bottom", 
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank()) +
    ggtitle("PCA for most influential numerical features")+
    guides(col=guide_legend("Attrition"),shape=guide_legend("Cluster"))+
    labs(subtitle = "Fig 1.2")
}

## Component Plots

component_plot <- function(data) {
  pca_data <- data %>%
    prcomp(scale = TRUE)
  comp_data <- data.frame(pca_data$rotation) 
  comp_data <- comp_data %>%
    rownames_to_column("Feature") %>%
    select(Feature, PC1, PC2)
  
  comp_data %>%
    pivot_longer(c(PC1, PC2), names_to = "components", values_to = "values") %>%
    ggplot(aes (values, reorder_within(Feature,values, components), fill = values)) +
    labs(y = "Features", x = "Components", subtitle = "Fig 1.1") +
    scale_y_reordered() +
    facet_wrap(~components, scales = "free_y") + 
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15)
    )
  
}

reset <- function(data, brush) {
  brushedPoints(data, brush, allRows = T)$selected_
}

cat_chi<- function(data) {
  Pvals <- rep(NA, length(categorical))
  
  for (i in 1:length(Pvals)) {
    Pvals[i] <- chisq.test(data[["Attrition"]], data[[categorical[i]]])$p.value
  }
  
  data_chi <- data.frame(Categories = categorical, PValue = Pvals) %>%
    mutate(Categories = reorder(Categories, -PValue))
  
  
  plot <- ggplot(data_chi, aes(y = Categories, x = Pvals)) +
    geom_segment(aes(x = Pvals,y = Categories, yend = Categories, xend = 0)) +
    geom_point(aes(col = Pvals), size  = 4) +
    scale_x_continuous(expand = c(0,0,0.1,0.1)) +
    scale_colour_gradient(low = "green", high = "red") +
    theme_bw()+
    theme(
      panel.border = element_blank(),
      axis.line = element_line(colour = "black")
    )+
    labs(x = "P-Value", 
         y = "Features")
  
  
  
  ggplotly(plot, tooltip = "x") %>%
    layout(title = list(text = paste0('<br>','<sup>','Fig 1.3','</sup>')))
  
}  

bar_plot <- function(data, variable)
{
  
  ggbar <- data %>%
    count(Attrition, .data[[variable]]) %>%
    ggplot(aes(y = .data[[variable]], x = n, fill = Attrition))+
    geom_col() +
    theme_bw()+
    theme(
      panel.border = element_blank(),
      axis.line = element_line(colour = "black")
    )+
    labs(x = "Count")+
    scale_x_continuous(expand = expansion(0))
  
  ggplotly(ggbar) %>%
    layout(title = list(text = paste0('<br>','<sup>','Fig 1.4','</sup>')))
  
}


combinations <- data.frame(matrix(nrow = 0, ncol = 4))


for(variable1 in categorical){
  for(variable2 in numeric){
    
    mod.c <- glm(job[["Attrition"]] ~ job[[variable1]] + job[[variable2]] + job[[variable1]]:job[[variable2]], family=binomial(link = "logit"))
    x <- lrtest(mod.c, glm(job[["Attrition"]] ~ job[[variable1]] + job[[variable2]], family=binomial(link = "logit")))
    
    
    combinations <- rbind(combinations, list(variable1, variable2, x$`Pr(>Chisq)`[2]))
  }
}

colnames(combinations) <- c("Categorical", "Numerical", "PValue")



### Harshita's code
ex_df <- tibble(var1_names = colnames(job)[c(3, 5, 7, 8, 11, 12, 14, 15, 16, 17, 18, 23, 25, 26, 28, 31)],
                var2_names = colnames(job)[c(3, 5, 7, 8, 11, 12, 14, 15, 16, 17, 18, 23, 25, 26, 28,31)])
att <- job$Attrition
ex_df <- ex_df %>%
  expand(var1_names, var2_names) %>%
  filter(!duplicated(paste0(pmax(var1_names, var2_names), pmin(var1_names, var2_names)))) %>%
  filter(var1_names != var2_names)

vector <-  c()

for(i in 1:nrow(ex_df)) {
  mod.b <- glm((job[["Attrition"]]) ~ (job[[ex_df$var1_names[i]]]) + (job[[ex_df$var2_names[i]]]) + (job[[ex_df$var1_names[i]]]):(job[[ex_df$var2_names[i]]]), family=binomial(link = "logit"))
  x <- lrtest(mod.b, glm((job[["Attrition"]]) ~ (job[[ex_df$var1_names[i]]]) + (job[[ex_df$var2_names[i]]]), family=binomial(link = "logit")))
  vector <- c(vector, x$`Pr(>Chisq)`[2])
}
new_df <- ex_df %>%
  mutate(p_val = vector) 


responsive_plot <- function(index_val, data_table){


  
  row <- data_table[index_val, ]
  facet_length <- length(unique(job[[row$var2_names]]))
 400 + facet_length* 12
  

  
 
  
}

## Decision Tree Part

best_predictors <- c("Age", "NumCompaniesWorked", "YearsSinceLastPromotion","DailyRate","EducationField", "JobRole",  "BusinessTravel", "MaritalStatus", 
                     "Gender",  "OverTime",  "YearsWithCurrManager", "YearsInCurrentRole", 
                     "YearsAtCompany", "TrainingTimesLastYear")
x <- job %>%
  select(best_predictors)

dtree <- rpart(formula = Attrition ~ Age + NumCompaniesWorked + YearsSinceLastPromotion +
                 DailyRate + EducationField + JobRole + BusinessTravel + MaritalStatus +
                 Gender + OverTime + YearsWithCurrManager + YearsInCurrentRole +
                 YearsAtCompany + TrainingTimesLastYear, data = job, control = rpart.control(cp = 0.005))



explaination <- explain(model = dtree, data = x, y = job$Attrition, label = "")


customLogo <- shinyDashboardLogoDIY(
  
  boldText = "HR Employee Attrition Analysis"
  ,mainText = ""
  ,textSize = 18
  ,badgeText = ""
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = ""
  ,badgeBorderRadius = 3
  
)


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = customLogo,
                                    titleWidth = 300), 
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(
                        id = "SideBarMENU", 
                        menuItem("PCA", tabName = "one", icon = icon("sliders"), selected = TRUE),
                        menuItem("Decision Tree", icon = icon("sitemap"),tabName = "two"),
                        menuItem("Feature Interactions", icon = icon("chart-bar"),tabName = "three")
                      )
                    ),
                    dashboardBody(
                      shinyDashboardThemes(
                        theme = "blue_gradient"
                      ), 
                      tabItems(
                        # First tab content
                        tabItem("one",
                                fluidPage(
                                  h2("Principal Component Analysis", align = "center"),
                                  p("Principal Component Analysis has been performed on important numerical features.
                                    Fig 1.1 shows the contribution of each feature to the 1st and 2nd Principal Components. 
                                    Fig 1.2 shows the scores plot on the first two principal components. The points are colored to reflect Attrition among employees and shaped according to their cluster obtained from hiearchial clustering.
                                    A corresponding data table shows the actual values/representations of the brushed points in Fig 1.2. 
                                    Fig 1.3 shows the P-values obtained from a chi-square test between the categorical variables and attrition based on the points selected in Fig 1.2.
                                    Fig 1.4 shows the counts of attrition for categorical variables for the points selected in Fig 1.2."),
                                  box(title = "The explainantion of the first two Principal Components", status = "warning", solidHeader = TRUE,withSpinner(plotOutput("Components")), width = 13),
                                  h4("Brush over the plot to select points", align = "center"),
                                  box(title = "Scores plot for PCA with hiearchial clustering", status = "warning", solidHeader = TRUE,withSpinner(plotOutput("PCA", brush = "brush")), width = 13),
                                  h4("View selected Points"),
                                  box(title = "Observations based on the brushed points above",withSpinner(DT::dataTableOutput("selected_points")),status = "warning", solidHeader = TRUE, width = 13),
                                  fluidRow(
                                    box(title = "Chi-square test of Attrition with various Categorical features", status = "warning", solidHeader = TRUE, withSpinner(plotlyOutput("Chi")), height = 550),
                                    box(title = "Proportion of Attrition based on Categorical features", status = "warning",  solidHeader = TRUE, selectInput("cats", "Choose Categorical Variable", categorical),
                                        withSpinner(plotlyOutput("bar_plot_cat")), height = 550)
                                  )
                                )
                        ),
                        # Second tab content
                        tabItem("two",
                                h2("Decision Tree", align = "center"),
                                p("A Decision Tree (Machine Learning) model was made to predict and analyze Attrition with the most influential features.
                                  Fig 2.1 shows the Decision Tree that we created for our analysis. The left branch represents when a node satisfy the condition based on feature is satisfied and vice-versa
                                  for the right. Fig 2.2 shows Ceteris Paribus Profiles for 'Years at Company' and 'Age', and allowing the user to group these profiles by the influential categorical predictors."),
                                box(title = "Decision Tree Graph", withSpinner(plotOutput("tree", height = "500px")), status = "warning", solidHeader = TRUE, width = 13), 
                                box(title = "Partial Dependency Profiles", selectInput("cat_var", "Choose a categorical predictor", c("EducationField", "JobRole",  "BusinessTravel", "MaritalStatus","Gender",  "OverTime")), withSpinner(plotOutput("profiles")), status = "warning", solidHeader = TRUE,  width = 13)
                        ),
                        
                        # Third tab content
                        tabItem("three",
                                h2("Feature Interactions", align = "center"),
                                p("Feature Interaction Analysis was done on different categorical and numerical features to see if their interactions would assist in understanding attrition.
                                  To understand the importance/significance of an iteraction to predict/analyze attrition, Likelihood Ratio Tests (LRT) were used. 
                                  In Fig 3.1, interactions between various numerical and categorical features were analyzed. The respective P-values from the LRT tests are stored in the corresponding data table.
                                  Ridge Density plots can be observed upon clicking a row in the first table. Similarly, In Fig 3.2, interactions between various categorical variables were analyzed.
                                  Faceted bar plots can be observed on clicking a row in the second table. Move the slider to filter feature interactions based on P-Values from the LRT Tests."),
                                sliderInput("slider", "Chose P-Value range", min = 0, max = 1e-1, value = c(0, 1e-1/2), width = "600px"),
                                h4("Click on a row of the Data Table to see a density plot depicting correlation between numerical features"),
                                fluidRow(
                                  box(title = "Categorical-Numerical Interactions LRT", status = "warning", solidHeader = TRUE, withSpinner(DT::dataTableOutput("output1"))),
                                  box(title = "Ridge-Density plots for Categorical-Numerical Interaction with Attrition", status = "warning",  solidHeader = TRUE, withSpinner(plotOutput("density")))
                                ),
                                
                                
                                h4("Click on a row of the Data Table to see a bar plot depicting correlation between categorical features"),
                                fluidRow(
                                  box(title = "Categorical-Categorical Interactions LRT", status = "warning", solidHeader = TRUE, withSpinner(DT::dataTableOutput("output2")), height = 500),
                                  box(title = "Bar plots for Categorical-Categorical Interaction with Attrition", status = "warning",  solidHeader = TRUE, withSpinner(plotOutput("barplot")), height = 500)
                                )
                                
                                
                        )
                        
                        
                        
                      )
                    )
) 




server <- function(input, output) {
  
  ###Neil's code
  
  selected <- reactiveVal(T)
  observeEvent(input$brush,
               {
                 selected(reset(pca_data,input$brush))
               }
  )
  
  
  output$PCA <- renderPlot({
    pca_plot()
  })
  
  output$Components <- renderPlot({
    
    component_plot(pca_data
                   %>% select(numeric_cols))
  })
  
  output$selected_points <- DT::renderDataTable({
    table <- pca_data%>%filter(selected())%>% select(numeric_cols, everything()) %>% select(-c(PC1, PC2, cluster))
    DT::datatable(table, options = list(scrollX = T))
    
    
  })
  
  
  
  
  output$Chi <- renderPlotly({
    cat_chi(pca_data%>%
              filter(selected()))
  })
  
  output$bar_plot_cat <- renderPlotly({
    
    bar_plot(pca_data%>%filter(selected()), input$cats)
  })
  
  
  ### Decision Tree Code
  
  
  output$tree <- renderPlot({
    rpart.plot(dtree, box.palette="RdBu", shadow.col="gray", nn=TRUE, type = 0, sub = "Fig 2.1")
    
  })
  
  output$profiles <- renderPlot({
    
    
    profile <- model_profile(explaination, groups = input$cat_var)
    plot(profile, geom = "profiles", variables = c("YearsAtCompany", "Age"))+
      labs(subtitle = "Fig 2.2")
    
  })
  
  
  
  
  ###Harshita's & Ayushi's code 
  cat_num <- reactive({
    combinations %>%
      filter(PValue >= input$slider[1] & PValue <= input$slider[2]) %>% 
      arrange(PValue)
  })
  cat_cat <- reactive({
    new_df %>%
      filter(p_val >= input$slider[1] & p_val <= input$slider[2]) %>% 
      arrange(p_val)
  })
  
  output$"output2" <- renderDataTable({
    cat_cat() %>%
      dplyr::rename("Categorical 1" = "var1_names", 
                    "Categorical 2" = "var2_names", 
                    "PValue" = "p_val") %>% 
      mutate(PValue=round(PValue,6))
    
    
  }, class="compact cell-border", selection = "single")
  output$"output1" <- renderDataTable({
    cat_num() %>% 
      
      mutate(PValue=round(PValue, 6)) 
    
    
  }, class="compact cell-border", selection = "single") 
  
  output$density <- renderPlot({
    
    
    s <- input$output1_rows_selected
    if(length(s)){
      row <- cat_num()[s, ]
      
      ggplot(job, aes(x = .data[[row$Numerical]], y = as.factor(.data[[row$Categorical]]), fill = Attrition)) +
        geom_density_ridges() +
        labs(title = paste0("Density Graph of ", row$Categorical, " and ", row$Numerical, " vs Attrition"), 
             subtitle = "Fig 3.1",y=paste0(row$Categorical)) +
        theme_bw() +
        theme(legend.position = "bottom", 
              plot.title = element_text(face = "bold", hjust = 0.5), 
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
    }
    
  })

  

  
  output$barplot <- renderPlot({
    s <- input$output2_rows_selected
    if(length(s)){
      row <- cat_cat()[s, ]
      d2 <- job %>%
        group_by(.data[[row$var1_names]], .data[[row$var2_names]],Attrition) %>%
        summarise(n = n()) %>%
        mutate(freq = n / sum(n))
      #geom_bar width=0.9 or 1
      
     
      
      
      ggplot(d2,aes(y=.data[[row$var1_names]],x=freq,fill=Attrition) ) +
        geom_bar(stat="identity")+
        theme(axis.text.x = element_text(angle = 45, hjust=1))+
        scale_x_continuous(expand = c(0,0)) +
        facet_grid(str_wrap(.data[[row$var2_names]], width = 8)~.)+
        labs(title = paste0("Frequency of Attrition in ", row$var1_names, " faceted by ", row$var2_names),y=paste0(row$var1_names),
             x="Proportions", 
             subtitle = "Fig 3.2") +
        theme_bw() +
        theme(axis.text.y = element_text(size = 10),
              strip.text.y = element_text(size = 8, face = "bold"))
    }
    
  }, height = reactive({
    index <- input$output2_rows_selected
    if(length(index)){
    responsive_plot(index, cat_cat())
    }
    else{
      
      200
    }
    }
    
    )
  
  )
  
  
  
  
  
}

# Run the application 

shinyApp(ui = ui, server = server)