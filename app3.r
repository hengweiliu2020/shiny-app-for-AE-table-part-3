#####################################
# Hengwei Liu                       #
# https://github.com/hengweiliu2020 #
#####################################


# Load R packages
library(haven)
library(shiny)
library(shinythemes)
library(tidyverse)
library(gt)
the_date <- as.character(Sys.Date())

# Define UI
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  theme = "united", 
                  "AE tables",
                  tabPanel("Input CSV file",
                           sidebarPanel(
                             fileInput(
                               inputId = "file", 
                               label = "Upload the file"
                             )), 
                           
                           mainPanel(
                             tableOutput("rawData")
                             
                           ) 
                           
                  ), 
                  
                  
                  
                  tabPanel("upload ADSL",
                           sidebarPanel(
                             fileInput(
                               inputId = "adsl", 
                               label = "Choose SAS datasets"
                             )), 
                           
                           mainPanel(
                             tableOutput("adsl")
                             
                           ) 
                           
                  ), 
                  
                  tabPanel("upload ADAE",
                           sidebarPanel(
                             fileInput(
                               inputId = "adae", 
                               label = "Choose SAS datasets"
                             )), 
                           
                           mainPanel(
                             tableOutput("adae")
                             
                           ) 
                  ), 
                  
                  tabPanel("One Level", 
                           mainPanel(tableOutput("unilevel"))
                  ),
                  
                  tabPanel("Two Levels", 
                           mainPanel(tableOutput("bilevel"))
                  ),
                  tabPanel("Three Levels", 
                           mainPanel(tableOutput("trilevel"))
                  )
                  
                ) 
)


# Define server function  
server <- function(input, output, session) {
  
  # returnTable1 is for AE table with preferred terms
  
  
  returnTable1 <- reactive({
    
    adsl <- read_sas(input$adsl$datapath)
    adsl <- data.frame(adsl)
    
    adae <- read_sas(input$adae$datapath)
    adae <- data.frame(adae)
    
    rawData <- read.csv(input$file$datapath, header=FALSE)
  
    vr <- data.frame(matrix(nrow=1, ncol=dim(rawData)[1]))
    
    for (i in 1:dim(rawData)[1]){
      vr[1,i] <- 
        eval(parse(text=paste0('paste(rawData[(rawData$V1==',i,'),]$V2)')))
      
      eval(parse(text=paste0(
        'adsl',i,' <- adsl %>% 
      filter(TRT01P==vr[1,',i,'] & SAFFL=="Y")')))
      
      eval(parse(text=paste0(
        'adsl',i,'$grp <- "grp',i,'"')))
      
      eval(parse(text=paste0(
        'adae',i,' <- adae %>% 
      filter(TRT01P==vr[1,',i,'] & SAFFL=="Y" & TRTEMFL=="Y")')))
      
      eval(parse(text=paste0(
        'adae',i,'$grp <- "grp',i,'"')))

    }
    
    adsl_tot <- do.call("rbind", mget(sprintf("adsl%d", 1:dim(rawData)[1])))
    adae_tot <- do.call("rbind", mget(sprintf("adae%d", 1:dim(rawData)[1])))

    adsl_tot2 <- adsl_tot
    num <- dim(rawData)[1] +1 
    eval(parse(text=paste0(
      'adsl_tot2$grp <- "grp',num,'"')))
    
    
    adae_tot2 <- adae_tot
    eval(parse(text=paste0(
      'adae_tot2$grp <- "grp',num,'"')))
    
    adsl_all <- rbind(adsl_tot, adsl_tot2)
    adae_all <- rbind(adae_tot, adae_tot2)
    
    # get the big N in column headers from adsl4
    bign <- table(group=adsl_all$grp)
    
    # get the number of subjects with at least one TEAE
    
    group_by_grp <- 
      adae_all %>%                   
      group_by(grp) %>%
      summarise(unique_subj = n_distinct(USUBJID))
    
    
    # get the count by preferred term 
    
    group_by_grp2 <- 
      adae_all %>%                   
      group_by(grp, AEDECOD) %>%
      summarise(unique_subj = n_distinct(USUBJID))
    
    
    # do the transpose 
    
    a1 <- spread(group_by_grp, grp, unique_subj)
    a3 <- spread(group_by_grp2, grp, unique_subj)
    
    a1$term <- "Subjects with at least one TEAE"
    a3$term <- a3$AEDECOD
    
    a1 <- a1[c("term", unique(adae_all$grp))]
    a3 <- a3[c("term", unique(adae_all$grp))]
    
    final <- rbind(a1, a3)
    
    #sort the ae data by descending order in the total column
  
    eval(parse(text=paste0(
      'df <-final[order(-final$grp',num,'),]')))
    
    #create the final data df for reporting
    
    for (j in 1:num){
      eval(parse(text=paste0(
        'df$perc',j,' <- 100*df$grp',j,'/bign[',j,']')))
      eval(parse(text=paste0(
        'df$perc',j,' <- format(round(df$perc',j,', 1), nsmall = 1)')))
      eval(parse(text=paste0(
        'df$grp',j,'_c <- paste(df$grp',j,', "(", df$perc',j,', ")")')))
      
      eval(parse(text=paste0(
        'df$grp',j,'_c <- ifelse(is.na(df$grp',j,'),"0", df$grp',j,'_c)')))
      
    }
    
    df <- df %>%
      select(term, ends_with("_c"))
    
    
    # use gt to do the reporting 
    
    tab_html <- df %>% 
      gt() %>%
      
      tab_header(
        title = "Table 14.3.1 Treatment Emergent Adverse Events by Preferred Term",
        subtitle = "Safety Population"
      ) %>%
      tab_source_note(
        source_note = "Note: TEAE is defined to be the AEs with start date >= first dose date and <= last dose date + 30."
      ) %>%
      tab_source_note(
        source_note = "The table is sorted by descending order in the total column."
      ) %>%
      
      tab_source_note(
        source_note = paste("Program Source: app3.R            Executed: (Draft)",  the_date)
      ) %>%
      
      cols_label(
        term = html("Preferred Term"),
        
      ) %>%
      
      tab_options(
        table.border.top.color = "white",
        heading.border.bottom.color = "black",
        table.border.bottom.color = "white",
        table_body.border.bottom.color = "black",
        table_body.hlines.color = "white", 
        row_group.border.bottom.color = "white", 
        row_group.border.top.color = "white", 
        column_labels.border.top.color = "black",
        column_labels.border.bottom.color = "black",
      )
    
    for (s in 1:dim(rawData)[1]){
      eval(parse(text=paste0(
    'tab_html <- tab_html %>%
        cols_label(
          grp',s,'_c = html(paste(vr[1,',s,'], " <br> (N=", bign[',s,'], ")")),
          
        )'
      )))
    }
   
    eval(parse(text=paste0(
      'tab_html <- tab_html %>%
        cols_label(
          grp',num,'_c = html(paste("Total <br> (N=", bign[',num,'], ")")),
        
        )'
    )))
    
    return (tab_html)
  })
  
  output$unilevel <- render_gt(
    
    expr = return(returnTable1()), 
    width=px(900)
  )
  
  # returnTable2 is for AE table with system organ class and preferred terms
  
  returnTable2 <- reactive({
    
    adsl <- read_sas(input$adsl$datapath)
    adsl <- data.frame(adsl)
    
    adae <- read_sas(input$adae$datapath)
    adae <- data.frame(adae)
    
    rawData <- read.csv(input$file$datapath, header=FALSE)
    
    vr <- data.frame(matrix(nrow=1, ncol=dim(rawData)[1]))
    
    for (i in 1:dim(rawData)[1]){
      vr[1,i] <- 
        eval(parse(text=paste0('paste(rawData[(rawData$V1==',i,'),]$V2)')))
      
      eval(parse(text=paste0(
        'adsl',i,' <- adsl %>% 
      filter(TRT01P==vr[1,',i,'] & SAFFL=="Y")')))
      
      eval(parse(text=paste0(
        'adsl',i,'$grp <- "grp',i,'"')))
      
      eval(parse(text=paste0(
        'adae',i,' <- adae %>% 
      filter(TRT01P==vr[1,',i,'] & SAFFL=="Y" & TRTEMFL=="Y")')))
      
      eval(parse(text=paste0(
        'adae',i,'$grp <- "grp',i,'"')))
      
    }
    
    adsl_tot <- do.call("rbind", mget(sprintf("adsl%d", 1:dim(rawData)[1])))
    adae_tot <- do.call("rbind", mget(sprintf("adae%d", 1:dim(rawData)[1])))
    
    adsl_tot2 <- adsl_tot
    num <- dim(rawData)[1] +1 
    eval(parse(text=paste0(
      'adsl_tot2$grp <- "grp',num,'"')))
    
    
    adae_tot2 <- adae_tot
    eval(parse(text=paste0(
      'adae_tot2$grp <- "grp',num,'"')))
    
    adsl_all <- rbind(adsl_tot, adsl_tot2)
    adae_all <- rbind(adae_tot, adae_tot2)
    
    # get the big N in column headers from adsl4
    bign <- table(group=adsl_all$grp)
    
    # get the number of subjects with at least one TEAE
    
    group_by_grp <- 
      adae_all %>%                   
      group_by(grp) %>%
      summarise(unique_subj = n_distinct(USUBJID))
    
    
    # get the count by System Organ class
    
    group_by_grp1 <- 
      adae_all %>%                   
      group_by(grp, AEBODSYS) %>%
      summarise(unique_subj = n_distinct(USUBJID))
    
    
    # get the count by preferred term 
    
    group_by_grp2 <- 
      adae_all %>%                   
      group_by(grp, AEBODSYS, AEDECOD) %>%
      summarise(unique_subj = n_distinct(USUBJID))
    
    
    # do the transpose 
    
    a1 <- spread(group_by_grp, grp, unique_subj)
    a2 <- spread(group_by_grp1, grp, unique_subj)
    a3 <- spread(group_by_grp2, grp, unique_subj)
    
    a1$term <- "Subjects with at least one TEAE"
    a1$AEBODSYS <- " "
    a2$term <- a2$AEBODSYS
    a3$term <- a3$AEDECOD
    
    a1 <- a1[c("AEBODSYS", "term", unique(adae_all$grp))]
    a2 <- a2[c("AEBODSYS", "term", unique(adae_all$grp))]
    a3 <- a3[c("AEBODSYS", "term", unique(adae_all$grp))]
    
    final <- rbind(a1, a2, a3)
    
    #sort the ae data by AEBODSYS and descending order in the total column
    eval(parse(text=paste0(
      'df <-final[order(final$AEBODSYS, -final$grp',num,'),]')))
    
    #create the final data df for reporting
    
    for (j in 1:num){
      eval(parse(text=paste0(
        'df$perc',j,' <- 100*df$grp',j,'/bign[',j,']')))
      eval(parse(text=paste0(
        'df$perc',j,' <- format(round(df$perc',j,', 1), nsmall = 1)')))
      eval(parse(text=paste0(
        'df$grp',j,'_c <- paste(df$grp',j,', "(", df$perc',j,', ")")')))
      
      eval(parse(text=paste0(
        'df$grp',j,'_c <- ifelse(is.na(df$grp',j,'),"0", df$grp',j,'_c)')))
      
    }
    
    df <- df %>%
      select(AEBODSYS, term, ends_with("_c"))
    
    # use gt to do the reporting 
    tab_html <- df %>% 
      gt() %>%
      
      tab_header(
        title = "Table 14.3.1 Treatment Emergent Adverse Events by System Organ Class and Preferred Term",
        subtitle = "Safety Population"
      ) %>%
      tab_source_note(
        source_note = "Note: TEAE is defined to be the AEs with start date >= first dose date and <= last dose date + 30."
      ) %>%
      tab_source_note(
        source_note = "System organ class is in light gray. The table is sorted by system organ class and descending order in the total column."
      ) %>%
      
      tab_source_note(
        source_note = paste('Program Source: app3.R            Executed: (Draft)',  the_date)
      ) %>%
      
      cols_label(
        term = html("System Organ Class <br> Preferred Term"),
        
      ) %>%
      
      tab_options(
        table.border.top.color = "white",
        heading.border.bottom.color = "black",
        table.border.bottom.color = "white",
        table_body.border.bottom.color = "black",
        table_body.hlines.color = "white", 
        row_group.border.bottom.color = "white", 
        row_group.border.top.color = "white", 
        column_labels.border.top.color = "black",
        column_labels.border.bottom.color = "black",
      ) %>%
      
      tab_style(
        style = list(
          cell_fill(color = "#D3D3D3")
        ),
        locations = cells_body(
          
          rows = AEBODSYS==term
        )
      ) %>%
      cols_hide(
        columns = c(AEBODSYS)
      ) 
    
    
    for (s in 1:dim(rawData)[1]){
      eval(parse(text=paste0(
        'tab_html <- tab_html %>%
        cols_label(
          grp',s,'_c = html(paste(vr[1,',s,'], " <br> (N=", bign[',s,'], ")")),
          
        )'
      )))
    }
    
    eval(parse(text=paste0(
      'tab_html <- tab_html %>%
        cols_label(
          grp',num,'_c = html(paste("Total <br> (N=", bign[',num,'], ")")),
        
        )'
    )))
    
    
    
    return (tab_html)
  })
  
  output$bilevel <- render_gt(
    
    expr = return(returnTable2()), 
    width=px(900)
  )
  
  # returnTable3 is for AE table with system organ class, high level term and preferred terms
  
  returnTable3 <- reactive({
    
    adsl <- read_sas(input$adsl$datapath)
    adsl <- data.frame(adsl)
    
    adae <- read_sas(input$adae$datapath)
    adae <- data.frame(adae)
    
    rawData <- read.csv(input$file$datapath, header=FALSE)
    
    vr <- data.frame(matrix(nrow=1, ncol=dim(rawData)[1]))
    
    for (i in 1:dim(rawData)[1]){
      vr[1,i] <- 
        eval(parse(text=paste0('paste(rawData[(rawData$V1==',i,'),]$V2)')))
      
      eval(parse(text=paste0(
        'adsl',i,' <- adsl %>% 
      filter(TRT01P==vr[1,',i,'] & SAFFL=="Y")')))
      
      eval(parse(text=paste0(
        'adsl',i,'$grp <- "grp',i,'"')))
      
      eval(parse(text=paste0(
        'adae',i,' <- adae %>% 
      filter(TRT01P==vr[1,',i,'] & SAFFL=="Y" & TRTEMFL=="Y")')))
      
      eval(parse(text=paste0(
        'adae',i,'$grp <- "grp',i,'"')))
      
    }
    
    adsl_tot <- do.call("rbind", mget(sprintf("adsl%d", 1:dim(rawData)[1])))
    adae_tot <- do.call("rbind", mget(sprintf("adae%d", 1:dim(rawData)[1])))
    
    adsl_tot2 <- adsl_tot
    num <- dim(rawData)[1] +1 
    eval(parse(text=paste0(
      'adsl_tot2$grp <- "grp',num,'"')))
    
    
    adae_tot2 <- adae_tot
    eval(parse(text=paste0(
      'adae_tot2$grp <- "grp',num,'"')))
    
    adsl_all <- rbind(adsl_tot, adsl_tot2)
    adae_all <- rbind(adae_tot, adae_tot2)
    
    # get the big N in column headers from adsl4
    bign <- table(group=adsl_all$grp)
    
    # get the number of subjects with at least one TEAE
    
    group_by_grp <- 
      adae_all %>%                   
      group_by(grp) %>%
      summarise(unique_subj = n_distinct(USUBJID))
    
    
    # get the count by System Organ class
    
    group_by_grp1 <- 
      adae_all %>%                   
      group_by(grp, AEBODSYS) %>%
      summarise(unique_subj = n_distinct(USUBJID))
    
    # get the count by High Level Term
    
    group_by_grp2 <- 
      adae_all %>%                   
      group_by(grp, AEBODSYS, AEHLT) %>%
      summarise(unique_subj = n_distinct(USUBJID))
    
    
    # get the count by preferred term 
    
    group_by_grp3 <- 
      adae_all %>%                   
      group_by(grp, AEBODSYS, AEHLT, AEDECOD) %>%
      summarise(unique_subj = n_distinct(USUBJID))
    
    
    # do the transpose 
    
    a1 <- spread(group_by_grp, grp, unique_subj)
    a2 <- spread(group_by_grp1, grp, unique_subj)
    a3 <- spread(group_by_grp2, grp, unique_subj)
    a4 <- spread(group_by_grp3, grp, unique_subj)
    
    
    a1$term <- "Subjects with at least one TEAE"
    a1$AEBODSYS <- " "
    a1$AEHLT <- " "
    
    a2$term <- a2$AEBODSYS
    a2$AEHLT <- " "
    
    a3$term <- a3$AEHLT
    a4$term <- a4$AEDECOD
    
    a1 <- a1[c("AEBODSYS", "AEHLT",  "term", unique(adae_all$grp))]
    a2 <- a2[c("AEBODSYS", "AEHLT",  "term", unique(adae_all$grp))]
    a3 <- a3[c("AEBODSYS", "AEHLT",  "term", unique(adae_all$grp))]
    a4 <- a4[c("AEBODSYS", "AEHLT",  "term", unique(adae_all$grp))]
    
    final <- rbind(a1, a2, a3, a4)
    
    #sort the ae data by AEBODSYS, AEHLT and descending order in the total column
    eval(parse(text=paste0(
      'df <-final[order(final$AEBODSYS,final$AEHLT,-final$grp',num,'),]')))
    
    #create the final data df for reporting
    for (j in 1:num){
      eval(parse(text=paste0(
        'df$perc',j,' <- 100*df$grp',j,'/bign[',j,']')))
      eval(parse(text=paste0(
        'df$perc',j,' <- format(round(df$perc',j,', 1), nsmall = 1)')))
      eval(parse(text=paste0(
        'df$grp',j,'_c <- paste(df$grp',j,', "(", df$perc',j,', ")")')))
      
      eval(parse(text=paste0(
        'df$grp',j,'_c <- ifelse(is.na(df$grp',j,'),"0", df$grp',j,'_c)')))
      
    }
    
    df <- df %>%
      select(AEBODSYS, AEHLT, term, ends_with("_c"))
    
    
    
    # use gt to do the reporting 
    tab_html <- df %>% 
      gt() %>%
      
      tab_header(
        title = "Table 14.3.1 Treatment Emergent Adverse Events by System Organ Class, High Level Term and Preferred Term",
        subtitle = "Safety Population"
      ) %>%
      tab_source_note(
        source_note = "Note: TEAE is defined to be the AEs with start date >= first dose date and <= last dose date + 30."
      ) %>%
      tab_source_note(
        source_note = "System organ class is in dark gray. High level term is in light gray."
      ) %>%
      tab_source_note(
        source_note ="The table is sorted by system organ class, high level term and descending order in the total column."
      ) %>%
      
      tab_source_note(
        source_note = paste('Program Source: app3.R            Executed: (Draft)',  the_date)
      ) %>%
      
      cols_label(
        term = html("System Organ Class <br> High Level Term <br> Preferred Term"),
        
      ) %>%
      
      tab_options(
        table.border.top.color = "white",
        heading.border.bottom.color = "black",
        table.border.bottom.color = "white",
        table_body.border.bottom.color = "black",
        table_body.hlines.color = "white", 
        row_group.border.bottom.color = "white", 
        row_group.border.top.color = "white", 
        column_labels.border.top.color = "black",
        column_labels.border.bottom.color = "black",
      ) %>%
      
      tab_style(
        style = list(
          cell_fill(color = "#A9A9A9")
        ),
        locations = cells_body(
          
          rows = AEBODSYS==term
        )
      ) %>%
      
      tab_style(
        style = list(
          cell_fill(color = "#D3D3D3")
        ),
        locations = cells_body(
          
          rows = AEHLT==term
        )
      ) %>%
      
      
      
      cols_hide(
        columns = c(AEBODSYS, AEHLT)
      ) 
    
    for (s in 1:dim(rawData)[1]){
      eval(parse(text=paste0(
        'tab_html <- tab_html %>%
        cols_label(
          grp',s,'_c = html(paste(vr[1,',s,'], " <br> (N=", bign[',s,'], ")")),
          
        )'
      )))
    }
    
    eval(parse(text=paste0(
      'tab_html <- tab_html %>%
        cols_label(
          grp',num,'_c = html(paste("Total <br> (N=", bign[',num,'], ")")),
        
        )'
    )))
    
    
    
    return (tab_html)
  })
  
  output$trilevel <- render_gt(
    
    expr = return(returnTable3()), 
    width=px(1000)
  )
  
} 


# Create Shiny object
shinyApp(ui = ui, server = server)