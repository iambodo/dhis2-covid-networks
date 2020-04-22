####This vesion is LOCKED to only accept inputs from dhis2 covid demo instance https://covid.dhis2.org/demo/

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(networkD3) 
library(stringr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(httr)
library(data.table)
library(readr)
library(DT)
library(purrr)
library(shiny)
library(tidyr)
library(plotly)
library(assertthat)
library(ggraph)
library(gridExtra)
library(tidygraph)


###Set themes and global functions

#login function
loginDHIS2<-function(baseurl,username,password) {
    url<-paste0(baseurl,"api/me")
    r<-GET(url,authenticate(username,password))
    if_else(r$status_code == 200L, TRUE, FALSE)}



#fix baseurl if no trailing "/" entered
trailing_backslash<-function(x){
                if_else((str_sub(x, - 1, - 1)!="/"), 
                            str_c(x,"/"), x)}  

#create pretty data tables
makeDT <-function(x){
    DT::datatable(x,
                  filter = 'bottom',
                  escape = FALSE,
                  rownames = FALSE,
                  width= "100%",
                  class = "compact",
                  extensions = 'Buttons',
                  options = list(
                      pageLength = 5,
                      dom = 'Blfrtip', 
                      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf')))
}

#number of columns in a dataframe
lengthcols <- function(x) length(colnames(x))

#define layouts for network models
igraph_layouts <- c('nicely', 'star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
                    'randomly', 'fr', 'kk', 'drl', 'lgl')

#theme set
thm <- theme_minimal() +
    theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
    ) 
theme_set(thm)

#custom node colors
nodetypes_list<-c("Indexed & Confirmed","Contact Indexed, Confirmed",
                  "Contact Indexed, Suspected","Indexed & Suspected","Contact")



js_colors<-list(default='d3.scaleOrdinal()
                        .domain(["Indexed & Confirmed", "Indexed & Suspected", 
                         "Contact Indexed, Confirmed", "Contact Indexed, Suspected", "Contact"])
                        .range([ "#C62828", "#BA68C8", "#FFCDD2","#4FC3F7","#ffcc00"]);',
                cat10a="d3.scaleOrdinal(d3.schemeCategory10);",
                cat20="d3.scaleOrdinal(d3.schemeCategory20);",
                cat20b="d3.scaleOrdinal(d3.schemeCategory20b);")

colourscale_options <- c("default","Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3")


###### Define UI for application #######
ui <- navbarPage("Covid-19 DHIS2 Relationships", id = "tabID", fluid = TRUE,
                 
        tabPanel("Login & Explore",  value = "t1", icon = icon("sign-in-alt", lib = "font-awesome"),
                          
            # Sidebar for log-in data
            sidebarLayout(
                sidebarPanel(
                    # Ask for base url 
                    textInput(inputId = "baseurl", "DHIS2 Covid Demo URL", value = "https://covid.dhis2.org/demo/"),
                    
                    # Ask for username
                    textInput(inputId = "username", "DHIS2 username, e.g. 'admin'", value = "COVID"),
                    
                    #Ask for password
                    passwordInput(inputId = "password", "Password for this username", value = "StopCovid19!"),
                    
                    #login button
                    actionButton(inputId = "login", label = "Log-In & Load Data"),
                    
                    #text break
                    tags$div("Optional: extract enrollments within date range"),
                    tags$br(),
                    
                    # Dates for Covid Index cases
                    dateRangeInput(inputId = "index_date_range", "Case-Based Surveillance Start/End Dates",  
                                   start = Sys.Date()-365, end = Sys.Date()),
                    
                    # Dates for Contact tracing cases
                    dateRangeInput(inputId = "contact_date_range", "Contact Registration Start/End Dates",  
                                   start = Sys.Date()-365, end = Sys.Date()),
                    
                    #options for network diagram
                    conditionalPanel(
                        condition = " output.loggedIn== 'Login = TRUE' ",
                        
                        column(6,
                    sliderInput(inputId = "distance", "Link Distance Multiplier", 
                                min= 0.5, max=1.5, value = 1)),
                        column(6, 
                    sliderInput(inputId = "radius", "Node Radius Multiplier", 
                                min = 0.5, max = 1.5, value = 1)),
                    
                    selectizeInput(inputId = "nodetype", "Node Type Filter",
                                   multiple = TRUE,
                                   choice =  nodetypes_list,
                                   selected = nodetypes_list),
                    
                    selectizeInput(inputId = "oulist", "OrgUnit Filter",
                                   choice="", multiple=TRUE),
                    
                    selectInput(inputId = "d3_color_choice", "Color Range - Interactive",
                                multiple = FALSE, choice = js_colors, selected = "default")
                    
                    
                    )
                    ),
        

                # Show a network diagram
                mainPanel(      
                    tableOutput("network_head"),
                    tableOutput("network_summary"),
                    h5(textOutput("text1")),
                    h5(textOutput("volume_alert")),
                    forceNetworkOutput("force"),
                    h6(textOutput("text2")),
                    tableOutput("loggedIn")) #need this for conditional panels above
                                
                )),
    
    
            tabPanel("Network Models", value = "t2", icon = icon("project-diagram", lib = "font-awesome"),
                         sidebarLayout(
                             sidebarPanel("Filters and Aesthetics",
                                     sliderInput(inputId = "radius_net2", "Node Radius Multiplier", 
                                                min = 0.5, max = 1.5, value = ""),
                                     selectizeInput(inputId = "nodetype_net2", "Node Type Filter",
                                                    multiple = TRUE,
                                                    choice =  nodetypes_list,
                                                    selected = ""),
                                     selectizeInput(inputId = "oulist_net2", "OrgUnit Filter",
                                                    choice="", multiple=TRUE),
                                     selectInput(inputId = "ggplot_color_choice2", "Color Range - Static",
                                         multiple = FALSE, choice = colourscale_options, selected = "default")),
                             
                            mainPanel(      
                                column(6,
                                       column(4,
                                    checkboxInput(inputId = "nodelabels", "Node Labels", 
                                       value = FALSE, width = NULL),
                                    checkboxInput(inputId = "nodepoints", "Node Points", 
                                       value = TRUE, width = NULL)),
                                    column(4, 
                                           selectInput(inputId = "layout_choice", "Choose Layout",
                                                       choice=igraph_layouts, selected ="nicely")),
                                    column(4, 
                                    downloadButton("download_network_png", ".png")),
                                        ),
                                column(6,
                                    tableOutput("graph_stats")),
                                column(12,
                                tags$div(),
                                    plotOutput("staticnet"),
                                    tags$p(),
                                tags$div("Centrality of cases in diagram"),
                                    tags$p(),
                                    DTOutput("centrality_stats")))
                )),
    
            tabPanel("Contact Tracer", value = "t3", icon = icon("user-tag", lib = "font-awesome"),
                         sidebarLayout(
                             sidebarPanel("Filters and Aesthetics",
                                          sliderInput(inputId = "radius_net3", "Node Radius Multiplier", 
                                                      min = 0.5, max = 1.5, value = ""),
                                         selectInput(inputId = "ggplot_color_choice3", "Color Range - Static",
                                                         multiple = FALSE, choice = colourscale_options, 
                                                        selected = "default")),
                             mainPanel( 
                                    column(4,
                                        selectizeInput(inputId = "trace_lookup", "Choose Root Node by ID",
                                        choice = "",  selected = "")),
                                    column(2,
                                        selectInput(inputId = "degreeTraced", "Degrees",
                                     choice = seq(2,7), selected = 3)),
                                    column(2,
                                        selectInput(inputId = "tracer_mode", "Mode",
                                        choice = c("tree","timeline"), selected = "tree")),
                                    column(2,
                                        selectInput(inputId = "trace_labels", "Node Labels",
                                        choice = c("Case ID", "Organisation Unit", "Symptom Onset"), 
                                        selected = "Case ID")),
                                    column(2,
                                         downloadButton("download_trace_png", ".png")),
                                    
                            column(12,
                         plotOutput("root_iso"),
                         
                         DTOutput("trace_select_help")))
                         )),
    
            tabPanel("Analyses", value = "t4",icon = icon("bar-chart", lib = "font-awesome"),
                     fluidPage(
                         tabsetPanel(
                             tabPanel("Network Analysis",
                              fluidRow(
                                  column(4, plotOutput("x2")),
                                  column(8,
                                         tags$strong(
                                             "Filter the table below to change the graphics."),
                                         tags$br(),
                                         DT::DTOutput("tbl"))),
                              fluidRow(
                                  tags$div("Alternate: Select dot for Case ID"),
                                  column(4, plotlyOutput("x3")),
                                  column(8, plotOutput("hists")))
                     ),
                            tabPanel("All Nodes",
                              fluidRow( 
                                  column(12, DT::DTOutput("nodesDT")),
                                  column(6, 
                                         plotOutput("nodes_chart_top")),
                                  column(6, 
                                         plotOutput("nodes_chart_btm"))
                                  
                              )),
                             tabPanel("All Links",
                              fluidRow(
                                  column(8, tags$div("Connections by Type of Node"),
                                         tableOutput("linkstab"),
                                         tags$br(),
                                         sankeyNetworkOutput("sankeyLinks", width="100%", height="100%")),
                                  column(4, tableOutput("linksDays"),
                                         tags$div("Distance between onset dates
                                                                      by 'from' node"),
                                         plotOutput("lollipop"))
                              ) 
                              
                     )) 
                 
             )
        
    ),
    
    tabPanel("Notes & Raw Data", value = "t5", icon = icon("table", lib = "font-awesome"),
             sidebarLayout(
                 sidebarPanel(
                     tags$div("Data are pulled from your API and processed by R.
                                     You can download all data used for analysis here"),
                     downloadButton("downloadAllData", "Download Data"),
                     tags$br(),
                     tags$br(),
                     tags$div(tags$em("API URLs Used")),
                     tags$p("These queries generate the app's analysis. Check them to troubleshoot."),
                     h6(uiOutput("url_1")),
                     h6(uiOutput("url_2")),
                     h6(uiOutput("url_3"))
                 ),
                 mainPanel(
                     tags$div(
                         "This Shiny application is based on the DHIS2 Covid-19
                             metadata pacakge, V 0.3.3. ", 
                         tags$a(href="https://www.dhis2.org/covid-19", 
                                "Click here to download. "),
                         "It will work on any DHIS2 instance that is v2.29 or above
                             and has this metadata package intalled. On log-in, the app
                            makes API request for TEI relationships, and one analytics
                            query from each program, then merges them by TEI (see data download)"),
                     tags$br(),
                     tags$p("The Shiny app makes a few assumptions:"),
                     tags$ul(
                         tags$li("Assumes relationships between Tracked Entity Instance (TEI) in 'Case Based Surveillance'
                                     and 'Contact Registration & Follow-up' Programs are used"), 
                         tags$li("Assumes the UIDs for case ID attribute, symptom onset date, 
                                     and positive test remain same as in the metadata package"),
                         tags$li("Assumes the login user has access to the root org unit,
                                        both Covid Tracker programs, and Tracker user roles."),
                         tags$li("Summarizes TEI by their best available data, and assumes the best available data 
                                    is in the Case-Based Surveillance (CBS) program. For example, the `tei_from_latest_onset` date, 
                                    takes the best available data for 'onset date' across both programs.
                                    If the TEI is not in the CBS program, the app uses data from contacts program. 
                                    If there is no date of symptoms reported, uses the Enrollment date." ),
                         tags$li("Excludes recorded relationships in API where the TEI is later deleted, 
                                    or the TEI's enrollment in the tracker program is deleted. 
                                    Also excludes all TEI without a System-Generated ID attribute.")),
                     tags$br(),
                     tags$div("Definitions:"),
                     tags$ul(
                         tags$li(tags$em("Indexed & Confirmed."), "Enrolled in Case Based Surveillance (CBS) and lab-confirmed positive"),
                         tags$li(tags$em("Indexed & Suspected."), "Enrolled in CBS but lab-unconfirmed or lab-negative"),
                         tags$li(tags$em("Contact Indexed, Confirmed."),"Enrolled in both CBS and Contact Registration program, and lab-confirmed positive"),
                         tags$li(tags$em("Contact Indexed, Suspected."), "Enrolled in both CBS and Contact Registration program, but lab-unconfirmed or lab-negative"),
                         tags$li(tags$em("Contact."),  "Enrolled in Contact Registration program, but not CBS program"),
                         tags$li("For explanations on graph algorithms or measures in the 'Network Models' tab, see"),
                     h6(tags$a(href = 'https://kateto.net/networks-r-igraph', "Ognyanova, K. (2016) 
                     Network analysis with R and igraph: NetSci X Tutorial")),
                     h6(tags$a(href = 'https://igraph.org/c/doc/igraph-Layout.html', "iGraph documentation")),
                     ),
                     tags$br(),
                     tags$div(
                         "Demo produced for HISP by Brian O'Donnell.",
                     tags$a(href = 'http://github.com/iambodo', "Github here", icon("github", lib = "font-awesome"))
                     )
                     )
                    
                     
                     
                 )
                 
                 
             )
             
             
    )


######## Define server logic ################
server <- function(input, output, session) {


    
#update baseurl if no trailing "/"
observe({
    x<-input$baseurl
    updateTextInput(session, "baseurl",
                    value = "https://covid.dhis2.org/demo/")
    })

#set login status
login_status <- eventReactive(input$login, {
        loginDHIS2(input$baseurl, input$username, input$password)})
    
    
output$loggedIn<-renderText(paste0("Login = ", {login_status()}))   

observeEvent(input$login, {
if (login_status() == FALSE){
        showNotification("Error! Could not log in", type="error")
}

if (login_status() == TRUE){
        
        showNotification("Logged in! Loading data...", type="message")

#fetch root ID
ou_id <- eventReactive(input$login, { 
     if(login_status() == TRUE){
 
 url<-paste0(input$baseurl, "api/organisationUnits.json?level=1&paging=false&fields=id")
 ou_id<-fromJSON(content(GET(url, 
                             authenticate(user=input$username, 
                                          password=input$password)), 
                         type="text", encoding="UTF-8"))
 #should only be one root OU - take the top one
 ou_id1<-ou_id$organisationUnits[1]$id
return(ou_id1)
 } })

####Generate Payload######=============================

payload <- eventReactive(input$login, { 
    if(login_status() == TRUE ){
        
    #set dateranges
        cont_start_date<-as.character(input$contact_date_range[1])
        cont_end_date<-as.character(input$contact_date_range[2])        
        ind_start_date<-as.character(input$index_date_range[1])
        ind_end_date<-as.character(input$index_date_range[2])
        

###API QUERIES####
        
        #fetch relationships from CBS program
        url_1<-paste0(input$baseurl, "api/trackedEntityInstances.json?ou=",ou_id(),
                    "&ouMode=DESCENDANTS",
                    "&program=uYjxkTbwRNf",
                    "&paging=true&pageSize=10000",
                    "&fields=relationships",
                    "&programStartDate=",ind_start_date,
                    "&programEndDate=",ind_end_date
                    )
        
        
        rels<-fromJSON(content(GET(url_1), type="text", encoding="UTF-8"))
        lolists<-rels$trackedEntityInstances$relationships[]
        
        #list out all to and from relationships
        tei_from<-lolists %>%
            map(pluck, "from") %>%
            unlist() %>% 
            data.frame()
        
        tei_to<-lolists %>%
            map(pluck, "to") %>%
            unlist() %>% 
            data.frame()
        
        tei_rel<-lolists %>%
            map(pluck, "relationship") %>%
            unlist() %>% 
            data.frame()
        
        #remove duplicates since links are bidrectional
        #set names
        network_base<- dplyr::bind_cols(tei_from, tei_to, tei_rel) %>% distinct()
        colnames(network_base)<- c("tei_from","tei_to", "tei_rel")
        
        if(length(network_base$tei_rel) > 1000){
            showNotification("1000+ relationships found! Wait for processing...", 
                             type="warning")
        }
        
        # Fetch analytics from CBS program (date of symptoms and case ID)
        url_2<- paste0(input$baseurl,
                              "api/29/analytics/enrollments/query/uYjxkTbwRNf.csv?",
                              "dimension=ou:",ou_id(),
                              "&paging=FALSE",
                              "&ouMode=ACCESSIBLE",
                              "&dimension=LpWNjNGvCO5.HAZ7VQ730yn&dimension=LpWNjNGvCO5.PFXeJV8d7ja&",
                              "dimension=dDHkBd3X8Ce.vVcjDvIsL78&stage=dDHkBd3X8Ce&displayProperty=NAME&",
                              "tableLayout=true&columns=ou;HAZ7VQ730yn;PFXeJV8d7ja;vVcjDvIsL78&",
                              "rows=ou;HAZ7VQ730yn;PFXeJV8d7ja;vVcjDvIsL78",
                              "&startDate=",ind_start_date,"&endDate=",ind_end_date)


        
        #read request into DF
        req <- httr::GET(url_2)
        req_parsed <- readr::read_csv(httr::content(req))
        cbs_df<-data.table(req_parsed)
        
        # Fetch analytics from Contacts program (date of symptoms and case ID)
        url_3<- paste0(input$baseurl,
                              "api/29/analytics/enrollments/query/DM9n1bUw8W8.csv?",
                              "dimension=ou:",ou_id(),
                              "&dimension=sAV9jAajr8x.tCQ0Q70ao2m&dimension=sAV9jAajr8x.jGh3SzagmVu&",
                              "dimension=sAV9jAajr8x.s3eoonJ8OJb&stage=sAV9jAajr8x&displayProperty=NAME&",
                              "tableLayout=true&columns=pe;ou;tCQ0Q70ao2m;jGh3SzagmVu;s3eoonJ8OJb&",
                              "rows=pe;ou;tCQ0Q70ao2m;jGh3SzagmVu;s3eoonJ8OJb",
                              "&startDate=",cont_start_date,"&endDate=",cont_end_date)
        

        
        #read request into DF
        req <- httr::GET(url_3)
        req_parsed <- readr::read_csv(httr::content(req))
        contacts_df<-data.table(req_parsed) %>% 
            rename("contact_tei"=`Tracked entity instance`)
        
        
###PROCESS PAYLOAD####
        
        
        index_df <-cbs_df %>% 
            select("Index_tei"=`Tracked entity instance`,
                   "Index_OU"=`Organisation unit name`,
                   "Index_ou_id"=`Organisation unit`,
                   "Index_id"=`System Generated Case ID`,
                   "Index_date"=`Enrollment date`,
                   (lengthcols(cbs_df)-1):lengthcols(cbs_df)) %>%  #fetch the last two 
            rename("Index_onset"=6) %>% 
            rename("Index_confirmed"=7)

        
        
        #smaller df for self-join below
        cbs_df2 <- cbs_df %>% 
            mutate("contact_in_CBS"=if_else(is.na(Enrollment), 0, 1)) %>% 
            select("contact_tei"=`Tracked entity instance`,
                   contact_in_CBS,
                   (lengthcols(cbs_df)-1)) %>% 
            rename(contact_confirmed=3)
                   
        
        
        
        #merge cbs dataframe with self and contacts program, via relationships
        merged_df <-index_df %>% 
            left_join(network_base, by = c("Index_tei"="tei_from")) %>%
            filter(!is.na(tei_to)) %>% 
            left_join(contacts_df, by = c("tei_to" = "contact_tei")) %>%             
            left_join(cbs_df2, by = c("tei_to"="contact_tei")) %>% 
            mutate("contact_registered"=if_else(is.na(Enrollment), 0, 1)) %>% 
            mutate_if(is.numeric, ~tidyr::replace_na(., 0))
        
        #summarize relationships that exist in each program
        summary <- merged_df %>%
            group_by(Index_tei) %>% 
            add_tally(name="contact_count") %>% 
            mutate(contacts_reg_total =sum(contact_registered)) %>% 
            mutate(contacts_cbs_total =sum(contact_in_CBS)) %>% 
            ungroup() %>% 
            mutate("Contacts registered (%)" = round(contacts_reg_total/contact_count,4)*100) %>% 
            mutate("Contacts in CBS (%)" = round(contacts_cbs_total/contact_count,4)*100) %>% 
            #mutate("Contacts Positive (%)" = round(contacts_/contact_count,4)*100) %>% 
            #mutate(percent_in_CBS_or_reg = round(contact_registered_or_CBS/contact_count,4)*100) %>% 
            select(Index_tei, Index_confirmed, contact_count, ends_with("(%)")) %>% 
            distinct()
        

        
        #start trimming data for Network diagram
        #just select the columns we want
        contacts_net <- contacts_df %>% 
            select(contact_tei, Enrollment, `Enrollment date`, 
                   contains('symptoms'), contains('name'), 
                   contains('unit'), contains('system'), -contains('code')) %>% 
            #clean up variable names
            select_all(~gsub(" ", "_", .)) %>% 
            rename_at(vars(-starts_with("contact_")), function(x) paste0("contact_", x)) %>% 
            distinct()
        
        
        index_net <- index_df %>%
            drop_na(Index_tei) %>% 
            select(contains('Index')) %>% 
            distinct()
        
        #handy list of all OUs in the export
        ou_list_contacts <- contacts_df %>% 
            select(ou=`Organisation unit`, ou_name= `Organisation unit name`)
        ou_list_index <- index_df %>% 
            select(ou=Index_ou_id, ou_name = Index_OU)
        ou_list_all<-bind_rows(ou_list_contacts, ou_list_index) %>% distinct()
        
        

        
        #because links are bidrectional, but can be started index program or contacts program...
        #we need to combine both directions for node statistics
        network_tofrom <- network_base %>% 
            select("tei_from"=tei_to, "tei_to"=tei_from, tei_rel) %>% 
            mutate(original_direction = "to_from") %>% 
            mutate_if(is.factor, as.character)
        
        
        network_base2 <- network_base %>% 
            mutate(original_direction = "from_to") %>% 
            mutate_if(is.factor, as.character) %>% 
            bind_rows(network_tofrom)
        
        #now we have a base that includes both directions
        #check if the TEI are in each programs
        #if the "to" node is in a program keep the tei
        #classify TEI by program status
        #show latest TEI
        #then collapse into "TEI_from" to create nodes list
        
        
        test2 <-network_base2 %>% 
            mutate(tei_to_indexed = if_else((tei_to %in% index_df$Index_tei), 1, 0)) %>% 
            mutate(tei_from_indexed = if_else((tei_from %in% index_df$Index_tei), 1, 0)) %>% 
            mutate(link_indexed = if_else((tei_from_indexed == 1 | tei_to_indexed == 1), 1, 0)) %>% 
            mutate(tei_to_contact = if_else((tei_to %in% contacts_df$contact_tei), 1, 0)) %>% 
            mutate(tei_from_contact = if_else((tei_from %in% contacts_df$contact_tei), 1, 0)) %>% 
            mutate(link_contact = if_else((tei_from_contact == 1 | tei_to_contact == 1), 1, 0)) %>% 
            #is this a to or from TEI in this link in Contacts or CBS program?
            mutate(included_link = if_else((link_contact == 1 | link_indexed == 1), 1, 0)) %>% 
            #fitler to only TEI in either program
            filter(included_link==1) %>% 
            #merge CBS dataframe to both "from" and "to" nodes
            left_join(index_net, by = c("tei_to"="Index_tei")) %>% 
            rename_at(vars(starts_with("Index")), function(x) paste0("tei_to_", x)) %>% 
            left_join(index_net, by = c("tei_from"="Index_tei")) %>% 
            rename_at(vars(starts_with("Index")), function(x) paste0("tei_from_", x)) %>% 
            #merge Contacts program dataframe to both "from" and "to" nodes
            left_join(contacts_net, by = c("tei_to"="contact_tei")) %>% 
            rename_at(vars(starts_with("contact_")), function(x) paste0("tei_to_", x)) %>% 
            left_join(contacts_net, by = c("tei_from"="contact_tei")) %>% 
            rename_at(vars(starts_with("contact_")), function(x) paste0("tei_from_", x)) %>% 
            mutate(tei_from_latest_type = if_else((tei_from_indexed==1 & tei_from_contact==0 & 
                                                       tei_from_Index_confirmed==1), 
                                                  "Indexed & Confirmed", if_else((
                                                      tei_from_indexed==1 & tei_from_contact==0 &
                                                          tei_from_Index_confirmed==0),
                                                      "Indexed & Suspected", if_else((
                                                          tei_from_indexed==1 & tei_from_contact==1 &
                                                              tei_from_Index_confirmed==1),
                                                          "Contact Indexed, Confirmed", if_else((
                                                              tei_from_indexed==1 & tei_from_contact==1 &
                                                                  tei_from_Index_confirmed==0),
                                                              "Contact Indexed, Suspected", 
                                                              "Contact"))))) %>% 
            mutate(tei_to_latest_type = if_else((tei_to_indexed==1 & tei_to_contact==0 & 
                                                     tei_to_Index_confirmed==1), 
                                                "Indexed & Confirmed", if_else((
                                                    tei_to_indexed==1 & tei_to_contact==0 &
                                                        tei_to_Index_confirmed==0),
                                                    "Indexed & Suspected", if_else((
                                                        tei_to_indexed==1 & tei_to_contact==1 &
                                                            tei_to_Index_confirmed==1),
                                                        "Contact Indexed, Confirmed", if_else((
                                                            tei_to_indexed==1 & tei_to_contact==1 &
                                                                tei_to_Index_confirmed==0),
                                                            "Contact Indexed, Suspected", 
                                                            "Contact"))))) %>% 
            #add ID info - from node
            mutate(tei_from_latest_ID = case_when(
                !is.na(tei_from_contact_System_Generated_Contact_ID) ~tei_from_contact_System_Generated_Contact_ID,
                !is.na(tei_from_Index_id) ~tei_from_Index_id)) %>% 
            filter(!is.na(tei_from_latest_ID)) %>% 
            mutate(tei_to_latest_ID = case_when(
                !is.na(tei_to_contact_System_Generated_Contact_ID) ~ tei_to_contact_System_Generated_Contact_ID, 
                !is.na(tei_to_Index_id) ~ tei_to_Index_id))
        
        
        #add OU info for "from" and "to"
        test2<-test2 %>% 
            mutate(tei_from_latest_ou = case_when((!is.na(tei_from_contact_Organisation_unit))
                                                  ~tei_from_contact_Organisation_unit,
                                                  (!is.na(tei_from_Index_ou_id)) ~ tei_from_Index_ou_id)) %>% 
            mutate(tei_to_latest_ou = case_when((!is.na(tei_to_contact_Organisation_unit))
                                                ~tei_to_contact_Organisation_unit,
                                                (!is.na(tei_to_Index_ou_id)) ~ tei_to_Index_ou_id)) %>% 
            left_join(ou_list_all, by = c("tei_to_latest_ou" = "ou")) %>% 
            rename("tei_to_OU_name"="ou_name") %>% 
            left_join(ou_list_all, by = c("tei_from_latest_ou"="ou")) %>% 
            rename("tei_from_OU_name"="ou_name")
        
        
        
        
        #add latest onset date information for "from" and "to"
        test2<-test2 %>%           
            mutate_at(vars(contains("date")), ~as.Date(., "%m/%d/%Y", tz = "UTC")) %>% 
            mutate_at(vars(contains("onset")), ~as.Date(., "%m/%d/%Y", tz = "UTC")) %>% 
            mutate(tei_from_latest_onset = 
                       if_else((is.na(tei_from_Index_date) & is.na(tei_from_Index_onset) & 
                                    !is.na(tei_from_contact_Enrollment_date) & is.na(tei_from_contact_Date_of_symptoms_onset)), tei_from_contact_Enrollment_date,
                               if_else((!is.na(tei_from_Index_date) & is.na(tei_from_Index_onset) & is.na(tei_from_contact_Date_of_symptoms_onset)), tei_from_Index_date, 
                                       if_else((is.na(tei_from_Index_onset) & !is.na(tei_from_contact_Date_of_symptoms_onset)), tei_from_contact_Date_of_symptoms_onset,
                                               if_else((!is.na(tei_from_Index_onset) & is.na(tei_from_contact_Date_of_symptoms_onset)), tei_from_Index_onset, as.Date("0000-01-01")))))) %>% 
            mutate(tei_to_latest_onset = 
                       if_else((is.na(tei_to_Index_date) & is.na(tei_to_Index_onset) & 
                                    !is.na(tei_to_contact_Enrollment_date) & is.na(tei_to_contact_Date_of_symptoms_onset)), tei_to_contact_Enrollment_date,
                               if_else((!is.na(tei_to_Index_date) & is.na(tei_to_Index_onset) & is.na(tei_to_contact_Date_of_symptoms_onset)), tei_to_Index_date, 
                                       if_else((is.na(tei_to_Index_onset) & !is.na(tei_to_contact_Date_of_symptoms_onset)), tei_to_contact_Date_of_symptoms_onset,
                                               if_else((!is.na(tei_to_Index_onset) & is.na(tei_to_contact_Date_of_symptoms_onset)), tei_to_Index_onset, as.Date("0000-01-01")))))) %>% 
            mutate(days_between = difftime(tei_from_latest_onset,tei_to_latest_onset, units=c("days")))        
        
        
        
        
        #latest Nodes information into single DF
        nodes2 <-test2 %>% 
            filter(!is.na(tei_to_latest_ID)) %>% 
            group_by(tei_from, tei_from_latest_type, 
                     tei_from_latest_ID, 
                     tei_from_OU_name, tei_from_latest_ou,
                     tei_from_latest_onset) %>% 
            summarize(`confirmed positive` = max(tei_from_Index_confirmed),
                      connections = n(),
                      contacts = sum(tei_to_contact),        
                      indexed = sum(tei_to_indexed),
                      `connections positive` = sum(tei_to_Index_confirmed)) %>% 
            ungroup() %>% 
            mutate_if(is.numeric, ~tidyr::replace_na(., 0)) %>% 
            arrange(desc(tei_from_latest_type)) %>% 
            tibble::rowid_to_column("id") %>%  #zero-index row
            mutate("id"=id-1) %>% 
            as.data.frame()
        
        
        #Create Links df
        ids_from_short<-nodes2 %>% select("id", "tei_from")
        
        links2 <- network_base2 %>% 
            select("tei_to","tei_from") %>% 
            left_join(ids_from_short, by=c("tei_from"="tei_from")) %>% 
            left_join(ids_from_short, by=c("tei_to"="tei_from")) %>% 
            select(id.x,id.y) %>% 
            na.omit()
        
    #data for "links" in Analyses tab
        links_summary <-test2 %>% 
            select("original_direction", ends_with("latest_type")) %>% 
            filter(original_direction=="from_to") %>% 
            group_by(tei_from_latest_type,tei_to_latest_type) %>% 
            summarize(count=n()) %>% 
            ungroup() %>% 
            mutate(percent=round(count/sum(count), 2)) %>% 
            arrange(desc(percent))
        
        
        #lollipop table for links
        lollipop_tbl <- test2 %>% 
            select("tei_rel","tei_from_latest_ID", "tei_from_latest_type", 
                   "tei_from_latest_onset","tei_to_latest_onset",
                   "days_between") %>% 
            filter(days_between < 1000) %>% 
            arrange(-desc(days_between))
        
        
        
        #create Nodes Summary for Datatable, and clean names
        summary_tbl <- nodes2 %>% 
            mutate(`confirmed positive`= as.factor(`confirmed positive`)) %>% 
            mutate("Case_ID"=paste0("<a href=\"", input$baseurl,
                                    "dhis-web-tracker-capture/index.html#/dashboard?",
                                    "tei=",tei_from,
                                    "&program=uYjxkTbwRNf&ou=",tei_from_latest_ou,
                                    "\">",`tei_from_latest_ID` ,"</a>")) %>% 
            #rearrange columns and clean names
            select("Case_ID", everything(), 
                   -one_of(c("id","tei_from", "tei_from_latest_ID","tei_from_latest_ou"))) %>% 
            select_all(~gsub("tei_from_", "", .)) %>% 
            select_all(~gsub("tei_", " ", .)) %>% 
            arrange(desc(connections))
        

        ou_list_nodes<- unique(nodes2$tei_from_OU_name)
        
        
    #download handler and other text
        output$downloadAllData <- downloadHandler(filename = function() {
                paste0("dhis_covid_networkData_",input$network_layour,Sys.Date(), ".csv")
            },
            content = function(file) {
                write.csv(test2, file, row.names = FALSE)
            }
        )
        
        output$text2 <-renderText("Nodes are classified by the person's status in each program.
                                 Some nodes may not be analyzed as their TEI have no relationships,
                                 were deleted after creation, or lack an autogenerated ID.
                                 See 'Raw Data & Notes' tab for details")
        

        
    #now combine all processed data outputs into a single payload list
    payload<-list("summary_tbl"=summary_tbl, 
                 "summary"=summary,
                 "network_base"=network_base2,
                 "merged_df"=merged_df,
                 "cbs_df"=cbs_df,
                 "index_df"=index_df,
                 "contacts_df"=contacts_df,
                 "links2"=links2,
                 "nodes2"=nodes2,
                 "ou_list"=ou_list_nodes,
                 "links_summary"=links_summary,
                 "lollipop_tbl"=lollipop_tbl,
                 "url_1"=url_1, 
                 "url_2"=str_replace(url_2,".csv",".html+css"), 
                 "url_3"=str_replace(url_3,".csv",".html+css"))
    
    
    
    return(payload)
    
    
            } })


####Reactive Expressions######=============================

######Update Selectors######

#show only linked org units, and update dates for latest/earliest onset
observe({
    
    if(login_status() == FALSE  | is.null(payload())){
        x<-NULL}
    
    if(login_status() == TRUE & !is.null(payload())){
    
             x <- as.list(payload()$ou_list)
             
             #if there are over 1000 nodes, only show a random org unit first
             if(length(payload()$nodes2$id) > 1000){
                 
                 base_ou <- sample(payload()$ou_list, 1)
                 
                 updateSelectizeInput(session, "oulist",
                                      choices = x,
                                      selected = base_ou)
                 
                 output$volume_alert <-renderText("More than 1000 nodes possible. 
                                                  Filters default to a random org unit")
                     
             } else {
        
               # Can also set the org unit selection
               updateSelectizeInput(session, "oulist",
                                 choices = x,
                                 selected = x)
               }
             }})
 
#persist ou selection from first tab across all tabs             
observe({

    if(login_status() == FALSE  | is.null(input$oulist)){
        x<-NULL}

    if(login_status() == TRUE & !is.null(input$oulist) & input$tabID=="t1"){

        x <- as.list(payload()$ou_list)

               updateSelectizeInput(session, "oulist_net2",
                                    choices = x,
                                    selected = input$oulist)

        #repeat same for node types
        updateSelectizeInput(session, "nodetype_net2",
                             choices = nodetypes_list,
                             selected = input$nodetype)
        
        
        #and the radius size
        updateSliderInput(session, "radius_net2",
                          value = input$radius)
        
        updateSliderInput(session, "radius_net3",
                          value = input$radius)
    }
    
    if(login_status() == TRUE & !is.null(network_update()) & input$tabID=="t2"){
        #repeat same for node types
        updateSelectizeInput(session, "nodetype",
                             choices = nodetypes_list,
                             selected = input$nodetype_net2)
        
        #and the radius size
        updateSliderInput(session, "radius",
                          value = input$radius_net2)
        
        updateSliderInput(session, "radius_net3",
                          value = input$radius_net2)
        
        #and colors to third one
        updateSelectizeInput(session, "ggplot_color_choice3",
                             choices = colourscale_options,
                             selected = input$ggplot_color_choice2)
        
    }
    

    })


####Interactive Model Tab #####


#prepare payload's links and nodes objects for the network diagram
network_update <- reactive({
    if(login_status() == FALSE  | is.null(payload())){
        nodes2<-NULL
        links2<-NULL}

    if(login_status() == TRUE & !is.null(payload())){
        

    
    #set to the filters on the current tab
        if(input$tabID!="t2"){
            nodes2<-payload()$nodes2 %>%  
                subset(tei_from_OU_name %in% input$oulist) %>% 
                subset(tei_from_latest_type %in% input$nodetype)
        }
        if(input$tabID=="t2"){
            nodes2<-payload()$nodes2 %>%  
                subset(tei_from_OU_name %in% input$oulist_net2) %>% 
                subset(tei_from_latest_type %in% input$nodetype_net2)
        }

        
        
    nodes2<-nodes2 %>% 
            select(-id) %>% 
            tibble::rowid_to_column("id") %>% 
            mutate("id"=id-1) %>% 
            as.data.frame()
    
    #Create Links df
    ids_from_short<-nodes2 %>% select("id", "tei_from")

    links2 <- payload()$network_base %>% 
        select("tei_to","tei_from") %>% 
        left_join(ids_from_short, by=c("tei_from"="tei_from")) %>% 
        left_join(ids_from_short, by=c("tei_to"="tei_from")) %>% 
        select(id.x,id.y) %>% 
        na.omit()}
    
    network_update<-list("nodes2"=nodes2,
                         "links2"=links2)
    
    return(network_update)
    
    })




#render interactive force network
output$force<-renderForceNetwork(
    forceNetwork(Links = network_update()$links2, 
                 Nodes =  network_update()$nodes2,
                 NodeID = "tei_from_latest_ID",
                 Source = "id.x", Target = "id.y", 
                 fontSize = 12, legend = TRUE,
                 Nodesize = "connections", 
                 linkDistance = 50*input$distance,
                 radiusCalculation = paste0("3+Math.sqrt(d.nodesize)*3*",input$radius),
                 colourScale = JS(input$d3_color_choice),
                 bounded = FALSE, height = 100, width = 100, charge = -10,
                 Group = "tei_from_latest_type", opacity = 1, zoom = TRUE))

#table above interactive graph
output$network_head<-renderTable({
    
    if(login_status() == TRUE & !is.null(payload())){
        
        all_tei_rels <- n_distinct(payload()$network_base$tei_from, na.rm=TRUE)
        all_tei_cbs <-n_distinct(payload()$index_df$Index_tei,na.rm=TRUE)
        all_tei_contacts<-n_distinct(payload()$contacts_df$contact_tei,na.rm=TRUE)
        all_tei_diagram<-n_distinct(network_update()$nodes2$tei_from,na.rm=TRUE)
        
        all_tei_summary<-data_frame(
            `Total TEI in Network Below`=all_tei_diagram,
            `TEI with Relationship to Case Based Surveillance`=all_tei_rels, 
            `TEI in Case Based Surveillance program`=all_tei_cbs, 
            `TEI in Contact Tracing program`=all_tei_contacts  
        )
        
        return(all_tei_summary)
               
    }
})




output$network_summary<-renderTable({
    
    if(login_status() == TRUE & !is.null(payload())){
    library(tibble)
        network_update()$nodes2 %>% 
            group_by(tei_from_latest_type) %>% 
            summarize("count"=n()) %>% 
            column_to_rownames(var="tei_from_latest_type") %>%
            as.data.frame() %>%
            t()
        # payload()$nodes2 %>%
        # subset(tei_from_OU_name %in% input$oulist) 
        }
    }, bordered = TRUE)


####Network Model Tab #####


##Reactive object for static graph data
graph2 <- reactive({
    if(login_status() == TRUE & !is.null(payload()) & !is.null(network_update())){

        
#need to adjust node ID from 0-base to 1-base
new_nodes<-network_update()$nodes2 %>%
    mutate(id=id+1) %>% 
    arrange(-desc(id))

new_links<-network_update()$links2 %>%
    mutate(id.x = id.x+1) %>%
    mutate(id.y = id.y+1)

#create graph object
graph2<-tidygraph::tbl_graph(nodes = new_nodes,
                             edges = new_links,
                             directed =FALSE)
return(graph2)

}})

static_graph<-reactive({
    
#Plot static graph
#plot the graph
p<-graph2() %>%
    ggraph(layout = input$layout_choice) +
    geom_edge_link(color = "black", alpha = 0.4)

#set parameters for node points and/or node labels    
if (input$nodelabels == TRUE){
 p<- p +
     geom_node_text(aes(label = tei_from_latest_ID, 
                       color = tei_from_latest_type), 
                   size = 3*input$radius_net2)
}

if (input$nodepoints == TRUE){
 p <- p +
        geom_node_point(aes(color = tei_from_latest_type), 
                       size = 2*input$radius_net2)
}

if (input$ggplot_color_choice2 != "default"){
 p <- p +
     scale_color_brewer(palette = input$ggplot_color_choice2)
}

return(p)
})

output$staticnet<-renderPlot(static_graph())
    

#download handler - static graph file
output$download_network_png <- downloadHandler(
    filename = function() {
        paste0("dhis_covid_netgraph_",input$layout_choice,"_", Sys.Date(), ".png")
    },
    content = function(file) {
        ggsave(file, static_graph(), device = "png", scale = 3)
    }
)


#Graph statistics
output$graph_stats<-renderTable({
    library(igraph)

        stats<-tibble("Density"=edge_density(graph2()),
                      "Diameter"=diameter(graph2()),
                      "Mean Distance" = mean_distance(graph2())
    )
    return(stats)
}
)
    

#Node centrality statistics
output$centrality_stats<-renderDT({
    stats<-graph2() %>% 
        activate(nodes) %>% 
        mutate("Case_ID"=paste0("<a href=\"", input$baseurl,
                                "dhis-web-tracker-capture/index.html#/dashboard?",
                                "tei=",tei_from,
                                "&program=uYjxkTbwRNf&ou=",tei_from_latest_ou,
                                "\">",`tei_from_latest_ID` ,"</a>")) %>% 
        select(Case_ID, "latest_type"=tei_from_latest_type, 
               "OU_Name" = tei_from_OU_name, connections) %>% 
        mutate("authority"=round(centrality_authority(), 4)) %>%
        mutate("closeness"=round(centrality_closeness(), 4)) %>% 
        mutate("betweenness"=round(centrality_betweenness(), 4)) %>% 
        mutate("eigen"=round(centrality_eigen(), 4)) %>% 
        as_tibble()
    
    makeDT(stats)
}
)



####Contact Trace Tab ####

#show only available IDs in contact trace selector
observe({

    x <- graph2() %>% 
        activate(nodes) %>% 
        as_tibble() %>% 
        filter(connections > 0) %>% 
        select(tei_from_latest_ID) %>% 
        arrange(desc(tei_from_latest_ID))

    
    x <-as.list(x)
    
    # Can use character(0) to remove all choices
    if (is.null(x)){
        x <- list(NULL) }
        
    
    
    # Can also set the label and select items
    updateSelectizeInput(session, "trace_lookup",
                         choices = x)
    
    })




##Contact trace output - update root node
id_lookup <-reactiveVal()

observeEvent(input$trace_lookup, {
library(tidygraph)
    newValue <-graph2() %>% 
        activate(nodes) %>%
        filter(tei_from_latest_ID == input$trace_lookup) %>% 
        select(id) %>% 
        mutate_all(as.integer) %>%
        distinct() %>% 
        as_tibble()
    
    newValue<-as.integer(head(newValue, 1))
    
    id_lookup(newValue)
    
    }
    )


##Contact trace output - network object
graph3<-reactive({

    x <-id_lookup()
    
    graph2() %>%
        activate(nodes) %>% 
        mutate(degree_from_root = dfs_dist(root= x)) %>% 
        filter(degree_from_root <= input$degreeTraced) %>% 
        mutate(short_tei_ID = str_replace(tei_from_latest_ID, "_"," ")) %>% 
        mutate(short_tei_ID = str_wrap(short_tei_ID, width = 0, exdent = 2)) %>% 
        mutate(short_tei_ou = str_wrap(tei_from_OU_name, width = 0, exdent = 2)) %>% 
        arrange(-desc(degree_from_root))
 
    
    
})

#create graph output

root_iso_chart<-reactive({
    
    #layout as date linear
    if (input$tracer_mode == "timeline"){
        timeline<- graph3() %>% 
            activate(nodes) %>% 
            arrange(-desc(tei_from_latest_onset))
        
       p<- ggraph(timeline, layout = "linear", circular = FALSE) +  
            geom_edge_arc(color="grey", alpha = 0.6) + 
            theme(legend.position="bottom") +
            labs(title = "Connections by Estimated Date of Onset") +
            coord_flip()
    }

        
    
    #layout by as node tree
    if (input$tracer_mode == "tree"){
        p<-ggraph(graph3(), layout='tree', root = 1) +
            geom_edge_link(color = "black", alpha = 0.3) +
            theme(legend.position = "bottom") +
            labs(title=paste0("Contacts for ",input$trace_lookup))
    }

    
    if (input$trace_labels == "Case ID"){
        p <- p +
            geom_node_label(aes(label=tei_from_latest_ID,
                                color = tei_from_latest_type),
                            size = 4*input$radius_net3)
    }
    
    if (input$trace_labels == "Organisation Unit"){
        p <- p +
            geom_node_label(aes(label=tei_from_OU_name,
                                color = tei_from_latest_type),
                            size = 4*input$radius_net3)
    }
        
        if (input$trace_labels == "Symptom Onset"){
            p <- p +
                geom_node_label(aes(label=tei_from_latest_onset,
                                    color = tei_from_latest_type),
                                size = 4*input$radius_net3)
    }
    
    if (input$ggplot_color_choice3 != "default"){
        p <- p +
            scale_color_brewer(palette = input$ggplot_color_choice3)
    }
    
    return(p)
    
})

#plot output
output$root_iso<-renderPlot(root_iso_chart() )

#download handler - tracer file
output$download_trace_png <- downloadHandler(
    filename = function() {
        paste0("dhis_covid_trace_",input$trace_lookup,"_", Sys.Date(), ".png")
    },
    content = function(file) {
        ggsave(file, root_iso_chart(), device = "png", scale = 3)
    }
)


#table underneath the contact tracing output
output$trace_select_help<-renderDT({
    
dt_help <- graph3() %>% 
        activate(nodes) %>% 
        as_tibble() %>% 
        mutate("Case_ID"=paste0("<a href=\"", input$baseurl,
                                "dhis-web-tracker-capture/index.html#/dashboard?",
                                "tei=",tei_from,
                                "&program=uYjxkTbwRNf&ou=",tei_from_latest_ou,
                                "\">",`short_tei_ID` ,"</a>")) %>% 
        #rearrange columns and clean names
        select("Case_ID", everything(), -one_of(c("tei_from", "id",
                                                  "short_tei_ID","short_tei_ou",
                                                  "tei_from_latest_ID",
                                                  "tei_from_latest_ou"))) %>% 
        select_all(~gsub("tei_from_", "", .)) %>% 
        select_all(~gsub("tei_", " ", .))
    
    #set caption for table
    dt_caption <-paste0("Contacts traced for ",  input$trace_lookup)
                        
    #make table
    DT::datatable(dt_help,
                  filter = 'top',
                  escape = FALSE,
                  rownames = FALSE,
                  caption = dt_caption,
                  width= "100%",
                  class = "compact",
                  extensions = 'Buttons',
                  options = list(
                      pageLength = 5,
                      dom = 'Blrtip', 
                      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf')))    
})
#######Analyses Tab######

###Summary####
output$tbl <- renderDT(makeDT(payload()$summary_tbl))

#ggplot2 bubble plot    
output$x2 <- renderPlot({
    set.seed(1);
    s = input$tbl_rows_all
    p = payload()$summary_tbl %>%  slice(s) %>% 
        ggplot(aes(x=`contacts`, 
                   y=`indexed`, 
                   size=`connections`, 
                   color=`confirmed positive`)) +
        geom_jitter(width = 0.1, height = 0.1) +
        scale_size(name="Total connections") +
        labs(title = element_text("Plot of nodes in network by relationships"),
             subtitle = element_text("In Case-Based Surveillance and Contacts programs")) +
        theme_minimal() +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 10),
              legend.direction = "horizontal", legend.box = "vertical") +
        scale_color_brewer(palette="Dark2") 
    
    plot(p)
})

####All Nodes####

output$nodesDT <- renderDT(makeDT(payload()$summary_tbl))

#charts under nodes table
output$nodes_chart_top <- renderPlot({
    y = input$nodesDT_rows_all
    z = payload()$summary_tbl %>%  slice(y) %>%
        mutate(count = 1) %>% 
        ggplot(aes(fill=latest_type,
                   y=count,
                   x=latest_onset)) + 
        geom_col(position="stack") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        labs(title="Count of Nodes by Date of Onset")

    plot(z)
    
})

output$nodes_chart_btm <- renderPlot({
    y = input$nodesDT_rows_all
    z = payload()$summary_tbl %>%  slice(y) %>%
        ggplot(aes(fill=latest_type, 
                   y=connections, 
                   x=latest_onset)) + 
        geom_col(position="stack") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        labs(title="Sum of Connections by Date of Node Onset")
    plot(z)
    
})

#####All Links######
#add the latest type to "to" column

links_days_between_table<-payload()$lollipop_tbl %>%
    group_by(tei_from_latest_type) %>% 
    summarize("mean_days_between_onsets"=mean(days_between))

lollipop<-payload()$lollipop_tbl %>% 
    distinct(tei_rel, .keep_all=TRUE) %>% 
    ggplot() +
    geom_segment( aes(x=tei_rel, xend=tei_rel, y=tei_from_latest_onset, yend=tei_to_latest_onset), color="grey") +
    geom_point( aes(x=tei_rel, y=tei_from_latest_onset), color="red", size=3 ) +
    geom_point( aes(x=tei_rel, y=tei_to_latest_onset), color="green", size=3 ) +
    coord_flip()+
    theme_minimal()+
    theme(axis.text.y = element_blank(),
          legend.position = "bottom") +
    xlab("Relationship") +
    ylab("Date of Symptom onset")+
    labs(title="Days Between Symptom Onsets",
         subtitle="Green = 'From' Node / Red = 'To' Node")

#sankey
sankey_links <- payload()$links_summary %>%
    select("sankey_from"=tei_from_latest_type, 
           "sankey_to"=tei_to_latest_type,
           "value"=count) %>% 
    mutate(sankey_from=paste0("FROM_",sankey_from),
           sankey_to=paste0("TO_",sankey_to))

sankey_nodes <- data.frame(
    name= c(as.character(sankey_links$sankey_from), 
            as.character(sankey_links$sankey_to))
    %>% unique()
)

#match on ID
sankey_links$IDsource <- match(sankey_links$sankey_from, sankey_nodes$name)-1 
sankey_links$IDtarget <- match(sankey_links$sankey_to, sankey_nodes$name)-1

#create Sankey obj
sankey_obj <- networkD3::sankeyNetwork(Links = sankey_links, 
                                       Nodes = sankey_nodes,
                                       Source = "IDsource", Target = "IDtarget",
                                       Value = "value", NodeID = "name", 
                                       sinksRight=FALSE)



output$linkstab <- renderTable(payload()$links_summary)

output$text1 <- renderText("Nodes are classified by their status in each program")
output$text2 <-renderText("Some nodes may not be analyzed as the 
                                      TEI had no relationships, were deleted, or lacked an autogenerated ID.
                                  See Notes tab")

output$lollipop<-renderPlot(lollipop)

output$linksDays<-renderTable(links_days_between_table)

output$sankeyLinks<-renderSankeyNetwork(sankey_obj)

#links to url in Notes section
output$url_1<-renderUI({a(href = paste0(payload()$url_1),"TEI Relationships")})
output$url_2<-renderUI({a(href = paste0(payload()$url_2),"CBS Events")}) 
output$url_3<-renderUI({a(href = paste0(payload()$url_3),"Contacts Events")}) 





}
}
)
}


# Run the application 
shinyApp(ui = ui, server = server)


#filter slider to org unit

#if 500+ initially downloaded...
#show message
#just take the latest 500 by enrollment date



