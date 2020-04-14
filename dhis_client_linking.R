# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#NOTE!  Is this your first time running this Shiny app on your local machine?
#Uncomment the "LOAD PACKAGES" functions to install all required packages.
#Recomment/remove when uploading to a shiny server.

# ### Load packages ####
# packages<-c("dplyr","networkD3","stringr","jsonlite","lubridate","ggplot2",
#             "httr","data.table","readr","DT","purrr","shiny","tidyr","plotly")
# install_or_load_pack <- function(pack){
#     create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
#     if (length(create.pkg))
#         install.packages(create.pkg, dependencies = TRUE)
#     sapply(pack, require, character.only = TRUE)
# }
# 
# install_or_load_pack(packages)

library(shinycustomloader)
library(dplyr)
library(networkD3) 
library(stringr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(httr)
library(data.table)
library(readr)
library(purrr)
library(shiny)
library(tidyr)
library(plotly)
library(DT)



#baseurl<-"https://covid.dhis2.org/demo/"
#username<-"COVID"
#password<-"StopCovid19!"

#### Global Functions ####

loginDHIS2<-function(baseurl,username,password) {
  url<-paste0(baseurl,"api/me")
  r<-GET(url,authenticate(username,password))
  if_else(r$status_code == 200L, TRUE, FALSE)}

theme_set(theme_minimal() )

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



#### Define UI ####
ui <- navbarPage("Covid-19 DHIS2 Relationships", fluid = TRUE,
                tabPanel("Login",
                         sidebarLayout(
                             sidebarPanel(
                                 # Ask for base url 
                                 textInput(inputId = "baseurl", "DHIS2 URL", value = "https://covid.dhis2.org/demo/"),
                                 
                                 # Ask for username
                                 textInput(inputId = "username", "DHIS2 username, e.g. 'admin'", value = "COVID"),
                                 
                                 #Ask for password
                                 passwordInput(inputId = "password", "Password for this username", value = "StopCovid19!"),
                                 
                                 #text break
                                 tags$div("Optional: extract enrollments between specified dates"),
                                 tags$br(),
                                 
                                 # Dates for Covid Index cases
                                 dateRangeInput(inputId = "index_date_range", "Case-Based Surveillance Start/End Dates",  
                                                start = Sys.Date()-365, end = Sys.Date()),

                                # Dates for Contact tracing cases
                                dateRangeInput(inputId = "contact_date_range", "Contact Registration Start/End Dates",  
                                               start = Sys.Date()-365, end = Sys.Date()),
                                 
                                 # Submit authentication
                                 actionButton(inputId = "login", label = "Log-In & Load Data")),
                             
                             
                         mainPanel(
                             tableOutput("splashTable2"),
                             # conditionalPanel(
                             #   condition = " output.loggedIn=='1' ",
                             #   sliderInput(inputId = "slider", "Network Charge", 
                             #               min=-50, max=50, value=-5)),
                             networkD3::forceNetworkOutput("ForceNet1"),
                             tableOutput("splashTable1"),
                             textOutput("text1"),
                             textOutput("text2")
                         ))),
                
               tabPanel("Analyses",
                        fluidPage(
                                    tabsetPanel(
                                        tabPanel("Network Analysis",
                                                 fluidRow(
                                                   column(4, withLoader(plotOutput("x2"),
                                                                        type="html",loader="loader3")),
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
                                                  column(12, plotOutput("nodes_chart_top")),
                                                  column(12, plotOutput("nodes_chart_btm"))
                                                  
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
                                        #tabPanel("Table", tableOutput("table"))
                                        #DTOutput("tbl"),
                                        #simpleNetworkOutput("simpleNet")
                                    )
                            ),
               
               tabPanel("Notes and Raw Data", 
                        sidebarLayout(
                          sidebarPanel(
                            tags$div("Data are pulled from your API and processed by R.
                                     You can download all data used for analysis here"),
                            downloadButton("downloadAllData", "Download Data")
                          
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
                                     and 'Contact Registration & Follow-up Program' are used"), 
                            tags$li("Assumes the database UIDs for case ID, symptom onset date, 
                                     and positive test remain same as in the metadata package"),
                            tags$li("Assumes thelogin user has access to the root org unit,
                                        both Covid Tracker programs, and Tracker user roles."),
                            tags$li("Summarizes TEI by their best available data, and assumes the best available data 
                                    is in the Case-Based Surveillance program. For example, the `latest_onset` date, takes the best available data for 'onset date' across both programs.
                                    If the TEI is not in the CBS program, the app uses data from contacts program. 
                                    If there is no date of symptoms reported, uses the Enrollment date." ),
                            tags$li("Excludes recorded relationships in API where the TEI is later deleted, 
                                    or theTEI's enrollment in the tracker program is deleted. 
                                    Also excludes all TEI without a System-Generated ID attribute.")),
                            tags$br(),
                            tags$div("Possible next steps for the app:"),
                            tags$ul(
                              tags$li("Interactivity. Right now the Shiny server only handles data processing, 
                                      but it could also filter network diagram based on user-defined parameters"),
                              tags$li("Second- and third- order links between TEI"),
                              tags$li("Add length between symptom onset of each TEI to network diagram")),
                            tags$br(),
                            tags$p("Demo produced for HISP by Brian O'Donnell"),
                            tags$a(href="https://github.com/iambodo/","Github here")
                        
                          )
                        
                              
                            )
                            
                            )
                          )
                          

    
            

##### Define server logic #####
server <- function(input, output, session) {
  
  login_status <- eventReactive(input$login, {
    loginDHIS2(input$baseurl, 
               input$username, 
               input$password)}
  )
  

  
  
observeEvent(input$login, {
    if (login_status() == FALSE){
      showNotification("Error! Could not log in", type="error")
    }
  
    if (login_status() == TRUE){
      
    showNotification("Logged in! Loading data...", type="message")
            # GET requests from the API
        
        #dates for API requests
        cont_start_date<-as.character(input$contact_date_range[1])
        cont_end_date<-as.character(input$contact_date_range[2])        
        ind_start_date<-as.character(input$index_date_range[1])
        ind_end_date<-as.character(input$index_date_range[2])
        
        #fix baseurl if no trailing "/"
        new_baseurl<-if_else((str_sub(input$baseurl, - 1, - 1)!="/"), 
                             str_c(input$baseurl,"/"), 
                             input$baseurl)  
        
        
        #say fetching API data...
        
            # Log in and Fetch Root OrgUnit ID
        url<-paste0(new_baseurl, "api/29/organisationUnits.json?level=1&fields=id")
        ou_id<-fromJSON(content(GET(url, 
                                    authenticate(user=input$username, 
                                                 password=input$password)), 
                                type="text", encoding="UTF-8"))
        #should only be one root OU - take the top one
        ou_id<-ou_id$organisationUnits[1]$id
        
        
        #fetch relationships from CBS program
        url<-paste0(input$baseurl, "api/trackedEntityInstances.json?ou=",ou_id,
                    "&ouMode=DESCENDANTS",
                    "&program=uYjxkTbwRNf&paging=true&pageSize=10000",
                    "&fields=relationships&programStartDate=",ind_start_date,
                    "&programEndDate=",ind_end_date)
      
        rels<-fromJSON(content(GET(url), type="text", encoding="UTF-8"))
        
        lolists<-rels$trackedEntityInstances$relationships[]
        
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
        network_base<- dplyr::bind_cols(tei_from, tei_to, tei_rel) %>% distinct()
        colnames(network_base)<-c("tei_from","tei_to", "tei_rel")
        

        # Fetch analytics from CBS program (date of symptoms and case ID)
        req <- httr::GET(url = paste0(new_baseurl,
                                         "api/29/analytics/enrollments/query/uYjxkTbwRNf.csv?",
                                         "dimension=ou:",ou_id,
                                         "&paging=FALSE",
                                         "&ouMode=ACCESSIBLE",
                                         "&dimension=LpWNjNGvCO5.HAZ7VQ730yn&dimension=LpWNjNGvCO5.PFXeJV8d7ja&",
                                         "dimension=dDHkBd3X8Ce.vVcjDvIsL78&stage=dDHkBd3X8Ce&displayProperty=NAME&",
                                         "tableLayout=true&columns=ou;HAZ7VQ730yn;PFXeJV8d7ja;vVcjDvIsL78&",
                                         "rows=ou;HAZ7VQ730yn;PFXeJV8d7ja;vVcjDvIsL78",
                                         "&startDate=",ind_start_date,"&endDate=",ind_end_date))
        
        #read request into DF
        req_parsed <- readr::read_csv(httr::content(req))
        cbs_df<-data.table(req_parsed)
            

        
        # Fetch analytics from Contacts program (date of symptoms and case ID)
        req <- httr::GET(url = paste0(new_baseurl,
                                      "api/29/analytics/enrollments/query/DM9n1bUw8W8.csv?",
                                      "dimension=ou:",ou_id,
                                      "&dimension=sAV9jAajr8x.tCQ0Q70ao2m&dimension=sAV9jAajr8x.jGh3SzagmVu&",
                                      "dimension=sAV9jAajr8x.s3eoonJ8OJb&stage=sAV9jAajr8x&displayProperty=NAME&",
                                      "tableLayout=true&columns=pe;ou;tCQ0Q70ao2m;jGh3SzagmVu;s3eoonJ8OJb&",
                                      "rows=pe;ou;tCQ0Q70ao2m;jGh3SzagmVu;s3eoonJ8OJb",
                                      "&startDate=",cont_start_date,"&endDate=",cont_end_date))
        #read request into DF
        req_parsed <- readr::read_csv(httr::content(req))
        contacts_df<-data.table(req_parsed) %>% 
            rename("contact_tei"=`Tracked entity instance`)
        

        #Index cases dataframe
        index_df <-cbs_df %>% 
            select("Index_tei"=`Tracked entity instance`,
                   "Index_OU"=`Organisation unit name`,
                   "Index_ou_id"=`Organisation unit`,
                   "Index_id"=`System Generated Case ID`,
                   "Index_date"=`Enrollment date`,
            #need to generify this in case the names of the indicators change!
                   "Index_onset"=`COVID-19 Calculated onset date (for Indicators)`,
                   "Index_confirmed"=`COVID-19 Confirmed hospitalised Cases`) 


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

        
        ## TEST 2
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
          summarize(`confirmed positive` = min(tei_from_Index_confirmed),
                    connections = n(),
                    contacts = sum(tei_to_contact),        
                    indexed = sum(tei_to_indexed),
                    `connections positive` = sum(tei_to_Index_confirmed)) %>% 
          ungroup() %>% 
          mutate_if(is.numeric, ~tidyr::replace_na(., 0)) %>% 
          arrange(desc(tei_from_latest_type)) %>% 
          tibble::rowid_to_column("id") %>%  #zero-index row
          mutate("id"=id-1)
        
        #Create Links df
        ids_from_short<-nodes2 %>% select("id", "tei_from")
        
        links2 <- network_base2 %>% 
          select("tei_to","tei_from") %>% 
          left_join(ids_from_short, by=c("tei_from"="tei_from")) %>% 
          left_join(ids_from_short, by=c("tei_to"="tei_from")) %>% 
          select(id.x,id.y) %>% 
          na.omit()
        

      #create Nodes Summary for Datatable, and clean names
      summary_tbl <- nodes2 %>% 
        mutate(`confirmed positive`= as.factor(`confirmed positive`)) %>% 
        mutate("Case_ID"=paste0("<a href=\"", new_baseurl,
                                "dhis-web-tracker-capture/index.html#/dashboard?",
                                "tei=",tei_from,
                                "&program=uYjxkTbwRNf&ou=",tei_from_latest_ou,
                                "\">",`tei_from_latest_ID` ,"</a>")) %>% 
        #rearrange columns and clean names
        select("Case_ID", everything(), -one_of(c("id","tei_from", 
                                                  "tei_from_latest_ID","tei_from_latest_ou"))) %>% 
        select_all(~gsub("tei_from_", "", .)) %>% 
        select_all(~gsub("tei_", " ", .)) %>% 
        arrange(desc(connections))
        
      nodes2<-data.frame(nodes2)
      
      ###custom node colors
      ColourScale <- 'd3.scaleOrdinal()
            .domain(["Indexed & Confirmed", "Indexed & Suspected", 
            "Contact Indexed, Confirmed", "Contact Indexed, Suspected", "Contact"])
           .range(["#C62828", "#BA68C8", "#FFCDD2","#4FC3F7","#ffcc00"]);'

      #create force Network object
        force_diagram<-forceNetwork(Links = links2, Nodes = nodes2, NodeID = "tei_from_latest_ID",
                                    Source = "id.x", Target = "id.y", fontSize = 12, legend = TRUE,
                                    Nodesize = "connections", radiusCalculation = "Math.sqrt(d.nodesize)*3+3",
                                    colourScale = JS(ColourScale),
                                    bounded = FALSE, height = 50, width = 50, charge = -10,
                                    Group = "tei_from_latest_type", opacity = 1, zoom = TRUE)
            
      #links summary
      #add the latest type to "to" column
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
        
        links_days_between_table<-lollipop_tbl %>%
          group_by(tei_from_latest_type) %>% 
          summarize("mean_days_between_onsets"=mean(days_between))
          
        lollipop<-lollipop_tbl %>% 
          distinct(tei_rel, .keep_all=TRUE) %>% 
          ggplot() +
          geom_segment( aes(x=tei_rel, xend=tei_rel, y=tei_from_latest_onset, yend=tei_to_latest_onset), color="grey") +
          geom_point( aes(x=tei_rel, y=tei_from_latest_onset), color="red", size=3 ) +
          geom_point( aes(x=tei_rel, y=tei_to_latest_onset), color="green", size=3 ) +
          coord_flip()+
          theme(axis.text.y = element_blank(),
            legend.position = "bottom") +
          xlab("Relationship") +
          ylab("Date of Symptom onset")+
          labs(title="Days Between Symptom Onsets",
               subtitle="Green = 'From' Node / Red = 'To' Node")
        
        #sankey
        sankey_links <- links_summary %>%
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
    
          
        
  
        #front page tables
        all_tei_rels <- n_distinct(network_base2$tei_from, na.rm=TRUE)
        all_tei_cbs <-n_distinct(index_df$Index_tei,na.rm=TRUE)
        all_tei_contacts<-n_distinct(contacts_df$contact_tei,na.rm=TRUE)
        all_tei_diagram<-n_distinct(nodes2$tei_from,na.rm=TRUE)
        
        all_tei_summary<-data_frame(
          `Total TEI in Network Above`=all_tei_diagram,
          `TEI with Relationship to Case Based Surveillance`=all_tei_rels, 
          `TEI in Case Based Surveillance program`=all_tei_cbs, 
          `TEI in Contact Tracing program`=all_tei_contacts  
        )
        
        #second splash table
        all_types <- summary_tbl %>% 
          group_by(latest_type) %>% 
          tally()
        x<-all_types$latest_type
        all_types<-as.data.frame(t(all_types[,-1]))
        colnames(all_types)<-x
        
        #histogram facet
        hist_facet <- function(X){
          X %>% 
          select(c(2, 6:9)) %>% 
          group_by(latest_type) %>% 
          gather(key="variable", value="value", -latest_type) %>% 
          ggplot(aes(x = value)) + 
          facet_grid(latest_type ~ variable, margins=FALSE) + 
          geom_histogram() +
          labs(title="Nodes x Connections Charts",
               x="connections by attributes" , 
               y = "node count by node attribute") +
          theme(strip.text.y = element_text(angle=0, hjust=0),
                panel.spacing = unit(2, "lines"))
          }
        

        nodes2 <- nodes2 %>% 
                   select(-id,-tei_from, -tei_from_latest_ou)
                        
                 

    ##Render objects
        output$tbl <- renderDT(makeDT(summary_tbl))
        
        output$ForceNet1<-networkD3::renderForceNetwork(force_diagram)
        
        output$splashTable1<-renderTable(all_tei_summary)
          
        output$splashTable2<-renderTable(all_types)
        
        output$nodesDT <- renderDT(makeDT(nodes2))
        
        output$linkstab <- renderTable(links_summary)
        
        output$text1 <- renderText("Nodes are classified by their status in each program")
        output$text2 <-renderText("Some nodes may not be analyzed as the 
                                      TEI had no relationships, were deleted, or lacked an autogenerated ID.
                                  See Notes tab")

        output$lollipop<-renderPlot(lollipop)
        
        output$linksDays<-renderTable(links_days_between_table)
        
        output$sankeyLinks<-renderSankeyNetwork(sankey_obj)
        
        output$downloadAllData <- downloadHandler(
          filename = function() {
            paste0("dhis_covid_networkData_",Sys.Date(), ".csv")
          },
          content = function(file) {
            write.csv(test2, file, row.names = FALSE)
          }
        )
        
        
        #histogram for types of connections
        output$hists <- renderPlot({
          a = input$tbl_rows_all
          summary_tbl %>%  slice(a) %>% 
          hist_facet()          
        }
        )

        
        
    #ggplot2 bubble plot    
        output$x2 <- renderPlot({
          set.seed(1);
          s = input$tbl_rows_all
          p = summary_tbl %>%  slice(s) %>% 
            ggplot(aes(x=`contacts`, 
                       y=`indexed`, 
                       size=`connections`, 
                       color=`confirmed positive`)) +
            geom_jitter(width = 0.1, height = 0.1) +
            scale_size(name="Total connections") +
            labs(title = element_text("Plot of nodes in network by relationships"),
                 subtitle = element_text("In Case-Based Surveillance and Contacts programs")) +
            theme(legend.position = "bottom",
                  legend.text = element_text(size = 10),
                  legend.direction = "horizontal", legend.box = "vertical") +
            scale_color_brewer(palette="Dark2")
          
          plot(p)
        })
        
        #plotly bubble plot
        output$x3 <- renderPlotly({
          set.seed(1);
          s = input$tbl_rows_all
          p = summary_tbl %>%  slice(s) %>%
            ggplot(aes(x=`contacts`, 
                       y=`indexed`,  
                       size=`connections`, 
                       color=`confirmed positive`,
                       text=paste0("Case ID: ", Case_ID))) +
            scale_color_brewer(palette="Dark2") +
            geom_jitter(width = 0.1, height = 0.1) +
            theme(legend.position = "none",
                  legend.text = element_text(size = 3)) +
            scale_size(name="Total contacts") +
            labs(caption="Hover over to highlight, click and drag to zoom")

            
            ggplotly(p)
            })
        
        #chart under nodes table
        output$nodes_chart_top <- renderPlot({
          y = input$nodesDT_rows_all
          z = nodes2 %>%  slice(y) %>%
          mutate(count = 1) %>% 
          ggplot(aes(fill=tei_from_latest_type,
                     y=count,
                     x=tei_from_latest_onset)) + 
          geom_col(position="stack") +
            labs(title="Count of Nodes by Date of Onset")
        
          plot(z)
          
})

        output$nodes_chart_btm <- renderPlot({
          y = input$nodesDT_rows_all
          z = nodes2 %>%  slice(y) %>%
            ggplot(aes(fill=tei_from_latest_type, 
                       y=connections, 
                       x=tei_from_latest_onset)) + 
            geom_col(position="stack") +
            labs(title="Sum of Connections by Date of Node Onset")
          
          
          plot(z)
          
        })

        
        

        showNotification("Data Loaded! Initializing Visuals..", type="message")
        
        #make simple network diagram
        #output$simpleNet<-renderSimpleNetwork(simpleNetwork(network_base, zoom=TRUE))
        
        #output$ForceNet1<-renderForceNetwork(force_diagram)
        
        #assign yes if logged in successfully
        # v$loggedin <- TRUE

    }
    
}
)}


            


   


# Run the application 
shinyApp(ui = ui, server = server)


#Next steps
#add initial step to login to server
#Download of root org unit & relationships into df
#all LOGIN AND DOWNLOAD steps on press of button

#structure - DONE
#title, tabs, fit two diagrams on right hand side
#add second tab to filters for showing all links, and network
#DONE: add third tab for notes
#DONE: merge relationships df with CBS df and contacts df for front page
#DONE: front page is collapsed case-contacts merge, percent of contacts positive
#DONE: put hyperlinks in DT to direct you to the case on the site (tei, enrollment, OU)
#DONE splash page of network, with index cases colored
#DONE: add positive cases...
#DONE: transform all-links df into networkd3 diagram
#DONE: size nodes by # contacts?
#DONE; add sessions for server ui
#DONE: bubble chart of index cases: number links (size), percent positive (y), percent w/ symptoms (x) 
##TEI class -- TEI class
#DONE: message header on log-in: loading data OR, please re-enter creds
#DONE: add "boxes" for headline figures to front page
#DONE: analysis box for links: numbers two/from different groups

#headline box:
#number of each class
#number excluded... #
##unique TEI in relationships
##unique TEI in analytics returned for each program
##TEI eligible (with System Gen IDs)



#next steps
#downloads from API should be in reactive objects
#use sliders on Network visual -- set charge of graph 
#network statistics tab 
#create custom network graphics??

#analyses should have two columns for DT and graphics
#link outputs with sliders (sliders from DT output?)
##select a node and highlight a row, vice versa 
###https://stackoverflow.com/questions/45217609/extracting-node-information-from-networkd3-to-a-reactive-variable-in-shiny
#add widgets for filtering


# 
# hist_facet <- nodes2 %>% 
#     select(c(3, 5:9)) %>% 
#     group_by(tei_from_latest_type) %>% 
#     gather(key="variable", value="value", -tei_from_latest_type) %>% 
#     ggplot(aes(x = value)) + 
#     facet_grid(variable ~ tei_from_latest_type) + 
#     geom_histogram()




#histograms for: 
#connections, contacts, indexed, positive
#linkages summary (indexed & suspected, etc)
#download boxes for all data
#then, upload to cloud



#Set legend for GGplot -- Positive Test is RED
#Set legend for network plot -- Positive test is RED
#Area chart for nodes diagram




