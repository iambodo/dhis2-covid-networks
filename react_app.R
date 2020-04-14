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
#library(data.tree)



#login function
loginDHIS2<-function(baseurl,username,password) {
    url<-paste0(baseurl,"api/me")
    r<-GET(url,authenticate(username,password))
    if_else(r$status_code == 200L, TRUE, FALSE)}

#theme set
thm <- theme_minimal() +
    theme(
        #legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
    ) 
theme_set(thm)


igraph_layouts <- c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
                    'randomly', 'fr', 'kk', 'drl', 'lgl')

###### Define UI for application #######
ui <- fluidPage(

    # Application title
    titlePanel("Test Reactivity"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Ask for base url 
            textInput(inputId = "baseurl", "DHIS2 URL", value = "https://covid.dhis2.org/demo/"),
            
            # Ask for username
            textInput(inputId = "username", "DHIS2 username, e.g. 'admin'", value = "COVID"),
            
            #Ask for password
            passwordInput(inputId = "password", "Password for this username", value = "StopCovid19!"),
            
            #login button
            actionButton(inputId = "login", label = "Log-In & Load Data"),
            
            #charge slider
            conditionalPanel(
                condition = " output.loggedIn== 'Login = TRUE' ",
                
            sliderInput(inputId = "distance", "Link Distance Multiplier", 
                        min= 0.5, max=1.5, value=1),
            
            sliderInput(inputId = "radius", "Node Radius Multiplier", 
                        min = 0.5, max = 1.5, value = 1),
            
            selectizeInput(inputId = "oulist", "OrgUnit Filter",
                           choice="", multiple=TRUE),
            
            selectizeInput(inputId = "nodetype", "Node Type Filter",
                           multiple = TRUE,
                           choice = c("Indexed & Confirmed",
                                      "Contact Indexed, Confirmed",
                                      "Contact Indexed, Suspected",
                                      "Indexed & Suspected",
                                      "Contact"),
                           selected = c("Indexed & Confirmed",
                                        "Contact Indexed, Confirmed",
                                        "Contact Indexed, Suspected",
                                        "Indexed & Suspected",
                                        "Contact"))
            
            )
            ),
        

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Interactive",
                        verbatimTextOutput("loggedIn"),
                        tableOutput("network_head"),
                           #verbatimTextOutput("ou"),
                           #verbatimTextOutput("ou_plus_blah"),
                           forceNetworkOutput("force")
                ),
                tabPanel("Static",
                         column(6,
                         checkboxInput(inputId = "nodelabels", "Node Labels", 
                                       value = FALSE, width = NULL),
                         checkboxInput(inputId = "nodepoints", "Node Points", 
                                       value = TRUE, width = NULL),
                         selectInput(inputId = "layout_choice", "Choose Layout",
                                        choice=igraph_layouts, selected ="kk")),
                         column(6,
                         tableOutput("graph_stats")),
                         
                         plotOutput("staticnet")
                ),
                tabPanel("Date Arc",
                         plotOutput("lineargraph"),
                         plotOutput("root_iso")
                         )
        )
    )
    )
)

######## Define server logic ################
server <- function(input, output, session) {

    login_status <- eventReactive(input$login, {
                                            loginDHIS2(input$baseurl, 
                                            input$username, 
                                            input$password)}
)
    
    
output$loggedIn<-renderText(paste0("Login = ", {login_status()}))   

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

payload <- eventReactive(input$login, { 
    if(login_status() == TRUE ){
        
        cont_start_date<-as.character("2019-04-01")
        cont_end_date<-as.character("2020-04-03")        
        ind_start_date<-as.character("2019-04-01")
        ind_end_date<-as.character("2020-04-03")
        
        #fetch relationships from CBS program
        url<-paste0(input$baseurl, "api/trackedEntityInstances.json?ou=",ou_id(),
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
        #set names
        network_base<- dplyr::bind_cols(tei_from, tei_to, tei_rel) %>% distinct()
        colnames(network_base)<- c("tei_from","tei_to", "tei_rel")
        
        
        
        # Fetch analytics from CBS program (date of symptoms and case ID)
        req <- httr::GET(url = paste0(input$baseurl,
                                      "api/29/analytics/enrollments/query/uYjxkTbwRNf.csv?",
                                      "dimension=ou:",ou_id(),
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
        req <- httr::GET(url = paste0(input$baseurl,
                                      "api/29/analytics/enrollments/query/DM9n1bUw8W8.csv?",
                                      "dimension=ou:",ou_id(),
                                      #"&ouMode=ACCESSIBLE",
                                      #"&dimension=pe:THIS_MONTH;LAST_12_MONTHS",
                                      "&dimension=sAV9jAajr8x.tCQ0Q70ao2m&dimension=sAV9jAajr8x.jGh3SzagmVu&",
                                      "dimension=sAV9jAajr8x.s3eoonJ8OJb&stage=sAV9jAajr8x&displayProperty=NAME&",
                                      "tableLayout=true&columns=pe;ou;tCQ0Q70ao2m;jGh3SzagmVu;s3eoonJ8OJb&",
                                      "rows=pe;ou;tCQ0Q70ao2m;jGh3SzagmVu;s3eoonJ8OJb",
                                      "&startDate=",cont_start_date,"&endDate=",cont_end_date))
        #read request into DF
        req_parsed <- readr::read_csv(httr::content(req))
        contacts_df<-data.table(req_parsed) %>% 
            rename("contact_tei"=`Tracked entity instance`)
        
        index_df <-cbs_df %>% 
            select("Index_tei"=`Tracked entity instance`,
                   "Index_OU"=`Organisation unit name`,
                   "Index_ou_id"=`Organisation unit`,
                   "Index_id"=`System Generated Case ID`,
                   "Index_date"=`Enrollment date`,
                   #need to generify this in case the names of the indicators change!
                   "Index_onset"=`COVID-19 Calculated onset date (for Indicators)`,
                   "Index_confirmed"=`COVID-19 Confirmed hospitalised Cases`) 
        
        
        
        #smaller df for self-join below
        cbs_df2 <- cbs_df %>% 
            mutate("contact_in_CBS"=if_else(is.na(Enrollment), 0, 1)) %>% 
            select("contact_tei"=`Tracked entity instance`,
                   contact_in_CBS,
                   "contact_confirmed"=`COVID-19 Confirmed hospitalised Cases`)
        
        
        
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
        
        
        #exclude unnecessary columns and prettify
        summary_tbl <- cbs_df %>% 
            mutate("Case_ID"=paste0("<a href=\"", input$baseurl,
                                    "dhis-web-tracker-capture/index.html#/dashboard?",
                                    "tei=",`Tracked entity instance`,
                                    "&program=uYjxkTbwRNf&ou=",`Organisation unit`,
                                    "\">",`System Generated Case ID` ,"</a>")) %>%
            mutate_at(vars(contains("date")), ~as.Date(., "%m/%d/%Y", tz = "UTC")) %>%
            rename("COVID onset date"=`COVID-19 Calculated onset date (for Indicators)`) %>%
            left_join(summary, by=c("Tracked entity instance"="Index_tei")) %>%
            mutate_if(is.numeric, ~tidyr::replace_na(., 0)) %>% 
            mutate_at(vars(Index_confirmed), factor) %>% 
            select(`Organisation unit name`,
                   `Case_ID`,
                   `Enrollment date`,
                   `COVID onset date`,
                   `COVID confirmed` = Index_confirmed,
                   "contact count" = contact_count,
                   `Contacts registered (%)`,
                   `Contacts in CBS (%)`) %>%
            arrange(desc(`contact count`))
        
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
        

        ou_list_nodes<- unique(nodes2$tei_from_OU_name)
        
    payload<-list("summary_tbl"=summary_tbl, 
             "summary"=summary,
             "network_base"=network_base2,
             "merged_df"=merged_df,
             "cbs_df"=cbs_df,
             "index_df"=index_df,
             "contacts_df"=contacts_df,
             "links2"=links2,
             "nodes2"=nodes2,
             "ou_list"=ou_list_nodes)
    
    return(payload)
    
            } })


output$ou<-renderText(paste0("Root = ", ou_id()) )

#show only certain org units
observeEvent(input$login,{    
             x <- as.list(payload()$ou_list)
               
               # Can use character(0) to remove all choices
               if (is.null(x))
                   x <- "test"
               
               # Can also set the label and select items
               updateSelectizeInput(session, "oulist",
                                 choices = x,
                                 selected = x)})



network_update <- reactive({
    if(login_status() == FALSE  | is.null(payload())){
        nodes2<-NULL
        links2<-NULL}

    if(login_status() == TRUE & !is.null(payload())){
        
    nodes2<-payload()$nodes2 %>%  
        subset(tei_from_OU_name %in% input$oulist) %>% 
        subset(tei_from_latest_type %in% input$nodetype) %>% 
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
                 colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                 bounded = FALSE, height = 100, width = 100, charge = -10,
                 Group = "tei_from_latest_type", opacity = 1, zoom = TRUE))

#table underneath interactive graph
output$network_head<-renderTable({
    
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
    })


##Reactive object for static graph
graph2 <- reactive({
    if(login_status() == TRUE & !is.null(payload())){

#need to adjust node ID from 0-base to 1-base
new_nodes<-network_update()$nodes2 %>%
    mutate(id=id+1) %>% 
    arrange(desc(tei_from_latest_onset))

new_links<-network_update()$links2 %>%
    mutate(id.x = id.x+1) %>%
    mutate(id.y = id.y+1)

#create graph object
graph2<-tidygraph::tbl_graph(nodes = new_nodes,
                             edges = new_links,
                             directed =FALSE)
return(graph2)

}})

#Plot static graph
output$staticnet<-renderPlot({
#plot the graph
p<-graph2() %>%
    ggraph(layout = input$layout_choice) +
    geom_edge_diagonal(color = "black", alpha = 0.4)

#set parameters for node points and/or node labels    
if (input$nodelabels == TRUE){
 p<- p +
     geom_node_text(aes(label = tei_from_latest_ID, 
                       color = tei_from_latest_type), 
                   size = 3*input$radius)
}

if (input$nodepoints == TRUE){
 p <- p +
        geom_node_point(aes(color = tei_from_latest_type), 
                       size = 2*input$radius)
}

return(p)
})

#Graph statistics
output$graph_stats<-renderTable({
    library(igraph)
    #density<-igraph::graph.density(graph2()) 
    stats<-data_frame("Density"=graph.density(graph2()),
                      "Diameter"=diameter(graph2()),
                      "Mean Distance" = mean_distance(graph2())
    )
    return(stats)
}
)
    
### Linear graph by date of last onset
output$lineargraph<-renderPlot({
    ggraph(graph2(), layout = "linear", circular = FALSE) + 
        geom_edge_arc(color="grey", alpha = 0.4) + 
        geom_node_text(aes(label = tei_from_latest_onset, 
                           color = tei_from_latest_type),
                           size = 2.5*input$radius,
                           hjust = 1,
                            nudge_y = 0.1,
                           check_overlap = TRUE) +
        theme(legend.position="bottom") +
        labs(title = "Connections by Estimated Date of Onset") +
        coord_flip()
})

#Isolate individual cases -- TEST/TODO
# output$root_iso<-renderPlot({
#     
#     
#     p<-graph2()
#     #hierarchy <- dendrogram(graph2())
#     
#     ggraph(p, layout = "dendrogram", circular = FALSE) + 
#         geom_edge_diagonal() +
#         geom_node_point() 
# })


#  #now use that to return a different value
# observeEvent(input$login, {
#      if(!is.null(ou_id())){
#          
# ou_plus_blah<-paste0(ou_id(), "_this one_isblaHH")
# 
# output$ou_plus_blah<-renderText(ou_plus_blah)
# 
# 
# 
#      } })

# # Load data
# data(MisLinks)
# data(MisNodes)
# 
# # Plot
# library(networkD3)
# reactive({
#     if(login_status() == TRUE){
# output$force<-renderForceNetwork({
#     forceNetwork(Links = MisLinks, Nodes = MisNodes,
#                  Source = "source", Target = "target",
#                  Value = "value", NodeID = "name",
#                  Group = "group", opacity = 0.8,
#                  charge= input$slider)})
# 
# })
#  
}

# Run the application 
shinyApp(ui = ui, server = server)


#next steps: add tree structure for node selection
