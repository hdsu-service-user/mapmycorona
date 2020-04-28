library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)


dashboardPage(
  title="MapMyCorona",
  #----------------------------------------------------------------------------#
  #                                Header                                      #
  #----------------------------------------------------------------------------#
  dashboardHeader(
    #title = "Map my Corona",
    title = tags$a(href=".",
                   tags$img(src="map_my_corona_logo.png",height="50px")),
    # title = tags$a(href="https://www.hdsu.org/", target="_blank",
    #                tags$img(src="map_my_corona_logo.png",height="50px")),
    
    tags$li(class = "dropdown",
            tags$a(href="https://www.hdsu.org/", target="_blank",
                   tags$style(".main-header {max-height: 55px}"),
                   tags$style(".main-header .logo {height: 55px}"),
                   tags$img(height = "30px", alt="SNAP Logo", src="logo_hdsu.png")
            )
    )
    
  ),
  
  # dashboardHeader(title = 'Reporting Dashboard',
  #                 tags$li(class = "dropdown",
  #                         tags$a(href="https://www.hdsu.org/", target="_blank", 
  #                                tags$img(height = "20px", alt="SNAP Logo", src="logo_hdsu.png")
  #                         )
  #                 ),
  #                 dropdownMenuOutput('messageMenu'),
  #                 dropdownMenu(type = 'notifications',
  #                              notificationItem(text = '5 new users today', icon('users')),
  #                              notificationItem(text = '12 items delivered', 
  #                                               icon('truck'), status = 'success'),
  #                              notificationItem(text = 'Server load at 86%', 
  #                                               icon = icon('exclamation-triangle'), 
  #                                               status = 'warning')),
  #                 dropdownMenu(type = 'tasks',
  #                              badgeStatus = 'success',
  #                              taskItem(value = 90, color = 'green', 'Documentation'),
  #                              taskItem(value = 17, color = 'aqua', 'Project X'),
  #                              taskItem(value = 75, color = 'yellow', 'Server deployment'),
  #                              taskItem(value = 80, color = 'red', 'Overall project'))
  # ),
  
  #----------------------------------------------------------------------------#
  #                                Sidebar                                     #
  #----------------------------------------------------------------------------#
  dashboardSidebar(
    
    # menuItem(
    #   "FAQ",
    #   tabName = "faq",
    #   icon = icon("question-circle")
    # ),
    
    sidebarMenu(
      br(),
      h6("MapMyCorona is a blast-server", align = "center"),
      h6("for SARS CoV-2 sequences (protein or DNA)", align = "center"),
      h6("which displays the top hits", align = "center"),
      h6("in a spatial and temporal fashion.", align = "center"),
      tags$hr(),
      
      
      h4("Search sequence", align = "center"),
      h6("from Fasta or from search box", align = "center"),
      
      menuItem(
        "Search ...",
        id = "searchseq",
        tabName = "searchseq",
        icon = icon("search"),
        startExpanded = FALSE,
        
        # Input: Select a file ----
        fileInput("file1", "Choose Fasta File",
                  multiple = FALSE,
                  accept = c(".RDS", ".fa", ".fasta")),
        
        # Search fasta sequence
        textAreaInput(
          inputId = "stringSequence", 
          label   = "Search sequence...", 
          placeholder = "> my_corona"
        ),
        actionBttn(
          inputId = "clear_stringSequence",
          label = "Clear", 
          style = "minimal",
          size = "xs",
          color = "default"
        ),
        
        awesomeRadio(
          inputId = "seq_type",
          label = "Sequence Type",
          choices = c("nucleotide", "protein"),
          selected = "nucleotide",
        ),
        
        menuItem(
          "BLAST options",
          tabName = "blastopt",
          #icon = icon("user-secret")
          icon = icon("cog"),
          
          numericInput(
            inputId = "blast_nres", 
            label = "Number of results:", 
            value = 500,
            min = 1, 
            max = NA, 
            step = NA, 
            width = "90%"),
          
          numericInput(
            inputId = "blast_evalt", 
            label = "Expectation value (E) threshold:", 
            value = 1000,
            min = 0.001, 
            max = NA, 
            step = NA, 
            width = "90%"),
          
          sliderTextInput(
            inputId = "blast_pident_range",
            label = "Percent identity (pident) :", 
            choices = seq(0, 100, 10),
            selected = c(0, 100),
            from_min = 0,
            from_max = 100,
            grid = TRUE
          )
          
        ),
        
        
        actionBttn(
          inputId = "searchSequence",
          label = "Submit",
          style = "jelly",
          size  = "sm",
          color = "danger"
        ),
        h6("Click Submit without any input", align = "center"),
        h6("to display results for MT042775 (nucleotide)", align = "center"),
        h6("or QHN73805 (protein)", align = "center"),
        br()
      ),
      
      tags$hr(),
      
      
      menuItem(
        "Map",
        tabName = "maps",
        icon = icon("globe")
      ),
      
      menuItem(
        "Data explorer",
        tabName = "dataexplore",
        icon = icon("bar-chart")
      ),
      
      
      menuItem(
        "Options",
        id = "viz_options",
        tabName = "options",
        #icon = icon("bars"),
        icon = icon("cogs"),
        
        # switchInput(
        #   inputId = "countrycol_switch2",
        #   label   = "Color country area",
        #   value   = FALSE,
        #   onStatus = "success", 
        #   offStatus = "danger"
        # ),
        
        materialSwitch(
          inputId = "countrycol_switch",
          label = tags$b("Color country area"), 
          value = FALSE,
          #width = "50%",
          status = "success"
        ),
        
        
        uiOutput("sel_country"),
        
        menuItem(
          "Coloring options",
          tabName = "color_opt",
          icon = icon("palette"),
          
          awesomeRadio(
            inputId = "score_id",
            label = "Select score metric to color individual hits",
            choices = c("Percent identity", "evalue", "bitscore"),
            selected = "Percent identity",
          ),
          
          pickerInput(
            inputId  = "sel_area_col",
            label    = "Color Area By:",
            choices  = unname(color_area_IDs),
            selected = unname(color_area_IDs)[3],
            multiple = FALSE
          ),
          
          # selectInput(
          #   inputId  = "sel_area_col",
          #   label    = "Color Area By:",
          #   choices  = unname(color_area_IDs),
          #   selected = unname(color_area_IDs)[3],
          #   multiple = FALSE
          # ),
          
          tags$hr()
          
        ),
        
        
        menuItem(
          "Score filtering",
          tabName = "score_filtering",
          icon = icon("filter"),
          
          
          
          uiOutput("blastrf_pident"),
          uiOutput("blastrf_evalue"),
          uiOutput("blastrf_bitscore")
          
        )
        
      ),
      
      tags$hr(),
      
      menuItem(
        "FAQ",
        tabName = "faq",
        #icon = icon("user-secret")
        icon = icon("question-circle")
      ),
      
      
      #br(),
      tags$hr()
      
      # h4("Options", align = "center"),
      # h5("Placeh", align = "center")
      #h6("from Fasta or from search box", align = "center"),
      
    )
  ),
  
  #----------------------------------------------------------------------------#
  #                              dashboardBody                                 #
  #----------------------------------------------------------------------------#
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
        )
      )
    ),
    
    tabItems(
      tabItem(
        tabName = "faq",
        fluidRow(
          #img(src="map_my_corona_logo.png", width=100),
          #tags$style(type = "text/css", "#help_main {height: calc(100vh ) !important;}"),
          box(
            id = "help_main",
            width = 5,
            height = 400,
            title = div(img(src="map_my_corona_logo.png", width="60%"), 
                        style="text-align: center;") ,
            #title = img(src="map_my_corona_logo.png", width="60%", align = "center" ),
            status = "warning",
            background = "black",
            tags$hr(),
            h4("MapMyCorona is a blast-server for SARS CoV-2 sequences 
            (protein or DNA) which displays the top hits in a spatial 
            and temporal fashion.", 
               align = "center"), 
            h4("Once displayed, you can filter the results by date or country.",
               align = "center")
          ),
          #tags$style(type = "text/css", "#help_hdsu {height: calc(100vh ) !important;}"),
          box(
            id = "help_hdsu",
            width = 7, 
            height = 400,
            title = img(src="logo_hdsu.png", width="100%"),
            #status = "warning",
            background = "black",
            #tags$hr(),
            h4("MapMyCorona was developed by the 
            Biomedical Genomics Group @ Health Data Science Unit 
            at the BioQuant Center and Medical Faculty Heidelberg",
               align = "center"),
            h4(tags$a(href="https://www.hdsu.org/", 
                      target="_blank",
                      "Visit us here!"),
               align = "center"),
            
            tags$hr(),
            
            h1(tags$a(href="https://github.com/hdsu-bioquant/mapmyflu", 
                      target="_blank",
                      icon("github")), align = "center"),
            #h1(icon("github"), align = "center"),
            h4("If you have any suggestion please create an issue in our GitHub repository",
               tags$a(href="https://github.com/hdsu-bioquant/mapmyflu", 
                      target="_blank", 
                      "hdsu-bioquant/mapmyflu"),
               align = "center")
            
          )
        ),
        
        fluidRow(
          box(
            collapsible = TRUE,
            collapsed   = TRUE,
            title = h4(icon("search"),"Search sequence", align = "center"),
            width = 4, 
            background = "blue",
            
            p("A nucleotide or protein BLAST is run using 
              as query the uploaded sequence or a specified
              sequence in the search box.",
              align = "center"),
            
            h4(icon("cog"), align = "center"),
            h4("BLAST Options", align = "center"),
            p("Number of results: Maximum number of aligned sequences to keep."),
            p("Expectation value (E) threshold: Expectation value threshold for saving hits."),
            p("Percent identity (pident): Only keep hits inside this range.")
            
          ),
          
          box(
            collapsible = TRUE,
            collapsed   = TRUE,
            title = h4(icon("globe"), "Map", align = "center"),
            width = 4,
            background = "blue",
            
            p("Displays a world map showing the geographical location 
              of each hit, hits are clustered together depending on 
              the zoom level, to easily visualize regions with more hits."),
            p("Clicking on a hit shows a stats summary of 
              the BLAST result and also provides a link to the 
              GenBank entry for that particular hit."),
            p("The area of each country with at least one hit 
              is also colored according to the collection date, 
              publication date or pident of its top  hit"),
            p("The map is responsive to all filters provided in the Options tab,
              and to the date range provided on top of the map.")
          ),
          
          box(
            collapsible = TRUE,
            collapsed   = TRUE,
            title = h4(icon("bar-chart"), "Data explorer", align = "center"),
            width = 4,
            background = "blue",
            p("Displays a table with all hits currently shown in the map."),
            p("A button is provided to download the table in csv format.")
          )
          
          
        ),
        
        
        
        fluidRow(
          
          box(
            collapsible = TRUE,
            collapsed   = TRUE,
            title = h4(icon("cogs"), "Options", align = "center"),
            width = 4,
            background = "red",
            p("Provides several filtering options on the displayed hits."),
            p("A drop down menu with a list of all countries with at least one 
              hit, is provided to keep only hits in the selected countries."),
            
            h4(icon("palette"), align = "center"),
            p("Coloring options: options to change the color of individual hits 
              or the country area"),
            
            h4(icon("filter"), align = "center"),
            p("Score filtering: options to filter hits according to their 
              BLAST results stats")
            
          ),
          
          box(
            collapsible = TRUE,
            collapsed   = TRUE,
            title = h4(icon("palette"), "Coloring options", align = "center"),
            width = 4,
            background = "yellow",
            
            p("Options to change the color of individual hits 
              or the country area"),
            p("Select score metric to color individual hits: select which 
              score metric you want to use to color the individual hits"),
            p("Color Area By: select option to change color of countries 
              with at least one hit")
          ),
          
          box(
            collapsible = TRUE,
            collapsed   = TRUE,
            title = h4(icon("filter"), "Score filtering", align = "center"),
            width = 4,
            background = "yellow",
            p("Options to filter hits according to their 
              BLAST results stats"),
            p("Percent identity (pident): keep only hits between this range 
              of pident scores"),
            p("Expectation value (E): keep only hits between this range 
              of evalue scores"),
            p("bitscore: keep only hits between this range 
              of bitscore scores")
            
          )
        ) 
      ),
      
      tabItem(
        tabName = "maps",
        fluidRow(
          
          box(
            height = 100,
            width = 4,
            background = "black",
            # addSpinner(plotOutput(outputId = "gg_data_months"),
            #            spin = "circle", color = "#E41A1C")
            plotOutput(outputId = "gg_data_months")
          ),
          
          # box(
          #   height = 100,
          #   width = 1,
          #   background = "black",
          #   
          #   h6("Color country area", align = "center"),
          #   prettyToggle(inputId = "toggle1",
          #                label_on = "Checked!",
          #                label_off = "Unchecked...",
          #                inline = TRUE),
          #   
          #   materialSwitch(
          #     inputId = "countrycol_switch",
          #     #label = "Color country area", 
          #     value = FALSE,
          #     width = "50%",
          #     status = "success"
          #   )
          # ),
          
          box(
            height = 100,
            width = 4,
            background = "black",
            uiOutput("date_range") 
            
            # sliderTextInput(
            #   inputId = "date_range",
            #   label = "Date Range:",
            #   choices = NULL,
            #   selected = NULL
            # )
            # sliderTextInput(
            #   inputId = "date_range",
            #   label = "Date Range:",
            #   choices = 1:5,
            #   selected = 1
            # )
            
            
          ),
          
          # box(
          #   height = 100,
          #   width = 4,
          #   background = "black",
          #   # Input: Selector for choosing Regulatory Variance metric ----
          #   "Placeholder"
          #   #uiOutput("sel_country")
          # ),
          
          # Dynamic valueBoxes
          # addSpinner(valueBoxOutput("totalhits", width = 2),
          #            spin = "circle", color = "#E41A1C"),
          
          valueBoxOutput("totalhits", width = 2),
          
          valueBoxOutput("hitsafterfil", width = 2)
          
        ),
        
        fluidRow(
          
          #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
          tags$style(type = "text/css", "#map {height: calc(80vh - 80px) !important;}"),
          box(
            width = 12,
            leafletOutput("map")
          )
        )
      ),
      
      tabItem(
        tabName = "dataexplore",
        div(style = 'overflow-x: scroll',
            #DT::DTOutput("blaster"))
            #checkboxInput("dt_sel", "sel/desel all")
            DT::dataTableOutput('blaster_ui')),
        #DT::dataTableOutput("blaster")
        downloadButton('downloadBlaster', 'Download')
      )
      
    )
  ),
  skin = "black"
)



