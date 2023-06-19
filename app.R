## NPF funded project to present citizen science data to the public and land managers
## This app is specific to Acadia National Park, though the area of interest is easily changed


#### Starting up ####

## Functions
# Source the functions
source("00_app_functions.R")

## Data
# Read in the base data
the_data <- read.csv("www/datasets/the_data.csv") %>% 
  arrange(common.name)

# Images
images <- data.frame(src = list.files('www/img/obs')) %>%
  tidyr::separate(col = 'src', c('id', 'user', "img.num", "type"), sep = '_|\\.', remove = FALSE) %>%
  rowwise() %>%
  mutate(user = str_replace_all(user, "\\+", "_"),
         src = paste0("img/obs/", src)) %>% 
  arrange(img.num)

tdate <- today()

options(dplyr.summarise.inform = FALSE)




#### Shiny ui ####

ui <- fluidPage(
  
  ## SET UP
  tags$head(
    tags$link(type = "text/css", rel = "stylesheet", href = "css/style.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$title("Acadia National Park Citizen Science Explorer"),
    tags$script(src = "css/index.js", type = "module", "defer")
  ),

  ### BODY
  tags$body(
    
    ## Navigation
    tags$header(class = "primary-header",
     div(class = "logo-one",
         tags$img(src = "img/schoodic_horizontal.png", alt = "Schoodic Institute at Acadia National Park",
                  class = "cs-logo")),
     div(class = "menu-nav-box",
         tags$nav(tags$ul(`aria-label` = "Primary navigation", role = "list",
                          tags$li(tags$a(href = "#", "Home")),
                          tags$li(tags$a(href = "#intro", "Introduction")),
                          tags$li(tags$a(href = "#summary", "Summary")),
                          tags$li(tags$a(href = "#gallery", "Gallery")),
                          tags$li(tags$a(href = "#spex", "Species Explorer")),
                          tags$li(tags$a(href = "#science", "Science")),
                          tags$li(tags$a(href = "#about", "About"))))),
     tags$nav(class = "mobile-nav",
              tags$button(class = "nav-toggle",
                          span(class = "vegburger")),
              tags$ul(class = "nav-list", `aria-label` = "Mobile navigation",
                      tags$li(class = "nav-item", tags$a(href = "#", class = "nav-link", "Home")),
                      tags$li(class = "nav-item", tags$a(href = "#intro", class = "nav-link", "Introduction")),
                      tags$li(class = "nav-item", tags$a(href = "#summary", class = "nav-link", "Summary")),
                      tags$li(class = "nav-item", tags$a(href = "#spex", class = "nav-link", "Species Explorer")),
                      tags$li(class = "nav-item", tags$a(href = "#science", class = "nav-link", "Science")),
                      tags$li(class = "nav-item", tags$a(href = "#gallery", class = "nav-link", "Gallery")),
                      tags$li(class = "nav-item", tags$a(href = "#about", class = "nav-link", "About"))))
    ),
    
    ## Home
    div(class = "titlebox",
       h1(textOutput("title"), class = "title-homepage"),
       h3("Citizen Science Explorer", class = "subtitle-homepage")
    ),
    div(class = "photo-cred",
        "Photo by Ben Tero"),
    
    ## Intro
    div(class = "spacer", 
        div(class = "intro-box",
            div(class = "anchors",  id = "intro"),
            div(class = "body-title-box",
                icon("book-open",  class = "body-box-icon"), 
                h4("Introduction", class = "body-titles")),
            div(class = "intro-content",
                img(src = "img/citsci.jpg", alt = "A group of citizen scientists collecting data
                    with iNatuarlist", class = "citsci-image"),
                div(class = "intro-text",
                    h2("Welcome to the Acadia National Park citizen science explorer!"),
                    h3("This display summarizes iNaturalist and eBird records from the last week in Acadia National Park.
                        In addition to this summary, we present some recent science that has been made possible
                        by citizen science participation in our work here at Schoodic Institute. To get involved with one of our projects,
                        take a brochure from this display, ask the welcome center hosts for more information, or visit our website!"),
                    div(class = "species-guess",
                        p("Can you guess these species?"),
                        tags$button(class = "btn-purple species-btn", "Reveal answers!"),
                        div(class = "tres-text",
                            p("tree swallow")),
                        div(class = "snap-text",
                            p("common snapping turtle")),
                        div(class = "mon-text",
                            p("monarch")),
                        div(class = "intro-three",
                            img(src = "img/tres.jpeg", alt = "Tree swallow sitting on a wooden post"),
                            img(src = "img/turtle.jpg", alt = "Northern leopard frog in a gravel path"),
                            img(src = "img/monarch.jpg", alt = "Monarch feeding from a red clover flower")))
                    ))
    )),
    
    ## Data summary
    div(class = "summary-box", 
        div(class = "anchors", id = "summary"),
        div(class = "body-title-box",
            icon("table",  class = "body-box-icon"), 
            h4("Data Summary", class = "body-titles")),
        div(class = "inat-box", 
            img(src = "img/inat.png", alt = "iNaturalist", class = "obs-logos"),
            div(class = "sep-line"),
            div(class = "inat-display-grid",
                div(class = "observers-format",
                    h4(tags$b("Observers")),
                    icon("users"),
                    h2(textOutput("total_observers"), class = "summary-stat-text")),
                div(class = "observations-format",
                    h4(tags$b("Observations")),
                    icon("camera-retro"),
                    h2(textOutput("total_observations"), class = "summary-stat-text")),
                div(class = "comgroup-format",
                    h4(tags$b("Most Common Group")),
                    icon("bacteria"),
                    h2(textOutput("top_taxa"), class = "summary-stat-text")),
                div(class = "comsp-format",
                    h4(tags$b("Most Common Species")),
                    icon("leaf"),
                    h2(textOutput("top_sp"), class = "summary-stat-text")),
                # div(class = "percent-format",
                #     icon("database"),
                #     h2(textOutput("percent_text_i"), class = "percent-stat-text"))
                )),
        div(class = "ebird-box",
            img(src = "img/ebird.png", alt = "eBird", class = "obs-logos"),
            div(class = "sep-line"),
            div(class = "ebird-display-grid",
                div(class = "observers-format",
                    h4(tags$b("Checklists")),
                    icon("list-check"),
                    h2(textOutput("total_checklists_e"), class = "summary-stat-text")),
                div(class = "observations-format",
                    h4(tags$b("Total Species")),
                    icon("feather"),
                    h2(textOutput("total_sp_e"), class = "summary-stat-text")),
                div(class = "comgroup-format",
                    h4(tags$b("Total Individuals")),
                    icon("binoculars"),
                    h2(textOutput("total_birds_e"), class = "summary-stat-text")),
                div(class = "comsp-format",
                    h4(tags$b("Most Common Species")),
                    icon("crow"),
                    h2(textOutput("top_sp_e"), class = "summary-stat-text")),
                # div(class = "percent-format",
                #     icon("database"),
                #     h2(textOutput("percent_text_e"), class = "percent-stat-text"))
                )),
        div(class = "data-table-box",
            h3("Explore the past week's data"),
            div(class = "dattab", 
                DT::dataTableOutput("tableout")),
            h4("Data from iNaturalist and eBird and modified by Schoodic Institute at Acadia National Park.")
        )
    ),
    
    ## Gallery
    div(class = "box-photo-gallery",
        div(class = "anchors", id = "gallery"),
        div(class = "body-title-box",
            icon("image",  class = "body-box-icon"),
            h4("Photo Gallery", class = "body-titles")),
        div(class = "grid-wrapper",
            div(tabindex = 0, class = ifelse(length(images$src) < 1, "no-imgs", "hidden"),
                img(src = "img/ice.jpg"),
                div(class = "no-photos",
                    h3("No research grade photos this week."),
                    h3("Go take some!"))),
            div(tabindex = 0, class = ifelse(length(images$src) >= 1, "img-container", "hidden"),
                img(src = images$src[1], alt = images$id[1]),
                div(class = "img-label",
                    h3(images$id[1]),
                    h4("©", images$user[1]))),
            div(tabindex = 0, class = ifelse(length(images$src) >= 2, "img-container", "hidden"),
                img(src = images$src[2], alt = images$id[2]),
                div(class = "img-label",
                    h3(images$id[2]),
                    h4("©", images$user[2]))),
            div(tabindex = 0, class = ifelse(length(images$src) >= 3, "img-container", "hidden"),
                img(src = images$src[3], alt = images$id[3]),
                div(class = "img-label",
                    h3(images$id[3]),
                    h4("©", images$user[3]))),
            div(tabindex = 0, class = ifelse(length(images$src) >= 4, "img-container", "hidden"),
                img(src = images$src[4], alt = images$id[4]),
                div(class = "img-label",
                    h3(images$id[4]),
                    h4("©", images$user[4]))),
            div(tabindex = 0, class = ifelse(length(images$src) >= 5, "img-container", "hidden"),
                img(src = images$src[5], alt = images$id[5]),
                div(class = "img-label",
                    h3(images$id[5]),
                    h4("©", images$user[5]))),
            div(tabindex = 0, class = ifelse(length(images$src) >= 6, "img-container", "hidden"),
                img(src = images$src[6], alt = images$id[6]),
                div(class = "img-label",
                    h3(images$id[6]),
                    h4("©", images$user[6]))),
            div(tabindex = 0, class = ifelse(length(images$src) >= 7, "img-container", "hidden"),
                img(src = images$src[7], alt = images$id[7]),
                div(class = "img-label",
                    h3(images$id[7]),
                    h4("©", images$user[7]))),
            div(tabindex = 0, class = ifelse(length(images$src) >= 8, "img-container", "hidden"),
                img(src = images$src[8], alt = images$id[8]),
                div(class = "img-label",
                    h3(images$id[8]),
                    h4("©", images$user[8])))
        )
    ),

    ## Species explorer
    div(class = "spex-box",
        div(class = "anchors", id = "spex"),
        div(class = "body-title-box",
            icon("tree",  class = "body-box-icon"), 
            h4("Species Explorer", class = "body-titles")),
        div(class = "picker-box",
            pickerInput("spselect",
                        label = "Select a species:",
                        choices = unique(the_data$common.name),
                        options = list(`live-search` = TRUE,
                                       size = 20,
                                       header = "Search Menu"),
                        selected = unique(the_data$common.name)[1],
                        width = "100%",
                        multiple = FALSE)),
          div(class = "map-box",
              leafletOutput("reactspmap", height = "100%"),
              # div(class = "help",
              #     icon("circle-info")),
              # div(class = "help-desc",
              #     h5("1. Click on a circle marker to expand the sightings."),
              #     h5("2. Hover your cursor over any blue marker to see the species."),
              #     h5("3. Click on any blue marker to open a window with a link to the observation."))
              )),
    
    ## Science
    div(class = "science-box",
        div(class = "anchors", id = "science"),
        div(class = "body-title-box",
            icon("microscope",  class = "body-box-icon"), 
            h4("Science", class = "body-titles")),
        div(class = "science-content-1",
            img(src = "img/atsp.jpg", alt = "An American Tree Sparrow perched in a shrub", class = "science-img"),
            div(class = "science-text purp",
                h3("Acadia National Park Winter Birds: 51 Years of Change Along the Coast of Maine"),
                h4("This project analyzed 51 years of citizen science effort from the National Audubon Society
                   Christmas Bird Counts of Acadia National Park. Visit our website for more information on this 
                   project."))),
        div(class = "science-content-2",
            img(src = "img/cape.jpg", alt = "A Calico Pennant in Acadia National Park", class = "science-img"),
            div(class = "science-text green",
                h3("Dragonfly Mercury Project"),
                h4("This project assessed mercury concentrations in dragonfly larvae from 100 different National Park
                   Service units and other protected lands, made possible by citizen scientists. Visit our website for 
                   more information on this project."))),
        div(class = "science-content-3",
            img(src = "img/asco.jpg", alt = "Ascophyllum nodosum or rockweed from Acadia National Park", class = "science-img"),
            div(class = "science-text purp",
                h3("Project ASCO (Assessing Seaweed via Community Observations)"),
                h4("This ongoing project aims to understand how much rockweed exists along the Maine coast so that 
                   we can inform management and harvesting regulations to ensure that this species can thrive in the future.
                   Visit our website for more information on this project.")))
        ),
    
    ## About
    div(class = "about-grid-box",
        div(class = "anchors", id = "about"),
        div(class = "body-title-box",
            icon("circle-info",  class = "body-box-icon"),
            h4("About This Page", class = "body-titles")),
        div(class = "about-info-box",
            h4("Land Acknowledgement Statement"),
            "The observations summarized here are from across the homeland of the Wabanaki, People of the Dawn.
            We recognize and respect Wabanaki relations past and present with this area and the surrounding waters. 
            We support the Penobscot, Passamaquoddy, Maliseet, and Mi’kmaq Nations as they continue to practice and 
            renew their cultural traditions and identities in Acadia and beyond. Honoring Wabanaki sovereignty benefits 
            all who live here, today and in the future.",
            br(),
            h4("Background"),
            "Established in 2004, Schoodic Institute at Acadia National Park is a 501(c)(3) nonprofit
            organization and Acadia National Park’s primary partner in science and education. Based at the
            largest National Park Service Research Learning Center in Winter Harbor, Maine, in
            Wabanaki homeland, Schoodic Institute’s focus is understanding environmental change taking
            place in Acadia and beyond, and helping managers of parks and other protected areas respond
            and adapt to change while engaging people of all ages in science.",
            br(),br(),
            "There is a wealth of scientific data collected by citizen scientists that exists 
            in protected areas like national parks. These data have generally not been analyzed 
            to inform park management or summarized and communicated back out to the park visitors 
            who helped collect the data. This project was created to address these points and assess 
            the biodiversity of Acadia National Park through building a citizen science analysis
            workflow that is transferable across protected areas.",
            br(),br(),
            "This project was made possible in part by a grant from the National Park Foundation.",
            # h4("Code"),
            # "Code and required elements to generate this Shiny app are available on ",
            # a("Github.", href = "https://github.com/Kylelima21/npf_cb_project",
            #   target = "_blank"),
            h4("Sources"),
            "iNaturalist. Available from https://www.inaturalist.org.", 
            "Accessed [", paste(tdate), "].",
            br(), br(),
            "eBird. eBird: An online database of bird distribution and abundance [web application]. 
            eBird, Cornell Lab of Ornithology, Ithaca, New York. Available: http://www.ebird.org.",
            "(Accessed: Date [", paste(tdate), "]).",
            h4("Get in Touch!"),
            "If you are interested in a product like this for a protected area near you, or if you have 
            any questions or concerns, contact Kyle Lima at klima@schoodicinstitute.org.",
            h5(textOutput("today")))
        # div(class = "about-download-box",
        #     #tags$img(src = "img/cit_sci_explorer.png", alt = "Citizen science explorer logo", class = "about-logo"),
        #     h4("Explore the past week's data"),
        #     div(class = "dattab", 
        #         DT::dataTableOutput("tableout")),
        #     # div(class = "download-button",
        #     #     downloadButton("downloadCsv", "Download as CSV", class = "btn-purple")),
        #     h5("Data from iNaturalist and eBird and modified by Schoodic Institute at Acadia National Park.")
        #     )
        ),
        
    ## Footer
    tags$footer(
          div(class = "footer-background",
              div(class = "footer-grid-box",
                  div(class = "footer-left-box",
                      div(tags$img(src = "img/schoodic_horizontal.png", alt = "Schoodic Institute", class = "footer-logo")),
                      div(class = "logo-text", tags$em("Our mission is inspiring science, learning, and community for a changing world."))),
  
                  div(class = "footer-nav-box",
                      tags$ul(`aria-label` = "Footer navigation", role = "list",
                              tags$li(tags$a(href = "#", "Home")),
                              tags$li(tags$a(href = "#intro", "Introduction")),
                              tags$li(tags$a(href = "#summary", "Summary")),
                              tags$li(tags$a(href = "#gallery", "Gallery")),
                              tags$li(tags$a(href = "#spex", "Species Explorer")),
                              tags$li(tags$a(href = "#science", "Science")),
                              tags$li(tags$a(href = "#about", "About")))),
  
                  div(class = "footer-copyright-box",
                      textOutput("copyright_txt")),
  
                  div(class = "footer-link-box",
                      div(class = "footer-image-box",
                            div(tags$img(src = "img/acad_back.jpg", alt = "Acadia National Park scenery", class = "footer-image")),
                            div(class = "bottom-right", h4("© Ben Tero"))))
                  )
                  )
      )
  )
)







### SHINY SERVER ###

server <- function(input, output, session) {
  
  ## Title page header
  output$title <- renderText("Acadia National Park")
  
  ## Leaflet for eBird obs
  output$emap <- renderLeaflet({ 
    leaflet_summary(the_data %>% filter(source == "eBird"))
  })
  
  ## Leaflet for eBird obs
  output$imap <- renderLeaflet({ 
    leaflet_summary(the_data %>% filter(source == "iNaturalist"))
  })
  
  ## Pie Chart
  output$percentplot <- renderPlot({
    the_data %>%
      group_by(source) %>% 
      summarise(count = length(source)) %>% 
      mutate(category = "citsci",
             percent = round((count/sum(count)*100), 0)) %>%
      ggplot(aes(x = category, y = percent, fill = source)) +
      geom_col() + 
      coord_flip() +
      guides(fill = guide_legend(reverse = TRUE)) +
      geom_text(aes(x = category, y = percent, label = percent, group = source),
                position = position_stack(vjust = 0.5), size = 7, color = "#eae7e7") +
      scale_fill_manual(values = c("#0d042ad9", "#0c3e13d9")) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            plot.background = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            legend.background = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 20, color = "black"),
            legend.key.size = unit(1, "cm"),
            legend.spacing.x = unit(1, "cm"),
            legend.position = "bottom")
  }, bg = "transparent")
  
  
  ## Reactive data frame for Species Explorer tab
  speciesreact <- reactive({
    the_data %>%
      filter(common.name == input$spselect)
  })
  
  ## Reactive map to display species obs for Species Explorer tab - uses species_reactive_db
  output$reactspmap <- renderLeaflet({ 
    species_leaflet(speciesreact())
  })
  
  ### iNat
  ## Text output for the top recorded species
  output$top_sp <- renderText({
    species <- the_data %>% 
      filter(source == "iNaturalist") %>% 
      group_by(scientific.name, common.name) %>%
      summarise(count = length(user.id)) %>%
      arrange(desc(count))
      
    paste(species$common.name[1])
          #, " (", species$count[1], " observations)")
  })
  
  ## Text output for the group with the most observations
  output$top_taxa <- renderText({
    taxon <- the_data %>%
      filter(source == "iNaturalist") %>% 
      group_by(iconic.taxon.name) %>%
      summarise(count = length(iconic.taxon.name)) %>%
      arrange(desc(count))
    
    paste(taxon$iconic.taxon.name[1])
          #" (", taxon$count[1], " observations)")
  })
  
  ## Text output for the number of total observers
  output$total_observers <- renderText({
    observers <- the_data %>%
      filter(source == "iNaturalist") %>% 
      group_by(user.id, user.login) %>%
      summarise(count = length(user.id)) %>%
      arrange(desc(count))
    
    paste0(length(observers$user.id))
  })
  
  ## Total observations
  output$total_observations <- renderText({
    observers <- the_data %>%
      filter(source == "iNaturalist") %>% 
      group_by(user.id, user.login) %>%
      summarise(count = length(user.id)) %>%
      arrange(desc(count))
    
    paste0(sum(observers$count))
  })
  
  ### eBird
  ## Text output for the top recorded species
  output$top_sp_e <- renderText({
    species <- tibble(the_data) %>% 
      filter(source == "eBird") %>% 
      group_by(scientific.name, common.name) %>%
      summarise(count = length(common.name)) %>%
      arrange(desc(count))
    
    paste(species$common.name[1])
  })
  
  ## Text output for the group with the most observations
  output$total_sp_e <- renderText({
    taxon <- the_data %>%
      filter(source == "eBird") %>% 
      group_by(common.name) %>%
      summarise(count = length(common.name)) %>% 
      arrange(desc(count))
    
    paste(length(taxon$common.name))
  })
  
  ## Text output for the number of total checklists
  output$total_checklists_e <- renderText({
    observers <- the_data %>%
      filter(source == "eBird") %>% 
      group_by(checklist) %>%
      summarise(count = length(unique(checklist))) %>%
      arrange(desc(count))
    
    paste(length(observers$checklist))
  })
  
  ## Text output for the number of total birds
  output$total_birds_e <- renderText({
    obs <- the_data %>%
      filter(source == "eBird") %>% 
      mutate(count = replace(count, is.na(count), 1))
    
    paste(sum(obs$count))
  })
  
  ## Data table for display
  output$tableout <- DT::renderDataTable({
    dat <- tibble(the_data) %>% 
      select(scientific.name, common.name, observed.on, place.guess, source)
    
    DT::datatable(dat, options = list(pageLength = 10, dom = 'Brtip', scrollX = TRUE),
                  rownames = FALSE, filter = 'top', colnames = c("Scientific name", 
                  "Common name", "Date observed", "Location", "Data source"))
  })
  
  
  # ## Text output for species of interest description
  # output$descrip_sp <- renderText({
  #   "Scientists at Acadia National Park are using your eBird and iNaturalist observations
  #        to find out when and where certain species are being seen. We have compiled a list of 
  #        species that are important to park managers. If they are reported by people like you,
  #        our program summarizes and reports that information to park managers."
  # })

  # ## Text output for new park species
  # output$newsp <- renderText({
  #   if (length(new_species$scientific.name) >= 1) {
  #     return(paste0("There were ", length(unique(new_species$scientific.name)), " species recorded 
  #                   in the last 7 days that have not been recorded in the park before."))
  #   } else {
  #     return(paste0("There were no species recorded in the last 7 days that have not been 
  #                   recorded in the park before."))
  #   }
  # })
  # 
  # ## Text output for T/E species
  # output$te <- renderText({
  #   if (length(te_species$scientific.name) >= 1) {
  #     return(paste0("There were ", length(unique(te_species$scientific.name)), " species recorded 
  #                   in the last 7 days that have not been recorded in the park before."))
  #   } else {
  #     return(paste0("There were no species recorded in the last 7 days that have not been 
  #                   recorded in the park before."))
  #   }
  # })
  # 
  # ## Text for rare species
  # output$rare <- renderText({
  #   if (length(rare_species$scientific.name) >= 1) {
  #     return(paste0("There were ", length(unique(rare_species$scientific.name)), " species recorded 
  #                   in the last 7 days that have not been recorded in the park before."))
  #   } else {
  #     return(paste0("There were no species recorded in the last 7 days that have not been 
  #                   recorded in the park before."))
  #   }
  # })
  # 
  # ## Text for invasives and pests
  # output$invasive <- renderText({
  #   if (length(invasive_species$scientific.name) >= 1) {
  #     return(paste0("There were ", length(unique(invasive_species$scientific.name)), 
  #                   " invasive and/or pest species recorded in the last 7 days."))
  #   } else {
  #     return(paste0("There were no invasive or pest species recorded in the last 7 days 
  #                   in the park."))
  #   }
  # })
  
  ## Text for today's date
  output$today <- renderText({
    date <- today()
    date <- format(date, "%B %d, %Y")
    paste0("Last updated: ", date)
  })
  
  ## Output to download data as a CSV
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste0("anp_citsci_data_", str_replace_all(today(), "-", ""), ".csv")
    },
    content = function(file) {
      inat_data_download = the_data %>% select(-user.id)
      
      write.csv(inat_data_download, file, row.names = F)
    })
  
  ## Copyright text
  output$copyright_txt <- renderText({
    date <- today()
    paste0("© ", year(date), " Schoodic Institute at Acadia National Park")
  })
  
}



#### Run the app ####

shinyApp(ui, server)



