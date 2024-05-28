#' @title quotes_sample
#' @description Samples one quote at a time to be used in the Quotes app.
#' @param x This argument takes any character vector, and will print one quote from the quptes vector.
#' @return Returns the imputed character vector, in this case quotes.
#' @export
quotes_sample <- function() {
  quotes <- c(
    "Don't count the days, make the days count. —Muhammad Ali",
    
    "There is only one corner of the universe you can be certain of improving, and that's your own self. -Aldous Huxley",
    
    "People do not decide their futures, they decide their habits and their habits decide their futures. —Frederick M. Alexander",
    
    "If it doesn't challenge you, it won't change you. —Fred DeVito",
    
    "You must be prepared to work always without applause. —Ernest Hemingway",
    
    "There is nothing permanent except change. —Heraclitus",
    
    "Discipline is choosing between what you want now and what you want most. —Abraham Lincoln",
    
    "The more relaxed you are, the better you are at everything: the better you are with your loved ones, 
    the better you are with your enemies, the better you are at your job, the better you are with yourself. —Bill Murray",
    
    "Never give up on a dream just because of the time it will take to accomplish it. 
    The time will pass anyway. —Earl Nightingale",
    
    "Personal growth becomes so addictive once you realize that it’s always 
    possible to improve your experience of being. —Xan Oku",
    
    "Your mind is a garden, your thoughts are the seeds, you can grow flowers or you can grow weeds. —Osho",
    
    "Change your thoughts and you change your world. —Norman Vincent Peale",
    
    "The virtue of self-discipline itself is a greater source of pleasure 
    than the external objects of our desire. —Donald Robertson",
    
    "I am learning everyday to allow the space between where I want to be and where 
    I am to inspire me and not terrify me. —Tracee Ellis Ross",
    
    "Mindfulness helps us get better at seeing the difference between what is happening 
    and the stories we tell ourselves about what is happening, stories that get in 
    the way of direct experience. Often such stories treat a fleeting state 
    of mind as if it were our entire and permanent self. —Sharon Salzberg",
    
    "Your mind is your instrument, learn to be its master and not its slave. —Remez Sasson",
    
    "You cannot change what you refuse to confront. —John Spence",
    
    "A man who procrastinates in his choosing will eventually have his 
    choice made for him by circumstance. —Hunter S. Thompson",
    
    "Self-control is just empathy with your future self. —Unknown",
    
    "Deal with things as they come, not as you fear them. —Unknown",
    
    "Everyone must choose one of two pains: The pain of discipline or the pain of regret. —Unknown",
    
    "Putting off something today means putting off something you could do tomorrow. —Unknown"
  )
  quote <- sample(quotes, 1)
  print(quote)
}
quotes_sample()

#' @title current
#' @description Outputs the current date in a suitable format for the journaling app.
#' @param x Takes any value and outputs the current date.
#' @return Outputs a string of the current date in the format Month Day-suffix Year
#' @export
current <- function(x) {
  day <- lubridate::day(Sys.Date())
  month <- lubridate::month(Sys.Date(), label = TRUE, abbr = FALSE)
  year <- lubridate::year(Sys.Date())
  
  suffix <- function(day) {
    if (day %% 10 == 1 && day != 11) {
      return(paste0(day, "st"))
    } else if (day %% 10 == 2 && day != 12) {
      return(paste0(day, "nd"))
    } else if (day %% 10 == 3 && day != 13) {
      return(paste0(day, "rd"))
    } else {
      return(paste0(day, "th"))
    }
  }
  
  current <- paste(month, suffix(day), year)                                                       # assigning the date as "Month Day Number Year" (e.g. May 21st 2024)
  
  paste(current)                                                                                   # file naming
}

#' @title journal_prompt
#' @description Calculates the number of pages necessary for the journal prompt, and creates a temporary PDF with the text correctly formatted.
#' @param text Takes any character vector to input as the journal prompt.
#' @return A temporary PDF with all the text from the journal prompt in the correct format for the journaling app.
#' @export
journal_prompt <- function(text) {
  
  max_words <- 240                                                                                 # maximum number of words allowed on each page
  
  page_text <- function(words, max_words) {                                                        # splitting text into different pages for proper formatting
    num_pages <- ceiling(length(words) / max_words)                                                # counting number of pages necessary based on maximum word count per page
    pages <- vector("list", num_pages)                                                             # creating a vector for the pages
    
    for (i in seq_len(num_pages)) {                                                                # iterating through all the pages
      start <- (i - 1) * max_words + 1
      end <- min(i * max_words, length(words))
      pages[[i]] <- paste(words[start:end], collapse = " ")                                        # pasting the text from the start to the end of what should be pasted for that page
    }
    return(pages)
  }
  
  words <- unlist(strsplit(text, " "))                                                             # making all text into character words (e.g. "hello world" becomes "hello" "world")
  pages <- page_text(words, max_words)                                                             # generating the pdf pages
  
  temp_pdf <- tempfile(fileext = ".pdf")
  
  pdf(temp_pdf, width = 5, height = 7)                                                             # creating the PDF with nice title for current date
  
  for (page_text in pages) {
    graphics::plot.new()
    graphics::par(mar = c(2, 2, 2, 2))
    graphics::text(x = 0.5, y = 0.5, labels = paste(strwrap(page_text, width = 50), collapse = "\n"), cex = 1)
    graphics::title(main = paste("Journal Prompt:", current()))                                    # writes the title with as "Journal_Prompt: current date"
  }
  grDevices::dev.off()
  
  return(temp_pdf)
}

#' @title shiny_app
#' @description This functions opens up a Shiny interface with interactive tools. Users can select tools to generate quotes, journal, use a pomodoro timer, and make a to do list. Once the user is satisfied with their journal prompt, they can export it as a PDF file.
#' @param app This argument opens up the journaling Shiny app.
#' @return Returns a user interface with tools.
#' @export
shiny_app <- function() {
  ui <- shiny::fluidPage(tags$style(HTML("
    body {background-color: #434537; color: #E7E9C3;}
    .quote-text {font-size: 20px; font-style: italic; margin-top: 20px;}
    .button-row {margin-top: 10px;}
    .selected-panel {border-radius: 10px; padding: 20px; box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1); margin-top: 20px;}
    .btn-custom {background-color: #000000; color: #fff; border: none; margin-right: 5px;}
    .form-control-custom {background-color: #000000; color: #fff; border: none; margin-bottom: 10px;}
    .darker-label {color: #000000; font-weight: bold;} /* Custom CSS class for darker label */
    .tab-title {color: #000000; font-weight: bold;} /* Custom CSS class for dark green tab title */
    .black-label {color: #000000; font-weight: bold;} /* Custom CSS class for black labels */
    .black-text {color: #000000; /* Black text color */}")),
    hr(),
    titlePanel("Digital Minimalist"),                                                                # title of the app
    fluidRow(column(4, div(style = "font-size: 40px; font-weight: bold;", textOutput("date")),       # display current date in large text
             div(style = "font-size: 30px;", textOutput("time")),                                    # display current time
             checkboxGroupInput("apps", "Select Tools:",                                             # apps to select from
                                choices = c("Pomodoro", "To-Do List", "Journaling", "Quotes"),
                                selected = "Quotes"),                                                # selected by default
             uiOutput("quote_buttons"),                                                              # buttons for quotes app
             textOutput("quote")                                                                     # quotes text
      ),
      column(7, uiOutput("selected"))
    )
  )
  
  # Server logic
  server <- function(input, output, session) {
    
    values <- shiny::reactiveValues(                                                                 # the quote to interact with
      current_quote = quotes_sample(),                                                               # sampling one quote at a time
      playing = TRUE,
      last_update = Sys.time()
    )
    
    shiny::observeEvent(input$next_quote, {                                                          # generating a new quote when clicking "Next"
      values$current_quote <- quotes_sample()                                                        # sampling one quote from the list randomly
      values$last_update <- Sys.time()                                                               # resetting to the last time a quote was generated
    })
    
    shiny::observeEvent(input$pause, {                                                               # when the pause button is clicked, then
      shinyjs::disable("pause")                                                                                             # disable the pause button from being clicked
      values$playing <- FALSE                                                                        # pause the quotes generation
      shinyjs::enable("play")                                                                                 # allow the play button to be clicked
    })
    
    shiny::observeEvent(input$play, {                                                                # when the play button is clicked, then
      shinyjs::disable("play")                                                                                              # disable play button from being clicked
      values$playing <- TRUE                                                                         # generate a quote every 10 seconds
      shinyjs::enable("pause")                                                                                # allow pause button to be clicked
    })
    
    shiny::observe({                                                                                 # when the play button is pressed, new quotes are generated every 10 seconds
      shiny::invalidateLater(1000, session)
      if (values$playing & difftime(Sys.time(), values$last_update, units = "secs") > 10) {          # if quotes are being generated & difference in time between the current and last quote time is greater than 10 seconds                                                                      # disable next quote button
        values$current_quote <- quotes_sample()                                                    # then generate a new quote
        values$last_update <- Sys.time()                                                           # and update Sys.time() with the time the new quote is generated
      }
    })
    
    # Quote generation logic
    output$quote <- shiny::renderText({
      if ("Quotes" %in% input$apps) {
        values$current_quote
      } else {
        NULL
      }
    })
    
    output$quote_buttons <- shiny::renderUI({
      if ("Quotes" %in% input$apps) {
        shiny::fluidRow(
          shiny::actionButton("next_quote", "Next"),
          shiny::actionButton("pause", "Pause"),
          shiny::actionButton("play", "Play")
        )
      } else {
        NULL
      }
    })
    
    observe({                                                                                        # INTIAL INTERFACE STATE: play is enabled and pause is disabled
      shinyjs::disable("play")
      shinyjs::enable("pause")
    })
    
    # To-Do List logic
    tasks <- shiny::reactiveValues(items = list())                                                   # making the tasks reactive
    
    shiny::observeEvent(input$addButton, {                                                           # clicking to "add" the task in the box to the to do list
      if (nzchar(input$customText)) {                                                                # checking if there is any text in the box; TRUE = run the inside of the brackets
        task_id <- paste0("task", length(tasks$items) + 1)                                           # assigns a "task_n" to each task imputed based on how many tasks there already are
        tasks$items[[task_id]] <- list(label = input$customText,                                     # for each "task_n", assign the imputed text from the box
                                       checked = FALSE)                                              # for each task, assign it as not checked (FALSE) when first adding it to the to do list
        shiny::updateTextInput(session, "customText", value = "")                                    # update the text box as empty once the new task is added
      }
    })
    
    output$checkboxes <- shiny::renderUI({                                                           # rendering the checkboxes
      
      # Adding all of the checked items into this list
      checked_items <- lapply(names(tasks$items), function(id) {                                     # (l)applying the function to each of the retrieved tasks_ids
        if (tasks$items[[id]]$checked) {                                                             # if the "checked" item is "TRUE" then run the next code (so for checked items)
          shiny::checkboxInput(id, label = tasks$items[[id]]$label,                                  # looking at each of the task_ids for the "checked" items
                               value = TRUE)                                                         # for the checked items, we assign them a "TRUE" value in the Shiny app
        }
      })
      
      # Adding all of the unchecked items into this list
      unchecked_items <- lapply(names(tasks$items), function(id) {
        if (!tasks$items[[id]]$checked) {                                                            # if the "checked" item is "FALSE" then run the next code (so for unchecked items)
          shiny::checkboxInput(id, label = tasks$items[[id]]$label,                                  # looking at each of the task_ids for the "unchecked" items 
                               value = FALSE)                                                        # for the unchecked items, we assign them a "FALSE" value in the Shiny app
        }
      })
      
      # Ordering the tasks: checked tasks move towards the bottom of the list
      do.call(tagList, c(unchecked_items, checked_items))
    })
    
    shiny::observe({
      lapply(names(tasks$items), function(id) {                                                      # (l)applying the function to all of the tasks
        shiny::observeEvent(input[[id]], {                                                           # whenever a checkbox state changes, this loop runs
          tasks$items[[id]]$checked <- input[[id]]                                                   # updating the checked field for its corresponding task_id based on the new state of the checkbox
        })
      })
    })
    
    # Pomodoro logic
    timer <- shiny::reactiveVal(0) 
    active <- shiny::reactiveVal(FALSE)
    period <- shiny::reactiveVal("work")
    pomodoros <- shiny::reactiveVal(0)                                                               # to count completed pomodoros
    
    shiny::observeEvent(input$start, {
      if (!active()) {
        timer(ifelse(period() == "work", as.numeric(input$work_length) * 60, as.numeric(input$break_length) * 60))
        active(TRUE)                                                                                 # starts the timer when the start button is pressed
      }
    })
    
    shiny::observeEvent(input$stop, {
      active(FALSE)                                                                                  # stops the timer when the pause button is pressed
    })
    
    shiny::observeEvent(input$reset, {
      timer(ifelse(period() == "work", as.numeric(input$work_length) * 60, as.numeric(input$break_length) * 60))
      active(FALSE)                                                                                  # resets the timer
      pomodoros(0)
      output$status <- shiny::renderText({
        stringr::str_glue("Completed rounds: {pomodoros()}")                                         # status message on reset
      })
    })                                                
    
    shiny::observe({
      if (period() == "work") {
        timer(as.numeric(input$work_length) * 60)                                                    # starts the timer for the specified work time
      } else {
        timer(as.numeric(input$break_length) * 60)                                                   # starts the timer for the specified break time if work complete
      }
    })
    
    output$timeleft <- shiny::renderText({
      paste('Time remaining: ', sprintf("%02d:%02d", timer() %/% 60, timer() %% 60))                 # gives you the amount of remaining time left
    })
    
    next_period <- function() {
      if (period() == "work") {
        beepr::beep(sound = "complete")                                                              # produces a sound notification when work timer is up
        showModal(modalDialog(title = HTML('<span style="color: black;">Break Time</span>'),         # tells you to take a break when work timer is up
                              HTML('<span style="color: black;">Time to take a break!</span>')))
        period("break")                                                                              # update period to break
        timer(as.numeric(input$break_length) * 60)
      } else {
        beepr::beep(sound = "ping")                                                                  # produces a sound notification when break timer is up
        shiny::showModal(modalDialog(title = HTML('<span style="color: black;">Work Time</span>'),   # tells you to get back to work when break timer is up
                                     HTML('<span style="color: black;">Time to focus!</span>')))
        period("work")                                                                               # update period to work
        pomodoros(pomodoros() + 1)                                                                   # add +1 to count of completed pomodoros
        timer(as.numeric(input$work_length) * 60)
      }
    }
    
    shiny::observe({                                                                                 # timer countdown by seconds
      shiny::invalidateLater(1000, session)
      shiny::isolate({
        if (active()) {
          timer(timer() - 1)
          if (timer() < 1) {
            next_period()
          }
        }
      })
    })
    
    output$downloadPDF <- shiny::downloadHandler(
      filename = paste("Journal_Prompt_", current(), ".pdf", sep = ""),                              # file naming
      content = function(file) {
        pdf_file <- journal_prompt(input$text)
        file.copy(pdf_file, file)
      }
    )
    
    output$status <- shiny::renderText({
      stringr::str_glue("Completed rounds: {pomodoros()}")                                           # displaying number of completed pomodoros
    })
    
    output$selected <- shiny::renderUI({                                                             # render selected UI based on apps selected
      selected <- input$apps
      
      selected_ui <- list()
      if ("Pomodoro" %in% selected) {                                                                # display pomodoro app if selected
        selected_ui[["pomodoro"]] <- div(
          tabsetPanel(
            tabPanel(tags$span("Pomodoro", class = "tab-title"),                                     # title of the app
                     hr(),
                     selectInput('work_length', tags$span('Length of focus', class = 'black-label'), 
                                 choices = c(15, 25, 60), selected = 25),                            # selecting the amount of work time
                     selectInput('break_length', tags$span('Length of break', class = 'black-label'), 
                                 choices = c(5, 10, 30), selected = 5),                              # selecting the amount of break time
                     actionButton('start', 'Start Timer', class = "btn-custom"),                     # starting the timer
                     actionButton('stop', 'Pause Timer', class = "btn-custom"),                      # pausing the timer
                     actionButton('reset', 'Reset Timer', class = "btn-custom"),                     # resetting everything
                     textOutput('timeleft'),                                                         # time remaining for the break / work timer
                     textOutput('status')                                                            # checking the status of the pomodoro
            )
          ),
          class = "selected-panel",
          style = "background-color: #7D8069;"
        )
      }
      if ("To-Do List" %in% selected) {                                                              # display To-Do List app if selected
        selected_ui[["todo"]] <- div(
          tabsetPanel(
            tabPanel(tags$span("To Do List", class = "tab-title"),
                     hr(),
                     sidebarLayout(
                       sidebarPanel(
                         textInput("customText", label = tags$span("Enter task below:", class = "darker-label"), value = ""),
                         actionButton("addButton", "Add Task", class = "btn-custom")
                       ),
                       mainPanel(
                         uiOutput("checkboxes")
                       )
                     )
            )
          ),
          class = "selected-panel",
          style = "background-color: #838459;"
        )
      }
      if ("Journaling" %in% selected) {                                                              # display Journaling app if selected
        selected_ui[["journaling"]] <- div(
          tabsetPanel(
            tabPanel(tags$span("Journaling", class = "tab-title"),
                     hr(),
                     textAreaInput("text", "How are you feeling today?", rows = 10,
                                   placeholder = "Write your journal entry here.\n\nWhen you're ready, click the 'Download PDF' button to download your journal entry."),
                     downloadButton("downloadPDF", "Download PDF", class = "btn-custom")
            )
          ),
          class = "selected-panel",
          style = "background-color: #5D6040;"
        )
      }
      do.call(tagList, selected_ui)                                                                  # renders multiple UI elements
    })
    output$date <- shiny::renderText({
      current()
    })
    observe({
      invalidateLater(1000, session)
      output$time <- renderText({
        format(Sys.time(), "%I:%M %p")
      })
    })
  }
  shinyApp(ui, server)
}
