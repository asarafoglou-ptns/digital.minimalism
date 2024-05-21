#' @title journal
#' @description This functions opens up a Shiny app where users can journal inside a text box. Once the user is satisfied with their journal prompt, they can export it as a PDF file with the title as the current day they wrote the journal prompt.
#' @param journal This argument opens up the journaling Shiny app.
#' @return Returns a PDF file of the entered text of the journal prompt.
#' @export
journal <- function(journal) {
  
  ui <- shiny::fluidPage(
    hr(),
    titlePanel("Journaling Tool"),                                                                  # app name
    fluidRow(
      column(4,
             textAreaInput("text", "How are you feeling today?", rows = 10, 
                           placeholder = "Write your journal entry here. 
                    
When you're ready, click the 'Download PDF' button to download your journal entry."),             # instructions inside of text box                               
             downloadButton("downloadPDF", "Download PDF")
      )
    )
  )
  
  journal_prompt <- function(text) {
    
    current_date <- Sys.Date()
    day <- lubridate::day(current_date)
    month <- lubridate::month(current_date, label = TRUE, abbr = FALSE)
    year <- lubridate::year(current_date)
    
    suffix <- function(day) {
      if (day == 11 | 
          day == 12 | 
          day == 13) {
        return(paste0(day, "th"))
      } else if (substr(day, nchar(day), nchar(day)) == "1") {
        return(paste0(day, "st"))
      } else if (substr(day, nchar(day), nchar(day)) == "2") {
        return(paste0(day, "nd"))
      } else if (substr(day, nchar(day), nchar(day)) == "3") {
        return(paste0(day, "rd"))
      } else {
        return(paste0(day, "th"))
      }
    }
    current <- paste(month, suffix(day), year)                                                      # assigning the date as "Month Day Number Year" (e.g. May 21rst 2024)
    
    
    max_words <- 240                                                                                # maximum number of words allowed on each page
    
    page_text <- function(words, max_words) {                                                       # splitting text into different pages for proper formatting
      num_pages <- ceiling(length(words) / max_words)                                               # counting number of pages necessary based on maximum word count per page
      pages <- vector("list", num_pages)                                                            # creating a vector for the pages
      
      for (i in seq_len(num_pages)) {                                                               # iterating through all the pages
        start <- (i - 1) * max_words + 1                                                               
        end <- min(i * max_words, length(words))
        pages[[i]] <- paste(words[start:end], collapse = " ")                                       # pasting the text from the start to the end of what should be pasted for that page
      }
      return(pages)
    }
    
    words <- unlist(strsplit(text, " "))                                                            # making all text into character words (e.g. "hello world" becomes "hello" "world")
    pages <- page_text(words, max_words)                                                            # generating the pdf pages
    
    pdf_file <- pdf(paste("Journal_Prompt:", current(), ".pdf"), width = 5, height = 7)             # creating the PDF with nice title for current date
    
    for (page_text in pages) {
      plot.new()
      par(mar = c(2, 2, 2, 2))
      text(x = 0.4, y = 0.5, labels = paste(strwrap(page_text, width = 50), collapse = "\n"), cex = 1)
      title(main = paste("Journal Prompt:", current()))                                             # writes the title with as "Journal_Prompt: current date"
    }
    dev.off()
    
    return(pdf_file)
  }
  
  
  server <- function(input, output) {                                                               # downloading the file as PDF to the computer
    output$downloadPDF <- downloadHandler(
      filename <- function() {
        paste("Journal Prompt:", current(), ".pdf", sep = "")                                       # file naming
      },
      
      content <- function(file) {
        pdf_file <- journal_prompt(input$text)
        file.copy(pdf_file, file)
      }
    )
  }
  
  
  shinyApp(ui, server)
}

#' @title pomodoro
#' @description This functions allows users to use a pomodoro timer for focused work or study. Users can specify differing work and break lengths.
#' @param pomodoro This argument opens up the pomodoro timer Shiny app.
#' @return Starts, pauses and resets a pomodoro timer
#' @export
pomodoro <- function(pomodoro){
  
  ui <- fluidPage(
  hr(),
  titlePanel('Pomodoro'),                                                                  # title of the app
  selectInput('work_length', 'Length of focus', choices = c(15, 25, 60), selected = 25),   # selecting the amount of work time
  selectInput('break_length', 'Length of break', choices = c(5, 10, 30), selected = 5),    # selecting the amount of break time
  actionButton('start', 'Start Timer'),                                                    # starting the timer
  actionButton('stop', 'Pause Timer'),                                                     # pausing the timer
  actionButton('reset', 'Reset Timer'),                                                    # resetting everything
  textOutput('timeleft'),                                                                  # time remaining for the break / work timer
  textOutput('status')                                                                     # checking the status of the pomodoro
)

server <- function(input, output, session) {
  timer <- reactiveVal(0) 
  active <- reactiveVal(FALSE)
  period <- reactiveVal("work")
  pomodoros <- reactiveVal(0)                                                              # count completed pomodoros
  
  observeEvent(input$start, {
    if (!active()) {
      timer(ifelse(period() == "work", as.numeric(input$work_length) * 60, as.numeric(input$break_length) * 60))
      active(TRUE)                                                                           # starts the timer when the start button is pressed
    }
  })
  
  observeEvent(input$stop, {
    active(FALSE)                                                                            # stops the timer when the pause button is pressed
  })
  
  observeEvent(input$reset, {
    timer(ifelse(period() == "work", as.numeric(input$work_length) * 60, as.numeric(input$break_length) * 60))
    active(FALSE)                                                                          # resets the timer
    pomodoros(0)
    output$status <- renderText(" ")                                                       # status message on reset
  })
  
  observe({
    if (period() == "work") {
      timer(as.numeric(input$work_length) * 60)                                            # starts the timer for the specified work time
    } else {
      timer(as.numeric(input$break_length) * 60)                                           # starts the timer for the specified break time if work complete
    }
  })
  
  output$timeleft <- renderText({
    paste('Time remaining: ', sprintf("%02d:%02d", timer() %/% 60, timer() %% 60))         # gives you the amount of remaining time left
  })
  
  next_period <- function() {
    if (period() == "work") {
      period("break")
      timer(as.numeric(input$break_length) * 60)
      beepr::beep(sound = "complete")                                                      # produces a sound notification when work timer is up
      showModal(modalDialog(title = 'Break Time', 'Time to take a break!'))                # tells you to take a break when work timer is up
    } else {
      period("work")
      pomodoros(pomodoros() + 1)                                                           # add +1 to count of completed pomodoros
      beepr::beep(sound = "ping")                                                          # produces a sound notification when break timer is up
      output$status <- renderText(stringr::str_glue("Completed rounds: {pomodoros()}"))    # provides number of completed pomodoros
      showModal(modalDialog(title = 'Work Time', 'Time to focus!'))                        # tells you to get back to work when break timer is up
      timer(as.numeric(input$work_length) * 60)
    }
  }
  
  observe({                                                                                # timer countdown by seconds
    invalidateLater(1000, session)
    isolate({
      if (active()) {
        timer(timer() - 1)
        if (timer() < 1) {
          next_period()
        }
      }
    })
  })
}

shinyApp(ui, server)
}


#' @title To do list
#' @description This function allows users to imput specified tasks, which then populate to the right side of the screen. Users can then check off all done tasks which automatically move down to the bottom.
#' @param todolist This argument opens up the to do list Shiny app.
#' @return Opens an interactive to do list with checkboxes
#' @export
todolist <- function(todolist) {

ui <- fluidPage(
  titlePanel("To Do List"),                                                       # title
  sidebarLayout(sidebarPanel(div(style="width: 200px;",                           # width of the panel
                                 textInput("customText", "Enter task below:", value = ""),               # task text input
                                 actionButton("addButton", "Add To List")                                # adding the task text to the to do list
  )
  ),
  mainPanel(
    uiOutput("checkboxes")                                                      # adding check boxes next to the items
  )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(items = list())                                        # making the tasks reactive
  
  observeEvent(input$addButton, {                                                 # clicking to "add" the task in the box to the to do list
    if (nzchar(input$customText)) {                                               # checking if there is any text in the box; TRUE = run the inside of the brackets
      task_id <- paste0("task", length(values$items) + 1)                         # assigns a "task_n" to each task imputed based on how many tasks there already are
      values$items[[task_id]] <- list(label = input$customText,                   # for each "task_n", assign the imputed text from the box
                                      checked = FALSE)                            # for each task, assign it as not checked (FALSE) when first adding it to the to do list
      updateTextInput(session, "customText", value = "")                          # update the text box as empty once the new task is added
    }
  })
  
  output$checkboxes <- renderUI({                                                 # rendering the checkboxes
    
    # Adding all of the checked items into this list
    checked_items <- lapply(names(values$items), function(id) {                   # (l)applying the function to each of the retrieved tasks_ids
      if (values$items[[id]]$checked) {                                           # if the "checked" item is "TRUE" then run the next code (so for checked items)
        checkboxInput(id, label = values$items[[id]]$label,                       # looking at each of the task_ids for the "checked" items
                      value = TRUE)                                               # for the checked items, we assign them a "TRUE" value in the Shiny app     
      }
    })
    
    # Adding all of the unchecked items into this list
    unchecked_items <- lapply(names(values$items), function(id) {                 # (l)applying the function to each of the retrieved tasks_ids
      if (!values$items[[id]]$checked) {                                          # if the "checked" item is "FALSE" then run the next code (so for unchecked items)
        checkboxInput(id, label = values$items[[id]]$label,                       # looking at each of the task_ids for the "unchecked" items 
                      value = FALSE)                                              # for the unchecked items, we assign them a "FALSE" value in the Shiny app
      }
    })
    
    # Ordering the tasks: checked tasks move towards the bottom of the list 
    do.call(tagList, c(unchecked_items, checked_items))
  })
  
  observe({lapply(names(values$items), function(id) {                             # (l)applying the function to all of the tasks
    observeEvent(input[[id]], {                                                 # whenever a checkbox state changes, this loop runs
      values$items[[id]]$checked <- input[[id]]                                 # updating the checked field for its corresponding task_id based on the new state of the checkbox
    })
  })
  })
}

shinyApp(ui, server)
}
