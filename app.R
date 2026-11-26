library(shiny)

op <- c(
  "Bucket of spiders (live)",
  "2 weeks in prison",
  "A real ghost",
  "An exciting trip to Malaga",
  "A pallet of teeth (assorted)",
  "Meet & greet lunch with Brookside's Tinhead"
)

final_result <- "An exciting trip to Malaga"

# Colours associated with each category
option_colours <- c(
  "Bucket of spiders (live)" = "#2166AC",
  "2 weeks in prison" = "sienna",
  "A real ghost" = "#4DAC26",
  "An exciting trip to Malaga" = "#FFD92F",
  "A pallet of teeth (assorted)"     = "#F28E2B",
  "Meet & greet lunch with Brookside's Tinhead" = "#D73027"
)

ui <- shiny::fluidPage(
  
  tags$head(
    tags$style(
      HTML("
      
      .selector {
        width: 300px;
        margin: 40px auto;
      }
      
      .selector-option {
        padding: 15px;
        margin: 8px;
        text-align: center;
        font-size: 24px;
        font-weight: 600;
        border-radius: 10px;
        
        /* Non-active categories are muted */
        opacity: 0.40;
        
        transition:
          transform 0.10s ease,
          opacity 0.10s ease,
          box-shadow 0.10s ease;
      }
      
      /* Highlight the currently selected category */
      .selector-option.active {
        opacity: 1;
        transform: scale(1.10);
        box-shadow: 0 5px 15px rgba(0,0,0,0.35);
      }
      
      .spin-button {
        text-align: center;
        margin-top: 25px;
      }
      
      ")
    )
  ),
  
  tags$h2(
    "Congratulations! You have won:",
    style = "text-align:center;"
  ),
  
  uiOutput("selector"),
  
  tags$div(
    class = "spin-button",
    actionButton(
      "spin",
      "Spin",
      class = "btn-primary btn-lg"
    )
  )
)

server <- function(input, output, session) {
  
  # -------------------------------------------------------
  # Reactive values
  # -------------------------------------------------------
  
  # Currently highlighted category
  position <- shiny::reactiveVal(1)
  
  # Whether the selector is currently spinning
  spinning <- shiny::reactiveVal(FALSE)
  
  # Position at which the animation should finish
  final_position <- shiny::reactiveVal(1)
  
  
  # -------------------------------------------------------
  # Draw selector
  # -------------------------------------------------------
  
  output$selector <- shiny::renderUI({
    
    current <- position()
    
    shiny::tags$div(
      class = "selector",
      
      lapply(seq_along(op), function(i) {
        
        # Highlight the current option
        class_name <- if (i == current) {
          "selector-option active"
        } else {
          "selector-option"
        }
        
        # Use dark text on lighter colours
        text_colour <- if (op[i] %in% c("Moderate", "Poor")) {
          "#222222"
        } else {
          "#FFFFFF"
        }
        
        shiny::tags$div(
          class = class_name,
          
          style = paste0(
            "background-color: ",
            option_colours[[op[i]]],
            "; color: ",
            text_colour,
            ";"
          ),
          
          op[i]
        )
      })
    )
  })
  
  
  # -------------------------------------------------------
  # Animation function
  # -------------------------------------------------------
  
  run_animation <- function(step_number, total_steps) {
    
    # later::later() operates outside a normal reactive
    # consumer, so reactive values are read using isolate().
    is_spinning <- shiny::isolate(spinning())
    
    if (!is_spinning) {
      return(invisible(NULL))
    }
    
    
    if (step_number <= total_steps) {
      
      # Get current highlighted position
      current_position <- shiny::isolate(position())
      
      # Move to next category and wrap around at the end
      next_position <-
        (current_position %% length(op)) + 1
      
      # Update highlighted category
      position(next_position)
      
      
      # Calculate progress through animation:
      # 0 = beginning
      # 1 = end
      progress <- step_number / total_steps
      
      # Start fast and gradually slow down
      delay_seconds <-
        0.07 + progress^3 * 0.50
      
      
      # Schedule the next animation step
      later::later(
        function() {
          
          run_animation(
            step_number = step_number + 1,
            total_steps = total_steps
          )
          
        },
        delay = delay_seconds
      )
      
      
    } else {
      
      # ---------------------------------------------------
      # Finish animation
      # ---------------------------------------------------
      
      final <- shiny::isolate(final_position())
      
      # Defensive check in case the final position
      # is somehow invalid
      if (
        length(final) == 1 &&
        !is.na(final) &&
        final >= 1 &&
        final <= length(op)
      ) {
        
        position(final)
        
      }
      
      spinning(FALSE)
    }
  }
  
  
  # -------------------------------------------------------
  # Start animation when Spin is clicked
  # -------------------------------------------------------
  
  shiny::observeEvent(input$spin, {
    
    # Don't allow another animation to start while spinning
    if (shiny::isolate(spinning())) {
      return()
    }
    
    
    # -----------------------------------------------------
    # SET YOUR FINAL RESULT HERE
    # -----------------------------------------------------
    
    # final_result <- "Moderate"
    
    
    # Find the result's position within op.
    #
    # match() is preferable to which() because a result
    # that doesn't exist produces NA rather than integer(0).
    final_pos <- match(
      final_result,
      op
    )
    
    
    # Check that final_result actually exists in op
    if (is.na(final_pos)) {
      
      warning(
        paste0(
          "Final result '",
          final_result,
          "' is not present in op."
        )
      )
      
      return()
    }
    
    
    # Store the final stopping position
    final_position(final_pos)
    
    
    # Reset the selector to its first category
    position(1)
    
    # Start animation
    spinning(TRUE)
    
    
    # Run animation
    run_animation(
      step_number = 1,
      total_steps = length(op) * 8
    )
  })
}

shinyApp(ui, server)
