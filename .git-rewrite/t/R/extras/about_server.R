# about_server.R - Fixed version with proper navigation handling

aboutServer <- function(input, output, session, current_theme = NULL) {
  
  # Handle navigation to methodology section
  observeEvent(input$nav_to_methodology, {
    # Use the same navigation pattern as other sections
    session$sendCustomMessage("navigate", list(target = "methodology"))
  }, ignoreInit = TRUE)
  
  # Optional: Add any other interactive functionality here
  # For example, if you want to track analytics or handle form submissions
  
  # Optional: Contact form handler (if you decide to add a contact form later)
  # observeEvent(input$submitContactForm, {
  #   # Validate form inputs
  #   req(input$contactName, input$contactEmail, input$contactMessage)
  #   
  #   # Here you could save to database or send email
  #   # For now, just show a success message
  #   showNotification(
  #     "¡Mensaje enviado! Gracias por su interés.",
  #     type = "success",
  #     duration = 5
  #   )
  # })
  
  # Optional: Download handler for additional resources
  # output$downloadBrochure <- downloadHandler(
  #   filename = function() {
  #     paste("brochure-plan-juarez-", Sys.Date(), ".pdf", sep = "")
  #   },
  #   content = function(file) {
  #     # Copy a brochure file if it exists
  #     brochure_path <- "www/documents/brochure.pdf"
  #     if (file.exists(brochure_path)) {
  #       file.copy(brochure_path, file)
  #     } else {
  #       # Create a placeholder file
  #       writeLines("Brochure not available", file)
  #     }
  #   }
  # )
}