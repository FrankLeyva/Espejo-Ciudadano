# about_server.R

aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # This module is primarily static content, so the server portion
    # is minimal. We could add functionality here if needed in the future.
    
    # For example, if you wanted to add download buttons for reports or
    # additional documentation, you could implement them here.
    
    # Example (commented out until needed):
    # output$downloadAnnualReport <- downloadHandler(
    #   filename = function() {
    #     paste("informe-anual-", Sys.Date(), ".pdf", sep = "")
    #   },
    #   content = function(file) {
    #     file.copy("www/reports/informe-anual.pdf", file)
    #   }
    # )
    
    # Or if you wanted to implement a contact form:
    # observeEvent(input$submitContactForm, {
    #   # Process contact form submission
    #   # Send email or store in database
    #   showNotification("¡Mensaje enviado! Gracias por su interés.", type = "success")
    # })
  })
}