// Register custom message handler for navigation
Shiny.addCustomMessageHandler("register_nav_handler", function(message) {
    console.log("Registered navigation handler");
  });
  
  // Handle navigation triggers from modules
  Shiny.addCustomMessageHandler("nav_trigger", function(nav_id) {
    console.log("Navigating to: " + nav_id);
    // Simulate click on the target navigation item
    document.getElementById(nav_id).click();
  });