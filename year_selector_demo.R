library(shiny)
library(bslib)

ui <- page_fluid(
  tags$head(
    tags$style(HTML("
      /* Timeline Slider */
      .year-timeline {
        position: relative;
        display: inline-flex;
        align-items: center;
        background: linear-gradient(to right, #7b68ee, #0d6efd);
        border-radius: 20px;
        padding: 6px 20px;
        margin: 20px 0;
        box-shadow: 0 2px 8px rgba(0,0,0,0.2);
        width: 200px;
      }
      .year-dot {
        position: relative;
        width: 30px;
        height: 30px;
        margin: 0 10px;
        cursor: pointer;
        z-index: 2;
      }
      .year-dot::before {
        content: '';
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 16px;
        height: 16px;
        background-color: white;
        border-radius: 50%;
        transition: all 0.3s;
      }
      .year-dot.active::before {
        width: 24px;
        height: 24px;
        background-color: #ffd700;
        box-shadow: 0 0 10px rgba(255, 215, 0, 0.8);
      }
      .year-label {
        position: absolute;
        top: -25px;
        left: 50%;
        transform: translateX(-50%);
        color: white;
        font-weight: bold;
        font-size: 14px;
        text-shadow: 0 1px 2px rgba(0,0,0,0.3);
      }
      .year-connector {
        position: absolute;
        top: 50%;
        left: 0;
        right: 0;
        height: 4px;
        background-color: rgba(255,255,255,0.5);
        transform: translateY(-50%);
        z-index: 1;
      }
      
      /* Calendar Tabs */
      .calendar-tabs {
        display: inline-flex;
        margin: 20px 0;
      }
      .calendar-tab {
        position: relative;
        padding: 5px 15px;
        margin: 0 5px;
        background-color: #f0f0f0;
        border-radius: 8px 8px 0 0;
        color: #333;
        cursor: pointer;
        transition: all 0.3s;
        box-shadow: 0 -2px 5px rgba(0,0,0,0.1);
        top: 5px;
      }
      .calendar-tab::before {
        content: '📅';
        margin-right: 5px;
        font-size: 14px;
      }
      .calendar-tab.active {
        background-color: white;
        top: 0;
        padding-top: 10px;
        z-index: 1;
        font-weight: bold;
        color: #0d6efd;
      }
      .calendar-tab.active::before {
        content: '📆';
      }

      /* Time Machine Toggle */
      .time-machine {
        display: inline-flex;
        align-items: center;
        margin: 20px 0;
        background-color: rgba(13, 110, 253, 0.2);
        padding: 5px 10px;
        border-radius: 30px;
      }
      .time-machine-label {
        color: #0d6efd;
        font-size: 14px;
        margin-right: 10px;
      }
      .time-machine-toggle {
        position: relative;
        display: inline-flex;
        align-items: center;
        width: 130px;
        height: 36px;
        background: linear-gradient(to right, #3a7bd5, #00d2ff);
        border-radius: 18px;
        cursor: pointer;
        overflow: hidden;
      }
      .time-machine-slider {
        position: absolute;
        width: 50%;
        height: 30px;
        top: 3px;
        left: 0;
        border-radius: 15px;
        background-color: white;
        box-shadow: 0 2px 5px rgba(0,0,0,0.2);
        transition: transform 0.3s cubic-bezier(0.68, -0.55, 0.27, 1.55);
      }
      .time-machine-option {
        position: relative;
        z-index: 1;
        display: flex;
        align-items: center;
        justify-content: center;
        width: 50%;
        color: #333;
        font-weight: bold;
        font-size: 15px;
        transition: color 0.3s;
      }
      .time-machine-slider.right {
        transform: translateX(100%);
      }
      .time-machine-past {
        padding-left: 5px;
      }
      .time-machine-present {
        padding-right: 5px;
      }
      .time-machine-option.active {
        color: #0d6efd;
      }
      .time-machine-option.inactive {
        color: white;
      }

      /* 3D Flip Cards */
      .year-flip-container {
        margin: 20px 0;
        perspective: 1000px;
        display: inline-block;
      }
      .year-flipper {
        width: 80px;
        height: 40px;
        position: relative;
        transform-style: preserve-3d;
        transition: transform 0.8s;
        cursor: pointer;
      }
      .year-front, .year-back {
        position: absolute;
        width: 100%;
        height: 100%;
        backface-visibility: hidden;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: bold;
        border-radius: 8px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .year-front {
        background-color: white;
        color: #0d6efd;
      }
      .year-back {
        background-color: #f8f9fa;
        color: #0d6efd;
        transform: rotateY(180deg);
      }
      .year-flip-container.flipped .year-flipper {
        transform: rotateY(180deg);
      }
      .year-flip-label {
        color: #0d6efd;
        margin-right: 10px;
        display: inline-block;
        vertical-align: middle;
      }
      
      /* Main container styling */
      .demo-container {
        max-width: 800px;
        margin: 0 auto;
        padding: 20px;
      }
      .option-section {
        margin-bottom: 40px;
        padding: 20px;
        border-radius: 10px;
        background-color: #f8f9fa;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      .selection-result {
        margin-top: 10px;
        padding: 10px;
        background-color: #e9ecef;
        border-radius: 5px;
        font-weight: bold;
      }
      h1 {
        color: #0d6efd;
        margin-bottom: 30px;
      }
      h2 {
        margin-bottom: 20px;
        color: #0d6efd;
      }
      .header-section {
        background-color: #0d6efd;
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 30px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.1);
      }
    ")),
    tags$script(HTML("
      // Timeline Slider
      function selectTimelineYear(year) {
        $('.year-dot').removeClass('active');
        $('#dot' + year).addClass('active');
        $('#timeline-result').text('Selected: ' + year);
        Shiny.setInputValue('timelineYear', year);
      }
      
      // Calendar Tabs
      function selectCalendarYear(year) {
        $('.calendar-tab').removeClass('active');
        $('#cal' + year).addClass('active');
        $('#calendar-result').text('Selected: ' + year);
        Shiny.setInputValue('calendarYear', year);
      }
      
      // Time Machine Toggle
      let currentToggleYear = '2024';
      function toggleYear() {
        if (currentToggleYear === '2024') {
          $('.time-machine-slider').addClass('right');
          $('.time-machine-option').toggleClass('active inactive');
          currentToggleYear = '2023';
        } else {
          $('.time-machine-slider').removeClass('right');
          $('.time-machine-option').toggleClass('active inactive');
          currentToggleYear = '2024';
        }
        $('#toggle-result').text('Selected: ' + currentToggleYear);
        Shiny.setInputValue('toggleYear', currentToggleYear);
      }
      
      // 3D Flip Cards
      let isFlipped = false;
      function flipYearCard() {
        $('#flipContainer').toggleClass('flipped');
        isFlipped = !isFlipped;
        const year = isFlipped ? '2023' : '2024';
        $('#flip-result').text('Selected: ' + year);
        Shiny.setInputValue('flipYear', year);
      }
    "))
  ),
  
  div(class = "demo-container",
      div(class = "header-section text-center",
          h1("Year Selection Options Demo"),
          p("This demo showcases four creative year selection options for your Shiny dashboard.")
      ),
      
      # Option 1: Timeline Slider
      div(class = "option-section",
          h2("Option 1: Interactive Timeline Slider"),
          p("A visual timeline with interactive dots representing years."),
          div(
            class = "year-timeline",
            div(class = "year-connector"),
            div(
              class = "year-dot", id = "dot2023",
              div(class = "year-label", "2023"),
              onclick = "selectTimelineYear('2023')"
            ),
            div(
              class = "year-dot active", id = "dot2024",
              div(class = "year-label", "2024"),
              onclick = "selectTimelineYear('2024')"
            ),
            div(
              class = "year-dot disabled", id = "dot2025",
              div(class = "year-label", "2025"),
              style = "opacity: 0.5;" # Dimmed future year
            )
          ),
          div(class = "selection-result", id = "timeline-result", "Selected: 2024")
      ),
      
      # Option 2: Calendar Tabs
      div(class = "option-section",
          h2("Option 2: Vintage Calendar Tabs"),
          p("Calendar-themed tabs for year selection."),
          div(
            class = "calendar-tabs",
            div(
              class = "calendar-tab", id = "cal2023",
              onclick = "selectCalendarYear('2023')",
              "2023"
            ),
            div(
              class = "calendar-tab active", id = "cal2024",
              onclick = "selectCalendarYear('2024')",
              "2024"
            )
          ),
          div(class = "selection-result", id = "calendar-result", "Selected: 2024")
      ),
      
      # Option 3: Time Machine Toggle
      div(class = "option-section",
          h2("Option 3: Time Machine Toggle"),
          p("A sleek toggle switch with sliding animation."),
          div(
            class = "time-machine",
            div(class = "time-machine-label", "Año:"),
            div(
              class = "time-machine-toggle", id = "timeToggle",
              onclick = "toggleYear()",
              div(class = "time-machine-slider"),
              div(class = "time-machine-option time-machine-past inactive", "2023"),
              div(class = "time-machine-option time-machine-present active", "2024")
            )
          ),
          div(class = "selection-result", id = "toggle-result", "Selected: 2024")
      ),
      
      # Option 4: 3D Flip Cards
      div(class = "option-section",
          h2("Option 4: 3D Flip Cards"),
          p("A visually interesting 3D card flip effect."),
          span(
            class = "year-flip-label",
            "Año:"
          ),
          div(
            class = "year-flip-container", id = "flipContainer",
            onclick = "flipYearCard()",
            div(
              class = "year-flipper",
              div(class = "year-front", "2024"),
              div(class = "year-back", "2023")
            )
          ),
          div(class = "selection-result", id = "flip-result", "Selected: 2024")
      )
  )
)

server <- function(input, output, session) {
  # Track selected years from each option
  observe({
    # For debugging - print to console whenever a selection changes
    if(!is.null(input$timelineYear)) {
      cat("Timeline selection:", input$timelineYear, "\n")
    }
    if(!is.null(input$calendarYear)) {
      cat("Calendar selection:", input$calendarYear, "\n")
    }
    if(!is.null(input$toggleYear)) {
      cat("Toggle selection:", input$toggleYear, "\n")
    }
    if(!is.null(input$flipYear)) {
      cat("Flip selection:", input$flipYear, "\n")
    }
  })
}

shinyApp(ui, server)