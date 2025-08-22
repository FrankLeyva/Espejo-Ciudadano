// www/custom.js
$(document).ready(function() {
  window.currentSelectedYear = new Date().getFullYear().toString();

  // Set up handler to receive year from Shiny
  Shiny.addCustomMessageHandler('setCurrentYear', function(year) {
    console.log("Received year from Shiny:", year);
    window.currentSelectedYear = year;
  });
  
  // Custom download function for Plotly charts - Simplified with no modal
  window.customPlotDownload = function(gd) {
    // Get the parent card header text
    var cardHeader = $(gd).closest('.card').find('.card-header').first().text().trim();
    console.log("Found card header:", cardHeader);
    
    // Get year directly or use current year as fallback
    var currentYear = new Date().getFullYear().toString();
    
    // Try to get the selected year from a global variable if it was set
    var selectedYear = window.currentSelectedYear || currentYear;
    console.log("Using year:", selectedYear);
    
    // Set fixed size to small (800x600)
    var width = 800;
    var height = 600;
    
    // Directly call the download function without showing modal
    performDownload(selectedYear, width, height, gd, cardHeader);
  };
  
  // Define the performDownload function in the global scope with improved text scaling and larger header/footer
  window.performDownload = function(year, width, height, gd, cardHeader) {
    console.log("Starting download process with size:", width, "x", height);
    
    // Calculate the aspect ratio of the original plot
    var aspectRatio = gd.offsetHeight / gd.offsetWidth;
    
    // If height not specified, calculate it based on the aspect ratio
    if (!height) {
      height = Math.round(width * aspectRatio);
    }
    
    // Create a download with header and footer
    Plotly.toImage(gd, {format: 'png', width: width, height: height})
      .then(function(dataUrl) {
        console.log("Plot image generated successfully");
        
        // Create a canvas
        var canvas = document.createElement('canvas');
        var ctx = canvas.getContext('2d');
        
        // Calculate dimensions with LARGER header and footer
        var headerHeight, footerHeight;
        var headerFontSize, footerFontSize;
        
        // Increase header height (was 0.06/0.08, now 0.09/0.11)
        if (cardHeader.length > 50 && width < 1000) {
          // For long headers on smaller widths, we need more space
          headerHeight = Math.round(width * 0.11);
        } else {
          // Standard header height
          headerHeight = Math.round(width * 0.09);
        }
        
        // Increase footer height (was 0.04, now 0.06)
        footerHeight = Math.round(width * 0.06);
        
        // Increased font sizes
        headerFontSize = 24; // Was 18 for small size
        footerFontSize = 16; // Was 12 for small size
        
        var totalHeight = headerHeight + height + footerHeight;
        
        // Set canvas size
        canvas.width = width;
        canvas.height = totalHeight;
        
        // Fill background
        ctx.fillStyle = '#ffffff';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        
        // Create an image element for the plot
        var plotImg = new Image();
        plotImg.onload = function() {
          console.log("Plot image loaded into canvas");
          
          // Draw header
          ctx.fillStyle = '#ffffff'; // White background
          ctx.fillRect(0, 0, width, headerHeight);
          ctx.fillStyle = '#000000'; // Black text
          ctx.font = 'bold ' + headerFontSize + 'px Arial';
          ctx.textAlign = 'center';
          ctx.textBaseline = 'middle';
          
          // Improved text wrapping logic based on title length and width
          var maxCharactersPerLine = Math.floor(width / (headerFontSize * 0.6)); // Approximate chars per line
          
          if (cardHeader.length > maxCharactersPerLine) {
            // Find a good breaking point
            var breakPoint = Math.min(
              cardHeader.lastIndexOf(' ', maxCharactersPerLine),
              Math.floor(cardHeader.length / 2)
            );
            
            if (breakPoint === -1) breakPoint = maxCharactersPerLine;
            
            var line1 = cardHeader.substring(0, breakPoint);
            var line2 = cardHeader.substring(breakPoint + 1);
            
            ctx.fillText(line1.trim(), width / 2, headerHeight / 3);
            ctx.fillText(line2.trim(), width / 2, (headerHeight * 2) / 3);
          } else {
            ctx.fillText(cardHeader, width / 2, headerHeight / 2);
          }
          
          // Draw the plot
          ctx.drawImage(plotImg, 0, headerHeight, width, height);
          
          // Draw footer
          ctx.fillStyle = '#ffffff'; // White background
          ctx.fillRect(0, headerHeight + height, width, footerHeight);
          ctx.fillStyle = '#000000'; // Black text
          ctx.font = footerFontSize + 'px Arial';
          ctx.textAlign = 'center';
          ctx.textBaseline = 'middle';
          ctx.fillText('Resultados de la Encuesta de Percepción y Participación Ciudadana y Buen Gobierno ' + year, 
                      width / 2, headerHeight + height + (footerHeight / 2));
          
          // Create a download link
          var link = document.createElement('a');
          link.href = canvas.toDataURL('image/png');
          link.download = 'Figura' + '.png';
          console.log("Initiating download");
          link.click();
        };
        
        // Handle errors in image loading
        plotImg.onerror = function() {
          console.error("Error loading plot image into canvas");
          alert("Error creating the download. Please try again.");
        };
        
        // Set image source to load the plot
        plotImg.src = dataUrl;
      })
      .catch(function(error) {
        console.error("Error generating plot image:", error);
        alert("Error creating the download. Please try again.");
      });
  };
  
});


// Add to custom.js or create a new file called tooltip.js
$(document).ready(function() {
  // Initialize all tooltips
  $('[data-bs-toggle="tooltip"]').tooltip();
  
  // Custom handler for dynamic tooltip updates
  Shiny.addCustomMessageHandler('update-tooltip', function(message) {
    eval(message.script);
  });
  
  // Re-initialize tooltips when tabs change
  $(document).on('shiny:inputchanged', function(event) {
    if (event.name.includes('_tabs') || event.name.includes('selected_')) {
      // Allow a small delay for the DOM to update
      setTimeout(function() {
        $('[data-bs-toggle="tooltip"]').tooltip('dispose');
        $('[data-bs-toggle="tooltip"]').tooltip();
      }, 100);
    }
  });
});

// Add CSS for the tooltips
const tooltipStyles = document.createElement('style');
tooltipStyles.textContent = `
  .tooltip-inner {
    max-width: 300px;
    padding: 8px 12px;
    background-color: rgba(0, 0, 0, 0.85);
    font-size: 14px;
    text-align: left;
    border-radius: 4px;
  }
  
  .info-tooltip {
    opacity: 0.7;
    transition: opacity 0.2s;
  }
  
  .info-tooltip:hover {
    opacity: 1;
  }
  
  /* Section-specific tooltip colors */
  .section-bienestar .info-tooltip {
    color: var(--bienestar-color);
  }
  
  .section-infraestructura .info-tooltip {
    color: var(--infraestructura-color);
  }
  
  .section-movilidad .info-tooltip {
    color: var(--movilidad-color);
  }
  
  .section-gobierno .info-tooltip {
    color: var(--gobierno-color);
  }
  
  .section-participacion .info-tooltip {
    color: var(--participacion-color);
  }
  
  .section-extras .info-tooltip {
    color: var(--extras-color);
  }
`;
document.head.appendChild(tooltipStyles);

// Add to www/tooltip.js or create this file if it doesn't exist

$(document).ready(function() {
  // Initialize all tooltips
  $('[data-bs-toggle="tooltip"]').tooltip();
  
  // Custom handler for dynamic tooltip updates
  Shiny.addCustomMessageHandler('update-tooltip', function(message) {
    eval(message.script);
  });
  
  // Re-initialize tooltips when tabs change or inputs change
  $(document).on('shiny:inputchanged', function(event) {
    if (event.name.includes('_tabs') || event.name.includes('selected_')) {
      // Allow a small delay for the DOM to update
      setTimeout(function() {
        $('[data-bs-toggle="tooltip"]').tooltip('dispose');
        $('[data-bs-toggle="tooltip"]').tooltip();
      }, 100);
    }
  });

  // Special handling for value boxes, which might be loaded dynamically
  $(document).on('shiny:value', function(event) {
    // Allow a small delay for the DOM to fully update
    setTimeout(function() {
      // Reinitialize tooltips for any new elements
      $('[data-bs-toggle="tooltip"]').tooltip('dispose');
      $('[data-bs-toggle="tooltip"]').tooltip();
    }, 200);
  });
  
  // Handle value box hover effect for tooltips
  $(document).on('mouseenter', '.value-box-tooltip', function() {
    $(this).css('transform', 'translateY(-3px)');
    $(this).css('box-shadow', '0 8px 15px rgba(0,0,0,0.15)');
  });
  
  $(document).on('mouseleave', '.value-box-tooltip', function() {
    $(this).css('transform', 'translateY(0)');
    $(this).css('box-shadow', '0 2px 10px rgba(0,0,0,0.1)');
  });
});