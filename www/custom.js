// www/custom.js
$(document).ready(function() {
    window.currentSelectedYear = new Date().getFullYear().toString();
  
    // Set up handler to receive year from Shiny
    Shiny.addCustomMessageHandler('setCurrentYear', function(year) {
      console.log("Received year from Shiny:", year);
      window.currentSelectedYear = year;
    });
    // Custom download function for Plotly charts
    window.customPlotDownload = function(gd) {
      // Get the parent card header text
      var cardHeader = $(gd).closest('.card').find('.card-header').first().text().trim();
      console.log("Found card header:", cardHeader);
      
      // Create a modal dialog for download options
      var modalHtml = `
        <div class="modal fade" id="downloadOptionsModal" tabindex="-1" aria-labelledby="downloadOptionsModalLabel" aria-hidden="true">
          <div class="modal-dialog">
            <div class="modal-content">
              <div class="modal-header">
                <h5 class="modal-title" id="downloadOptionsModalLabel">Opciones de descarga</h5>
                <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
              </div>
              <div class="modal-body">
                <div class="mb-3">
                  <label class="form-label">Tamaño de la imagen:</label>
                  <select class="form-select" id="downloadSizeSelect">
                    <option value="screen">Tamaño actual en pantalla</option>
                    <option value="small" selected>Pequeño (800 x 600)</option>
                    <option value="medium">Mediano (1024 x 768)</option>
                    <option value="large">Grande (1280 x 960)</option>
                  </select>
                </div>
              </div>
              <div class="modal-footer">
                <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Cancelar</button>
                <button type="button" class="btn btn-primary" id="confirmDownloadBtn">Descargar</button>
              </div>
            </div>
          </div>
        </div>
      `;
      
      // Remove any existing modal
      $('#downloadOptionsModal').remove();
      
      // Add modal to the document
      $('body').append(modalHtml);
      
      // Show the modal
      var downloadModal = new bootstrap.Modal(document.getElementById('downloadOptionsModal'));
      downloadModal.show();
      
      // Store reference to the graph and header for the download function
      var graphElement = gd;
      var graphHeader = cardHeader;
      
      // Handle the download confirmation - directly call the function instead of relying on Shiny messages
      $('#confirmDownloadBtn').click(function() {
        console.log("Download button clicked");
        
        var sizeOption = $('#downloadSizeSelect').val();
        var width, height;
        
        switch(sizeOption) {
          case 'small':
            width = 800;
            height = 600;
            break;
          case 'medium':
            width = 1024;
            height = 768;
            break;
          case 'large':
            width = 1280;
            height = 960;
            break;
          case 'screen':
          default:
            width = graphElement.offsetWidth;
            height = graphElement.offsetHeight;
            break;
        }
        
        console.log("Selected size:", sizeOption, width, "x", height);
        
        // Close the modal
        downloadModal.hide();
        
        // Get year directly or use current year as fallback
        var currentYear = new Date().getFullYear().toString();
        
        // Try to get the selected year from a global variable if it was set
        var selectedYear = window.currentSelectedYear || currentYear;
        console.log("Using year:", selectedYear);
        
        // Directly call the download function
        performDownload(selectedYear, width, height, graphElement, graphHeader);
      });
    };
    
    // Define the performDownload function in the global scope
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
          
          // Calculate dimensions
          var headerHeight = Math.round(width * 0.06); // Scale header height with width
          var footerHeight = Math.round(width * 0.04); // Scale footer height with width
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
            var headerFontSize = Math.max(12, Math.round(width * 0.018)); // Responsive font size
            ctx.font = 'bold ' + headerFontSize + 'px Arial';
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            
            // Draw header text (potentially wrapping long headers)
            if (cardHeader.length > 50 && width < 1000) {
              var words = cardHeader.split(' ');
              var line1 = '';
              var line2 = '';
              var halfway = Math.ceil(words.length / 2);
              
              for (var i = 0; i < words.length; i++) {
                if (i < halfway) {
                  line1 += words[i] + ' ';
                } else {
                  line2 += words[i] + ' ';
                }
              }
              
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
            var footerFontSize = Math.max(10, Math.round(width * 0.012)); // Responsive font size
            ctx.font = footerFontSize + 'px Arial';
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText('Resultados de la encuesta de percepción y participación ciudadana y buen gobierno ' + year, 
                        width / 2, headerHeight + height + (footerHeight / 2));
            
            // Create a download link
            var link = document.createElement('a');
            link.href = canvas.toDataURL('image/png');
            link.download = 'grafica_' + (cardHeader ? cardHeader.replace(/[^a-z0-9]/gi, '_').toLowerCase().substring(0, 30) : 'plot') + '.png';
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