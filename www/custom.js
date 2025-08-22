// Minimal custom.js - Simple dropdown prevention without breaking normal functionality

$(document).ready(function() {
  window.currentSelectedYear = new Date().getFullYear().toString();
  window.transitionInProgress = false;
  window.lastTransitionSection = null;

  // Set up handler to receive year from Shiny
  Shiny.addCustomMessageHandler('setCurrentYear', function(year) {
    console.log("Received year from Shiny:", year);
    window.currentSelectedYear = year;
  });
  
  // Add custom message handlers for modals
  Shiny.addCustomMessageHandler('show-full-data-dictionary-modal', function(message) {
    console.log("🔍 Received show-full-data-dictionary-modal message:", message);
    console.log("🔍 Message type:", typeof message);
    console.log("🔍 Message keys:", Object.keys(message));
    console.log("🔍 Data length:", message.data ? message.data.length : "no data");
    
    if (message.data && Array.isArray(message.data)) {
      console.log("🔍 First data item:", message.data[0]);
    }
    
    try {
      // Store the data globally for download functionality
      window.currentDictionaryData = message.data;
      window.currentDictionarySurveyId = message.survey_id;
      window.currentDictionaryTitle = message.title;
      
      // Create modal content with pagination
      const modalContent = createDataDictionaryModalContent(message);
      console.log("🔍 Modal content created successfully");
      
      // Show the modal
      showDataDictionaryModal(message.title, modalContent);
      console.log("✅ Modal should be visible now");
      
    } catch (error) {
      console.error("❌ Error creating modal:", error);
      // Fallback: show simple alert
      alert("Error al mostrar el diccionario de datos: " + error.message);
    }
  });
  
  // ===== IMMEDIATE CLEANUP FOR STUCK DROPDOWNS =====
  
  function forceCleanupDropdowns() {
    $('.dropdown-menu.show').removeClass('show');
    $('.dropdown.show').removeClass('show');
    $('.nav-item.dropdown.show').removeClass('show');
    $('[aria-expanded="true"]').attr('aria-expanded', 'false');
    $('.dropdown-menu').removeAttr('style');
    console.log('Force cleaned stuck dropdowns');
  }
  
  // Run immediate cleanup
  forceCleanupDropdowns();
  
  // ===== SIMPLE DROPDOWN PREVENTION =====
  // Only prevent dropdown when the click is specifically on nav card content
  
$(document).on('click', '.section-nav-card, [class*="nav-card"]:not(.page-card)', function(e) {
  // Add visual feedback
  var $card = $(this);
  $card.addClass('clicking');
  setTimeout(() => $card.removeClass('clicking'), 200);
  
  // Only prevent dropdown if this card is inside a dropdown structure
  var $parentDropdown = $card.closest('.dropdown');
  if ($parentDropdown.length > 0) {
    // This card is inside a dropdown, so prevent dropdown behavior
    e.stopPropagation();
    
    // Close any open dropdowns
    setTimeout(forceCleanupDropdowns, 10);
  }
  
  // Let the onclick handler work normally
});

// Special handling for wellness section navigation cards (they use page-card class)
$(document).on('click', '.section-bienestar .page-card[onclick]', function(e) {
  // Only handle if this is actually a navigation card (has onclick attribute)
  if ($(this).attr('onclick')) {
    // Add visual feedback
    var $card = $(this);
    $card.addClass('clicking');
    setTimeout(() => $card.removeClass('clicking'), 200);
    
    // Prevent dropdown behavior
    var $parentDropdown = $card.closest('.dropdown');
    if ($parentDropdown.length > 0) {
      e.stopPropagation();
      setTimeout(forceCleanupDropdowns, 10);
    }
  }
});
  
 // Prevent highlight tags from bubbling up ONLY on navigation cards
$(document).on('click', '.section-nav-card .highlight-tag, [class*="nav-card"]:not(.page-card) .highlight-tag', function(e) {
  e.stopPropagation();
  
  // Close dropdowns just in case
  setTimeout(forceCleanupDropdowns, 10);
});
  
  // ===== ESCAPE KEY CLEANUP =====
  
  $(document).on('keydown', function(e) {
    if (e.key === 'Escape') {
      forceCleanupDropdowns();
    }
  });
  // Ensure external links work in ALL sections
$(document).on('click', 'a[href^="http"], a[href^="mailto:"]', function(e) {
  console.log('External link clicked:', $(this).attr('href'));
  
  // Stop any parent handlers from interfering
  e.stopPropagation();
  e.preventDefault();
  
  var href = $(this).attr('href');
  var target = $(this).attr('target');
  
  setTimeout(() => {
    try {
      if (href.startsWith('mailto:')) {
        console.log('Opening mailto:', href);
        window.location.href = href;
      } else if (href.startsWith('http')) {
        console.log('Opening external link:', href);
        if (target === '_blank') {
          window.open(href, '_blank', 'noopener,noreferrer');
        } else {
          window.location.href = href;
        }
      }
    } catch (error) {
      console.error('Error opening link:', error);
      // Fallback
      window.open(href, '_blank', 'noopener,noreferrer');
    }
  }, 10);
  
  return false;
});
  // ===== YEAR DROPDOWN FUNCTIONALITY =====
  
  $('.year-selector .dropdown-item').on('click', function(e) {
    e.preventDefault();
    e.stopPropagation();
    
    var year = $(this).text();
    var $button = $('#yearDropdown');
    
    $button.addClass('updating').text(year + ' ⟳');
    
    Shiny.setInputValue('surveyYear', year, {priority: 'event'});
    
    $button.dropdown('hide');
    
    $('.year-selector .dropdown-item').removeClass('active');
    $(this).addClass('active');
    
    return false;
  });
  
  // ===== NAVBAR COLLAPSE ON MOBILE =====
  
  $(document).on('click', '[onclick*="nav_target"]', function() {
    // Close mobile navbar after navigation
    setTimeout(function() {
      $('.navbar-collapse.show').collapse('hide');
      forceCleanupDropdowns();
    }, 10);
  });
  
  // ===== CUSTOM DOWNLOAD FUNCTION (KEEP EXISTING) =====
  
  function getCSSFontProperty(propertyName) {
    return getComputedStyle(document.documentElement).getPropertyValue(propertyName).trim();
  }
  
  function getCanvasFontFamily(cssFontFamily) {
    return cssFontFamily
      .split(',')
      .map(font => font.trim().replace(/['"]/g, ''))
      .map(font => {
        if (font.includes(' ') && !font.includes('-')) {
          return `"${font}"`;
        }
        return font;
      })
      .join(', ');
  }
  
  function buildCanvasFont(weight, size, fontFamily) {
    const cleanFontFamily = getCanvasFontFamily(fontFamily);
    return `${weight} ${size}px ${cleanFontFamily}`;
  }
  
  window.customPlotDownload = function(gd) {
    $(gd).closest('.plot-card').addClass('downloading');
    
    const $plotCard = $(gd).closest('.plot-card');
    const $overlay = $('<div class="download-overlay"><span>Preparando descarga...</span></div>');
    $plotCard.append($overlay);
    
    var cardHeader = $(gd).closest('.card').find('.card-header').first().text().trim();
    var currentYear = new Date().getFullYear().toString();
    var selectedYear = window.currentSelectedYear || currentYear;
    
    var width = 800;
    var height = 600;
    
    performDownload(selectedYear, width, height, gd, cardHeader);
    
    setTimeout(() => {
      $plotCard.removeClass('downloading');
      $overlay.remove();
    }, 2000);
  };
  
  window.performDownload = function(year, width, height, gd, cardHeader) {
    const headingFontFamily = getCSSFontProperty('--font-heading') || 'serif';
    const primaryFontFamily = getCSSFontProperty('--font-primary') || 'sans-serif';
    
    var aspectRatio = gd.offsetHeight / gd.offsetWidth;
    
    if (!height) {
      height = Math.round(width * aspectRatio);
    }
    
    Plotly.toImage(gd, {format: 'png', width: width, height: height})
      .then(function(dataUrl) {
        var canvas = document.createElement('canvas');
        var ctx = canvas.getContext('2d');
        
        var headerHeight, footerHeight;
        var headerFontSize, footerFontSize;
        
        if (cardHeader.length > 50 && width < 1000) {
          headerHeight = Math.round(width * 0.11);
        } else {
          headerHeight = Math.round(width * 0.09);
        }
        
        footerHeight = Math.round(width * 0.06);
        headerFontSize = 24;
        footerFontSize = 16;
        
        var totalHeight = headerHeight + height + footerHeight;
        
        canvas.width = width;
        canvas.height = totalHeight;
        
        ctx.fillStyle = '#ffffff';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        
        var plotImg = new Image();
        plotImg.onload = function() {
          ctx.fillStyle = '#ffffff';
          ctx.fillRect(0, 0, width, headerHeight);
          ctx.fillStyle = '#000000';
          
          ctx.font = buildCanvasFont('bold', headerFontSize, headingFontFamily);
          ctx.textAlign = 'center';
          ctx.textBaseline = 'middle';
          
          var maxCharactersPerLine = Math.floor(width / (headerFontSize * 0.6));
          
          if (cardHeader.length > maxCharactersPerLine) {
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
          
          ctx.drawImage(plotImg, 0, headerHeight, width, height);
          
          ctx.fillStyle = '#ffffff';
          ctx.fillRect(0, headerHeight + height, width, footerHeight);
          ctx.fillStyle = '#000000';
          
          ctx.font = buildCanvasFont('normal', footerFontSize, primaryFontFamily);
          ctx.textAlign = 'center';
          ctx.textBaseline = 'middle';
          ctx.fillText('Resultados de la Encuesta de Percepción y Participación Ciudadana y Buen Gobierno ' + year, 
                      width / 2, headerHeight + height + (footerHeight / 2));
          
          var link = document.createElement('a');
          link.href = canvas.toDataURL('image/png');
          link.download = 'Figura' + '.png';
          link.click();
        };
        
        plotImg.onerror = function() {
          alert("Error creating the download. Please try again.");
        };
        
        plotImg.src = dataUrl;
      })
      .catch(function(error) {
        alert("Error creating the download. Please try again.");
      });
  };
  
  // ===== LOADING STATE MANAGEMENT (SIMPLIFIED) =====

  function createLoadingOverlay(message = 'Cargando...') {
    return `
      <div class="section-loading-overlay">
        <div class="loading-content">
          <div class="loading-spinner">
            <div class="spinner-ring"></div>
            <div class="spinner-ring"></div>
            <div class="spinner-ring"></div>
          </div>
          <div class="loading-text">${message}</div>
        </div>
      </div>
    `;
  }

  function showLoadingState(message) {
    if ($('.section-loading-overlay').length > 0) {
      $('.loading-text').text(message);
      return;
    }
    
    let $targetContainer = $('.tab-content, main, .container-fluid:not(.navbar .container-fluid)').first();
    
    if (!$targetContainer.length) {
      $targetContainer = $('body');
    }
    
    if ($targetContainer.css('position') === 'static') {
      $targetContainer.css('position', 'relative');
    }
    
    const overlay = createLoadingOverlay(message);
    $targetContainer.append(overlay);
    
    $targetContainer.find('.section-loading-overlay').last().css('opacity', 0).animate({
      opacity: 1
    }, 300);
  }

  function hideLoadingState() {
    $('.section-loading-overlay').animate({
      opacity: 0
    }, 200, function() {
      $(this).remove();
    });
  }

  // ===== TRANSITION SYSTEM (SIMPLIFIED) =====
  
  Shiny.addCustomMessageHandler('transition-start', function(data) {
    console.log('Transition start:', data.section);
    window.transitionInProgress = true;
    window.lastTransitionSection = data.section;
    
    showLoadingState(data.message);
    $('body').addClass('section-transitioning');
  });

  Shiny.addCustomMessageHandler('transition-complete', function(data) {
    console.log('Transition complete:', data.section);
    
    if (window.lastTransitionSection === data.section || window.transitionInProgress) {
      window.transitionInProgress = false;
      window.lastTransitionSection = null;
      
      $('body').removeClass('section-transitioning');
      hideLoadingState();
      
      setTimeout(() => {
        $('.page-card, .plot-card, .card').not('.navbar *').addClass('fade-in');
        setTimeout(() => {
          $('.fade-in').removeClass('fade-in');
        }, 800);
      }, 50);
    }
  });

  Shiny.addCustomMessageHandler('year-change-start', function(data) {
    console.log('Year change started:', data.year);
    $('body').addClass('year-changing');
    $('.year-selector').addClass('updating');
    showLoadingState(data.message || `Actualizando datos para ${data.year}...`);
  });

  Shiny.addCustomMessageHandler('year-change-complete', function(data) {
    console.log('Year change completed:', data.year);
    $('body').removeClass('year-changing');
    $('.year-selector').removeClass('updating');
    $('#yearDropdown').text(data.year);
    
    hideLoadingState();
    
    $('.plotly, .leaflet-container').each(function(index) {
      setTimeout(() => {
        $(this).addClass('refresh-fade');
        setTimeout(() => {
          $(this).removeClass('refresh-fade');
        }, 200);
      }, index * 100);
    });
  });

  // ===== TOOLTIP SYSTEM =====
  
  $('[data-bs-toggle="tooltip"]').tooltip();
  
  Shiny.addCustomMessageHandler('simple-tooltip-update', function(message) {
    var tooltipEl = $('#' + message.id);
    
    if(tooltipEl.length) {
      try {
        var tooltipInstance = bootstrap.Tooltip.getInstance(tooltipEl[0]);
        if (tooltipInstance) {
          tooltipInstance.dispose();
        }
      } catch (e) {
        console.log('Error disposing old tooltip:', e);
      }
      
      tooltipEl.attr('title', message.content);
      
      setTimeout(function() {
        tooltipEl.tooltip({
          html: true,
          container: 'body'
        });
      }, 50);
    }
  });
  
  $(document).on('shiny:value', function(event) {
    const outputId = event.target.id;
    
    if (outputId.includes('ui_container')) {
      setTimeout(() => {
        $('[data-bs-toggle="tooltip"]').tooltip('dispose');
        $('[data-bs-toggle="tooltip"]').tooltip({
          html: true,
          container: 'body'
        });
      }, 100);
    }
  });

  // ===== DROPDOWN POSITIONING FIXES =====
  
  $('#yearDropdown').on('shown.bs.dropdown', function() {
    var $dropdown = $(this).next('.dropdown-menu');
    $dropdown.css({
      'position': 'absolute',
      'top': '100%',
      'left': '0',
      'right': 'auto',
      'z-index': '1051',
      'min-width': '90px'
    });
  });

  // ===== GLOBAL FUNCTION EXPORTS =====
  
  window.showLoadingState = showLoadingState;
  window.hideLoadingState = hideLoadingState;
  window.createLoadingOverlay = createLoadingOverlay;
  window.forceCleanupDropdowns = forceCleanupDropdowns;

  console.log('✅ Minimal dashboard system initialized with simple dropdown prevention');
  
  // ===== ANALYTICS TRIGGER SYSTEM =====
  
  let keySequence = [];
  let clickSequence = [];
  let clickTimer = null;
  
  // Method 1: Ctrl+Shift+A (most reliable)
  $(document).keydown(function(e) {
    if (e.ctrlKey && e.shiftKey && e.code === 'KeyA') {
      e.preventDefault();
      triggerAnalytics('keyboard');
      return false;
    }
    
    // Method 2: Triple Escape
    if (e.code === 'Escape') {
      keySequence.push('Escape');
      setTimeout(() => {
        keySequence = keySequence.filter(k => k !== 'Escape');
      }, 2000);
      
      if (keySequence.filter(k => k === 'Escape').length >= 3) {
        triggerAnalytics('triple_escape');
        keySequence = [];
      }
    }
  });
  
  // Method 3: Triple click title
  $(document).on('click', '.vista-rapida-title', function(e) {
    clickSequence.push('title');
    resetClickTimer();
    
    if (clickSequence.filter(c => c === 'title').length >= 3) {
      triggerAnalytics('title_click');
      clickSequence = [];
    }
  });
  
  // Method 4: Footer trigger (hidden element)
  $(document).on('click', '[data-analytics-secret]', function(e) {
    clickSequence.push('footer');
    resetClickTimer();
    
    if (clickSequence.filter(c => c === 'footer').length >= 5) {
      triggerAnalytics('footer_click');
      clickSequence = [];
    }
  });
  
  function resetClickTimer() {
    if (clickTimer) clearTimeout(clickTimer);
    clickTimer = setTimeout(() => { clickSequence = []; }, 3000);
  }
  
  function triggerAnalytics(method) {
    console.log('🎯 Analytics triggered:', method);
    
    // Simple toast notification
    const toast = $('<div style="position:fixed;top:20px;right:20px;background:#007bff;color:white;padding:10px 15px;border-radius:5px;z-index:9999;font-size:14px;">📊 Analytics Panel</div>');
    $('body').append(toast);
    setTimeout(() => toast.fadeOut(() => toast.remove()), 2000);
    
    // Trigger Shiny
    Shiny.setInputValue('secret_analytics_trigger', 'show_analytics', {priority: 'event'});
  }
  
  // Console access
  window.showAnalytics = function() {
    triggerAnalytics('console');
  };
  
  // Help command
  window.analyticsHelp = function() {
    console.log('📊 Analytics Access Methods:\n- Ctrl+Shift+A\n- Triple-click "Vista Rápida"\n- Escape×3\n- showAnalytics()');
  };
  
  console.log('📊 Analytics trigger system ready - use Ctrl+Shift+A or showAnalytics()');
});

// ===== MINIMAL CSS FOR BASIC FUNCTIONALITY =====

const minimalStyles = document.createElement('style');
minimalStyles.textContent = `
  .clicking {
    transform: scale(0.98) !important;
    opacity: 0.8 !important;
    transition: all 0.1s ease !important;
  }
  
  .downloading {
    opacity: 0.8;
    pointer-events: none;
  }
  
  .download-overlay {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(255, 255, 255, 0.9);
    display: flex;
    align-items: center;
    justify-content: center;
    font-weight: 500;
    color: var(--section-primary, var(--primary-color));
    z-index: 10;
    animation: pulse 1s infinite;
  }
  
  .fade-in {
    animation: fadeIn 0.4s ease forwards;
  }
  
  @keyframes fadeIn {
    0% {
      opacity: 0;
      transform: translateY(15px);
    }
    100% {
      opacity: 1;
      transform: translateY(0);
    }
  }
  
  .refresh-fade {
    animation: refreshFade 0.4s ease;
  }
  
  @keyframes refreshFade {
    0%, 100% { opacity: 1; }
    50% { opacity: 0.3; }
  }
  
  .year-changing .year-selector {
    opacity: 0.8;
  }
  
  .year-selector.updating {
    animation: pulse 1s infinite;
  }
`;
document.head.appendChild(minimalStyles);

// Helper function to create modal content
function createDataDictionaryModalContent(message) {
  console.log("🔧 Creating modal content for:", message.title);
  
  if (!message.data || !Array.isArray(message.data)) {
    throw new Error("Invalid data structure received");
  }
  
  const itemsPerPage = 15; // Show 15 items per page
  const totalPages = Math.ceil(message.data.length / itemsPerPage);
  
  // Create table rows for first page
  const firstPageData = message.data.slice(0, itemsPerPage);
  const tableRows = firstPageData.map(function(row) {
    var scaleTypeClass = '';
    switch((row.scale_type || '').toLowerCase()) {
      case 'nominal':
        scaleTypeClass = 'bg-primary';
        break;
      case 'ordinal':
        scaleTypeClass = 'bg-success';
        break;
      case 'interval':
        scaleTypeClass = 'bg-warning';
        break;
      case 'ratio':
        scaleTypeClass = 'bg-danger';
        break;
      case 'binary':
        scaleTypeClass = 'bg-info';
        break;
      default:
        scaleTypeClass = 'bg-secondary';
    }
    
    return `
      <tr>
        <td><code>${row.variable || ''}</code></td>
        <td style="font-size: 0.9em;">${row.label || ''}</td>
        <td style="font-size: 0.85em;"><small>${row.value_labels || ''}</small></td>
        <td><span class="badge ${scaleTypeClass}" style="font-size: 0.75em;">${row.scale_type || ''}</span></td>
      </tr>
    `;
  }).join('');
  
  // Create pagination controls
  let paginationHtml = '';
  if (totalPages > 1) {
    paginationHtml = `
      <div class="d-flex justify-content-between align-items-center mt-3">
        <div class="text-muted">
          Mostrando 1-${Math.min(itemsPerPage, message.data.length)} de ${message.data.length} variables
        </div>
        <nav aria-label="Navegación de páginas">
          <ul class="pagination pagination-sm mb-0">
            <li class="page-item disabled">
              <a class="page-link" href="#" tabindex="-1">Anterior</a>
            </li>
            <li class="page-item active">
              <a class="page-link" href="#" data-page="1">1</a>
            </li>
    `;
    
    // Add page numbers (max 5 pages shown)
    const maxPagesToShow = 5;
    let startPage = 1;
    let endPage = Math.min(totalPages, maxPagesToShow);
    
    if (totalPages > maxPagesToShow) {
      endPage = maxPagesToShow;
    }
    
    for (let i = 2; i <= endPage; i++) {
      paginationHtml += `
        <li class="page-item">
          <a class="page-link" href="#" data-page="${i}">${i}</a>
        </li>
      `;
    }
    
    if (totalPages > maxPagesToShow) {
      paginationHtml += `
        <li class="page-item disabled">
          <a class="page-link" href="#">...</a>
        </li>
        <li class="page-item">
          <a class="page-link" href="#" data-page="${totalPages}">${totalPages}</a>
        </li>
      `;
    }
    
    paginationHtml += `
            <li class="page-item">
              <a class="page-link" href="#" data-page="2">Siguiente</a>
            </li>
          </ul>
        </nav>
      </div>
    `;
  }
  
  return `
    <div class="row mb-3">
      <div class="col-md-6">
        <p><strong>Encuesta:</strong> ${message.survey_id}</p>
      </div>
      <div class="col-md-6">
        <p><strong>Variables:</strong> ${message.row_count}</p>
      </div>
    </div>
    
    <div class="table-responsive" style="max-height: 400px; overflow-y: auto;">
      <table class="table table-striped table-bordered table-hover table-sm" style="width:100%; font-size: 0.9em;">
        <thead class="table-dark">
          <tr>
            <th style="width: 15%;">Variable</th>
            <th style="width: 45%;">Descripción</th>
            <th style="width: 25%;">Etiquetas de Valores</th>
            <th style="width: 15%;">Tipo de Escala</th>
          </tr>
        </thead>
        <tbody id="dictionaryTableBody">
          ${tableRows}
        </tbody>
      </table>
    </div>
    
    ${paginationHtml}
    
    <script>
      // Pagination functionality
      $(document).ready(function() {
        const allData = ${JSON.stringify(message.data)};
        const itemsPerPage = ${itemsPerPage};
        let currentPage = 1;
        
        function updateTable(page) {
          const startIndex = (page - 1) * itemsPerPage;
          const endIndex = startIndex + itemsPerPage;
          const pageData = allData.slice(startIndex, endIndex);
          
          const tableRows = pageData.map(function(row) {
            var scaleTypeClass = '';
            switch((row.scale_type || '').toLowerCase()) {
              case 'nominal': scaleTypeClass = 'bg-primary'; break;
              case 'ordinal': scaleTypeClass = 'bg-success'; break;
              case 'interval': scaleTypeClass = 'bg-warning'; break;
              case 'ratio': scaleTypeClass = 'bg-danger'; break;
              case 'binary': scaleTypeClass = 'bg-info'; break;
              default: scaleTypeClass = 'bg-secondary';
            }
            
            return \`
              <tr>
                <td><code>\${row.variable || ''}</code></td>
                <td style="font-size: 0.9em;">\${row.label || ''}</td>
                <td style="font-size: 0.85em;"><small>\${row.value_labels || ''}</small></td>
                <td><span class="badge \${scaleTypeClass}" style="font-size: 0.75em;">\${row.scale_type || ''}</span></td>
              </tr>
            \`;
          }).join('');
          
          $('#dictionaryTableBody').html(tableRows);
          
          // Update pagination info
          $('.text-muted').text(\`Mostrando \${startIndex + 1}-\${Math.min(endIndex, allData.length)} de \${allData.length} variables\`);
          
          // Update pagination buttons
          $('.pagination .page-item').removeClass('active disabled');
          $(\`.pagination .page-link[data-page="\${page}"]\`).parent().addClass('active');
          
          // Update prev/next buttons
          if (page === 1) {
            $('.pagination .page-item:first').addClass('disabled');
          } else {
            $('.pagination .page-item:first').removeClass('disabled');
            $('.pagination .page-item:first .page-link').attr('data-page', page - 1);
          }
          
          if (page === ${totalPages}) {
            $('.pagination .page-item:last').addClass('disabled');
          } else {
            $('.pagination .page-item:last').removeClass('disabled');
            $('.pagination .page-item:last .page-link').attr('data-page', page + 1);
          }
        }
        
        // Handle pagination clicks
        $(document).on('click', '.pagination .page-link', function(e) {
          e.preventDefault();
          const page = parseInt($(this).attr('data-page'));
          if (page && page !== currentPage) {
            currentPage = page;
            updateTable(page);
          }
        });
      });
    </script>
  `;
}

// Helper function to show the modal
function showDataDictionaryModal(title, content) {
  console.log("🔧 Showing modal with title:", title);
  
  // Create modal HTML
  const modalHtml = `
    <div class="modal fade" id="fullDataDictionaryModal" tabindex="-1" aria-labelledby="fullDataDictionaryModalLabel" aria-hidden="true">
      <div class="modal-dialog modal-lg">
        <div class="modal-content">
          <div class="modal-header">
            <h5 class="modal-title" id="fullDataDictionaryModalLabel">${title}</h5>
            <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
          </div>
          <div class="modal-body" style="max-height: 70vh; overflow-y: auto;">
            ${content}
          </div>
          <div class="modal-footer">
            <button type="button" class="btn btn-primary" onclick="downloadDictionaryCSV()">
              <i class="bi bi-download"></i> Descargar Diccionario (CSV)
            </button>
            <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Cerrar</button>
          </div>
        </div>
      </div>
    </div>
  `;
  
  // Remove any existing modal
  $('#fullDataDictionaryModal').remove();
  
  // Add modal to page
  $('body').append(modalHtml);
  
  // Show modal
  const modal = new bootstrap.Modal(document.getElementById('fullDataDictionaryModal'));
  modal.show();
  
  // Clean up when modal is hidden
  $('#fullDataDictionaryModal').on('hidden.bs.modal', function() {
    $(this).remove();
    // Clear global data when modal is closed
    window.currentDictionaryData = null;
    window.currentDictionarySurveyId = null;
    window.currentDictionaryTitle = null;
  });
  
  console.log("✅ Modal should now be visible");
}

// Download function for CSV export
window.downloadDictionaryCSV = function() {
  console.log("🔧 Downloading CSV...");
  
  if (!window.currentDictionaryData || !Array.isArray(window.currentDictionaryData)) {
    alert("No hay datos disponibles para descargar");
    return;
  }
  
  try {
    // Create CSV content
    const headers = ['Variable', 'Descripción', 'Etiquetas de Valores', 'Tipo de Escala'];
    const csvContent = [
      headers.join(','),
      ...window.currentDictionaryData.map(row => [
        `"${(row.variable || '').replace(/"/g, '""')}"`,
        `"${(row.label || '').replace(/"/g, '""')}"`,
        `"${(row.value_labels || '').replace(/"/g, '""')}"`,
        `"${(row.scale_type || '').replace(/"/g, '""')}"`
      ].join(','))
    ].join('\n');
    
    // Create download link
    const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
    const link = document.createElement('a');
    
    if (link.download !== undefined) {
      const url = URL.createObjectURL(blob);
      link.setAttribute('href', url);
      link.setAttribute('download', `${window.currentDictionarySurveyId}_Diccionario_${new Date().toISOString().split('T')[0]}.csv`);
      link.style.visibility = 'hidden';
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
      URL.revokeObjectURL(url);
      
      console.log("✅ CSV download completed");
    } else {
      // Fallback for older browsers
      alert("Su navegador no soporta descargas automáticas. Los datos han sido copiados al portapapeles.");
      navigator.clipboard.writeText(csvContent);
    }
    
  } catch (error) {
    console.error("❌ Error downloading CSV:", error);
    alert("Error al descargar el archivo CSV: " + error.message);
  }
};
