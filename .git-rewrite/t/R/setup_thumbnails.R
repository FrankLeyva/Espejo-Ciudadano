# setup_thumbnails.R
# Run this script once to set up PDF thumbnail generation

# Function to install required packages
install_thumbnail_packages <- function() {
  required_packages <- c("pdftools", "magick")
  
  cat("Checking required packages for PDF thumbnail generation...\n")
  
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, dependencies = TRUE)
      
      if (require(pkg, character.only = TRUE, quietly = TRUE)) {
        cat("✓", pkg, "installed successfully\n")
      } else {
        cat("✗ Failed to install", pkg, "\n")
        cat("You may need to install system dependencies.\n")
        
        if (pkg == "magick") {
          cat("For magick package, you may need:\n")
          cat("- Ubuntu/Debian: sudo apt-get install libmagick++-dev\n")
          cat("- CentOS/RHEL: sudo yum install ImageMagick-c++-devel\n")
          cat("- macOS: brew install imagemagick\n")
          cat("- Windows: Install Rtools and ImageMagick\n")
        }
        
        if (pkg == "pdftools") {
          cat("For pdftools package, you may need:\n")
          cat("- Ubuntu/Debian: sudo apt-get install libpoppler-cpp-dev\n")
          cat("- CentOS/RHEL: sudo yum install poppler-cpp-devel\n")
          cat("- macOS: brew install poppler\n")
          cat("- Windows: Should work with base R installation\n")
        }
      }
    } else {
      cat("✓", pkg, "already installed\n")
    }
  }
  
  cat("\nPackage installation complete.\n")
}

# Function to create directory structure
setup_directories <- function() {
  cat("Setting up directory structure...\n")
  
  # Create reports directory if it doesn't exist
  if (!dir.exists("www")) {
    dir.create("www")
    cat("✓ Created www directory\n")
  }
  
  if (!dir.exists("www/reports")) {
    dir.create("www/reports")
    cat("✓ Created www/reports directory\n")
  } else {
    cat("✓ www/reports directory already exists\n")
  }
  
  # Create thumbnails directory
  if (!dir.exists("www/thumbnails")) {
    dir.create("www/thumbnails")
    cat("✓ Created www/thumbnails directory\n")
  } else {
    cat("✓ www/thumbnails directory already exists\n")
  }
}

# Function to test thumbnail generation
test_thumbnail_generation <- function() {
  cat("\nTesting thumbnail generation...\n")
  
  # Source the thumbnail generator
  if (file.exists("R/extras/thumbnail_generator.R")) {
    source("R/extras/thumbnail_generator.R")
    
    if (check_thumbnail_dependencies()) {
      cat("✓ All dependencies are available\n")
      
      # Check for existing PDFs
      pdf_files <- list.files("www/reports", pattern = "\\.pdf$", ignore.case = TRUE)
      
      if (length(pdf_files) > 0) {
        cat("Found", length(pdf_files), "PDF files\n")
        cat("Generating test thumbnail for first PDF...\n")
        
        test_result <- generate_pdf_thumbnail(
          file.path("www/reports", pdf_files[1]),
          "www/thumbnails"
        )
        
        if (test_result$success) {
          cat("✓ Test thumbnail generated successfully\n")
        } else {
          cat("✗ Test thumbnail generation failed:", test_result$error, "\n")
        }
      } else {
        cat("ℹ No PDF files found in www/reports for testing\n")
        cat("Add some PDF files and run generate_all_thumbnails() later\n")
      }
    } else {
      cat("✗ Missing required dependencies\n")
    }
  } else {
    cat("✗ thumbnail_generator.R not found. Make sure all files are in place.\n")
  }
}

# Function to generate initial thumbnails
generate_initial_thumbnails <- function() {
  cat("\nGenerating thumbnails for all existing PDFs...\n")
  
  if (file.exists("R/extras/thumbnail_generator.R")) {
    source("R/extras/thumbnail_generator.R")
    
    if (check_thumbnail_dependencies()) {
      result <- generate_all_thumbnails()
      
      if (nrow(result) > 0) {
        successful <- sum(result$success)
        total <- nrow(result)
        cat("✓ Generated", successful, "out of", total, "thumbnails\n")
        
        if (successful < total) {
          failed_files <- result$pdf_file[!result$success]
          cat("Failed files:", paste(failed_files, collapse = ", "), "\n")
        }
      } else {
        cat("ℹ No PDF files found to process\n")
      }
    }
  }
}

# Main setup function
setup_thumbnails <- function(generate_thumbnails = TRUE) {
  cat("=== PDF Thumbnail Setup for Reports Dashboard ===\n\n")
  
  # Step 1: Install packages
  install_thumbnail_packages()
  
  # Step 2: Setup directories
  setup_directories()
  
  # Step 3: Test functionality
  test_thumbnail_generation()
  
  # Step 4: Generate initial thumbnails (optional)
  if (generate_thumbnails) {
    generate_initial_thumbnails()
  }
  
  cat("\n=== Setup Complete ===\n")
  cat("You can now use the reports dashboard with thumbnail support.\n")
  cat("\nUseful commands:\n")
  cat("- generate_all_thumbnails(): Generate thumbnails for all PDFs\n")
  cat("- cleanup_thumbnails(): Remove orphaned thumbnails\n")
  cat("- check_thumbnail_dependencies(): Check if packages are working\n")
  cat("\nNote: Thumbnails are generated automatically when loading reports metadata.\n")
}

# Usage instructions
cat("PDF Thumbnail Setup Script\n")
cat("==========================\n")
cat("Run: setup_thumbnails() to complete the setup\n")
cat("Run: setup_thumbnails(FALSE) to setup without generating thumbnails\n")
cat("\nThis will:\n")
cat("1. Install required packages (pdftools, magick)\n")
cat("2. Create necessary directories\n")
cat("3. Test thumbnail generation\n")
cat("4. Generate thumbnails for existing PDFs\n")