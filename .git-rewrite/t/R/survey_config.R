survey_config <- list(
  PER_2023 = list(
    district_col = "Q2",
    gender_col = "Q101",  # This is correct
    age_col = "Q103",
    # FIXED: Added missing gender mapping for value "0"
    gender_mapping = c(
      "0" = "No especificado",  # ✅ Added this mapping
      "1" = "Mujer",
      "2" = "Hombre",
      "3" = "Otro"
    )
  ),
  PER_2024 = list(
    district_col = "Q2",
    gender_col = "Q101",
    age_col = "Q103",
    gender_mapping = c(
      "0" = "No especificado",  # ✅ Added for consistency
      "1" = "Mujer",
      "2" = "Hombre",
      "3" = "Otro"
    )
  ),
  PAR_2023 = list(
    district_col = "Q2",
    gender_col = "Q144",
    age_col = "Q146",
    gender_mapping = c(
      "0" = "No especificado",  # ✅ Added for consistency
      "1" = "Mujer",
      "2" = "Hombre",
      "3" = "Otro"
    ),
    age_mapping = c(
      "1" = "18 a 29 anos",
      "2" = "18 a 44 anos",
      "3" = "18 a 59 anos",
      "4" = "60 anos o mas"
    )
  ),
  PAR_2024 = list(
    district_col = "Q2",
    gender_col = "Q144",
    age_col = "Q146",
    gender_mapping = c(
      "0" = "No especificado",  # ✅ Added for consistency
      "1" = "Mujer",
      "2" = "Hombre",
      "3" = "Otro"
    ),
    age_mapping = c(
      "1" = "18 a 29 anos",
      "2" = "18 a 44 anos",
      "3" = "18 a 59 anos",
      "4" = "60 anos o mas"
    )
  ),
  binary_response_config = list(
    treat_na_as_negative_by_default = TRUE,
    question_exceptions = list(
      "PAR:Q5" = FALSE,   
      "PAR:Q88" = FALSE
    )
  )
)