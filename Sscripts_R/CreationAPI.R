
# 0.Librairies  
library(vetiver)
library(pins)
library(here)
library(plumber)

# 1. Charger le modèle
rf_model <- readRDS(here("models_R", "RandomForestVersion1.rds"))

# 2. Créer vetiver model
v <- vetiver_model(rf_model, model_name = "RandomForestVersion1")

# 3. Versionner
pins_dir <- here("version_models_R", "VetiverPinsVersion1")
board <- board_folder(pins_dir)
vetiver_pin_write(board, v)

# 4. Générer plumber.R
vetiver_write_plumber(board, name = "RandomForestVersion1", version = "1.0.0")

# 5. Lancer l’API
pr("plumber.R")$run(port = 8088)



# JSON file for example

# {
#   "age": 52,
#   "gender": "1",
#   "imc": 27.4,
#   "ap_hi": 135,
#   "ap_lo": 85,
#   "cholesterol": "1",
#   "gluc": "3",
#   "smoke": "0",
#   "alco": "1",
#   "active": "1"
# }

