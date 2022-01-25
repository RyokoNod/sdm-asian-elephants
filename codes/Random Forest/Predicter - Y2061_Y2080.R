library(randomForest)

setwd("/Users/mechenic/Projects/SDM Rewilding")

# -----------------------------------------------------------------------------
species <- read.table("Elephas_Maximus/Elephas_Maximus_PA_Natural_O20.txt",
                      header = TRUE, sep = "\t")

bio <- read.table("WC30AS_V14_BIO/ISEA3H09_WC30AS_V14_BIO.txt",
                  header = TRUE, sep = "\t")

climdex <- read.table("CCSM4_ETCCDI/ISEA3H09_CCSM4_Y1950_Y2000_ETCCDI_IDW1N10.txt",
                      header = TRUE, sep = "\t")

species <- merge(species, bio, by = "HID")
species <- merge(species, climdex, by = "HID")
species$PAF <- factor(species$PA)

# -----------------------------------------------------------------------------
terra <- read.table("../Ecosphere Analysis/Ecosphere Datasets/ISEA3H09_NE10M_V040100_Terra_Fractions.txt",
                    header = TRUE, sep = "\t")

bio <- read.table("WC30AS_V14_CMIP5_CCSM4_RCP85_2070_BIO/ISEA3H09_WC30AS_V14_CMIP5_CCSM4_RCP85_2070_BIO.txt",
                  header = TRUE, sep = "\t")

climdex <- read.table("CCSM4_ETCCDI/ISEA3H09_CCSM4_Y2061_Y2080_ETCCDI_IDW1N10.txt",
                      header = TRUE, sep = "\t")

terra <- merge(terra, bio, by = "HID")
terra <- merge(terra, climdex, by = "HID")

terra <- terra[terra$Terra_Fraction >= 0.5, ]
terra <- terra[terra$BIO01_Mean != -1000, ]

# - R -------------------------------------------------------------------------
features <- c("BIO03_Mean", "BIO14_Mean", "BIO18_Mean", "GSL_IDW1N10", "ID_IDW1N10",
              "RX1DAY_IDW1N10", "WSDI_IDW1N10", "TN10P_IDW1N10", "CWD_IDW1N10", "TNX_IDW1N10")

m.fit <- glm(PA ~ ., data = species[, c("PA", features)], family = "binomial")
m.pre.r <- predict(m.fit, newdata = terra, type = "response")

# - S -------------------------------------------------------------------------
features <- c("ID_IDW1N10", "RX1DAY_IDW1N10", "TXX_IDW1N10", "WSDI_IDW1N10", "BIO18_Mean",
              "BIO02_Mean", "BIO08_Mean", "TN90P_IDW1N10", "BIO14_Mean", "CWD_IDW1N10")

m.fit <- glm(PA ~ ., data = species[, c("PA", features)], family = "binomial")
m.pre.s <- predict(m.fit, newdata = terra, type = "response")

m.frame <- data.frame(HID = sprintf("%i", terra$HID),
                      GLM_R10 = sprintf("%0.6f", m.pre.r),
                      GLM_S10 = sprintf("%0.6f", m.pre.s),
                      GLM_DIF = sprintf("%0.6f", m.pre.s - m.pre.r),
                      stringsAsFactors = FALSE)

write.table(m.frame, file = "Elephas_Maximus/Elephas_Maximus_Predictions_GLM_Y2061_Y2080.txt",
            quote = FALSE, sep = "\t", row.names = FALSE)

# - R -------------------------------------------------------------------------
features <- c("CSDI_IDW1N10", "SU_IDW1N10", "TX10P_IDW1N10", "TN90P_IDW1N10", "CDD_IDW1N10",
              "BIO15_Mean", "BIO07_Mean", "RX1DAY_IDW1N10", "CWD_IDW1N10", "WSDI_IDW1N10")

pre.matrix <- matrix(nrow = nrow(terra), ncol = 100)

for (seed in 3001:3100) {
  set.seed(seed)
  
  m.fit <- randomForest(PAF ~ ., data = species[, c("PAF", features)], ntree = 100)
  pre.matrix[, seed - 3000] <- predict(m.fit, newdata = terra, type = "prob")[, "1"]
}

m.frame <- data.frame(HID = sprintf("%i", terra$HID),
                      RF_R10_MIN = sprintf("%0.6f", apply(pre.matrix, 1, min)),
                      RF_R10_MAX = sprintf("%0.6f", apply(pre.matrix, 1, max)),
                      RF_R10_MEDIAN = sprintf("%0.6f", apply(pre.matrix, 1, median)),
                      RF_R10_MEAN = sprintf("%0.6f", apply(pre.matrix, 1, mean)),
                      RF_R10_SD = sprintf("%0.6f", apply(pre.matrix, 1, sd)),
                      stringsAsFactors = FALSE)

write.table(m.frame, file = "Elephas_Maximus/Elephas_Maximus_Predictions_RF_R10_Y2061_Y2080.txt",
            quote = FALSE, sep = "\t", row.names = FALSE)

# - S -------------------------------------------------------------------------
features <- c("CWD_IDW1N10", "RX1DAY_IDW1N10", "WSDI_IDW1N10", "TNX_IDW1N10", "R20MM_IDW1N10",
              "BIO19_Mean", "CDD_IDW1N10", "TX10P_IDW1N10", "SU_IDW1N10", "TN90P_IDW1N10")

pre.matrix <- matrix(nrow = nrow(terra), ncol = 100)

for (seed in 4001:4100) {
  set.seed(seed)
  
  m.fit <- randomForest(PAF ~ ., data = species[, c("PAF", features)], ntree = 100)
  pre.matrix[, seed - 4000] <- predict(m.fit, newdata = terra, type = "prob")[, "1"]
}

m.frame <- data.frame(HID = sprintf("%i", terra$HID),
                      RF_S10_MIN = sprintf("%0.6f", apply(pre.matrix, 1, min)),
                      RF_S10_MAX = sprintf("%0.6f", apply(pre.matrix, 1, max)),
                      RF_S10_MEDIAN = sprintf("%0.6f", apply(pre.matrix, 1, median)),
                      RF_S10_MEAN = sprintf("%0.6f", apply(pre.matrix, 1, mean)),
                      RF_S10_SD = sprintf("%0.6f", apply(pre.matrix, 1, sd)),
                      stringsAsFactors = FALSE)

write.table(m.frame, file = "Elephas_Maximus/Elephas_Maximus_Predictions_RF_S10_Y2061_Y2080.txt",
            quote = FALSE, sep = "\t", row.names = FALSE)
