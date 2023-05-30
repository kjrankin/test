
library(pavo)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(openxlsx)
library(writexl)

## Set the working directory
## for the line of code below, select a folder of spec files to be compiled into a spreadsheet

setwd("~/Dropbox/HFSP 2021 Pteridines/South Australia Lizard Spec data & fotos 2022")
getwd()


## Import all spec from directory (add your own directory here but e.g.listed below)
specs <- getspec("~/Dropbox/HFSP 2021 Pteridines/South Australia Lizard Spec data & fotos 2022/context 2 SA lizards dec22/linga cold", 
                 ext = "txt", lim = c(300, 1700))

specs2 <- getspec("~/Dropbox/HFSP 2021 Pteridines/South Australia Lizard Spec data & fotos 2022/context 2 SA lizards dec22", 
                  ext = "txt", lim = c(300, 1700), subdir = TRUE)

specs3 <- specs2 %>%
  # Make dataset long
  pivot_longer(2:323, names_to = "specID", values_to = "Refl") %>%
  # Create new columns for each variable based on spec name
  mutate(spp = str_split(specID, pattern = "_", simplify = TRUE)[,1],
         region = str_split(specID, pattern = "_", simplify = TRUE)[,3],
         context = str_split(specID, pattern = "_", simplify = TRUE)[,2],
         replicate = str_split(specID, pattern = "_", simplify = TRUE)[,6]) %>%
  # Create column for individual ID
  mutate(indID = str_sub(spp,-1, -1),
         # Create species column without number and all lowercase       
         spp = tolower(str_sub(spp, 0, -2)),
         # fix up errors with file names
         region = case_when(
           region == "lips" ~"lip",
           region == "Lip" ~ "lip",
           region == "bars" ~ "bar",
           TRUE ~ region
         ),
         context = case_when(
           context == "coolcalm" ~ "calmcool",
           TRUE ~ context
         ))
unique(specs3$spp)
unique(specs3$region) # check groups in your variables
unique(specs3$context) # check groups in your variables

write.csv(specs3, "~/Dropbox/HFSP 2021 Pteridines/South Australia Lizard Spec data & fotos 2022/context 2 SA lizards dec22/allspecdatalong.csv",
          row.names = FALSE)


###############################################################################

specs3 <- read.csv("~/Dropbox/HFSP 2021 Pteridines/South Australia Lizard Spec data & fotos 2022/context 2 SA lizards dec22/allspecdatalong.csv")

# Amanda did this bit and it works
# Linga - change the body region to produce plots
lingadat <- specs3 %>%
  filter(spp == "linga",
         region == "bib")

plotlinga <- ggplot(lingadat, aes(x = wl, y = Refl, group = specID, colour = context)) +
  geom_line()
plotlinga


# Vadnappa
vadnappadat <- specs3 %>%
  filter(spp == "vadnappa",
         region == "tail")

plotvadnappa <- ggplot(vadnappadat, aes(x = wl, y = Refl, group = specID, colour = context)) +
  geom_line()
plotvadnappa


# Longi
longidat <- specs3 %>%
  filter(spp == "longi",
         region == "lip")

plotlongi <- ggplot(longidat, aes(x = wl, y = Refl, group = specID, colour = context)) +
  geom_line()
plotlongi









###########################################################################

## Kat learning - 
## Import all spec from directory (add your own directory here but e.g.listed below)
specs <- getspec("~/Dropbox/HFSP 2021 Pteridines/South Australia Lizard Spec data & fotos 2022/Spec Data - SA Lizards Dec 2022 AL&KR/VadnappaCombined", 
                 ext = "txt", lim = c(300, 1700))

write_xlsx(specs, "~/Dropbox/HFSP 2021 Pteridines/South Australia Lizard Spec data & fotos 2022/Spec Data - SA Lizards Dec 2022 AL&KR/VadnappaCombined\\VadnappaComb.xlsx")

