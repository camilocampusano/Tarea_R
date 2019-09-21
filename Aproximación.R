fp_01 <- PanTheria %>% dplyr::select(MSW05_Order, MSW05_Genus, MSW05_Species, "5-1_AdultBodyMass_g", "5-3_NeonateBodyMass_g", "13-1_AdultHeadBodyLen_mm", "13-2_NeonateHeadBodyLen_mm", "9-1_GestationLen_d", "17-1_MaxLongevity_m") %>%
  rename(Orden = MSW05_Order, Genero = MSW05_Genus, Especie = MSW05_Species, MC_A = "5-1_AdultBodyMass_g", MC_Neo = "5-3_NeonateBodyMass_g", Tamanho_A = "13-1_AdultHeadBodyLen_mm", Tamanho_Neo = "13-2_NeonateHeadBodyLen_mm", Tiempo_Gestacion = "9-1_GestationLen_d", Longevidad = "17-1_MaxLongevity_m") 
#fp_01: filro de PanTHERIA 01, acá seleccioné algunas columnas que me interesaron y les cambié el nombre

fp_02 <- fp_01 %>% filter(MC_A != -999, MC_Neo != -999, Tamanho_A != -999, Tamanho_Neo != -999, Tiempo_Gestacion != -999, Longevidad != -999) %>% 
  group_by(Orden)
  #Hice filtro quitando los "-999" y ordené por orden, pq por "genero" no podía hacer "estadística" 

fp_Media_SD <- fp_02 %>% summarise(Mean.MC_A = mean(MC_A), Mean.MC_Neo = mean(MC_Neo), Mean.Tamanho_A = mean(Tamanho_A), Mean.Tamanho_Neo = mean(Tamanho_Neo), Mean.Tiempo_Gestacion = mean(Tiempo_Gestacion), Mean.Longevidad = mean(Longevidad), DS.MC_A = sd(MC_A), DS.MC_Neo = sd(MC_Neo), SD.Tamanho_A = sd(Tamanho_A), SD.Tamanho_Neo = sd(Tamanho_Neo), SD.Tiempo_Gestacion = sd(Tiempo_Gestacion), SD.Longevidad = sd(Longevidad))  %>% group_by(Orden, )
#acá hice el promedio y DS de cada columna, pero agrupado por "orden"

fp_N_Variables_Genero <- fp_02 %>% summarise(N = n()) %>% arrange(-N)

final <- full_join(fp_Media_SD, fp_N_Variables_Genero) %>%  arrange(-N)


ggplot(fp_Media_SD, aes(x = Mean.Longevidad, y = Mean.Tiempo_Gestacion)) +
  geom_point(aes(colour = Orden)) + 
  theme_classic()



fp_01 %>% kable() %>%
  kable_styling()

fp_02 %>% kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

kable(cbind(fp_02, fp_02)) %>%
  kable_styling() %>%
  scroll_box(width = "500px", height = "200px")



