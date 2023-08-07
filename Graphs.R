## Top 10 actividades con mayor número de empresas a nivel nacional

top_5_actividades <- empresas |> 
  count(ciiu4_nivel_1, desc_ciiu_nivel_1) |> 
  slice_max(n, n = 5)

top_4_cantones <- empresas |> 
  semi_join(top_5_actividades, by = "ciiu4_nivel_1") |> 
  count(canton) |> 
  slice_max(n, n = 4)

conteo_empresas <- empresas |> 
  semi_join(top_5_actividades, by = "ciiu4_nivel_1") |> 
  semi_join(top_4_cantones, by = "canton") |> 
  count(canton, ciiu4_nivel_1, desc_ciiu_nivel_1) |> 
  mutate(ciiu4_nivel_1 = factor(ciiu4_nivel_1, levels = top_5_actividades$ciiu4_nivel_1, ordered = TRUE),
         canton = factor(canton, levels = top_4_cantones$canton, ordered = TRUE),
         desc_ciiu_nivel_1 = factor(desc_ciiu_nivel_1, levels = top_5_actividades$desc_ciiu_nivel_1, ordered = TRUE))

conteo_empresas |> 
  ggplot(aes(n, fct_rev(ciiu4_nivel_1), fill = desc_ciiu_nivel_1)) +
  geom_col() +
  facet_wrap(~canton) +
  labs(title = "Distribución de métricas por provincia",
       y = NULL,
       x = NULL,
       subtitle = "liq_mean (azul), liq_max (rojo), liq_min (verde), sol_mean (morado), sol_max (naranja), sol_min (marrón)") +
  theme(legend.direction = vertical,
        legend.position = bottom,
        legend.box = vertical,
        legend.key.width = unit(1, cm))

## Distribución de los indicadores por las tres provincias con mayor número de empresas activas
tipo_principal <- empresas |> 
  count(situacion_legal) |> 
  slice_max(n, n = 1)
  
top_3_provincias <- empresas |> 
  semi_join(tipo_principal, by = "situacion_legal") |> 
  count(provincia) |> 
  slice_max(n, n = 5)  

indicadores_activas <- tipo_principal |> 
  left_join(liquidez_solvencia_provincia_status, by = "situacion_legal")

provincias_mayor_participacion <- top_3_provincias |> 
  left_join(indicadores_activas, by = "provincia")
  
provincias_mayor_participacion |> 
  pivot_longer(
    cols = starts_with("liq") | starts_with("sol"),
    names_to = c("indicador", "estadistico"),
    names_sep = "_",
    values_to = "valor") |>
  ggplot(aes(provincia, valor, label = round(valor, 1))) +
  geom_line(aes(group = provincia)) +
  geom_point(aes(color = estadistico), size = 3) +
  geom_text_repel(aes(color = estadistico)) +
  scale_color_viridis_d() +
  facet_wrap(~indicador)

## Liquidez y solvencia por tipo de empresa 
liquidez_solvencia_tipo_empresa |> 
  pivot_longer(
    cols = starts_with("liq") | starts_with("sol"),
    names_to = c("indicador", "estadistico"),
    names_sep = "_",
    values_to = "valor") |>
  ggplot(aes(valor, tipo, label = round(valor, 1))) +
  geom_line(aes(group = tipo)) +
  geom_point(aes(color = estadistico), size = 2) +
  geom_text_repel(aes(color = estadistico)) +
  scale_color_viridis_d() +
  facet_wrap(~indicador)

## Endeudamiento del activo por tamaño de empresa
endeudamiento_tamano_empresa |> 
  pivot_longer(
    cols = starts_with("end"),
    names_to = c("indicador", "estadistico"),
    names_sep = "_",
    values_to = "valor") |>
  ggplot(aes(valor, tamano, label = round(valor, 1))) +
  geom_line(aes(group = tamano)) +
  geom_point(aes(color = estadistico), size = 2) +
  geom_text_repel(aes(color = estadistico)) +
  scale_color_viridis_d() 

## Liquidez por número de trabajadores

liquidez_trabajadores_empresa |> 
  ggplot(aes(liq_mean, fct_rev(tipo_por_trabajadores), fill = tipo_por_trabajadores)) +
  geom_col() + 
  scale_fill_viridis_d()

# Top 10 empresas con mayor apalancamiento

apalancamiento_empresa |> 
  ggplot(aes(ind_apalancamiento, fct_inorder(nombre_de_la_compania) |> 
               fct_rev(), fill = nombre_de_la_compania)) + 
  geom_col() +
  scale_fill_viridis_d()
  