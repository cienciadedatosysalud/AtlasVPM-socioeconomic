pacman::p_load(
  tidyverse,
  sf,
  Hmisc,
  openxlsx,
  reshape2,
  ggstatsplot,
  ineAtlas,
  patchwork,
  nngeo
)

mapa_zbs <- read_sf('../outputs/1_outputs/new_PCA_ESP_4258_2022_all.shp')
sec_cen_csv <- read.csv(file="../outputs/1_outputs/census_tracts_intersection_pca_point_on_surface_with_percent_2022_2020_after_corrected.csv",
                        sep="|", colClasses = c(CUSEC="character",codatzbs="character"))

data_sec_cen_renta <- ineAtlas::get_atlas(category = 'income',level = 'tract') %>% dplyr::filter(year %in% 2022) %>% 
  dplyr::select(tract_code,net_income_pc,net_income_hh)

data_sec_cen_gini <- get_atlas(category = "gini_p80p20", level = "tract") %>% dplyr::filter(year %in% 2022) %>% 
  dplyr::select(tract_code,gini,p80p20)

data_sec_cen_pobla <- get_atlas(category = 'demographics', level = "tract") %>% dplyr::filter(year %in% 2022) %>% 
  dplyr::select(tract_code,population)

data_sec_cen_renta <- data_sec_cen_renta %>% filter(tract_code %in% sec_cen_csv$CUSEC) 
data_sec_cen_gini <- data_sec_cen_gini %>% filter(tract_code %in% sec_cen_csv$CUSEC) 
data_sec_cen_pobla <- data_sec_cen_pobla %>% filter(tract_code %in% sec_cen_csv$CUSEC) 

sec_cen_csv <- left_join(sec_cen_csv,data_sec_cen_renta[c('tract_code','net_income_pc','net_income_hh')],by=c('CUSEC'='tract_code'))
sec_cen_csv <- left_join(sec_cen_csv,data_sec_cen_gini[c('tract_code','gini','p80p20')],by=c('CUSEC'='tract_code'))
sec_cen_csv <- left_join(sec_cen_csv,data_sec_cen_pobla[c('tract_code','population')],by=c('CUSEC'='tract_code'))


zbs_csv <- sec_cen_csv %>% group_by(n_zbs,codatzbs,ccaa_cd) %>% 
  summarise(mean_net_income_pc = mean(net_income_pc,na.rm=TRUE),
            mean_net_income_hh = mean(net_income_hh,na.rm=TRUE),
            mean_gini = mean(gini,na.rm=TRUE),
            mean_p80p20 = mean(p80p20,na.rm=TRUE),
            sum_pobla = sum(population,na.rm=TRUE))

write.table(zbs_csv,'../outputs/2_outputs/pca_after_corrected_with_indicators.csv',sep='|',row.names=FALSE)
write.table(sec_cen_csv,'../outputs/2_outputs/census_tracts_intersection_pca_point_on_surface_with_percent_2022_2020_after_corrected_with_indicators.csv',
            sep='|',row.names=FALSE)


###### All (median) #######


median_rent_por_persona <- sec_cen_csv %>% 
  group_by(codatzbs) %>% 
  summarise(median_all = median(net_income_pc,na.rm = TRUE))  


perc_90_more <- sec_cen_csv %>% 
  filter(perc_area >= 90) %>% 
  group_by(codatzbs) %>% 
  summarise(median_90_more = median(net_income_pc,na.rm = TRUE),
            #         count=n()
  )

perc_80_more <- sec_cen_csv %>% 
  filter(perc_area >= 80) %>%
  group_by(codatzbs) %>% 
  summarise(median_80_more = median(net_income_pc,na.rm = TRUE)) 


perc_67_more <- sec_cen_csv %>% 
  filter(perc_area >= ((2/3)*100)) %>%
  group_by(codatzbs) %>% 
  summarise(median_67_more = median(net_income_pc,na.rm = TRUE)) 


median_rent_por_persona <- left_join(median_rent_por_persona, perc_90_more, by='codatzbs') %>%
  left_join(., perc_80_more, by='codatzbs') %>% 
  left_join(., perc_67_more, by='codatzbs')


#### ggwithinstats ####

data_plot <- melt(median_rent_por_persona, id.vars='codatzbs', measure.vars=c('median_all','median_90_more'))

levels(data_plot$variable)[levels(data_plot$variable) == "median_90_more"] <- 'Only census tracts included with \n>=90% area inside the PCA boundaries'
levels(data_plot$variable)[levels(data_plot$variable) == "median_all"] <- "All census tracts included"


p <- ggwithinstats(data=data_plot, x = variable, y = value, type = 'nonparametric',
                   centrality.plotting = TRUE, centrality.type = 'nonparametric',
                   pairwise.comparisons = TRUE,
                   pairwise.display = 'all',
                   pairwise.annotation = "p.value") +
  labs(title = 'Boxplot comparison: Median average net income (€) per capita (PCA)',x='',y='')

p
png('../outputs/2_outputs/supplementary/additional/ggplotwithin_median_net_income.png',units = 'mm', width = 210, height = 148, res=300)
print(p)
dev.off()

tiff('../outputs/2_outputs/supplementary/additional/ggplotwithin_median_net_income.tiff',units = 'mm', width = 210, height = 148, res=300)
print(p)
dev.off()


#### density ####

data_sec_cen_renta_ <- data_sec_cen_renta %>% dplyr::select(tract_code,net_income_pc) %>% 
  dplyr::rename(unit = tract_code, value = net_income_pc)

data_sec_cen_renta_$descrp <- 'Average net income (€) per \ncapita (N census tracts = 36080)'

data_sec_cen_renta_$alpha <- 0.5
data_sec_cen_renta_$lty <- 2


median_rent_por_persona_ <- median_rent_por_persona %>% dplyr::select(codatzbs,median_all) %>% 
  dplyr::rename(unit = codatzbs, value = median_all)
median_rent_por_persona_$descrp <- 'Median average net income (€) per \ncapita (N PCA = 2405)'
median_rent_por_persona_$alpha <- 0.8
median_rent_por_persona_$lty <- 1

data_density <- rbind(data_sec_cen_renta_,median_rent_por_persona_)



p1_density <- ggplot(data=data_density, aes(x=value, group=descrp, fill = descrp,alpha=alpha,linetype=lty)) + geom_density() + 
  scale_fill_manual(name='', values = c('Average net income (€) per \ncapita (N census tracts = 36080)'="#fdcc8a",
                                        'Median average net income (€) per \ncapita (N PCA = 2405)'="#edf8b1")) + 
  labs(x='Income (€) per capita',
       y='Density') +
  scale_alpha_identity() + scale_linetype_identity() + 
  theme(panel.background = element_blank(),
        axis.title=element_text(size=10),legend.position = 'top')
p1_density



intersect <- function(x, y, bw = "nrd0", from, to,  ...) {
  #
  # Compute a density for all points combined.
  # 
  largs <- list(x = c(x,y), bw = bw)
  if (!missing(from)) largs <- c(largs, from = from)
  if (!missing(to)) largs <- c(largs, to = to)
  largs <- c(largs, list(...))
  obj <- do.call(density, largs) # Compute a common density
  #
  # Compute densities for the datasets separately.
  #
  x.0 <- obj$x
  f.x <- density(x, bw = obj$bw, from = min(x.0), to = max(x.0), ...)
  f.y <- density(y, bw = obj$bw, from = min(x.0), to = max(x.0), ...)
  #
  # Find the crossings.
  #
  d <- zapsmall(f.y$y - f.x$y)
  abscissae <- sapply(which(d[-1] * d[-length(d)] < 0), function(i) {
    w <- d[i+1] - d[i]
    if (w > 0) (d[i+1] * x.0[i] - d[i] * x.0[i+1]) / w else (x.0[i] + x.0[i+1]) / 2
  })
  list(Points = abscissae, xlim = range(x.0), f = f.x, g = f.y)
}

data_sec_cen_renta_1 <- na.omit(data_sec_cen_renta_)

dens1 <- as.data.frame(density(data_sec_cen_renta_1$value))
dens2 <- as.data.frame(density(median_rent_por_persona_$value))



bw <- 0.25
obj <- intersect(dens1$x, dens2$x, kernel = "gaussian", n = 512, bw = bw, from = 15000, to = 18000)

p1_density <- p1_density + geom_vline(xintercept = c(9447.162,16414.83),lty=4,color='firebrick')
p1_density
png('../outputs/2_outputs/supplementary/additional/density_median_income_all_censustracts.png',units = 'mm', width = 210, height = 148, res=300)
print(p1_density)
dev.off()

tiff('../outputs/2_outputs/supplementary/additional/density_median_income_all_censustractss.tiff',units = 'mm', width = 210, height = 148, res=300)
print(p1_density)
dev.off()



a <- data_sec_cen_renta_1 %>% filter(value < 9447.162)
a <- data_sec_cen_renta_1 %>% filter(value > 16414.83)
(7342+2479)/36080

#### map ####

mapa_zbs_ <- left_join(x = mapa_zbs, y = median_rent_por_persona, by = 'codatzbs')


mapa_zbs_$Quintil[mapa_zbs_$median_all <= quantile(mapa_zbs_$median_all, 0.2, na.rm=TRUE)] <- "#fef0d9"

mapa_zbs_$Quintil[quantile(mapa_zbs_$median_all, 0.2, na.rm=TRUE) < mapa_zbs_$median_all &
                    mapa_zbs_$median_all <= quantile(mapa_zbs_$median_all, 0.4, na.rm=TRUE)] <- "#fdcc8a"

mapa_zbs_$Quintil[quantile(mapa_zbs_$median_all, 0.4, na.rm=TRUE) < mapa_zbs_$median_all &
                    mapa_zbs_$median_all <= quantile(mapa_zbs_$median_all, 0.6, na.rm=TRUE)] <- "#fc8d59"

mapa_zbs_$Quintil[quantile(mapa_zbs_$median_all, 0.6, na.rm=TRUE) < mapa_zbs_$median_all &
                    mapa_zbs_$median_all <= quantile(mapa_zbs_$median_all, 0.8, na.rm=TRUE)] <- "#e34a33"

mapa_zbs_$Quintil[quantile(mapa_zbs_$median_all, 0.8, na.rm=TRUE) < mapa_zbs_$median_all &
                    mapa_zbs_$median_all <= quantile(mapa_zbs_$median_all, 1, na.rm=TRUE)] <- "#b30000"


a <- mapa_zbs_ %>%
  mutate(
    quintil_valor1 = ntile(median_all, 5)
  )
quintil <- mapa_zbs_ %>% as_tibble() %>% 
  summarise(q1=quantile(median_all, 0.2, na.rm=TRUE),
            q2=quantile(median_all, 0.4, na.rm=TRUE),
            q3=quantile(median_all, 0.6, na.rm=TRUE),
            q4=quantile(median_all, 0.8, na.rm=TRUE),
            q5=quantile(median_all, 1, na.rm=TRUE))


plot <- ggplot() +
  geom_sf(data = mapa_zbs_ %>% filter(!is.na(Quintil)), aes(fill = Quintil)) + # color = NA para quitar borders
  scale_fill_identity(name= '',
                      labels=c("#fef0d9"= paste0('Q1: ',quintil$q1),"#fdcc8a" = paste0('Q2: ',quintil$q2),
                               "#fc8d59" = paste0('Q3: ',quintil$q3),"#e34a33" = paste0('Q4: ',quintil$q4), '#b30000'=paste0('Q5: ',quintil$q5)), 
                      guide = "legend") +
  
  labs(title = 'Median average net income (€) per capita (PCA) in 2022') +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0, size = 20),
        legend.position= c(1, 0.85),
        legend.text = element_text(size = 10),
        legend.margin = margin(), panel.background = element_blank(),plot.caption = element_text(hjust = 0,size=12))

plot


png('../outputs/2_outputs/supplementary/additional/map_median_income_all_censustracts.png',units = 'mm', width = 250, height = 148, res=300)
print(plot)
dev.off()

tiff('../outputs/2_outputs/supplementary/additional/map_median_income_all_censustracts.tiff',units = 'mm', width = 250, height = 148, res=300)
print(plot)
dev.off()

#### All (mean and weigth) ######


mean_rent_por_persona <- sec_cen_csv %>% 
  group_by(codatzbs) %>% 
  summarise(mean_all = mean(net_income_pc,na.rm = TRUE),
            weighted_mean = weighted.mean(net_income_pc,w=population / sum(population,na.rm=TRUE),na.rm = TRUE))  


perc_90_more <- sec_cen_csv %>% 
  filter(perc_area >= 90) %>% 
  group_by(codatzbs) %>% 
  summarise(mean_90_more = mean(net_income_pc,na.rm = TRUE),
            #         count=n()
  )


perc_80_more <- sec_cen_csv %>% 
  filter(perc_area >= 80) %>%
  group_by(codatzbs) %>% 
  summarise(mean_80_more = mean(net_income_pc,na.rm = TRUE)) 


perc_67_more <- sec_cen_csv %>% 
  filter(perc_area >= ((2/3)*100)) %>%
  group_by(codatzbs) %>% 
  summarise(mean_67_more = mean(net_income_pc,na.rm = TRUE)) 


mean_rent_por_persona <- left_join(mean_rent_por_persona, perc_90_more, by='codatzbs') %>%
  left_join(., perc_80_more, by='codatzbs') %>% 
  left_join(., perc_67_more, by='codatzbs')

#### ggwithinstats ####

data_plot <- melt(mean_rent_por_persona, id.vars='codatzbs', measure.vars=c('mean_all','mean_90_more'))

levels(data_plot$variable)[levels(data_plot$variable) == "mean_90_more"] <- 'Only census tracts included with \n>=90% area inside the PCA boundaries'
levels(data_plot$variable)[levels(data_plot$variable) == "mean_all"] <- "All census tracts included"


p <- ggwithinstats(data=data_plot, x = variable, y = value, type = 'nonparametric',
                   centrality.plotting = TRUE, centrality.type = 'nonparametric',
                   pairwise.comparisons = TRUE,
                   pairwise.display = 'all',
                   pairwise.annotation = "p.value") +
  labs(title = 'Boxplot comparison: Mean average net income (€) per capita (PCA)',x='',y='')

p
png('../outputs/2_outputs/supplementary/additional/ggplotwithin_mean_net_income.png',units = 'mm', width = 210, height = 148, res=300)
print(p)
dev.off()

tiff('../outputs/2_outputs/supplementary/additional/ggplotwithin_mean_net_income.tiff',units = 'mm', width = 210, height = 148, res=300)
print(p)
dev.off()

#### density ####

mean_rent_por_persona_ <- mean_rent_por_persona %>% dplyr::select(codatzbs,mean_all) %>% 
  dplyr::rename(unit = codatzbs, value = mean_all)
mean_rent_por_persona_$descrp <- 'Mean average net income (€) per \ncapita (N PCA = 2405)'
mean_rent_por_persona_$alpha <- 0.8
mean_rent_por_persona_$lty <- 1

data_density <- rbind(data_sec_cen_renta_,mean_rent_por_persona_)


p2_density <- ggplot(data=data_density, aes(x=value, group=descrp, fill = descrp,alpha=alpha,linetype=lty)) + geom_density() + 
  scale_fill_manual(name='', values = c('Average net income (€) per \ncapita (N census tracts = 36080)'="#fdcc8a",
                                        'Mean average net income (€) per \ncapita (N PCA = 2405)'="#edf8b1")) + 
  labs(x='Income (€) per capita',
       y='Density') +
  scale_alpha_identity() + scale_linetype_identity() + 
  theme(panel.background = element_blank(),
        axis.title=element_text(size=10),legend.position = 'top')

p2_density

dens1 <- as.data.frame(density(data_sec_cen_renta_1$value))
dens2 <- as.data.frame(density(mean_rent_por_persona_$value))

bw <- 0.25
obj <- intersect(dens1$x, dens2$x, kernel = "gaussian", n = 512, bw = bw, from = 15000, to = 17000)

p2_density <- p2_density + geom_vline(xintercept = c(9966.671,16528.38),lty=4,color='firebrick')

a <- data_sec_cen_renta_ %>% filter(value < 9966.671)
a <- data_sec_cen_renta_ %>% filter(value > 16528.38)
(7120+4105)/36080




png('../outputs/2_outputs/FIGURE_2_density_mean_income_all_censustracts.png',units = 'mm', width = 210, height = 148, res=300)
print(p2_density)
dev.off()

tiff('../outputs/2_outputs/FIGURE_2_density_mean_income_all_censustracts.tiff',units = 'mm', width = 210, height = 148, res=300)
print(p2_density)
dev.off()

mean_rent_por_persona_ <- mean_rent_por_persona %>% dplyr::select(codatzbs,weighted_mean) %>% 
  dplyr::rename(unit = codatzbs, value = weighted_mean)
mean_rent_por_persona_$descrp <- 'Weighted mean average net income (€) per \ncapita (N PCA = 2405)'
mean_rent_por_persona_$alpha <- 0.8
mean_rent_por_persona_$lty <- 1

data_density <- rbind(data_sec_cen_renta_,mean_rent_por_persona_)


p3_density <- ggplot(data=data_density, aes(x=value, group=descrp, fill = descrp,alpha=alpha,linetype=lty)) + geom_density() + 
  scale_fill_manual(name='', values = c('Average net income (€) per \ncapita (N census tracts = 36080)'="#fdcc8a",
                                        'Weighted mean average net income (€) per \ncapita (N PCA = 2405)'="#edf8b1")) + 
  labs(x='Income (€) per capita',
       y='Density') +
  scale_alpha_identity() + scale_linetype_identity() + 
  theme(panel.background = element_blank(),
        axis.title=element_text(size=10),legend.position = 'top')

p3_density

# png('../outputs/2_outputs/density_weighted_mean_income_all_censustracts.png',units = 'mm', width = 210, height = 148, res=300)
# print(p3_density)
# dev.off()
# 
# tiff('../outputs/2_outputs/density_weighted_mean_income_all_censustracts.tiff',units = 'mm', width = 210, height = 148, res=300)
# print(p3_density)
# dev.off()

plot <- (p1_density + (p2_density + theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.title.y = element_blank())))
plot
png('../outputs/2_outputs/supplementary/additional/density_compose_censustracts.png',units = 'mm', width = 350, height = 148, res=300)
print(plot)
dev.off()

tiff('../outputs/2_outputs/supplementary/additional/density_compose_censustracts.tiff',units = 'mm', width = 350, height = 148, res=300)
print(plot)
dev.off()


#### map ####

mapa_zbs_ <- left_join(x = mapa_zbs, y = mean_rent_por_persona, by = 'codatzbs')


mapa_zbs_$Quintil[mapa_zbs_$mean_all <= quantile(mapa_zbs_$mean_all, 0.2, na.rm=TRUE)] <- "#fef0d9"

mapa_zbs_$Quintil[quantile(mapa_zbs_$mean_all, 0.2, na.rm=TRUE) < mapa_zbs_$mean_all &
                    mapa_zbs_$mean_all <= quantile(mapa_zbs_$mean_all, 0.4, na.rm=TRUE)] <- "#fdcc8a"

mapa_zbs_$Quintil[quantile(mapa_zbs_$mean_all, 0.4, na.rm=TRUE) < mapa_zbs_$mean_all &
                    mapa_zbs_$mean_all <= quantile(mapa_zbs_$mean_all, 0.6, na.rm=TRUE)] <- "#fc8d59"

mapa_zbs_$Quintil[quantile(mapa_zbs_$mean_all, 0.6, na.rm=TRUE) < mapa_zbs_$mean_all &
                    mapa_zbs_$mean_all <= quantile(mapa_zbs_$mean_all, 0.8, na.rm=TRUE)] <- "#e34a33"

mapa_zbs_$Quintil[quantile(mapa_zbs_$mean_all, 0.8, na.rm=TRUE) < mapa_zbs_$mean_all &
                    mapa_zbs_$mean_all <= quantile(mapa_zbs_$mean_all, 1, na.rm=TRUE)] <- "#b30000"


a <- mapa_zbs_ %>%
  mutate(
    quintil_valor1 = ntile(mean_all, 5)
  )
quintil <- mapa_zbs_ %>% as_tibble() %>%
  summarise(q1=quantile(mean_all, 0.2, na.rm=TRUE),
            q2=quantile(mean_all, 0.4, na.rm=TRUE),
            q3=quantile(mean_all, 0.6, na.rm=TRUE),
            q4=quantile(mean_all, 0.8, na.rm=TRUE),
            q5=quantile(mean_all, 1, na.rm=TRUE))


plot <- ggplot() +
  geom_sf(data = mapa_zbs_ %>% filter(!is.na(Quintil)), aes(fill = Quintil)) + # color = NA para quitar borders
  scale_fill_identity(name= '',
                      labels=c("#fef0d9"= paste0('Q1: ',round(quintil$q1,2)),"#fdcc8a" = paste0('Q2: ',round(quintil$q2,2)),
                               "#fc8d59" = paste0('Q3: ',round(quintil$q3,2)),"#e34a33" = paste0('Q4: ',round(quintil$q4,2)),
                               '#b30000'=paste0('Q5: ',round(quintil$q5,2))),
                      guide = "legend") +
  
  labs(title = 'Mean average net income (€) per capita (PCA) in 2022') +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0, size = 20),
        legend.position= c(1, 0.85),
        legend.text = element_text(size = 10),
        legend.margin = margin(), panel.background = element_blank(),plot.caption = element_text(hjust = 0,size=12))

plot

png('../outputs/2_outputs/FIGURE_1_map_mean_income_all_censustracts.png',units = 'mm', width = 250, height = 148, res=300)
print(plot)
dev.off()

tiff('../outputs/2_outputs/FIGURE_1_map_mean_income_all_censustracts.tiff',units = 'mm', width = 250, height = 148, res=300)
print(plot)
dev.off()


#### CORRELATION POPULATION #####


pobla_original <- read.csv('../inputs/pobla_tsi.csv',sep='|')

pobla_original <- pobla_original %>% group_by(ccaa_cd,codatzbs) %>% 
  summarise(total_pobla = sum(n_poblacion,na.rm=TRUE)) %>% ungroup()

zbs_csv_ <- zbs_csv %>% filter(codatzbs %in% pobla_original$codatzbs)

pobla <- left_join(zbs_csv_,pobla_original[c('codatzbs','ccaa_cd','total_pobla')],by=c('codatzbs','ccaa_cd'))

pobla <- pobla %>% dplyr::select(codatzbs,ccaa_cd,n_zbs,sum_pobla,total_pobla) %>% 
  dplyr::rename(pobla_tsi = total_pobla, pobla_ine = sum_pobla)

pobla <- na.omit(pobla)

a <- cor(pobla$pobla_ine,pobla$pobla_tsi,method = 'pearson')

ggplot(data=pobla,aes(x=pobla_ine,y=pobla_tsi)) + geom_smooth(method = 'lm') +
  labs(x="PCA's population calculated from census tracts (Source: INE)",y="PCA's population with Individual Health Insurance Card (Source: Atlas VPM)") + 
  theme_minimal()

summary(lm(pobla_tsi ~ pobla_ine, data=pobla))

pobla_ <- pobla %>% ungroup() %>% dplyr::select(codatzbs,pobla_ine,pobla_tsi) %>% 
  pivot_longer(!c(codatzbs),names_to = 'pobla_cd',values_to = 'pop')

pobla_$alpha[pobla_$pobla_cd %in% 'pobla_ine'] <- 0.4
pobla_$lty[pobla_$pobla_cd %in% 'pobla_ine'] <- 1
pobla_$descr[pobla_$pobla_cd %in% 'pobla_ine'] <- "PCA's population calculated \nfrom census tracts"


pobla_$alpha[pobla_$pobla_cd %nin% 'pobla_ine'] <- 0.8
pobla_$lty[pobla_$pobla_cd %nin% 'pobla_ine'] <- 1
pobla_$descr[pobla_$pobla_cd %nin% 'pobla_ine'] <- "PCA's population with \nIndividual Health Insurance Card"

plot <- ggplot(data=pobla_, aes(x=pop, group=descr, fill = descr,alpha=alpha,linetype=lty)) + geom_density() + 
  scale_fill_manual(name='', values = c("PCA's population calculated \nfrom census tracts"="firebrick",
                                        "PCA's population with \nIndividual Health Insurance Card"="#edf8b1")) + 
  labs(x='Population',
       y='Density') +
  scale_alpha_identity() + scale_linetype_identity() + 
  theme(panel.background = element_blank(),
        axis.title=element_text(size=10),legend.position = 'top')

plot
png('../outputs/2_outputs/supplementary/FIGURE_S1_density_population.png',units = 'mm', width = 250, height = 148, res=300)
print(plot)
dev.off()

tiff('../outputs/2_outputs/supplementary/FIGURE_S1_density_population.tiff',units = 'mm', width = 250, height = 148, res=300)
print(plot)
dev.off()
