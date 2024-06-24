library(ggspatial)

plot_transitions <- function(data,t0,period_length,shapefile,type='project_only'){
  
  if(type=='project_only') {
    
    # filter to project pixels
    
    data <- data %>% filter(type=='Project')
    
  }
  
  # count number of 1s at project start
  
  t0_index <- grep(paste0('luc_',t0),colnames(data))
  
  data_filtered <- data[data[,t0_index]==1,]
  
  # identify where there have been changes
  
  tend <- t0 + period_length
  
  luc_t5 <- data_filtered %>% 
    select(paste0('luc_',tend))
  
  response <- case_when(
    luc_t5==1 ~ NA,
    luc_t5==2 ~ 'deg',
    luc_t5==3 ~ 'def',
    luc_t5==4 ~ 'ref',
    luc_t5>4 ~ NA)
  
  data_filtered$response <- as.factor(response)
  data_filtered <- data_filtered %>% filter(!is.na(response))
  
  # adding deg --> def transition
  
  # count number of 2s at project start
  
  t0_index <- grep(paste0('luc_',t0),colnames(data))
  
  data_filtered_2s <- data[data[,t0_index]==2,]

  # identify where there have been changes
  
  luc_t5 <- data_filtered_2s %>% 
    select(paste0('luc_',tend))
  
  response <- case_when(
    luc_t5==1 ~ NA,
    luc_t5==2 ~ NA,
    luc_t5==3 ~ 'deg_to_def',
    luc_t5==4 ~ NA,
    luc_t5>4 ~ NA)
  
  data_filtered_2s$response <- as.factor(response)
  data_filtered_2s <- data_filtered_2s %>% filter(!is.na(response))
  
  combined_dat <- bind_rows(data_filtered, data_filtered_2s)
  combined_dat$response <- factor(combined_dat$response, levels=c('deg','deg_to_def','def','ref'))
  
  # plotting 
  
  plot <- combined_dat %>% 
    filter(response != 0) %>% 
    ggplot(aes(x=lng,y=lat,colour=response))+
    geom_sf(data=shapefile,inherit.aes=F,fill='grey80',colour=NA)+
    geom_point(alpha=0.5)+

    scale_colour_manual(values=c('yellow','orange','red','green'),name='Transition',labels=c('Undisturbed to degraded','Degraded to deforested','Undisturbed to deforested','Undisturbed to reforested'))+
    annotation_scale(text_cex = 1.3)+
    theme_void()
  
  return(plot)
  
}
