plot_matching_variables <- function(data){
  
  cont_data <- data %>% dplyr::select(type,elevation,slope,access,
                                      starts_with('cpc'))
  cont_data[,5:length(cont_data)] <- 100*cont_data[,5:length(cont_data)] # cpcs as percentages
  cont_data <- melt(cont_data)
  
  # rename labels
  
  cont_data$variable <- factor(cont_data$variable,levels=c('access','cpc0_u','cpc0_d',
                                                           'slope','cpc5_u','cpc5_d',
                                                           'elevation','cpc10_u','cpc10_d'))
  
  levels(cont_data$variable) <- c('Inaccessibility',
                                  'Forest~cover~t[0]',
                                  'Deforestation~t[0]',
                                  'Slope',
                                  'Forest~cover~t[-5]',
                                  'Deforestation~t[-5]',
                                  'Elevation',
                                  'Forest~cover~t[-10]',
                                  'Deforestation~t[-10]')
  
  # plot
  
  matchingvars <- ggplot(data=cont_data,mapping=aes(x=value,colour=type))+
    geom_density(adjust=10,size=1)+
    facet_wrap(scales='free',nrow=3,~variable,labeller=label_parsed)+
    ylab('Density')+
    scale_colour_manual(values=c('blue','red'),labels=c('Counterfactual','Project'))+
    theme_classic()+
    theme(legend.title=element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  return(matchingvars)
  
}