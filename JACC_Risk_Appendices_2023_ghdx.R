##########################################################################################################################
##########################################################################################################################
# Author: Megan Lindstrom 
# Date: November 15, 2023
# Purpose: Produce pdf files with plots and maps for JACC Risk Appendices. This can be run for each risk by specifying 
#          the output directory and risk_id.
##########################################################################################################################
##########################################################################################################################
rm(list=ls())

# read in necessary packages and shared functions

packages <- c("data.table", "R.utils", "raster", "data.table", "ggplot2", "rgdal", "sf", "stringr", "tidyverse",
              "cowplot", "scales", "ggrepel", "kableExtra", "dplyr", "gridExtra", "formattable", "extrafont", 
              "openxlsx", "gridExtra", "grid", "gtable", "viridis")

suppressMessages(invisible(lapply(packages, library, character.only = TRUE)))

# Load central libraries
suppressMessages(sourceDirectory("ADDRESS", modifiedOnly=FALSE))

###### Functions ######
`%ni%` <- Negate(`%in%`)
date <- gsub("-", "_", Sys.Date())
proper=function(s) sub("(.)", ("\\U\\1"), tolower(s), pe=TRUE)
'%!in%' <- Negate('%in%')
###### Paths ######
outdir <- "ADDRESS"

###### Arguments to pull the data ######
gbd_round <- 9
decomp <- 'iterative'
year <- 2022
compare_version <- 7936

# list the causes and risks we are creating maps for
cause <- 491
risk_id <- 125

# read in mapping function for this year
if(gbd_round==9){
  source("ADDRESS")
} else {
  source("ADDRESS")
}

# make data frame for name fixes
name_fixes = data.frame("metric_name"=c(rep("Number", 6), rep("Rate", 6)), 
                        "measure_name"=rep(c("Deaths", "DALYs (Disability-Adjusted Life Years)", "YLDs (Years Lived with Disability)", "YLLs (Years of Life Lost)", "Prevalence", "Incidence"), 2),
                        "fixed_measure_name"=c("Deaths", "DALYs", "YLDs", "YLLs", "Prevalence", "Incidence", "Mortality", "DALYs (Disability-Adjusted Life Years)", "YLDs (Years Lived with Disability)", "YLLs (Years of Life Lost)", "Prevalence", "Incidence"))


##########################################################################################################################
# Pull data
##########################################################################################################################

# pull in cause/risk/location/age metadata
cause_metadata <- get_cause_metadata(cause_set_id = 3, release_id=9) # reporting hierarchy
risk_metadata <- get_rei_metadata(rei_set_id = 1, release_id=9) # reporting hierarchy
locs <- get_location_metadata(location_set_id = 91, release_id=9) # set 91 = locations in the public domain
locs1 <- get_location_metadata(location_set_id = 107, release_id=9) # set 107 = The hierarchy used to produce maps for the GBD capstone
age_meta <- get_age_metadata(age_group_set_id = 19, release_id=9) #age metadata 

#data for plot 1
mapdata<-get_outputs(topic='rei', release_id = 9, compare_version_id=compare_version,
                    cause_id = cause, rei_id= risk_id, year_id = 2022, age_group_id = 27, sex_id = 3, 
                    metric_id = 3, measure_id = 1, location_id='all')

#data for plot 2
agesexdiff<-get_outputs(topic='rei', release_id = 9, compare_version_id=compare_version,
                        cause_id = cause,  rei_id= risk_id, year_id = 2022, sex_id=c(1,2), age_group_id=c(1, 188, 9:20, 30:32, 235),
                        metric_id = 3, measure_id = c(1,2), location_id=1)

#apply age metadata to plot 2 data
source("ADDRESS")
age_meta <- get_age_metadata(release_id = 9)
agesexdiff <- merge(agesexdiff, age_meta[,.(age_group_id, age_group_years_start, age_group_years_end)], by = 'age_group_id', all.x=T)
agesexdiff[age_group_id == 1, `:=` (age_group_years_start = 0, age_group_years_end = 5)]
agesexdiff[age_group_id == 188, `:=` (age_group_years_start = 5, age_group_years_end = 20)]

# set non-estimated ages
source("ADDRESS")
rei_meta <- get_rei_metadata(rei_set_id = 1, release_id = 9)

fatal_age_start <- age_meta[age_group_id == rei_meta[rei_id == risk_id, yll_age_group_id_start], age_group_years_start]
min_age_start <- min(age_meta[age_group_id %in% c(rei_meta[rei_id == risk_id, yll_age_group_id_start], 
                                                  rei_meta[rei_id == risk_id, yld_age_group_id_start]), age_group_years_start])
agesexdiff[measure_id == 1 & age_group_years_start < fatal_age_start & age_group_years_end <= fatal_age_start, `:=` (val = 0, sex = 'Not estimated')]
agesexdiff[measure_id == 2 & age_group_years_start < min_age_start & age_group_years_end <= min_age_start, `:=` (val = 0, sex = 'Not estimated')]

age_group_order <- unique(agesexdiff[order(age_group_years_start), age_group_name])

#data for plot 3
regionbysex<-get_outputs(topic='rei', release_id = 9, compare_version_id=compare_version,
                    cause_id = cause,  rei_id= risk_id, year_id = 2022, sex_id=c(1,2), age_group_id=27,
                    measure_id=2, metric_id=3, location_id=c(5,9,21,32,42,56,65,70,73,96,100,104,120,124,134,138,159,167,174,192,199))

#data for plot 4

panels <- get_outputs(topic='rei', release_id = 9, compare_version_id=compare_version,
                      cause_id = cause,  rei_id= risk_id, location_id=1, year_id=c(1990:2022), sex_id=3, 
                     age_group_id=c(22, 27), measure_id=c(1:4), metric_id=3)

causename <- unique(mapdata$cause_name)
riskname <-unique(mapdata$rei_name)

riskname <- riskname
riskname <- paste(tolower(substr(riskname, 1, 1)), substr(riskname, 2, nchar(riskname)), sep="")

riskname <- ifelse(riskname=="high body-mass index", "high body mass index", riskname)
print(riskname)
#########################################################
########## PLOT 1: MAP DATA #############################


# multiple by 100000 for rates
mapdata <- merge(mapdata, locs1, by = "location_id")
mapdata<- mapdata[!grepl("RUS_", mapdata$ihme_loc_id),] #remove russian subnats from data
mapdata$mapvar<- mapdata$val*100000


xmin <- min(mapdata$mapvar, na.rm=TRUE)
xmax <- max(mapdata$mapvar,na.rm=TRUE)/1.25
# set bin limits based on the 'xmin' and 'xmax'
lim <- seq(xmin, xmax, length.out = 11)
# resets upper and lower limit of bins to match the actual max and min values
# set bin limits

# resets upper limit of final bin if max_val was changed from max(temp_mort$val)
lim[11] = ceiling(max(mapdata$mapvar))

limlen =length(lim)-1
lim2 <- round(seq((xmin), xmax, length.out = 11),2)
# label bins
labs <- c(paste0("< ", lim2[2]), paste0(lim2[2:(limlen-1)], " to < ", lim2[3:limlen]), paste0(">= ", lim2[limlen]))

message(paste0(lim, sep=" | "))
message(paste0(labs, sep=" | "))

pdf(file = paste0(outdir, risk_id, "_map1.pdf"), width=8,height=6)
gbd_map(mapdata, 
        limits=lim, # change to whatever bins make sense for your data
        sub_nat="capstone", # prints map with topic paper level subnationals
        labels=labs, # label bins in the legend
        title= str_to_upper(riskname),
        pattern=NULL, 
        #col="RdYlBu", 
        col= c('#d3ffbe','#9fef9f','#82e0a2','#67d0ae','#50c0c1','#3b8ab1','#2851a2',
        '#181892','#320b83','#4c0073'), #custom colors to match global/regional 2 pagers
        col.reverse=FALSE, na.color="gray",
        legend.title=NULL, legend.columns=1, legend.cex=1, legend.shift=c(0, 0), inset=FALSE)
dev.off()

###########################################################################################################
########## PLOT 2: AGE/SEX DIFFERENCES 
agesexdiff<- agesexdiff[!(agesexdiff$measure=="prevalence" & is.na(agesexdiff$val)), ]
agesexdiff$valrate<- agesexdiff$val*100000
agesexdiff$sex <- ifelse(is.na(agesexdiff$val),'Not estimated',agesexdiff$sex)
agesexdiff$valrate <- ifelse(is.na(agesexdiff$valrate),0,agesexdiff$valrate)


#pull in age start is an option too for sorting or relevel age name as a factor 
sortageframe <- data.frame(age_group_name = c("Under 5", "5 to 19","20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
                                              "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",
                                              "85 to 89", "90 to 94", "95 plus"), sortage = c(1:18))
agesexdiff2 <- merge(agesexdiff, sortageframe, by ="age_group_name")


#make dataframe wide instead of long (dont split them)

Males <- agesexdiff2 %>%
  filter(sex== "Male" | sex == "Not estimated")%>%
  mutate(location_name = fct_reorder(age_group_name, sortage))
Females <- agesexdiff2  %>%
  filter(sex == "Female" | sex == "Not estimated")%>%
  mutate(location_name = fct_reorder(age_group_name, sortage))
head(Females)

#merge males and females, create a difference column, and then also a mean column for position 
#geom_text for label 

measure_names <- c(
  `daly` = "DALYs (Disability-Adjusted Life Years)",
  `death` = "Mortality",
  `prevalence` = "Prevalence"
)


p <- ggplot(agesexdiff2)+
  geom_segment(data = Males,
               aes(x = valrate, y = reorder(age_group_name,sortage),
                   yend = reorder(Females$age_group_name,sortage), xend = Females$valrate), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  geom_point(aes(x = valrate, y = age_group_name, color = sex, shape=sex), size = 4, show.legend = TRUE)+
  scale_color_manual(values=c("orange", "cadetblue3", "cornsilk4"))+
  ylab("Age Group")+
  xlab("Rate per 100,000")+
  labs(caption = str_wrap(paste0("Figure 4. Global cardiovascular disease age-specific estimates attributable to ", riskname," per 100,000 by sex in 2022, difference in sex estimates indicated by gray bar"),100))+
  scale_x_continuous(label= ~ ifelse(.x < 1, scales::comma(.x), scales::comma(.x, accuracy = 1)), breaks = scales::pretty_breaks(n = 8)) + 
  facet_wrap(~measure,scales="free", labeller = as_labeller(measure_names))+
  theme(text=element_text(size=14),legend.title=element_blank())+
  theme(axis.text.x=element_text(angle=45,  hjust = 1))+
  theme_bw(base_size=12)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title.position = "plot",legend.title=element_blank(), 
        axis.text.x=element_text(angle=45,  hjust = 1),
        plot.margin=unit(c(.5,.5,.5,.5), "cm"),
        strip.text = element_text(size = 8),text=element_text(size=12),plot.caption = element_text(size = 14, hjust = 0))
p

tiff(paste0(outdir, risk_id,"_plot1.tif"), width=10,
     height=8,
     units="in",
     res=300)
p
dev.off()



#######################################################
##########graph by sex 

regionbysex$valrate <- round(regionbysex$val*100000, digits = 0)
regionbysex<- merge(regionbysex, locs1[,c('location_id','sort_order')], by="location_id")




plot2 <- ggplot(regionbysex, aes( y = valrate, x = reorder(location_name, sort_order), fill=sex)) +
  geom_bar(stat = "identity",width=0.5, position=position_dodge(width = .5), alpha=.7)+
  geom_errorbar( aes(x=factor(location_name), ymin=lower*100000, ymax=upper*100000), width=0.4, colour="black", alpha=0.6, size=.5, position=position_dodge(width = .5))+
  geom_hline(yintercept = 0 , size = 0.2, alpha = 0.5)+
  scale_fill_manual(values=c("orange", "cadetblue3"))+
  scale_y_continuous(label= ~ ifelse(.x < 1, scales::comma(.x), scales::comma(.x, accuracy = 1)))+
  scale_x_discrete(breaks=regionbysex$location_name, labels= str_wrap(regionbysex$location_name,20))+
  ylab("Age-Standardized DALYs per 100,000")+
  xlab("Global Burden of Disease Regions")+
  labs(fill = "Sex",  caption = str_wrap(paste0("Figure 3. Cardiovascular disease age-standardized disability-adjusted life years (DALYs) attributable to ", riskname," per 100,000 by Global Burden of Disease region and sex in 2022, 95% uncertainty interval indicated by black bar"),100))+
  theme_bw(base_size=12)+
  theme(axis.text.x=element_text(angle=45,  hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title.position = "plot",legend.title=element_blank(),
        plot.margin=unit(c(.5,.5,.5,.5), "cm"),
        #text=element_text(size=12),
        plot.caption = element_text(size = 14, hjust = 0))

tiff(paste0(outdir, risk_id, "_plot2.tif"), width=10,
     height=8,
     units="in",
     res=300)
plot2
dev.off()



#######################################################################################################
###################################### Plot 4: Panels ###################################### 
#######################################################################################################

#shift legend function
shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}


  # add in fixed measure names
  panels = merge(panels, name_fixes, by=c("measure_name", "metric_name"))
  
  # multiple values by 100000 for rates, change unit name
  panels$val=panels$val*100000
  panels$upper = panels$upper*100000
  panels$lower = panels$lower*100000
  panels$metric_name = "Rate per 100,000"
  panels$fixed_measure_name = factor(panels$fixed_measure_name,levels=c( "YLDs (Years Lived with Disability)", "YLLs (Years of Life Lost)","Mortality",  "DALYs (Disability-Adjusted Life Years)"))
  
  panels1<- subset(panels, panels$val!='NA')
  
  q2 <-  ggplot(data=panels1, aes(x=year_id, y=val, colour=age_group_name)) +
    geom_line(aes(x=year_id, y=val, colour=age_group_name))+ geom_ribbon(aes(ymin=lower, ymax=upper, fill=age_group_name), linetype=0, alpha=0.15) + 
    facet_wrap(~fixed_measure_name, scales="free", ncol=2) + 
    theme_bw(base_size=12) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title=element_blank()) + 
    xlab("Year") + ylab("Rate per 100,000") +
    scale_color_brewer(palette="Set1") + scale_y_continuous(label= ~ ifelse(.x < 20, scales::comma(.x), scales::comma(.x, accuracy = 2)), limits=c(ifelse(min(panels1$lower)<0, NA, 0), NA)) + 
    geom_hline(yintercept = 0 , size = 0.2, alpha = 0.5)+
    scale_x_continuous(breaks = c(1990, 2000, 2010, 2022)) + scale_fill_brewer(palette="Set1") 

  

  q2new <- q2 +
    guides(fill = guide_legend(title.position = "top",
                               label.position = "bottom",
                               nrow = 1)) +
    labs(caption= str_wrap(paste0("Figure 2. Global cardiovascular disease attributable to ", riskname, " estimates per 100,000 by measure with shaded 95% uncertainty interval, 1990-2022"),100))+
    theme(legend.direction = "horizontal",plot.caption = element_text(hjust = 0, size=14), 
          plot.margin=unit(c(.5,.5,.5,.5), "cm"), 
          legend.position="bottom")

  tiff(paste0(outdir, risk_id,"_plot3.tif"), width=10,
       height=8,
       units="in",
       res=300)
  grid.draw(shift_legend(q2new))
  dev.off()

#####################################################################
  ####################################################################
##risk PRINT OUT

  library(png)
  library(tiff)
  library(grid)
  library(gridExtra)
  library(magick)
  
  p_1 <- grid::rasterGrob(image_read_pdf(paste0(outdir,risk_id,'_map1.pdf')))
  p_2 <- grid::rasterGrob(readTIFF(paste0(outdir,risk_id,'_plot1.tif')))
  p_3 <- grid::rasterGrob(readTIFF(paste0(outdir,risk_id,'_plot2.tif')))
  p_4 <- grid::rasterGrob(readTIFF(paste0(outdir,risk_id,'_plot3.tif')))    
  
  title <- textGrob("")
  
  # Add a zeroGrob of height 2cm on top of the title
  title <- arrangeGrob(zeroGrob(), title, 
                       widths = unit(3, 'npc'), 
                       heights = unit(c(3, 3), c('cm', 'npc')),
                       as.table = FALSE)
  
  pdf(paste0(outdir,risk_id,'_twopagerappendix.pdf'), width = 8.5, height=11)
  tryCatch(grid.arrange( top=title,p_1,p_4,ncol=1, bottom=title)+ grid.text(str_wrap(paste0("Figure 1. Global map of cardiovascular disease mortality attributable to ", riskname," per 100,000 in 2022 with equal interval classification"),100),x=unit(.2, 'npc'), y=unit(.56, 'npc'),gp = gpar(col = "black", fontsize =8), just="left"), error=function(e){})
  tryCatch(grid.arrange(p_3,p_2, ncol=1, top=title, bottom= title), error=function(e){})
  dev.off()
  