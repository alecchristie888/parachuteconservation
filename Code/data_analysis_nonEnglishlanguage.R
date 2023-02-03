#Hashes are used to make comments in the code, hopefully these comments help you understand it!

#first you'll need to install these packages. You can install each one by either
#putting in the code install.packages("YOURPACKAGE") or go to the packages tab on the right next to
#the tabs 'file' and 'plots'
#install.packages('circlize')
library(circlize)
library(data.table)
library(viridis)
library(ggplot2)
library(dplyr)

# read in Alec's file of non-English language studies - nonenglish_singleaffiliation_allauthors.csv
cedata <- fread(choose.files())

#this code looks at the structure of the data
str(cedata)

#######################################################################################################
############## CHORD DIAGRAM - lead authors only ##########################################################################
#######################################################################################################

#this code is to restrict the dataset to lead authors only for this analysis, should be ~50 studies
cedata_lead_authors <- cedata[lead_author==1]

nrow(cedata_lead_authors)

#now we can make a data table with just the affiliation continents and study continents in for each study
df_leadauthors <- data.table(table(cedata_lead_authors[,list(affiliation_continent,study_continent)]))

#check which continents represented
unique(df_leadauthors$affiliation_continent)
unique(df_leadauthors$study_continent)

#order the continents so that they appear together on the graph in an appealing way
df_leadauthors$affiliation_continent <- factor(df_leadauthors$affiliation_continent,levels=c("Europe","North America","Africa","Asia","Oceania","Latin America & Caribbean"))
df_leadauthors$study_continent <- factor(df_leadauthors$study_continent,levels=c("Europe","North America","Africa","Asia","Oceania","Latin America & Caribbean"))
df_leadauthors <- df_leadauthors[order(affiliation_continent)]

#rename columns just so we're clear where researchers are coming from and going to
#i.e., from an affiliation continent and going to a study continent
colnames(df_leadauthors) <- c("from","to","N")

#here we can pick some colours, viridis is a colour-blind package, but we can specify our own.
#viridis(7,option='A')
clrblindrs=c(rgb(230/255,159/255,0),rgb(86/255,180/255,233/255),"grey60",rgb(0,114/255,178/255),rgb(213/255,94/255,0))

#now we plot the chord diagram. there are tonnes of commands here to customise.
#transparency sets the transparency of the chords.
#directional tells R that we want it to do chords going from the first column (from) to the second column (to)
#direction.type tells it we want to show arrows
#link.arr.lwd is the line width of arrows, link.arr.lty is the line type (dashed), 
#link.arr.width is the width of arrowheads
#link.lwd is the width of the line borders for the chords, link.lty is the line type of the line borders for the chords
#link.border is the colour of the border for the chords
#self.link=1 sets the chords that go from and to the same continent as 'mountains' (instead of a chord that just up and comes back down to same continent, try it with self.link=2)
png("Chord_Diagram_lead_authors_noneng.png", width=15, height=15, units="cm", res=300)
chordDiagram(df_leadauthors,grid.col=clrblindrs, transparency = 0.5, directional = 1,
            direction.type = "arrows",link.arr.lwd=0.1,link.arr.lty = 2, link.arr.width = 0.1,
            link.lwd = 0.1, link.lty = 1, link.border = "black",self.link=1)
circos.clear() #need to do this to make sure it plots properly
dev.off()
write.csv(df_leadauthors,"chord_diagram_data_leadauthors_noneng.csv")

#######################################################################################################
############## CHORD DIAGRAM - all authors ##########################################################################
#######################################################################################################

#make a data table with just the affiliation continents and study continents in for each study
df_allauthors <- data.table(table(cedata[,list(affiliation_continent,study_continent)]))

#check that we have all continents represented
unique(df_allauthors$affiliation_continent)
unique(df_allauthors$study_continent)

#order the continents so that they appear together on the graph in an appealing way
df_allauthors$affiliation_continent <- factor(df_allauthors$affiliation_continent,levels=c("Europe","North America","Africa","Asia","Oceania","Latin America & Caribbean"))
df_allauthors$study_continent <- factor(df_allauthors$study_continent,levels=c("Europe","North America","Africa","Asia","Oceania","Latin America & Caribbean"))
df_allauthors <- df_allauthors[order(affiliation_continent)]

#rename columns just so we're clear where researchers are coming from and going to
#i.e., from an affiliation continent and going to a study continent
colnames(df_allauthors) <- c("from","to","N")

#now we plot the chord diagram. there are tonnes of commands here to customise.
#transparency sets the transparency of the chords.
#directional tells R that we want it to do chords going from the first column (from) to the second column (to)
#direction.type tells it we want to show arrows
#link.arr.lwd is the line width of arrows, link.arr.lty is the line type (dashed), 
#link.arr.width is the width of arrowheads
#link.lwd is the width of the line borders for the chords, link.lty is the line type of the line borders for the chords
#link.border is the colour of the border for the chords
#self.link=1 sets the chords that go from and to the same continent as 'mountains' (instead of a chord that just up and comes back down to same continent, try it with self.link=2)
# chordDiagram(df_allauthors,grid.col=clrblindrs, transparency = 0.5, directional = 1,
#              direction.type = "arrows",link.arr.lwd=0.5,link.arr.lty = 2, link.arr.width = 0.2,
#              link.lwd = 1.1, link.lty = 1, link.border = "black",self.link=1)
# circos.clear() #need to do this to make sure it plots properly

png("Chord_Diagram_all_authors_noneng.png",width=15, height=15, units="cm", res=300)
chordDiagram(df_allauthors,grid.col=clrblindrs, transparency = 0.5, directional = 1,
             direction.type = "arrows",link.arr.lwd=0.1,link.arr.lty = 2, link.arr.width = 0.1,
             link.lwd = 0.1, link.lty = 1, link.border = "black",self.link=1, )
circos.clear() #need to do this to make sure it plots properly
dev.off()


write.csv(df_allauthors,"chord_diagram_data_allauthors_noneng.csv")

#######################################################################################################
############## BAR GRAPHS #############################################################################
#######################################################################################################

#1% no authors from study country
#2% authors from study country
#3% lead authors from study country
#4% all authors from study country
#hierarchical so if 4 is true, so is 2 and 3, but not 1.

#create list of unique non-english studies (100)
unique_studies <- unique(cedata$sampid)
#create new dataframe to hold new data
cedata_uniq_studies <- unique(cedata[,list(sampid,study_year,study_country,study_continent)])

#create some new columns to hold this data, set them all to 0 (then we can change to 1 if they are true)
cedata_uniq_studies[,`:=`(no_local_country_authors=0,any_local_country_authors=0,
             lead_local_country_authors=0,all_local_country_authors=0)]
cedata_uniq_studies[,`:=`(no_local_continent_authors=0,any_local_continent_authors=0,
             lead_local_continent_authors=0,all_local_continent_authors=0)]
#i=1
for(i in 1:length(unique_studies)){
  #select rows for each study
  cedatasample <- cedata[sampid==unique_studies[i]]
  #check whether each affiliation country matches the study country (or continent)
  country_match <- as.numeric(cedatasample$affiliation_country %in% cedatasample$study_country)
  continent_match <- as.numeric(cedatasample$affiliation_continent %in% cedatasample$study_continent)
  
  if(sum(country_match)==0){ #if no matches, then record that no authors are local
    cedata_uniq_studies[sampid==unique_studies[i],no_local_country_authors:=1]
  }
  if(sum(country_match)>0){ #if any matches, record that at least 1 author is local
    cedata_uniq_studies[sampid==unique_studies[i],any_local_country_authors:=1]
    if(country_match[1]==1 | country_match[length(country_match)]==1){ #if first or last author is local, record that lead author is local
      cedata_uniq_studies[sampid==unique_studies[i],lead_local_country_authors:=1]
      if(sum(country_match)==length(country_match)){ #if all authors are local, record this.
        cedata_uniq_studies[sampid==unique_studies[i],all_local_country_authors:=1]
      }
    }
  }
  
  #repeat above code, but this time for continents instead of countries
  if(sum(continent_match)==0){
    cedata_uniq_studies[sampid==unique_studies[i],no_local_continent_authors:=1]
  }
  if(sum(continent_match)>0){
    cedata_uniq_studies[sampid==unique_studies[i],any_local_continent_authors:=1]
    if(continent_match[1]==1 | continent_match[length(continent_match)]==1){
      cedata_uniq_studies[sampid==unique_studies[i],lead_local_continent_authors:=1]
      if(sum(continent_match)==length(continent_match)){
        cedata_uniq_studies[sampid==unique_studies[i],all_local_continent_authors:=1]
      }
    }
  }

}

head(cedata_uniq_studies)

#check this is 100
nrow(cedata_uniq_studies)

no_local_country <- nrow(cedata_uniq_studies[no_local_country_authors==1])/100
any_local_country <- nrow(cedata_uniq_studies[any_local_country_authors==1])/100
lead_local_country <- nrow(cedata_uniq_studies[lead_local_country_authors==1])/100
all_local_country <- nrow(cedata_uniq_studies[all_local_country_authors==1])/100

no_local_continent <- nrow(cedata_uniq_studies[no_local_continent_authors==1])/100
any_local_continent <- nrow(cedata_uniq_studies[any_local_continent_authors==1])/100
lead_local_continent <- nrow(cedata_uniq_studies[lead_local_continent_authors==1])/100
all_local_continent <- nrow(cedata_uniq_studies[all_local_continent_authors==1])/100

percentage_author_data <- data.table(value=c(no_local_country,any_local_country,lead_local_country,all_local_country,
                                     no_local_continent,any_local_continent,lead_local_continent,all_local_continent)*100,
                                     author_label=rep(c("No authors", "Any authors", "Lead authors", "All authors"),2),
                                     country_continent=c(rep("Country",4),rep("Continent",4)))

percentage_author_data$author_label <- factor(percentage_author_data$author_label,levels=c("No authors", "Any authors", "Lead authors", "All authors"))

#simple bar plot of percentage data for different authors
ggplot(data=percentage_author_data) + geom_col(aes(x=author_label,y=value)) + facet_wrap(.~country_continent) + scale_y_continuous(name="Percentage of studies (%)", breaks=seq(0,100,20))

#do some checks for different studies
unique_continents <- unique(cedata_uniq_studies$study_continent)
no_local_country_list<-list();any_local_country_list<-list();lead_local_country_list<-list();all_local_country_list<-list()
no_local_continent_list<-list();any_local_continent_list<-list();lead_local_continent_list<-list();all_local_continent_list<-list()

#do the same but for different continents
for(i in 1:length(unique_continents)){
no_local_country_list[[i]] <- nrow(cedata_uniq_studies[no_local_country_authors==1 & study_continent==unique_continents[i],])/25
any_local_country_list[[i]] <- nrow(cedata_uniq_studies[any_local_country_authors==1 & study_continent==unique_continents[i],])/25
lead_local_country_list[[i]] <- nrow(cedata_uniq_studies[lead_local_country_authors==1 & study_continent==unique_continents[i],])/25
all_local_country_list[[i]] <- nrow(cedata_uniq_studies[all_local_country_authors==1 & study_continent==unique_continents[i],])/25
no_local_continent_list[[i]] <- nrow(cedata_uniq_studies[no_local_continent_authors==1 & study_continent==unique_continents[i],])/25
any_local_continent_list[[i]] <- nrow(cedata_uniq_studies[any_local_continent_authors==1 & study_continent==unique_continents[i],])/25
lead_local_continent_list[[i]] <- nrow(cedata_uniq_studies[lead_local_continent_authors==1 & study_continent==unique_continents[i],])/25
all_local_continent_list[[i]] <- nrow(cedata_uniq_studies[all_local_continent_authors==1 & study_continent==unique_continents[i],])/25
}

percentage_author_data_continent <- data.table(value=c(unlist(no_local_country_list),unlist(any_local_country_list),unlist(lead_local_country_list),unlist(all_local_country_list),
                                                    unlist(no_local_continent_list),unlist(any_local_continent_list),unlist(lead_local_continent_list),unlist(all_local_continent_list))*100,
                                     author_label=rep(c(rep("No authors",4),rep("Any authors",4),rep("Lead authors",4),rep("All authors",4)),2),
                                     country_continent=c(rep("Country",4*4),rep("Continent",4*4)),
                                     continent_label = rep(unique_continents,4*4*2))

percentage_author_data_continent$author_label <- factor(percentage_author_data_continent$author_label,levels=c("No authors", "Any authors", "Lead authors", "All authors"))

percentage_author_data_continent$continent_label <- factor(percentage_author_data_continent$continent_label,levels=c("Africa","Asia","Latin America & Caribbean","Europe","North America","Oceania"))

#simple bar plot of percentage data for different authors
#Main text
ggplot(data=percentage_author_data_continent[country_continent=="Continent"]) + 
  geom_col(aes(x=continent_label,y=value,fill=author_label), position = "dodge", width=0.6)+
  scale_y_continuous(name="Percentage of studies", breaks=seq(0,100,20)) + xlab("\nContinent") + labs(fill='Author type')+ggtitle("Non-english language literature")+
  scale_fill_viridis_d()+theme_classic()
  
ggsave("Breakdown_by_author_type_noneng.png", width=30, height=10, units="cm", dpi=300)

#Appendix
ggplot(data=percentage_author_data_continent[country_continent=="Country"]) + 
  geom_col(aes(x=continent_label,y=value,fill=author_label), position = "dodge", width=0.6)+
  scale_y_continuous(name="Percentage of studies", breaks=seq(0,100,20)) + xlab("\nContinent") + labs(fill='Author type')+ggtitle("Non-english language literature")+
  scale_fill_viridis_d()+theme_classic()

ggsave("Breakdown_by_author_type_country_noneng.png", width=30, height=10, units="cm", dpi=300)

write.csv(percentage_author_data_continent,"perc_studies_by_continent_and_author_cat_noneng.csv")

############################### by publication year - appendix #############################################################
# cedata_uniq_studies
# library(dplyr)
# summary(cedata_uniq_studies$study_year)
# sort(unique(cedata_uniq_studies$study_year))
# cedata_uniq_studies %>% count(study_year)

#make sure years are treated as numeric variables
cedata_uniq_studies$study_year <- as.numeric(cedata_uniq_studies$study_year)
#find unique years
unique_year <- unique(cedata_uniq_studies$study_year)
#make some lists to store data on % of studies for each author category in each year
no_local_country_yr_list<-list();any_local_country_yr_list<-list();lead_local_country_yr_list<-list();all_local_country_yr_list<-list()
no_local_continent_yr_list<-list();any_local_continent_yr_list<-list();lead_local_continent_yr_list<-list();all_local_continent_yr_list<-list()

#do the same as before but for different years
for(i in 1:length(unique_year)){
  no_local_country_yr_list[[i]] <- nrow(cedata_uniq_studies[no_local_country_authors==1 & study_year==unique_year[i],])/nrow(cedata_uniq_studies[study_year==unique_year[i],])
  any_local_country_yr_list[[i]] <- nrow(cedata_uniq_studies[any_local_country_authors==1 & study_year==unique_year[i],])/nrow(cedata_uniq_studies[study_year==unique_year[i],])
  lead_local_country_yr_list[[i]] <- nrow(cedata_uniq_studies[lead_local_country_authors==1 & study_year==unique_year[i],])/nrow(cedata_uniq_studies[study_year==unique_year[i],])
  all_local_country_yr_list[[i]] <- nrow(cedata_uniq_studies[all_local_country_authors==1 & study_year==unique_year[i],])/nrow(cedata_uniq_studies[study_year==unique_year[i],])
  no_local_continent_yr_list[[i]] <- nrow(cedata_uniq_studies[no_local_continent_authors==1 & study_year==unique_year[i],])/nrow(cedata_uniq_studies[study_year==unique_year[i],])
  any_local_continent_yr_list[[i]] <- nrow(cedata_uniq_studies[any_local_continent_authors==1 & study_year==unique_year[i],])/nrow(cedata_uniq_studies[study_year==unique_year[i],])
  lead_local_continent_yr_list[[i]] <- nrow(cedata_uniq_studies[lead_local_continent_authors==1 & study_year==unique_year[i],])/nrow(cedata_uniq_studies[study_year==unique_year[i],])
  all_local_continent_yr_list[[i]] <- nrow(cedata_uniq_studies[all_local_continent_authors==1 & study_year==unique_year[i],])/nrow(cedata_uniq_studies[study_year==unique_year[i],])
}

#bring dataset together
percentage_author_data_continent_yr <- data.table(value=c(unlist(no_local_country_yr_list),unlist(any_local_country_yr_list),unlist(lead_local_country_yr_list),unlist(all_local_country_yr_list),
                                                       unlist(no_local_continent_yr_list),unlist(any_local_continent_yr_list),unlist(lead_local_continent_yr_list),unlist(all_local_continent_yr_list))*100,
                                               author_label=rep(c(rep("No authors",length(unique_year)),rep("Any authors",length(unique_year)),rep("Lead authors",length(unique_year)),rep("All authors",length(unique_year))),2),
                                               country_continent=c(rep("Country",length(unique_year)*4),rep("Continent",length(unique_year)*4)),
                                               year_label = rep(unique_year,4*2))

percentage_author_data_continent_yr$author_label <- factor(percentage_author_data_continent_yr$author_label,levels=c("No authors", "Any authors", "Lead authors", "All authors"))
percentage_author_data_continent_yr$continent_label <- factor(percentage_author_data_continent_yr$continent_label,levels=c("Africa","Asia","Latin America & Caribbean","Europe","North America","Oceania"))

samplesizeyears <- data.table(count=c(paste0("n=",table(cedata_uniq_studies$study_year))),years=as.numeric(names(table(cedata_uniq_studies$study_year))))

percentage_author_data_continent_yr_plot<-percentage_author_data_continent_yr#[year_label!=2018]

#simple scatter plot of percentage data for different authors
#Main text
ggplot(data=percentage_author_data_continent_yr_plot[country_continent=="Continent"]) + geom_point(aes(x=year_label,y=value,colour=author_label),size=3)+
  geom_line(aes(x=year_label,y=value,group=author_label,colour=author_label),lwd=0.75)+ 
  scale_colour_viridis_d()+
  scale_y_continuous(name="Percentage of studies",breaks=seq(0,100,20),limits=c(-3,101))+ 
  scale_x_continuous(name="Publication Year", breaks=seq(2009,2020,1)) + labs(colour="Author Type") +
  geom_text(data=samplesizeyears,aes(label=count,x=years,y=0),vjust=2,size=2.5)+
  theme(axis.text=element_text(size=7.5),axis.title=element_text(size=11))+
  ggtitle("Non-english language literature")+
  theme_classic()

ggsave("Publications_over_time_noneng.png", width=15, height=8, dpi=300, units="cm")
write.csv(percentage_author_data_continent_yr,"perc_studies_overtime_by_author_cat_noneng.csv")

write.csv(cedata_uniq_studies, "cedata_unique_studies_forstats_noneng.csv")

# #Appendix
# ggplot(data=percentage_author_data_continent_yr_plot[country_continent=="Country"]) + geom_point(aes(x=year_label,y=value,colour=author_label),size=3)+
#   geom_line(aes(x=year_label,y=value,group=author_label,colour=author_label),lwd=0.75)+ 
#   scale_colour_viridis_d()+
#   scale_y_continuous(name="Percentage of studies",breaks=seq(0,100,20),limits=c(-3,101))+ 
#   scale_x_continuous(name="Publication Year", breaks=seq(2009,2020,1)) + labs(colour="Author Type") +
#   geom_text(data=samplesizeyears,aes(label=count,x=years,y=0),vjust=2,size=2.5)+
#   theme(axis.text=element_text(size=7.5),axis.title=element_text(size=11))+
#   ggtitle("Non-english language literature")+
#   theme_classic()
# 
# ggsave("Publications_over_time_noneng_country_appendix.png", width=15, height=8, dpi=300, units="cm")


#########LINEAR MODELS#########
#######LINEAR MODEL#######

######### Authorship by continent ########
##### lead host continent author ########
testlinearmodelleadauthor<-glm(lead_local_continent_authors~study_continent, data=cedata_uniq_studies, family=binomial)
summary.glm(testlinearmodelleadauthor)
anova(testlinearmodelleadauthor,
      update(testlinearmodelleadauthor, ~1), # update here produces null model for comparison
      test="Chisq")

##### all host continent author ########
testlinearmodelallauthor<-glm(all_local_continent_authors~study_continent, data=cedata_uniq_studies, family=binomial)
summary.glm(testlinearmodelallauthor)
anova(testlinearmodelallauthor,
      update(testlinearmodelallauthor, ~1), # update here produces null model for comparison
      test="Chisq")

###### Authorship over time ########
###### lead host continent author ######
testlinearmodeltime<-glm(lead_local_continent_authors~study_year, data=cedata_uniq_studies, family=binomial)
summary.glm(testlinearmodeltime)
anova(testlinearmodeltime,
      update(testlinearmodeltime, ~1), # update here produces null model for comparison
      test="Chisq")

###### all host continent authors #######
testlinearmodeltimeallauthors<-glm(all_local_continent_authors~study_year, data=cedata_uniq_studies, family=binomial)
summary.glm(testlinearmodeltimeallauthors)
anova(testlinearmodeltimeallauthors,
      update(testlinearmodeltimeallauthors, ~1), # update here produces null model for comparison
      test="Chisq")

###### no host continent authors #######
testlinearmodelnoauthors<-glm(no_local_continent_authors~study_year, data=cedata_uniq_studies, family=binomial)
summary(testlinearmodelnoauthors)
anova(testlinearmodelnoauthors,
      update(testlinearmodelnoauthors, ~1), # update here produces null model for comparison
      test="Chisq")


