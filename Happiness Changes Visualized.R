
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(repr)
library(treemap)
library(plotly)
library(countrycode)
library(maps)
library(rworldmap)

## read in data
df5 <- read.csv('2015.csv')
df6 <- read.csv('2016.csv')

str(df5)
str(df6)

#fix bad column names, tselect columns of interes


c_df5 <- df5 %>% select(Country,Region,Happiness_Rank=Happiness.Rank,Happiness_Score=Happiness.Score,GDP=Economy..GDP.per.Capita.,Family,Life_Expectancy=Health..Life.Expectancy.,Government_Corruption=Trust..Government.Corruption.)
c_df6 <- df6 %>% select(Country,Region,Happiness_Rank=Happiness.Rank,Happiness_Score=Happiness.Score,GDP=Economy..GDP.per.Capita.,Family,Life_Expectancy=Health..Life.Expectancy.,Government_Corruption=Trust..Government.Corruption.)


dim(c_df5)
dim(c_df6)
# mismatched dim -> need to clean country entries

c_df6$Country[df6$Country == 'Somaliland Region Namibia'] =  'Somaliland region'
dropm6 <- c_df6$Country[!c_df6$Country %in% c_df5$Country] #missing country filter
dropm5 <- c_df5$Country[!c_df5$Country %in% c_df6$Country] #missing country filter
f_df5 <- c_df5 %>% filter(!Country %in% dropm5) #apply filter
f_df6 <- c_df6 %>% filter(!Country %in% dropm6) #apply filter

dim(f_df5)
dim(f_df6)
#it matches!
f_df6$Country %in% f_df5$Country #all good!

head(f_df5)
head(f_df6)
unique(f_df5$Region)

#merge data from 2015 & 2016
df1<-merge(f_df5[,], f_df6[,], by.x = "Country", by.y = "Country") %>% select(-Region.y)

#rename merge suffix .x = 2015, .y = 2016
colnames(df1)[9:14] <- gsub('.{2}$', '.2016', colnames(df1)[9:14])
colnames(df1)[1:8] <- gsub('.{2}$','.2015',colnames(df1)[1:8])
colnames(df1)[1] = "Country"
colnames(df1)[2] = 'Region'
#add new columns for Y/Y changes
df1 <- df1 %>% mutate(Happiness_Rank_Change=Happiness_Rank.2016 - Happiness_Rank.2015,Happiness_Score_Change=Happiness_Score.2016 - Happiness_Score.2015, GDP_Change = GDP.2016 - GDP.2015, Life_Expectancy_Change = Life_Expectancy.2016 - Life_Expectancy.2015, Government_Corruption_Change = Government_Corruption.2016 - Government_Corruption.2015)

top30inc <- df1 %>% arrange(desc(Happiness_Rank_Change)) %>%  mutate(Country=factor(Country,Country)) %>% head(30)
head(top30inc)
top30dec <- df1 %>% arrange(Happiness_Rank_Change) %>% mutate(Country = factor(Country,Country))%>% head(30)
head(top30dec)


#### treepolots
top25 <- df1 %>% arrange(Happiness_Rank.2015) %>% mutate(Country=factor(Country,Country)) %>% head(25)
bottom25 <- df1 %>% arrange(Happiness_Rank.2015) %>% mutate(Country=factor(Country,Country)) %>% tail(25)

top25_ <- df1 %>% arrange(Happiness_Rank.2016) %>% mutate(Country=factor(Country,Country)) %>% head(25)
bottom25_ <- df1 %>% arrange(Happiness_Rank.2016) %>% mutate(Country=factor(Country,Country)) %>% tail(25)

top25$label<-paste(top25$Country,top25$Happiness_Score.2015,paste0('Rank: ',top25$Happiness_Rank.2015),sep="\n ")
bottom25$label<-paste(bottom25$Country,bottom25$Happiness_Score.2015,bottom25$Happiness_Rank.2015,paste0('Change: ', bottom25$Happiness_Rank_Change),sep='\n')

top25_$label<-paste(top25_$Country,top25_$Happiness_Score.2016,paste0('Change: ',top25_$Happiness_Rank_Change),paste0('Rank: ',top25_$Happiness_Rank.2016),sep="\n ")
bottom25_$label<-paste(bottom25_$Country,bottom25_$Happiness_Score.2016,bottom25_$Happiness_Rank.2016,paste0('Change: ', bottom25$Happiness_Rank_Change),paste0('Rank: ',top25_$Happiness_Rank.2016),sep='\n')


options(repr.plot.width=12, repr.plot.height=8) 
treemap(top25,
        index=c("label"),
        vSize="Happiness_Score.2015",
        vColor="Happiness_Rank.2015",
        type="value",
        title="Top25 Happiest Countries 2015",
        palette=colorRampPalette(rev(brewer.pal(7, "RdBu")))(20),
        command.line.output = TRUE, 
        fontsize.legend = 12, fontsize.title = 12, fontsize.labels=8,
        format.legend = list(scientific = FALSE, big.mark = " "))

options(repr.plot.width=12, repr.plot.height=8) 
treemap(bottom25,
        index=c("label"),
        vSize="Happiness_Score.2015",
        vColor="Happiness_Rank.2015",
        type="value",
        title="25 Unhappiest Countries 2015",
        palette=colorRampPalette(rev(brewer.pal(7, "RdBu")))(20),
        command.line.output = TRUE,
        fontsize.legend = 12, fontsize.title = 12, fontsize.labels = 8,
        format.legend = list(scientific = FALSE, big.mark = " "))


options(repr.plot.width=12, repr.plot.height=8) 
treemap(top25_,
        index=c("label"),
        vSize="Happiness_Score.2016",
        vColor="Happiness_Rank_Change",
        type="value",
        title="Top25 Happiest Countries 2016 colored by Rank change",
        palette=colorRampPalette(rev(brewer.pal(7, "RdBu")))(20),
        command.line.output = TRUE, 
        fontsize.legend = 12, fontsize.title = 12, fontsize.labels=8,
        format.legend = list(scientific = FALSE, big.mark = " "))

options(repr.plot.width=12, repr.plot.height=8) 
treemap(bottom25_,
        index=c("label"),
        vSize="Happiness_Score.2016",
        vColor="Happiness_Rank_Change",
        type="value",
        title="25 Unhappiest Countries 2016 colored by Rank change",
        palette=colorRampPalette(rev(brewer.pal(7, "RdBu")))(20),
        command.line.output = TRUE,
        fontsize.legend = 12, fontsize.title = 12, fontsize.labels = 7,
        format.legend = list(scientific = FALSE, big.mark = " "))

z <- ggplot(df1)
z_ <- ggplot(df1 %>% arrange(Happiness_Rank.2015) %>% mutate(Country=factor(Country,Country)) %>% head(80))
z_1 <- ggplot(df1 %>% arrange(Happiness_Rank.2015) %>% mutate(Country=factor(Country,Country)) %>% tail(80))
z + geom_point(aes(Happiness_Rank_Change,GDP_Change,color=Region),size=3,shape=18,alpha=.4) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + geom_smooth(aes(Happiness_Rank_Change,GDP_Change))

z_ + geom_pointrange(aes(Country,y=Happiness_Score.2016,ymin=Happiness_Score.2015,ymax=Happiness_Score.2016,color=Region),shape=15,alpha=.5) + theme_minimal() + scale_color_brewer(palette='RdBu') +  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_pointrange(aes(Country,y=GDP.2016,ymin=GDP.2015,ymax=GDP.2016,color=Region),shape=18,alpha=.5) + geom_pointrange(aes(Country,y=Life_Expectancy.2016,ymin=Life_Expectancy.2015,ymax=Life_Expectancy.2016,color=Region),shape=16,alpha=.5)  #

z_1 + geom_pointrange(aes(Country,y=Happiness_Score.2016,ymin=Happiness_Score.2015,ymax=Happiness_Score.2016,color=Region),alpha=.5) + theme_minimal() + scale_color_brewer(palette='RdBu') +  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + coord_flip() #

g + geom_histogram(stat='identity') + theme_minimal() + scale_fill_brewer(palette = 'RdBu')  + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab('') + ylab('Positive Change in Happiness Ranking 2015-2016)')  + ggtitle('Top 30 Countries with Improving Happiness Ranking 2015-2016')
g + geom_histogram(stat='identity',alpha=.5) + theme_minimal() + scale_fill_brewer(palette = 'RdBu')  + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab('') + ylab('')  + ggtitle('Top 30 Countries with Improving Happiness Ranking 2015-2016') + coord_polar()


plot_ly(df1,x=~Region,
        y=~Happiness_Score_Change,
        type="box",
        boxpoints="all",
        title("Happiness Score Change By Region")
        pointpos = -1.8,
        color=~Region,
        colors = 'RdBu')%>%
  layout(xaxis=list(showticklabels = FALSE),
         margin=list(b = 40))

plot_ly(df1,x=~Happiness_Score_Change,
        y=~Life_Expectancy_Change,
        color=~Region,
        colors="RdBu",
        size=~Happiness_Score_Change) %>%
  layout(xaxis=list(title="Happiness Score"),
         yaxis=list(title="Health Life Expectancy"))


h <- ggplot(top30dec,aes(x=Country,Happiness_Rank_Change,fill=Region))
h + geom_histogram(stat='identity',alpha=.5) + theme_minimal() + scale_fill_brewer(palette='RdBu') + theme(axis.text.x=element_text(angle=60,hjust=1)) + xlab('') + ylab('Decrease in Happiness Ranking 2015 - 2016') + ggtitle('Top 30 Countries with Declining Happiness Ranking 2015-2016')
h + geom_histogram(stat='identity',alpha=.5) + theme_minimal() + scale_fill_brewer(palette='RdBu') + theme(axis.text.x=element_text(angle=60,hjust=1)) + xlab('') + ylab('Decrease in Happiness Ranking 2015 - 2016') + ggtitle('Top 30 Countries with Declining Happiness Ranking 2015-2016') + coord_polar()

ggplot(df1,aes(Region,Happiness_Score_Change,fill=Region),alpha=.5) + geom_violin(alpha=0.5) + theme_minimal()  + scale_fill_brewer(palette = 'RdBu')+ theme(axis.text.x=element_text(angle=60,hjust=1)) + xlab('') + ylab('Change in Happiness Score 2015-2016')
ggplot(df1,aes(Region,GDP_Change,fill=Region),alpha=.5) + geom_violin(alpha=0.5) + theme_minimal()  + scale_fill_brewer(palette = 'RdBu')+ theme(axis.text.x=element_text(angle=60,hjust=1)) + xlab('') + ylab('Change in GDP 2015-2016')
ggplot(df1,aes(Region,Life_Expectancy_Change,fill=Region),alpha=.5) + geom_violin(alpha=0.5) + theme_minimal()  + scale_fill_brewer(palette = 'RdBu')+ theme(axis.text.x=element_text(angle=60,hjust=1)) + xlab('') + ylab('Change in Life Expectancy 2015-2016')
ggplot(df1,aes(Region,Government_Corruption_Change,fill=Region),alpha=.5) + geom_violin(alpha=0.5) + theme_minimal()  + scale_fill_brewer(palette = 'RdBu')+ theme(axis.text.x=element_text(angle=60,hjust=1)) + xlab('') + ylab('Change in Government Corruption 2015-2016')

ggplot(df1,aes(Happiness_Score_Change,Government_Corruption_Change,color=Region)) + geom_point(shape=18,size=4,alpha=.5) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + xlab('Happiness Score Change 2015-2016') + ylab('Government Corruption Change 2015-2016') + ggtitle('Happiness Score Change Vs Government Corruption Change 2015-2016')
ggplot(df1,aes(Happiness_Score.2015,Government_Corruption.2015,color=Region)) + geom_point(shape=18,size=4,alpha=.5) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + xlab('Happiness Score 2015') + ylab('Government Corruption Change 2015') + ggtitle('Happiness Score Vs Government Corruption 2015')
ggplot(df1,aes(Happiness_Score.2016,Government_Corruption.2016,color=Region)) + geom_point(shape=18,size=4,alpha=.5) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + xlab('Happiness Score 2016') + ylab('Government Corruption Change 2016') + ggtitle('Happiness Score Vs Government Corruption 2016') 


ggplot(df1,aes(Life_Expectancy_Change,GDP_Change,color=Region)) + geom_point(shape=18,size=4,alpha=.5) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + xlab('Life Expectancy Change 2015-2016') + ylab('GDP Change 2015-2016') + ggtitle('Life Expectancy Change vs GDP Change 2015-2016') + geom_smooth(method=lm,se=FALSE)
ggplot(df1,aes(Life_Expectancy.2015,GDP.2015,color=Region,size=Happiness_Score.2015)) + geom_point(shape=18,alpha=.5) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + xlab('Life Expectancy 2015') + ylab('GDP 2015') + ggtitle('Life Expectancy Vs GDP 2015') 
ggplot(df1,aes(Life_Expectancy.2015,GDP.2016,size=Happiness_Score.2016)) + geom_point(aes(size=GDP.2016,color=GDP.2016),shape=18,alpha=.8) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + xlab('Life Expectancy 2016') + ylab('GDP 2016') + ggtitle('Life Expectancy Vs GDP 2016') + geom_smooth(method='lm',formula = y ~ splines::bs(x, 3)) + scale_colour_gradientn(colours =colorRampPalette(rev(brewer.pal(7, "RdBu")))(100), values=seq(0, 100, length.out=100)/100) + 
  geom_text(aes(label=Country),nudge_y = -.05, size=2.5,color="black",alpha=.95)+labs(title="Economy vs Life Expectancy - 2015",x="Life Expectancy - 2015") + theme(legend.position='none')

df1_ <- joinCountryData2Map(df1, joinCode="NAME", nameJoinColumn="Country")
mapCountryData(df1_, nameColumnToPlot="Happiness_Score.2015", mapTitle="World Map for Happiness Score -2015",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="Happiness_Score.2016", mapTitle="World Map for Happiness Score -2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="Happiness_Rank_Change", mapTitle="World Map for Change in Happiness Rank 2015-2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')

mapCountryData(df1_, nameColumnToPlot="GDP.2015", mapTitle="World Map for GDP -2015",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="GDP.2016", mapTitle="World Map for GDP -2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="GDP_Change", mapTitle="World Map for Change in GDP 2015-2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')

mapCountryData(df1_, nameColumnToPlot="Life_Expectancy.2015", mapTitle="World Map for Life Expectancy -2015",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="Life_Expectancy.2016", mapTitle="World Map for Life Expectancy -2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="Life_Expectancy_Change", mapTitle="World Map for Change in Life Expectancy 2015-2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')

mapCountryData(df1_, nameColumnToPlot="Government_Corruption.2015", mapTitle="World Map for Government Corruption -2015",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="Government_Corruption.2016", mapTitle="World Map for Government Corruption -2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="Government_Corruption_Change", mapTitle="World Map for Change in Government Corruption 2015-2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')


