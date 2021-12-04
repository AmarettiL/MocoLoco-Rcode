#THIS PROGRAM IS USED TO HAVE A CLOSE UP ON IMPLANTING FREQUENCES FROM 6 TO 9%
#SUGGESTION: RUN THE CODE IN THE SAME FOLDER OF .TXT FILES

library("ggplot2")
library("ggpubr")
library("tidyverse")


closeup <- function(file){
  #Using gsub I extract from the file only the name of the TF (for example from Hit_NFY_R_c1000_f0.01_tot.txt file the name is NFY)
  name <- gsub("Hit*[_]([^_]+)[_].*", "\\1", file)
  #Now I extract only the part of filename before .txt, this will be the name of the sub-folders inside the main folder declared as "name".
  filename <- substr(file,1,nchar(file)-4)
  #Control, if the folder exist the user is informed 
  if (file.exists(name)) {
    
    cat("\nThe folder already exists\n")
    
  } else {
    #If the folder doesn't exist is created
    dir.create(name)
    
  }
  #Same thing with the sub-folders
  if (file.exists(file.path(name, filename))) {
    
    cat("\nThe folder already exists\n")
    
  } else {
    
    dir.create(file.path(name, filename))
    
  }
  
  data <- read.table(file, sep = "\t", header = TRUE)
  freq <- ggplot(data, aes(x = HIT, y = PVAL.LOG10, group = HIT)) + 
  #geom_boxplot(outlier.size = 0.4) + 
  geom_jitter(size=0.2) +
  ylim(0, 150) + xlim(0,300) + facet_wrap(vars(FREQ), scales = "free")
  
  endplot <- annotate_figure(freq, top = text_grob(paste(file), face = "bold", size = 14))
  ggsave(file.path(name, filename, "frequences.png"), width = 1000, height = 465, units='mm', dpi = 600)
  
  data <- read.table(file, sep = "\t", header = TRUE)
  freq <- ggplot(data, aes(x = HIT, y = LOG10BONF, group = HIT)) + 
    #geom_boxplot(outlier.size = 0.4) + 
    geom_jitter(size=0.2) +
    ylim(0, 150) + xlim(0,300) + facet_wrap(vars(FREQ), scales = "free")
  
  endplot <- annotate_figure(freq, top = text_grob(paste(file), face = "bold", size = 14))
  ggsave(file.path(name, filename, "Bonferronifrequences.png"), width = 1000, height = 465, units='mm', dpi = 600)
}
#With this line of code is possible to tell to the command line that the user can store some arguments to the Rscript
args = commandArgs(trailingOnly=TRUE)

for (i in 1:length(args[])){
  file <- args[i]
  closeup(file)
}

