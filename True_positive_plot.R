#IN THIS SCRIPT YOU CAN PLOT ALL THE RESULTS FROM TRUE POSITIVE TESTS
#SUGGESTION: RUN THE CODE IN THE SAME FOLDER OF THE .TXT FILES

#Call of all the libraries
library("ggplot2")
library("tidyr")
library("dplyr")
library("ggpubr")

#With this line of code is possible to tell to the command line that the user can store some arguments to the Rscript
args = commandArgs(trailingOnly=TRUE)

#Here I define the function funplot, this function need as arguments the files where there are the results from our tests
#each file is the argument of this function.
funplot <- function(file){
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
  #Now there is the main part of the function with the creation of all the graphs
  plot <- read.table(file, sep = "\t", header = TRUE)
  pval <- ggplot(plot, aes(x=FREQ, y=PVAL.LOG10, group = FREQ)) +
    geom_boxplot() +
    ylim(0,300)
  
  bonf <- ggplot(plot, aes(x=FREQ, y=LOG10BONF, group = FREQ)) +
    geom_boxplot() +
    ylim(0,300)

  hit <- ggplot(plot, aes(x=FREQ, y=HIT, group = FREQ)) +
    geom_boxplot() +
    ylim(0,300) 
  
  #Here i make one single image from three different plots
  figure <- ggarrange(hit,
                      ggarrange(bonf, pval,ncol = 2, labels = c( "Bonferroni p-value", "Normal p-value")),
                      nrow = 2,
                      labels = c("Hits position"))
  if (grepl("_tot", file, fixed=TRUE)){
    #And with annotate_figure I insert title to the plot
    firstplot <- annotate_figure(figure, top = text_grob(paste(file, "\nAll"), face = "bold", size = 14))
    #The plot is saved inside the right folder
    ggsave(file.path(name, filename, "comparison.png"), width = 465, height = 465, units='mm', dpi = 600)
    #bonferroni <- annotate_figure(bonf,top = text_grob(paste(file, "\nAll"), face = "bold", size = 14))
    #ggsave(file.path(name, filename, "bonferroni.png"), width = 465, height = 465, units='mm', dpi = 600)
    
  } else{
    firstplot <- annotate_figure(figure, top = text_grob(paste(file,"\nBest pvalue for cycle"), face = "bold", size = 14))
    ggsave(file.path(name, filename, "comparison.png"), width = 465, height = 465, units='mm', dpi = 600)
    #bonferroni <- annotate_figure(bonf,top = text_grob(paste(file, "\nAll"), face = "bold", size = 14))
    #ggsave(file.path(name, filename, "bonferroni.png"), width = 465, height = 465, units='mm', dpi = 600)
  
  }
  if (grepl("_tot", file, fixed=TRUE)){ 
    
    freq <- ggplot(plot, aes(x = HIT, y = PVAL.LOG10, group = HIT)) + geom_boxplot(outlier.size = 0.2) + ylim(0, 300) + xlim(0,300) + facet_wrap(vars(FREQ), scales = "free")
    endplot <- annotate_figure(freq, top = text_grob(paste(file, "\nAll with Bonferroni"), face = "bold", size = 14))
    ggsave(file.path(name, filename, "frequences.png"), width = 800, height = 465, units='mm', dpi = 600)
    
    figure2bonf <- ggplot(plot, aes(x = HIT, y = LOG10BONF, group = HIT)) + geom_boxplot(outlier.size = 0.2) + ylim(0, 300) + xlim(0,300) + facet_wrap(vars(FREQ), scales = "free")
    endplot2 <- annotate_figure(figure2bonf, top = text_grob(paste(file, "\nAll with Bonferroni"), face = "bold", size = 14))
    ggsave(file.path(name, filename, "frequencesBonferroni.png"), width = 800, height = 465, units='mm', dpi = 600)
    
    }
}


for (i in 1:length(args[])){
  file <- args[i]
  funplot(file)
}


