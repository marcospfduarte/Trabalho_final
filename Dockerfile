
FROM rocker/tidyverse:4.0.0

# Install packages needed 
RUN R -e "install.packages('geobr')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('tidyr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('RColorBrewer')"
RUN R -e "install.packages('gridExtra')"
RUN R -e "install.packages('hrbrthemes')"
RUN R -e "install.packages('viridis')"





#copy the necessary files from the folder into the image

COPY /codigos.R /codigos.R

COPY /banco.csv /banco.csv


#run the R Script

CMD Rscript /codigos.R