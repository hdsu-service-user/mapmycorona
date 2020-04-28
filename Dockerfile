FROM rocker/shiny-verse:3.6.3
#FROM rocker/rstudio

LABEL maintainer="Andres Quintero a.quintero@dkfz-heidelberg.de"

#RUN install2.r --error \
#    devtools


# from the tensorflow dockerfile: 
# https://github.com/tensorflow/tensorflow/blob/master/tensorflow/tools/dockerfiles/dockerfiles/cpu-jupyter.Dockerfile



# See http://bugs.python.org/issue19846
ENV LANG C.UTF-8

# Download and install library
RUN R -e "install.packages(c('leaflet', 'shinydashboard', 'shinyWidgets', 'shinyjs', 'RColorBrewer', 'scales'))"

# copy the app to the image COPY shinyapps /srv/shiny-server/
COPY mapmycorona/ /srv/shiny-server/



RUN chmod -R 755 /srv/shiny-server/


