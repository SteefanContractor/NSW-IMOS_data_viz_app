# start from the rocker/shiny-verse image
# it include Ubuntu, R 3.5.3, shiny server and tidyverse
# this is the latest shiny-verse image
# written by Steefan Contractor
FROM rocker/shiny-verse

# image metadata
LABEL maintainer="s.contractor@unsw.edu.au"

# install system libraries
RUN apt-get update && apt-get install -y \
	sudo \
	systemd \
	cron \
	vim \
	cdo

# create a root user ubuntu
RUN useradd -rm -d /home/ubuntu -s /bin/bash -g root -G sudo -u 1000 ubuntu
# let user ubuntu run all commands without a password
RUN echo "ubuntu ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers 
# switch to user ubuntu and working dir to home/ubuntu
USER ubuntu
WORKDIR /home/ubuntu

# Copy git repo data dir into the data dir on container
COPY Simple_shiny_Climatology_dashboard_app/data /srv/shiny-server/data

# copy getData.sh script to home dir
COPY getData.sh /srv/shiny-server/

# make getData.sh executable
RUN sudo chmod +x /srv/shiny-server/getData.sh

# make a data directory for downloaded realtime SST data
#RUN mkdir /home/ubuntu/data

# run getData.sh in /srv/shiny-server
RUN sudo /srv/shiny-server/getData.sh

# copy preprocessing scripts
COPY Simple_shiny_Climatology_dashboard_app/preprocessing.R /srv/shiny-server/
COPY Simple_shiny_Climatology_dashboard_app/processRealtimeSSTdata.R /srv/shiny-server/

# run the preprocessing scripts 
RUN sudo Rscript /srv/shiny-server/preprocessing.R
RUN sudo Rscript /srv/shiny-server/processRealtimeSSTdata.R

# Copy hello-cron file to the cron.d directory
COPY cron-job /etc/cron.d/cron-job

# Give execution rights on the cron job
RUN sudo chmod 0644 /etc/cron.d/cron-job

# Apply cron job
RUN sudo crontab /etc/cron.d/cron-job

# Create the log file to be able to run tail
# RUN touch /var/log/cron.log

# install the netcdf libraries
RUN sudo apt-get install -y libnetcdf-dev libudunits2-dev

# install R packages
RUN sudo R -e "install.packages(c('ncdf4','zoo','lubridate','readODS','plotly','shinydashboard','leaflet'))"

# Copy the shiny app to /srv/shiny-server/
COPY Simple_shiny_Climatology_dashboard_app /srv/shiny-server/

# Give R read/write permissions in directory
RUN sudo chown -R shiny:shiny /srv/shiny-server

# Run the command on container startup
CMD sudo cron && sudo /usr/bin/shiny-server.sh 

#ENTRYPOINT ["/usr/bin/shiny-server.sh"]

#EXPOSE 3838

#COPY shiny-server.sh /usr/bin/shiny-server.sh

#CMD ["/usr/bin/shiny-server.sh"]

# build as follows
# docker build –t unswoceanography/shiny-verse:1.0.0 
# run as follows
# docker run -p 80:3838 --rm -d unswoceanography/shiny-verse:1.0.0
# docker exec -it 6bf /bin/bash
