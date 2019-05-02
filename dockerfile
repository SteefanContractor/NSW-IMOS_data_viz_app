# start from the rocker/shiny-verse image
# it include Ubuntu, R 3.5.3, shiny server and tidyverse
# this is the latest shiny-verse image
# written by Steefan Contractor
FROM rocker/shiny-verse

# image metadata
LABEL maintainer="s.contractor@unsw.edu.au"

# install system libraries
RUN apt-get update && apt-get install -y \
	systemd \
	cron \
	vim 

# install the netcdf libraries
RUN apt-get install -y libnetcdf-dev libudunits2-dev

# install R packages
RUN R -e "install.packages(c('ncdf4','zoo','lubridate','readODS','plotly','shinydashboard','leaflet'))"

# Copy the shiny app to /srv/shiny-server/
COPY Simple_shiny_Climatology_dashboard_app /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]

# build as follows
# docker build –t unswoceanography/shiny-verse:1.0.0 .
# run as follows
# docker run -p 80:3838 --rm -d unswoceanography/shiny-verse:1.0.0
