[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://img.shields.io/badge/license-MIT-blue.svg)


# NYCEM Weather Dashboard

This web application provides up to date meteorological information for New York City.

<!-- ![overview](/assets/map.png) -->

## Running the application

The application is currently hosted at http://18.221.238.149:3838/NYCEM-Weather-Dashboard/. It can also be run locally by running the `main.R` file after installing all dependencies.

## Documentation

### Site Design

The site is built in R using [shiny](https://shiny.posit.co/) and [shinydashboard](https://rstudio.github.io/shinydashboard/). The map was constructed using [leaflet](https://rstudio.github.io/leaflet/) and the table was made with formattable.

### Hosting

The application is currently hosted on a NYCEM AWS EC2 instance named `Weather_Dashboard`. To update the dashboard, first log into the EC2 instance using the IP address provided in the AWS console. You will need to use the private key. Next navigate to `/srv/shiny-dashboard/` and modify the files in the `NYCEM-Weather-Dashboard` directory. The application will automatically update.

Log files can be found in the `/var/log/shiny-server` directory. Logs can be turned off by modifying the configuration file `/etc/shiny-server/shiny-server.conf`. Helpful guides on hosting the app can be found [here](https://www.charlesbordet.com/en/guide-shiny-aws/#how-to-install-shiny-server) and [here](https://towardsdatascience.com/how-to-host-a-r-shiny-app-on-aws-cloud-in-7-simple-steps-5595e7885722).

### Updating the Alert Boxes and Notes

To update the values of the four alert boxes at the bottom of the dashboard edit the `output$tempWarningText`, `output$windWarningText`, and `output$coastalWarningText` functions in the `server.R` file. A function needs to be created for the precipitation section. The notes can be updating by editing the paragraph on line 47 of `ui.R`.

## TODOs

### Frontend

Additional layers need to be added to the map. Text needs to be added to the precipitation warning box.

### Backend

The dashboard loads slowly because it queries the API every time it is loaded. The current AWS design consists of a EC2 instance that the performs all aspects of the dashboard.

![design](/assets/one.png)

A more sophisticated design could help reduce latency. One possible design would involve a Lambda that calls the API intermittently and stores it in a S3 bucket. When the EC2 is invoked by a user it would then use the data stored in the S3 bucket instead of making an API call.

![design](/assets/two.png)

## Acknowledgements

![logos](/assets/comb.png)

This project was a collaboration between the Cornell University Department of Statistics and Data Science, Cornell Tech's Public Interest Tech Initiative, and the New York City Department of Emergency Management. Funding was provided in part by the Siegel Family Foundation.

## Contact

For questions or comments contact Zachary James, zj37@cornell.edu or Joshua Rapp jrapp@oem.nyc.gov.

## Disclaimer

The information provided by the weather dashboard is for general information purposes only. The organizations or individuals involved in creating this software assume no responsibility for errors or omissions in the contents of the service.
