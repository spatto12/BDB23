# Big Data Bowl 2023
This Github repository houses code I used for the model and visualizations along with the images that are not generated in the Kaggle notebook. I used the provided data from Kaggle along with data from the libraries; nflfastR and nflreadr. 

All of the code files are still set to my personal directories. bdb serves as the initial data cleaning script. The dataframe produced with this file is used in a majority of the other scripts.

The cluster script generates the histogram and path plot in the Kaggle notebook. The purpose of this file is to cluster pass rush paths for use in the binary logistic regression. The leverage script calculates the leverage for the rusher and nearest blocker. 

The pocket script utilizes the functions file to generate a convex hull for the offense. Further analysis is done to see which pass rushers intersect the pocket throughout the duration of the play.

Two scripts that also go hand in hand are; play_animation and gg_field (not authored by me). They animate plays on a horizontal football field. 

Finally the app file is included to see the logic behind the Shiny app.
