# Elexion

## Introduction
This project aims to analyze and study the relationship between various demographic, geographic, and historical factors and the 2016 US presidential election. 

## Summary
We include the report of our study in the document folder. The keynote folder stores the presentation on this project. The report in particular, contains a lot of interactive visualization; so to enhance the reading experience, we setup a local server that serves the documents and the apps that power it. Please refer ot the report to get a grasp of our study and results. There are also more information on how to use the codes and the data sets used. The main data set used for this study is taken from [USA.county.data](https://github.com/Deleetdk/USA.county.data); we have gone through a rigorous process of updating and cleaning data to serve our purpose of study. The code used to build up can be found in the document folder.

## Disclaimer
Apprently, the election data provided by [USA.county.data](https://github.com/Deleetdk/USA.county.data) is scapred from the New York Times, who bought the data set from Associated Press. Since this might have complicated legal issue, we wish to express that we are not responsible for the election data as we are just taking it _as is_ from the noted source. Also, since this is data-mining project, we wish to show the step through which we clean and transform the data set so that people can reproduce the results that we obtained, as such, it is almost impossible to not have the election data anywhere in this repository. For healthcare and human-development data sets, we also have to include them in our project because we use them for data preparation step. As such, we just really want to express that we use this data set merely for study purpose and have NO intention to redistribute, nor to commercialize it in anyway. So please __do not cite us__ as the source of these data sets if you choose to use them yourselves. Please refer to the original sources of these data files as we have detailed in [main_source.csv](data/misc/main_source.csv) and [source.csv](data/misc/source.csv).  

This project was built as part of our class project, all conclusion obtained in the paper are based on data-mining, figures and numbers. We do not want the results to be used in anyway to favor any party, i.e. we take a political-free and neutral stance while working on this project. Also, we deem ourselves as lacking both political understanding and data-mining related knowledge so the results obtained might just be taken lightly and should not be cited in any publications.

## Usage
To start the local servers serving the keynote, report, and Shiny apps, run the bash script in the code folder. To adjust the ports used for each app, see the configs.R file.

```
  ./start.sh
```

To make a full run of the project, uses the automated R script. Refer to the configs file to see available settings. _Beware that the script might take a long time to run (7 ~ 8 hours on my machine)._ First start with opening R shell (you can also run from bash using R CMD BATCH).

```
  R
```

Then source the automated script

```
  source("auto.R")
```

Potentially, while running either of these task, you will need to install R libraries and packages. To do this, you can preemptively run the utils.R and the app.R scripts. It is likely that you will be prompted for packages installations. The utils.R will install packages used by main body of the scripts

```
  source("utils.R")
```

app.R will install packages used for the Shiny apps. 

```
  source("app.R")
```

## Notice

It has been brought to my attention that the script [does not work so well](https://github.com/blacksteed232/elexion/issues/1) with Windows. The script is just a collection of bash commands to essentially source the R files in background--there are a few of them, each of which is designated for one Shiny app, so they will run until you kill them. If you find yourself in the same situation, try to change the script file or just go bac kto the good old _source()_ command in native R shell. I suggest the first thing to try out is just

```
  R
  source("start.R")
```

To make sure that the result that we obtained are reproducible, we set seed for each script files where randomization is needed. This can be turned off using the config file.

Notice that we use rgl and ShinyRGL packages for some of the visualizations. This requires XQuartz for Mac OSX and X11 for Linux. We have experienced inconsistent results on different Linux distros. As pointed out in [this](https://github.com/trestletech/shinyRGL/issues/5) thread, you might need to have OpenGL installed properly and XVFB running since we are rendering the plots in the background using headless server mode.

We recommend using Chrome to view the report for smoother animations and visual effects. Also, since Keynote works best on Mac OSX platform, viewing the keynote on Mac is preferred; but because the keynote has been converted to HTML, it should work well at least 90% of the time on other platforms. The default addresses for these documents are

```
  Keynote: http://localhost:2301
  Report: http://localhost:2302
  Keynote Visualization Support: http://localhost:2303
```
