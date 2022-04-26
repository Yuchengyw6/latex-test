# Repository for the Allegheny County Health Department Air Quality and Dispersion Report's Automated Process

This repository is adapted from the GitHub action for [compiling a LaTeX document](https://github.com/xu-cheng/latex-action). It was developed by a team from The Master of Statistical Practice program at Carnegie Mellon University as a part of a capstone project for the Allegheny County Health Department. The repository belongs to ACHD, so any questions about this repository should be sent to ACHD.AirQuality@gmail.com. However, future developers for this process can contact the original team: Clare Cruz (cbrightly1@gmail.com),  Naijia Liu (liunj16@gmail.com), Yucheng Wang(ychwang0331@gmail.com), and Kevin Yang (kjy110498@gmail.com). 

Thanks a lot to [Cheng Xu](https://github.com/xu-cheng) for his repo https://github.com/xu-cheng/latex-action and thanks a lot to the instruction written by [David Haberthür](https://github.com/habi).

#### Please note that anything in this repository can be updated by 'commits' including the main.tex and the **pdf report will be automatically generated again** [gh-pages](https://github.com/ACHDAirQuality/ACHD_Report_Generate/tree/gh-pages). 

## Table of Contents
* [Files](#files)
* [General Logic](#general-logic)
* [Workflow files](#workflow-files)
* [Possible Errors](#possible-errors)
* [Guidance for adding new functions](#guidance-for-adding-new-functions)
* [Resources for LaTex](#resources-for-latex)

## Files
<img width="786" alt="image" src="https://user-images.githubusercontent.com/89940553/163876583-0bc90d73-3eec-4903-8ac0-cec3da0523f0.png">

The files with red squares are the most important files in this project.


## General Logic

Generally, this report is generated in two steps:

1. **Data Collection**

> [job.R](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/R/job.R) is the data scraping part of this project, the daily data we need is scraped from 6 different sources and automatically generate a .tex file in the [data-raw](https://github.com/ACHDAirQuality/ACHD_Report_Generate/tree/master/data-raw) folder.

2. **Generate Latex Report**

> [main.tex](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/main.tex) is the main part of the report, it will be generated automatically everyday. The final report is under the other branch called [gh-pages](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/gh-pages/main.pdf)

## Workflow files

The workflow files here is controlling the schedules of the tasks. 

- [main.yaml](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/.github/workflows/main.yaml) is the workflow file for compiling the LaTeX file automatically.
- [schedule-commit.yaml](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/.github/workflows/schedule-commit.yaml) is the workflow file for the data scraping process.

**Ideally you do not need to change anything inside these files.**


## Possible Errors

1. I**ncorrect / Missing Data*

> Some websites sources we are using might be non-functional on some specific days. If you receive an error message via email, and find that the file [data_X07.tex](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/data-raw/data_X07.tex) did not update itself or some data is wrong, you may need to modify this file manually, details please refer to [example.tex](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/data-raw/example.tex). 
> If you change the [data_X07.tex](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/data-raw/data_X07.tex) and commit the changes manually, the github action will run again automatically, wait for a few minute and check whether [main.pdf](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/gh-pages/main.pdf) is updated or not.

For example, one day you find that the data is not updated, you should firstly check the documentation in [example.tex](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/data-raw/example.tex), and change the value in [data_X07.tex](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/data-raw/data_X07.tex) manually.

Firstly, click the little pen on the top right corner<img width="1288" alt="image" src="https://user-images.githubusercontent.com/89940553/163880549-e02f264c-fe62-408c-8c7d-bd5a74ee998d.png">

Then, change the values inside the {} for each variables<img width="1222" alt="image" src="https://user-images.githubusercontent.com/89940553/163880635-f98b339f-e0f9-49bd-a404-ce766af429f4.png">

After that, commit changes, <img width="1086" alt="image" src="https://user-images.githubusercontent.com/89940553/163880859-1e58c689-0b2a-47fd-b5f5-f5d6b8c44251.png">

After the changes are commited, in [Github Action](https://github.com/ACHDAirQuality/ACHD_Report_Generate/latex-test/actions), you can see the new tasks.<img width="1317" alt="image" src="https://user-images.githubusercontent.com/89940553/163880923-90bce789-3200-4cd0-a6fe-10fb60deef7d.png">

After the tasks complete, you have your report updated, <img width="925" alt="image" src="https://user-images.githubusercontent.com/89940553/163881228-85c6dec9-77f6-4c1a-963b-599a4c6ce064.png">




2. **Failed Actions**

> If you see that some github action tasks have failed, you may want to navigate to the [Action Page](https://github.com/ACHDAirQuality/ACHD_Report_Generate/actions), where you can find an error message for each task. For the data scraping tasks, it is normal to have some failures each day since some of the websites may not always be accesable, you only need need to care about whether [data_X07.tex](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/data-raw/data_X07.tex) update it self or not. Ideally the latex report generating process would not fail, if it fails after changing the data manually, you need to be sure the format of the data is correct.

The error message could be found as follow, firstly click on [Github Action](https://github.com/ACHDAirQuality/ACHD_Report_Generate/actions) and navigate to the tasks that failed.
<img width="905" alt="image" src="https://user-images.githubusercontent.com/89940553/163881570-754b14b6-6f1b-46db-ac0d-6a2922f322c7.png">

<img width="558" alt="image" src="https://user-images.githubusercontent.com/89940553/163881714-705c34c5-b412-47c2-8c05-c6a28be8ed8c.png">
<img width="389" alt="image" src="https://user-images.githubusercontent.com/89940553/163881741-f6c2c318-8176-40fe-8d9c-29270a08b308.png">
<img width="669" alt="image" src="https://user-images.githubusercontent.com/89940553/163881800-4a303ead-dd7c-40f8-9cf0-ca8fa349880b.png">
If the error messages are related to connections, for example "port 443", 503, 504, then you should check whether [data_X07.tex](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/data-raw/data_X07.tex) is updated or not, if it is updated normally, then in most of the case, the report would be fine.

## Guidance for Adding New Functionality

1. **Adding a New Data Source**

> If you need to add a new data source or change the webscraping source in the report, firstly you need to add/change the webscraping process in [job.R](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/R/job.R), the current webscraping processes are conducted by rvest package. After webscraping from the new source, you need to add new variables at end of the script(e.g.: New_Variables = paste("\\newcommand\\New_Variables_Name{",New_Variables_Data,"}",sep="") 
output = paste(output,New_Variables,sep="\n")). It should be in the same format like: 
<img width="594" alt="image" src="https://user-images.githubusercontent.com/89940553/163864052-64695084-8d70-4681-af18-ea5c939c5d6c.png">

So for example, if you want to add a new variable with name "NewVariable", you should firstly claim this variable, and then add it to the .tex using the method below.
<img width="767" alt="image" src="https://user-images.githubusercontent.com/89940553/163883442-b5043b29-6e5b-4abe-9f88-c753d1a83395.png">


2. **Adding Information to the Report**

> If you would like to add new information to the report (either through an existing or new data source) then you need to add a variable that can be called by the LateX report. To do this, you first need to add the new variable in the [job.R](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/R/job.R) file. Then,  the new variables should appears in the new .tex file, you can use this variable in [main.tex](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/main.tex) to add some new tables, etc.

For example, you added the NewVariable to the .tex file then, you can use this variable in [main.tex](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/main.tex) like
<img width="1126" alt="image" src="https://user-images.githubusercontent.com/89940553/163883322-599f909d-e334-4f67-8aba-5180fe6bd42d.png">.
Please make sure the [data_X07.tex](https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/data-raw/data_X07.tex) is updated, and the NewVariable is inside, or errors might occur.

## Resources for LaTex

For the "Air Quality Forecast and Dispersion Outlook" report, we are using the template from https://www.overleaf.com/latex/templates/uq-beamerposter-template/svbpbndqdpqv.

If you need more information and instrucitons on the Latex writing, it would be helpful to refer to the document provided by Overleaf: https://www.overleaf.com/learn.

And, for more specific LaTex using in our report, please refer to https://github.com/ACHDAirQuality/ACHD_Report_Generate/blob/master/main.tex file, detailed comments and documentations are included.


