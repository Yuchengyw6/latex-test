# Latex GitHub Actions For ACHD weather report

This repository is adapted from the GitHub action for [compiling a LaTeX document](https://github.com/xu-cheng/latex-action).
Thanks a lot to [Cheng Xu](https://github.com/xu-cheng) for his repo https://github.com/xu-cheng/latex-action and thanks a lot to the instruction written by [David Haberthür](https://github.com/habi).

This by commit and push, you can compile the main.tex, the pdf report is in the [gh-pages](https://github.com/Yuchengyw6/latex-test/tree/gh-pages) branch, it is generated automatically. 

## Files
<img width="786" alt="image" src="https://user-images.githubusercontent.com/89940553/163876583-0bc90d73-3eec-4903-8ac0-cec3da0523f0.png">

The files with red squares are the most important files in this project.


## General Logic

Generally, this report is generated in two steps:

1, Data Collection

[job.R](https://github.com/Yuchengyw6/latex-test/blob/master/R/job.R) is the data scraping part of this project, the daily data we need is scraped from 6 different sources and automatically generate a .tex file in the [data-raw](https://github.com/Yuchengyw6/latex-test/tree/master/data-raw) folder.

2, Generate Latex report

[main.tex](https://github.com/Yuchengyw6/latex-test/blob/master/main.tex) is the main part of the report, it will be generated automatically everyday. 

## Workflow files

The workflow files here is controlling the schedules of the tasks. 

[main.yaml](https://github.com/Yuchengyw6/latex-test/blob/master/.github/workflows/main.yaml) is the workflow file for compiling the LaTeX file automatically.

[schedule-commit.yaml](https://github.com/Yuchengyw6/latex-test/blob/master/.github/workflows/schedule-commit.yaml) is the workflow file for the data scraping process.

Ideally you do not need to change anything inside these files.


## Possible Errors

1, Some websites sources we are using might be unfunctional in some specific days. If you receive an error message via email, and find that the file [data_X07.tex](https://github.com/Yuchengyw6/latex-test/blob/master/data-raw/data_X07.tex) did not update itself or some data is wrong, you may need to modify this file manually, details please refer to [example.tex](https://github.com/Yuchengyw6/latex-test/blob/master/data-raw/example.tex). After change the [data_X07.tex](https://github.com/Yuchengyw6/latex-test/blob/master/data-raw/data_X07.tex) and commit the changes manually, the github action will run again automatically, wait for a few minute and check whether [main.pdf](https://github.com/Yuchengyw6/latex-test/blob/gh-pages/main.pdf) is updated or not. 

2, If you find some github action tasks failed, you may want to navigate to the [Action Page](https://github.com/Yuchengyw6/latex-test/actions), you may find error massage for each task. For the data scraping tasks, it is normal to have some failures each day since some of the websites may not always be accesable, you only need need to care about whether [data_X07.tex](https://github.com/Yuchengyw6/latex-test/blob/master/data-raw/data_X07.tex) update it self or not. Ideally the latex report generating process would not fail, if it fails after changing the data manually, you need to be sure the format of the data is correct.

## Guidance of adding new functions

1, If you need to add new webscraping data or change the webscraping source into the report, firstly you need to add/change the webscraping process in [job.R](https://github.com/Yuchengyw6/latex-test/blob/master/R/job.R), the current webscraping processes are conducted by rvest package. After webscraping from the new source, you need to add new variables at end of the script(e.g.: New_Variables = paste("\\newcommand\\New_Variables_Name{",New_Variables_Data,"}",sep="") 
output = paste(output,New_Variables,sep="\n")). It should be in the same format like: 
<img width="594" alt="image" src="https://user-images.githubusercontent.com/89940553/163864052-64695084-8d70-4681-af18-ea5c939c5d6c.png">

2, After adding the new variables in the [job.R](https://github.com/Yuchengyw6/latex-test/blob/master/R/job.R), the new variables should appears in the new .tex file, you can use this variable in [main.tex](https://github.com/Yuchengyw6/latex-test/blob/master/main.tex) to add some new tables, etc.
