# InvestmentCostSpecificationsRevisited
Code related to "Investment Cost Specification Revisited"

Preparations
1) Download GAMZ.7z.
2) Unpack GAMS.7z.
3) Install GAMS.
4) Get CPLEX or GUROBI solver license (e.g., via https://www.gurobi.com/academia/academic-program-and-licenses/) or any other one.

Database
5) Contact mier@ifo.de to receive two excel files (database and timeseries) that you need to place in "database/data". 
   Alternatively, consult https://zenodo.org/records/10143471 to obtain the file with timeseries (then only the other excel file needs to get demanded from mier@ifo.de). 
   There, you can also find the compiled gdx-database (setpar...). You can place this database in "database" and skip to 8) to run the model code. 

6) Navigate to "database", open "choose.bat" (or navigate to the folder in terminal/cmd), and run it via terminal/cmd (by writing "choose" and enter).
		
   Wait some time (weighting and final decision writing takes some time).

7) Navigate to "database", open "data.bat", and run it via terminal.

   Wait some time (importing database and timeseries takes some time and also processing timeseries as well as creating final database).

Running the model
8) Navigate to main folder, open "run", and run it via terminal (run) to obtain all except uncertainty runs. 

   Wait some time (all runs need some days, depending on the engine you run it and the respective solver; there are solver option files for CPLEX and GUROBI; program uses GUROBI per default, to change solver you need to change hard coded "GUROBI" in "euregen_v7d.gms"; best: search and replace).

Uncertainty analysis
9) Navigate to main folder, open "run_random", and run it via terminal (run_random).
10) Open, e.g., "run_draw1_01.bat", and run it via terminal (run_draw1_01).

    Wait some time (all runs take multiple days to weeks, depending on the engine you run it and the respective solver; try to run uncertainty runs in paraellel but be aware that memory is the limiting factor).

Ex-post analysis
11) Find output files in "output_dynamic", find condensed output in "report_verysimple_dynamic" (via "euregen_rpt_3b.gms"), and some excel output in "excel_dynamic".
12) There are some excel files to produce figures and tables. The sheets are normally linked with excel output (this needs to be done manually in the version provided).
