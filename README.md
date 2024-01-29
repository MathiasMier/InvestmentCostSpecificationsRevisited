# InvestmentCostSpecificationsRevisited
Code related to "Investment Cost Specification Revisited"

Preparations
1) Download GAMZ.7z.
2) Unpack GAMS.7z.
3) Install GAMS.
4) Get CPLEX or GUROBI solver license (e.g., via https://www.gurobi.com/academia/academic-program-and-licenses/) or any other one.

Database
5) Consult https://zenodo.org/records/10579902. There, you can afind the compiled gdx-databases (setpar...). You can place these databases in "database". 

Running the model
6) Navigate to main folder, open "run", and run it via terminal (run). 

Wait some time (all runs need some time, depending on the engine you run it and the respective solver; there are solver option files for CPLEX and GUROBI; program uses GUROBI per default, to change solver you need to change hard coded "GUROBI" in "euregen.gms"; best: search and replace; check option file for number of threads the respective engine is using to solve the program).

Ex-post analysis
7) Find output files in "output_dynamic", find condensed output in "report_verysimple_dynamic" (via "euregen_rpt.gms"), and some excel output in "excel_dynamic".
8) There are some excel files to produce figures and tables. The sheets are linked with "Diagram..."-excel files.
