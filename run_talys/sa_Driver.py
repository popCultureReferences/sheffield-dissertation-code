from subprocess import call
import multiprocessing
import os
import pandas as pd

# Read in the Latin Hypercube Samples ofr the parameter settings
runs = pd.read_csv("test_samples.csv")
# rename with the tokens in the TALYS input template
runs.columns = ["$v1adjust$", "$v2adjust$", "$v3adjust$", "$v4adjust$", "$rvadjust$",
                "$avadjust$", "$rwadjust$", "$awadjust$", "$w1adjust$", "$w2adjust$",
                "$w3adjust$", "$w4adjust$", "$rvdadjust$", "$avdadjust$", "$rwdadjust$",
                "$awdadjust$", "$d1adjust$", "$d2adjust$", "$d3adjust$", "$vso1adjust$", 
                "$vso2adjust$","$wso1adjust$", "$wso2adjust$", "$rvsoadjust$", 
                "$avsoadjust$", "$rwsoadjust$", "$awsoadjust$", "$rcadjust$" ]
# unpack each row into a dictionary 
# with which to format the TALYS input
run_index = []
param_settings = []
for i in range( len(runs) ):
    run = runs.iloc[i].to_dict()
    run_index.append(i)
    param_settings.append(run)
# zip up with an index for each run    
run_info = tuple(zip( run_index, param_settings))     

basedir = os.getcwd()

def run_job(details):

    run_num = str(details[0])
    fnr = details[1]
    dirname =  "test_run_" + run_num
    call("mkdir {}".format(dirname),shell=True)
    call("cp energy_grid_design example.in {}".format(dirname),shell=True )
    os.chdir(dirname)
    print(os.getcwd())
    text = ""
    with open("example.in","r") as f:
        text = f.read()
        for token, value in fnr.items():
            text = text.replace(token, str(value) ) 
    outfile =  "talys.in"        
    with open(outfile,"w") as f:
        f.write(text)    
    call("~/talys/talys < talys.in > talys.out",shell=True)
    os.chdir(basedir)
    return 0
     
     
pool = multiprocessing.Pool(7)    
    
pool.map(run_job, run_info)    







