#-----------------------------------------
# commands for scp'ing sce and cent over
#-----------------------------------------
cd ~/Dropbox/R/haltmle.sim/sandbox
scp  cent.R sce.sh dbenkese@snail.fhcrc.org:~/haltmle.sim
# scp mergeOnly.R subMerge.sh dbenkese@snail.fhcrc.org:~/haltmle.sim

ssh dbenkese@snail.fhcrc.org
cd haltmle.sim
# scp mergeOnly.R subMerge.sh dbenkese@rhino.fhcrc.org:~/haltmle.sim
scp cent.R sce.sh dbenkese@rhino.fhcrc.org:~/haltmle.sim

ssh dbenkese@rhino.fhcrc.org
cd haltmle.sim
./sce.sh ./cent.R run_V2_3

#-----------------------------------------
# commands for scp'ing mergeOnly
#-----------------------------------------
cd ~/Dropbox/R/haltmle.sim/tests
scp mergeOnly.R subMerge.sh dbenkese@snail.fhcrc.org:~/haltmle.sim

ssh dbenkese@snail.fhcrc.org
cd haltmle.sim
scp mergeOnly.R subMerge.sh dbenkese@rhino.fhcrc.org:~/haltmle.sim

ssh dbenkese@rhino.fhcrc.org
cd haltmle.sim
./subMerge.sh

#-----------------------------------------
# commands to get into rhino and load R
#-----------------------------------------
ssh dbenkese@snail.fhcrc.org
ssh dbenkese@rhino.fhcrc.org
 # enter password
ml R/3.2.2
R

#-----------------------------------------
# scp results from rhino to local machine
#-----------------------------------------
# from rhino
cd haltmle.sim/out
scp allOut.RData dbenkese@snail.fhcrc.org:~/haltmle.sim
	# enter snail password
 	# ctrl + shift + t to open up new term
# scp to snail
cd ~/Dropbox/R/haltmle.sim/results
scp dbenkese@snail.fhcrc.org:~/haltmle.sim/allOut.RData . 

#-----------------------------------------
# misc commands 
#-----------------------------------------
# squeue -u dbenkese
# scancel `seq 51239645 51239655`
