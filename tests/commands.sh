# commands
# cd ~/Dropbox/UW\ Classes/Consulting/Nita\ Khandelwal/Methods\ paper/healthCosts
# scp healthcosts.R dbenkese@snail.fhcrc.org:~/hc
cd ~/Dropbox/R/haltmle.sim/tests
scp  cent.R sce.sh dbenkese@snail.fhcrc.org:~/haltmle.sim

ssh dbenkese@snail.fhcrc.org
# scp ~/hc/healthcosts.R dbenkese@rhino.fhcrc.org:~/hc/sim/
scp ~/haltmle.sim/sce.sh ~/haltmle.sim/cent.R  dbenkese@rhino.fhcrc.org:~/haltmle.sim

ssh dbenkese@rhino.fhcrc.org
cd haltmle.sim
chmod +x sce* cent*
./sce.sh ./cent.R run13

# scancel `seq 4888000 4890000`

