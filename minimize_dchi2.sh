#!/bin/bash
if [[ "$1" == "-h" ]]; then
    echo ""
    echo "Usage: minimize_dchi2.sh [run_name] [run_mode] [mail]"
    echo ""
    exit
fi
maindir=`cat maindir.txt`
selfdir=$(cd $(dirname $0);pwd)

run=$1
mail=$2

cluster=kekcc
jobsys=bsub
que=s
rm -rf rslt_$run
infile=params.card
min_CP=0
max_CP=359
step_CP=180

i=1
CP=$min_CP
xx_CP=`echo "$CP > $max_CP" | bc`
while [ $xx_CP -eq 0 ];do
    dCP[$i]=$CP
    jobname="mindchi2"$RANDOM
    ${maindir}/submit_job.sh $jobsys $que $i $jobname "sed -e 's/ fdCP .*/ fdCP   ${dCP[$i]}/' ../$infile > $infile; \
 ${maindir}/run.sh run 0 0 0"
    i=`expr $i + 1`
    CP=`echo "scale=5; $CP + $step_CP" | bc`
    xx_CP=`echo "$CP > $max_CP" | bc` 
done
n=$i
${maindir}/monitor

file1=CPscan.dat
rm -rf $file1
touch $file1
dchi2min=1000
i=1
while [ $i -lt $n ];do
    dchi2[$i]=`cat par_${i}/rslt_run/data/dchi2.dat`
    echo ${dCP[$i]} ${dchi2[$i]} >> $file1
    comp=`echo "${dchi2[$i]}<$dchi2min" | bc`
    if [ $comp -eq 1 ];then
	dchi2min=${dchi2[$i]}
	index=$i
    fi
    i=`expr $i + 1`
done

i=1
while [ $i -lt $n ];do
    if [ $index -eq $i ];then
	mv par_${i}/rslt_run rslt_$run
    fi
    i=`expr $i + 1`
done

mv $file1 rslt_$run/.
rm -rf tmp
rm -rf par_*

if [ $mail -eq 1 ]; then
    if [ $cluster == "kekcc" ]; then
	bsub -q e -J $run -u takaesu@post.kek.jp nulljob.sh >/dev/null 2>&1
    else
	echo "Notification mail cannot be send from this cluster system. Exting..."
	echo " "
	exit
    fi
fi