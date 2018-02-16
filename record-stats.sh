#!/bin/sh

if [ $# -ne 2 ]; then
        echo " $0  arguments: <logdir> <executable>"
        exit 1
fi

echo " starting: "
echo " $0 $1 $2 ($#)"
date

#WATCHPROC=`basename $2`
# it is the service that we are searching
WATCHPROC=cardano-node
WATCHPID=`systemctl show ${WATCHPROC} -p MainPID --value`

if [ -z "${WATCHPID}" ]; then
        echo " No service ${WATCHPROC} found."
        exit 1
fi

echo "  watching service ${WATCHPROC} with PID ${WATCHPID}"
echo

SLEEPTIME=5
RECNAME1=statm
RECNAME2=stat
RECNAME3=io
PROC=/proc/$WATCHPID

while [ -e ${PROC}/${RECNAME1} ]; do
        NOW=`date +%s`
        { echo -n "${NOW} ${RECNAME1} " && cat ${PROC}/${RECNAME1}; }
        { echo -n "${NOW} ${RECNAME2} " && cat ${PROC}/${RECNAME2}; }
        { echo -n "${NOW} ${RECNAME3} " && cat ${PROC}/${RECNAME3} | cut -d " " -f 2 | tr '\n' ' ' && echo; }
        sleep $SLEEPTIME
done

echo
cat $PROC/status
date
echo " $0 done."
