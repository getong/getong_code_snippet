#/bin/bash

while :
do
        thread_num=`ps -e |grep -i $1 | wc -l`
        if [ $thread_num -eq 0 ]; then
                date >> shutdowntime.log
                shutdown -h now
                exit
        else
                echo "Sleeping $2 second..."
                sleep $2
        fi
done
