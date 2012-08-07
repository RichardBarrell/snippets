#/bin/sh
# Join an existing screen session or start a new one.
#exec 1>~/scsc.stdout
#exec 2>~/scsc.stderr
if [ `screen -ls | wc -l` -eq 2 ]
        then screen -m;
        else exec screen -x
fi
