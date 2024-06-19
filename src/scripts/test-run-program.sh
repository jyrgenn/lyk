#!/bin/ksh

count=0

stdout () {
    count=$(($count + 1))
    echo o$count:"$@"
}

stderr() {
    count=$(($count + 1))
    echo e$count:"$@" 1>&2
}

stderr lala legt sich
sleep 0.1
stdout lulu liebt sich
sleep 0.1
stdout huhu hält sich
sleep 0.1
stderr momo müht sich
sleep 0.1
stdout gaga gibt sich
sleep 0.1
exit $1
