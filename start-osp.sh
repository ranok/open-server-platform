#!/bin/bash

echo "Starting Open Server Platform"
echo "v0.4 (C) 2009 Jacob Torrey"
echo "http://www.openserverplatform.com"
epmd -daemon
erl -smp auto -detached -mnesia dir "mnesia" -boot osp_rel-0.4 -pa ./ebin
sleep 9
nc localhost -w 1 9876 > /dev/null
if [ $? -eq 1 ]; then
    echo "There was an error starting OSP"
    exit -1
fi
echo "Open Server Platform started"
echo "Telnet to localhost port 9876 or go to http://localhost:9877 to continue"
