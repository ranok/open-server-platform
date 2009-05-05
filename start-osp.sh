#!/bin/bash

echo "Starting Open Server Platform"
echo "v0.4 (C) 2009 Jacob Torrey"
echo "http://www.openserverplatform.com"
epmd -daemon
erl -smp auto -detached -mnesia dir "mnesia" -boot osp_rel-0.4 -pa ./ebin
sleep 10
echo "Open Server Platform started"
echo "Telnet to localhost port 9876 or go to http://localhost:9877 to continue"
