#!/bin/bash

echo "Starting OSP"
epmd -daemon
erl -smp auto -run osp -detached
echo "OSP started, telnet to localhost port 9876 to continue"
