#!/bin/bash

echo "Starting OSP"
epmd -daemon
erl -smp auto -run osp -detached
echo "OSP started, telnet to the localhost management port (default 9876) to continue"
