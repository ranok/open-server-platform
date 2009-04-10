#!/bin/bash

echo "Starting OSP"
epmd -daemon
erl -smp auto -detached -boot osp_rel-0.4
echo "OSP started, telnet to localhost port 9876 to continue"
