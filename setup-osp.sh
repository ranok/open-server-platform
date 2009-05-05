#!/bin/bash

echo "Setting up OSP"
epmd -daemon
erl -smp auto -detached -pa ./ebin -mnesia dir "mnesia" -run osp setup
