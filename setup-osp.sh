#!/bin/bash

echo "Setting up OSP"
epmd -daemon
erl -smp auto -pa ./ebin -detached -run osp setup
