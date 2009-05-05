#!/bin/bash

echo "Setting up OSP"
epmd -daemon
erl -smp auto -detached -pa ./ebin -run osp setup
