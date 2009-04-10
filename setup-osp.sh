#!/bin/bash

echo "Setting up OSP"
erl -smp auto -pa ./ebin -detached -run osp setup
