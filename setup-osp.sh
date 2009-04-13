#!/bin/bash

echo "Setting up OSP"
erl -smp auto -detached -pa ./ebin -run osp setup
