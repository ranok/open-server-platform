#!/bin/bash

echo "Setting up OSP"
erl -smp auto -detached -run osp setup
