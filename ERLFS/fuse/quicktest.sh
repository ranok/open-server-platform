#!/bin/bash
DIR=testdir123
sudo killall erlfs
mkdir ${DIR}
sudo ./erlfs ${DIR}
echo Mounted Test Directory
