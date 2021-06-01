#!/bin/bash

current_date=`date +%Y%m%d`

cd ~/Library/Mobile\ Documents/com~apple~CloudDocs/Files/UTM

zip -r ${current_date}.zip ./*

mv ${current_date}.zip ~/OneDrive\ -\ University\ of\ Toronto/UTM/
