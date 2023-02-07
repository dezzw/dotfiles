#!/bin/bash

yabai -m query --spaces | jq -r '.[] | select(."has-focus" == true) | .index'
