#!/bin/bash

amixer sget Master | awk -F"[][]" '/dB/ {print $2, $6}'
