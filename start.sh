#!/bin/bash

cd /apps


# stop nginx if it is running
nginx -s stop 2>/dev/null || true
sleep 1

# Start App 1 on port 8080 (adjust command for how you run your R Shiny apps)
cd sota2026
R -f app.R &
PID_APP1=$!
cd ..

# Start App 2 on port 8081
# cd sota2026
# R -f app.R &
# PID_APP2=$!
# cd ..

# Start Nginx in the foreground (or background, depending on setup)
# Note: Nginx usually daemonizes by default, so we force it to run in the foreground (-g 'daemon off;')
nginx -g 'daemon off;' &
PID_NGINX=$!

# Trap termination signals so we can gracefully kill child processes if the container stops
# trap "kill -TERM $PID_APP1 $PID_APP2 $PID_NGINX; exit 0" SIGTERM SIGINT
trap "kill -TERM $PID_APP1 $PID_NGINX; exit 0" SIGTERM SIGINT

# Wait for any process to exit. If one dies, exit the script so Docker knows it failed.
wait -n

# Exit with status of whichever process exited first
exit $?
