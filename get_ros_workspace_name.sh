#!/bin/bash
# get_ros_workspace_name.sh
if [ -z "$CMAKE_PREFIX_PATH" ]; then
    echo "No ROS workspace found"
else
    echo $CMAKE_PREFIX_PATH
fi
