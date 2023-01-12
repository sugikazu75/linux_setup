#!/usr/bin/env python

import rosbag
import sys
import os
import math
# from aerial_robot_msgs.msg import PoseControlPid


args = sys.argv

assert len(args)>=2, "you must specify the rosbag by assigning the argument."

filename=os.path.normpath(os.path.join(os.getcwd(),args[1]))
bag = rosbag.Bag(filename)

topics = bag.get_type_and_topic_info()[1].keys()

pid_topic = [topic for topic in topics if 'pid' in topic][0]
flight_state_topic = [topic for topic in topics if 'flight_state' in topic][0]
robot_name = pid_topic.split("/")[1]

LAND_STATE = 5 # TODO: include from aerial_robot_control
HOVER_STATE = 4 # TODO: include from aerial_robot_control

start_t = None
offset_t = 5.0 # TODO: set as parameter
pose_squared_errors_sum = [0] * 6
pose_cnt = 0

for i, (topic, msg, t) in enumerate(bag.read_messages(topics=[pid_topic, flight_state_topic])):

    # print(topic, t.to_sec())

    if topic == flight_state_topic:
        if msg.state == HOVER_STATE:

            if start_t is None:
                start_t = t.to_sec()

        if msg.state == LAND_STATE:

            rms = [math.sqrt(i / pose_cnt) for i in pose_squared_errors_sum]

            print("RMS of pos errors: [{}, {}, {}], rpy att errors: [{}, {}, {}]".format(rms[0], rms[1], rms[2], rms[3], rms[4], rms[5]))

            break

    if topic == pid_topic:
        if start_t is None:
            continue

        if t.to_sec() > start_t + offset_t:
            pose_squared_errors_sum[0] = pose_squared_errors_sum[0] + msg.x.pos_error * msg.x.pos_error
            pose_squared_errors_sum[1] = pose_squared_errors_sum[1] + msg.y.pos_error * msg.y.pos_error
            pose_squared_errors_sum[2] = pose_squared_errors_sum[2] + msg.z.pos_error * msg.z.pos_error

            pose_squared_errors_sum[3] = pose_squared_errors_sum[3] + msg.roll.pos_error * msg.roll.pos_error
            pose_squared_errors_sum[4] = pose_squared_errors_sum[4] + msg.pitch.pos_error * msg.pitch.pos_error
            pose_squared_errors_sum[5] = pose_squared_errors_sum[5] + msg.yaw.pos_error * msg.yaw.pos_error

            pose_cnt += 1

            # rms = [math.sqrt(i / pose_cnt) for i in pose_squared_errors_sum]
            # print(rms)


bag.close()
