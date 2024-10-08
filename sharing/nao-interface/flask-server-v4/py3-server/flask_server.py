#!/usr/bin/env python

# Import packages
from __future__ import print_function
import json
import argparse

#import flask
from flask import Flask, request

#import vision class and methods
from vision import Vision
#import vision configurations
from vision_config import VisionConfig


# load in the visionConfig class and store it in vision_config
vision_config = VisionConfig()


# Flask server
py3_server = Flask(__name__)

@py3_server.route("/test_connection", methods=["POST"])
def make_connection():
    '''Test the connection by sending back the received message'''
    request_data = request.get_json(force=True)
    errors = check_request_data(request_data, ['message'])
    if errors:
        return json.dumps({'errors': errors}), 400
    else:
        return json.dumps({'message': request_data['message']}), 200

# checking data retrieved from lisp
def check_request_data(request_data, keys):
    '''Helper function that checks if certain key(s) are present
    in the POST data and if they are, returns the data.'''
    errors = []
    for key in keys:
        if key not in request_data:
            errors.append('No data provided using the "{}" key.'.format(key))
    return errors


@py3_server.route("/vision/analyse", methods=["POST"])
def analyze_image():
    '''Analyze the image at the given pathname.
    Returns both the data of the analysis and the pathname
    of the image + bboxes'''
    # decodes json into request_data
    request_data = request.get_json(force=True)
    # if request_data does not have a filename, then an error is thrown
    errors = check_request_data(request_data, ['filename'])
    # if there are errors, analysis is not performed
    if errors:
        return json.dumps({'errors' : errors}), 400
    # if there are no errors, the image located at filename is analyzed
    # the data from analysis is encoded to json and returned by the function
    else:
        vision = Vision(vision_config)
        pathname, data = vision.analyze(request_data['filename'])
        return json.dumps({'pathname': pathname,
                           'data': data}), 200

# Running the py-3 server

if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('--server-host',
                        action='store',
                        dest='server_host',
                        default='127.0.0.1',
                        help='The server host address')
    parser.add_argument('--server-port',
                        action='store',
                        dest='server_port',
                        default=7851,
                        help='The server port number')
    cmd = parser.parse_args()
    
    py3_server.run(host=cmd.server_host, port=cmd.server_port)
