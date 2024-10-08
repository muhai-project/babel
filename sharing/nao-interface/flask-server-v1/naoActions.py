#!python2.7
#!/usr/bin/env python

from __future__ import print_function

import time

import naoqi
from naoqi import ALProxy, ALModule


class NaoMovement(object):

    def __init__(self, ip, port, *args, **kwargs):
        # Init proxies.
        try:
            self.motionProxy = ALProxy("ALMotion", ip, port)
        except Exception as e:
            print("Could not create proxy to ALMotion")
            print("Error was: ", e)

        try:
            self.postureProxy = ALProxy("ALRobotPosture", ip, port)
        except Exception as e:
            print("Could not create proxy to ALRobotPosture")
            print("Error was: ", e)

    def stiffness_on(self, proxy):
        if not proxy.robotIsWakeUp():
            proxy.wakeUp()

    def stiffness_off(self, proxy):
        if proxy.robotIsWakeUp():
            proxy.rest()


class NaoPosture(NaoMovement):

    def __init__(self, ip, port, *args, **kwargs):
        super(NaoPosture, self).__init__(ip, port, *args, **kwargs)

    def set(self, posture="Stand", speed=0.3):
        """ Go to the given posture """
        self.stiffness_on(self.motionProxy)
        success = self.postureProxy.goToPosture(posture, speed)
        return success

    def get(self):
        """ Get the current posture """
        posture = self.postureProxy.getPosture()
        return posture


class NaoJoints(NaoMovement):

    def __init__(self, ip, port, *args, **kwargs):
        super(NaoJoints, self).__init__(ip, port, *args, **kwargs)

    def set(self, joint="HeadPitch", value=0.0, speed=0.3):
        """ Move the given joint to a given value """
        self.stiffness_on(self.motionProxy)
        self.motionProxy.setAngles(joint, value, speed)
        return True

    def raise_left(self):
        LScurrentPitch = self.motionProxy.getAngles("LShoulderPitch", False)[0]
        LScurrentRoll = self.motionProxy.getAngles("LShoulderRoll", False)[0]
        LEcurrentRoll = self.motionProxy.getAngles("LElbowRoll", False)[0]

        jointList = ["LShoulderPitch", "LShoulderRoll", "LElbowRoll"]
        angleList = [[0.4, LScurrentPitch],
                     [0.0, LScurrentRoll],
                     [-0.1, LEcurrentRoll]]
        timeList = [[2.0, 6.0], [2.0, 6.0], [2.0, 6.0]]
        isAbsolute = True

        self.motionProxy.angleInterpolation(jointList, angleList, timeList, isAbsolute)
        return True

    def raise_right(self):
        RScurrentPitch = self.motionProxy.getAngles("RShoulderPitch", False)[0]
        RScurrentRoll = self.motionProxy.getAngles("RShoulderRoll", False)[0]
        REcurrentRoll = self.motionProxy.getAngles("RElbowRoll", False)[0]

        jointList = ["RShoulderPitch", "RShoulderRoll", "RElbowRoll"]
        angleList = [[0.4, RScurrentPitch],
                     [0.0, RScurrentRoll],
                     [0.1, REcurrentRoll]]
        timeList = [[2.0, 6.0], [2.0, 6.0], [2.0, 6.0]]
        isAbsolute = True

        self.motionProxy.angleInterpolation(jointList, angleList, timeList, isAbsolute)
        return True

    def raise_both(self):
        LScurrentPitch = self.motionProxy.getAngles("LShoulderPitch", False)[0]
        LScurrentRoll = self.motionProxy.getAngles("LShoulderRoll", False)[0]
        LEcurrentRoll = self.motionProxy.getAngles("LElbowRoll", False)[0]
        RScurrentPitch = self.motionProxy.getAngles("RShoulderPitch", False)[0]
        RScurrentRoll = self.motionProxy.getAngles("RShoulderRoll", False)[0]
        REcurrentRoll = self.motionProxy.getAngles("RElbowRoll", False)[0]

        jointList = ["LShoulderPitch", "LShoulderRoll", "LElbowRoll",
                     "RShoulderPitch", "RShoulderRoll", "RElbowRoll"]
        angleList = [[0.4, LScurrentPitch], [-0.3, LScurrentRoll], [-0.1, LEcurrentRoll],
                     [0.4, RScurrentPitch], [0.3, RScurrentRoll], [0.1, REcurrentRoll]]
        timeList = [[2.0, 6.0], [2.0, 6.0], [2.0, 6.0],
                    [2.0, 6.0], [2.0, 6.0], [2.0, 6.0]]
        isAbsolute = True
        self.motionProxy.angleInterpolation(jointList, angleList, timeList, isAbsolute)
        return True

    def raise_arm(self, arm=None):
        if arm == "Left":
            return self.raise_left()
        elif arm == "Right":
            return self.raise_right()
        elif arm == "Both":
            return self.raise_both()
        else:
            return False

    def say_yes(self):
        current_head_pitch = self.motionProxy.getAngles("HeadPitch", False)[0]
        names = ["HeadPitch"]
        angleList = [-0.4, 0.3, -0.4, 0.3, current_head_pitch]
        timeList = [1.0, 1.5, 2.0, 2.5, 3.0]
        self.motionProxy.angleInterpolation(names, angleList, timeList, True)
        return True

    def say_no(self):
        current_head_yaw = self.motionProxy.getAngles("HeadYaw", False)[0]
        names = ["HeadYaw"]
        angleList = [1.0, -1.0, 1.0, -1.0, current_head_yaw]
        timeList = [1.0, 1.5, 2.0, 2.5, 3.0]
        self.motionProxy.angleInterpolation(names, angleList, timeList, True)
        return True

    def move_head(self, yesno=None):
        if yesno == "yes":
            return self.say_yes()
        elif yesno == "no":
            return self.say_no()
        else:
            return False


class NaoSpeak(object):

    def __init__(self, ip, port):
        # Init proxies.
        try:
            self.ttsProxy = ALProxy("ALTextToSpeech", ip, port)
        except Exception as e:
            print("Could not create proxy to ALTextToSpeech")
            print("Error was: ", e)

    def say(self, speech=""):
        """ Say something """
        self.ttsProxy.say(str(speech))
        return True


class NaoHeadTouch(object):

    def __init__(self, ip, port):
        # Set memoryproxy
        try:
            self.memoryProxy = ALProxy("ALMemory", ip, port)
        except Exception as e:
            print("Could not create proxy to ALMemory")
            print("Error was: ", e)

        try:
            self.ledProxy = ALProxy("ALLeds", ip, port)
        except Exception as e:
            print("Could not create proxy to ALLeds")
            print("Error was: ", e)

        self.front_sensor = "Device/SubDeviceList/Head/Touch/Front/Sensor/Value"
        self.middle_sensor = "Device/SubDeviceList/Head/Touch/Middle/Sensor/Value"
        self.rear_sensor = "Device/SubDeviceList/Head/Touch/Rear/Sensor/Value"
        self.front_leds = "BrainLedsFront"
        self.middle_leds = "BrainLedsMiddle"
        self.back_leds = "BrainLedsBack"
        self.brain_leds = "BrainLeds"
        self.delay = 0.5

    def all_leds_on(self):
        self.ledProxy.on(self.brain_leds)
        #for led in [self.front_leds, self.middle_leds, self.back_leds]:
        #    self.ledProxy.on(led)

    def all_leds_off(self):
        self.ledProxy.off(self.brain_leds)
        #for led in [self.front_leds, self.middle_leds, self.back_leds]:
        #    self.ledProxy.off(led)

    def front_or_back(self):
        self.all_leds_on()
        self.ledProxy.off(self.middle_leds)
        while True:
            if self.memoryProxy.getData(self.front_sensor) > 0.5:
                self.all_leds_on()
                return "front"
            elif self.memoryProxy.getData(self.rear_sensor) > 0.5:
                self.all_leds_on()
                return "back"
            time.sleep(self.delay)

    def detect_touch(self, region="Front"):
        self.all_leds_off()
        if region == "Front":
            self.ledProxy.on(self.front_leds)
            while self.memoryProxy.getData(self.front_sensor) < 0.5:
                time.sleep(self.delay)
            self.all_leds_on()
            return True
        elif region == "Rear":
            self.ledProxy.on(self.back_leds)
            while self.memoryProxy.getData(self.rear_sensor) < 0.5:
                time.sleep(self.delay)
            self.all_leds_on()
            return True
        elif region == "Middle":
            self.ledProxy.on(self.middle_leds)
            while self.memoryProxy.getData(self.middle_sensor) < 0.5:
                time.sleep(self.delay)
            self.all_leds_on()
            return True


class NaoSpeechRecognition(object):

    def __init__(self, ip, port):
        try:
            self.asrProxy = ALProxy("ALSpeechRecognition", ip, port)
        except Exception as e:
            print("Could not create proxy to ALSpeechRecognition")
            print("Error was: ", e)

        try:
            self.memoryProxy = ALProxy("ALMemory", ip, port)
        except Exception as e:
            print("Could not create proxy to ALMemory")
            print("Error was: ", e)

        try:
            self.ledProxy = ALProxy("ALLeds", ip, port)
        except Exception as e:
            print("Could not create proxy to ALLeds")
            print("Error was: ", e)

    def start_speech_recognition(self, vocabulary=[]):
        ''' Start the speech recognition, given a vocabulary '''
        self.asrProxy.setLanguage("English")
        vocab = [str(v) for v in vocabulary]
        self.asrProxy.setVocabulary(vocab, False)
        subscriber = "Nao_ASR_" + str(int(time.time()))
        self.asrProxy.subscribe(subscriber)
        self.ledProxy.on("EarLeds")
        return subscriber

    def stop_speech_recognition(self, subscriber=""):
        ''' Stop the speech recognition, get the detected word(s) '''
        detected = self.memoryProxy.getData("WordRecognized")
        self.asrProxy.unsubscribe(str(subscriber))
        self.ledProxy.off("EarLeds")
        return detected
