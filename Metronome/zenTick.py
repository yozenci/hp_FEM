#! /usr/bin/env python
"""
Training Metronome. More docstrings coming soon. Revamp on the GUI needed.
Currently requires keyboard stop. Threading required for GUI interaction.

"""
from Tkinter import *
from tkSnack import *
from time import sleep

class metronome:
    root = Tkinter.Tk()
    initializeSnack(root)
    click = Sound()
    
    filt = Filter('generator', 440, 30000, 0.9, 'sine', 100)
    filt2 = Filter('generator', 440, 0, 0.9, 'sine', 100)
    filtacc = Filter('generator', 440, 60000 , 0.9, 'sine', 100) # Accent the 1

    def __init__ (self, timesig, bpm, accent):
        self.timesig = timesig
        self.bpm = bpm
        self.accent = accent
    def play(self):

        
        rest = 60.0/self.bpm
        if self.accent == True:
            while(True):
                for i in range(self.timesig):
                    if i==0:
                        self.click.play(filter = self.filtacc, blocking = 1)
                        sleep(rest)
                    else:                        
                        self.click.play(filter = self.filt, blocking = 1)                
                        sleep(rest)
                        
        if self.accent == False:
            while(True):
                for i in range(self.timesig):                        
                    self.click.play(filter = self.filt, blocking = 1)                
                    sleep(rest)
           

                
class trainMet(metronome):
#sequence length sets on/off metronome i.e. X will be 1 bar of metronome, X-1 bars silent metronome

    def __init__(self, timesig, bpm, accent, sequence_length):
        metronome.__init__(self, timesig, bpm, accent)
        self.accent = accent
        self.sequence_length = sequence_length
    def play(self):

        rest = 60.0/self.bpm
        if self.accent == True:
            while(True):
                for i in range(self.timesig):
                    if i==0:
                        self.click.play(filter = self.filtacc, blocking = 1)
                        sleep(rest)
                    else:    
                        self.click.play(filter = self.filt, blocking = 1)                
                        sleep(rest)

                for i in range(self.timesig*(self.sequence_length - 1)):
                     
                    self.click.play(filter = self.filt2, blocking = 1)                
                    sleep(rest)            

                        
        if self.accent == False:
            while(True):
                for i in range(self.timesig):                        
                    self.click.play(filter = self.filt, blocking = 1)                
                    sleep(rest)

                for i in range(self.timesig*(self.sequence_length - 1)):
                     
                    self.click.play(filter = self.filt2, blocking = 1)                
                    sleep(rest)


#Test it
print "Welcome to ZenTick Version 0.7 \n"
timesig = input("How many beats in your bar? Typically between 3 and 7.:\n")
bpm = input("Choose a BPM:\n")
accent = input("Accent on the first beat?(True or False):\n")
sequence = input("How long is your training sequence?\n")
print ""

if accent==True:    
    print "Time signiture: %d/4   BPM: %d   Accent on the 1     Sequence Length: %d " % (timesig, bpm, sequence)

else:
    print "Time signiture: %d/4   BPM: %d   Sequence Length: %d " % (timesig, bpm, sequence)
    
zigzag = trainMet(timesig ,220, accent, sequence)
zigzag.play()




