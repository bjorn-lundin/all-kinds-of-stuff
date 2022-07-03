#!/bin/bash
export SO=/home/bnl/svn/lundin/trunk/python/pong/pickles
rsync -azvhr bnl@192.168.1.202:$SO/ ./pickles/


