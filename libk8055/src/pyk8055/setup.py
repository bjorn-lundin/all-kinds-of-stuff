#!/usr/bin/env python

from distutils.core import setup, Extension


setup(
	name='pyk8055',
	version='0.1',
	author='Pjetur G. Hjaltason',
	author_email='pjetur@pjetur.net',
	description='K8055 library wrapper',
	url='http://libk8055.sourceforge.net/',
	ext_modules = [Extension('_pyk8055', libraries=["usb"], sources=['libk8055.i',"../libk8055.c"])],
	py_modules=['pyk8055']
)
