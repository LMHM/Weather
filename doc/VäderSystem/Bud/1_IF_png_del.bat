@ECHO OFF
color 7F
:beginning
if not exist klar. goto beginning
c:\program\irfanview\i_view32 /capture=3 /convert=summering.png
copy summering.png T:\weather\
copy summering.png N:
PING -n 3 localhost
del klar.
GOTO beginning