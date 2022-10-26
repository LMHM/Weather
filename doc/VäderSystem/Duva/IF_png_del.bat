@ECHO OFF
color 7F
:beginning
if not exist klar. goto beginning
c:\program1\irfanview\i_view32 /capture=3 /convert=weather.png
copy weather.png T:\weather\
copy weather.png N:
PING -n 3 localhost
del klar.
GOTO beginning