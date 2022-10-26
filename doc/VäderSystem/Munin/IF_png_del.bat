@ECHO OFF
color 7F
:beginning
if not exist klar20. goto beginning
c:\program\irfanview\i_view32 /capture=3 /convert=20dag.png
copy 20dag.png T:\weather\
copy 20dag.png N:
PING -n 3 localhost
del klar20.
GOTO beginning