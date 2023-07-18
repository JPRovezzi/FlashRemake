@echo off
mkdir src
move /y "function.js" src
move /y "gruposram.js" src
move /y "main.js" src
move /y "rqpar.js" src
@echo on
python3 OpenServer.py