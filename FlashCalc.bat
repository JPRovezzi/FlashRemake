@echo off
@echo on
python manage.py migrate
python manage.py runserver
pause