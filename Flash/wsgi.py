"""
WSGI config for Flash project.

It exposes the WSGI callable as a module-level variable named ``application``.

For more information on this file, see
https://docs.djangoproject.com/en/4.2/howto/deployment/wsgi/
"""

import os

from django.core.wsgi import get_wsgi_application
print("\033[1;33m\nIf you want to quit FlashCalc, press Ctrl+C\n\033[0;37m")
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'Flash.settings')

application = get_wsgi_application()

