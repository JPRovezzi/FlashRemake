import os
import webbrowser
import subprocess

# Define a function to install a module using pip3
def install_module(module):
    # Run the pip3 install command as a subprocess
    subprocess.run(["pip3", "install", module])

# Check if Django is installed
install_module("django")

# Changes default PORT, if not PORT=8000
PORT = 6900

# Set webpage URL
url = f"http://127.0.0.1:{PORT}/" 

# Open URL in a new tab, if a browser window is already open.
webbrowser.open_new_tab(url)

# Open URL in new window, raising the window if possible.
webbrowser.open_new(url)

# Run migration in Django
try:
    os.system("python manage.py migrate")
except: 
    os.system("python3 manage.py migrate")

# Run Django server
try:
    os.system(f"python manage.py runserver {PORT}")
except:
    os.system(f"python3 manage.py runserver {PORT}")




