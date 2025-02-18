import os
import webbrowser
import subprocess

# xapp should have been installed in order to_
# solve "Failed to load module "xapp-gtk3-module" error.
# Try Sudo apt install xapp

# Define a function to install a module using pip3
def install_module(module):
    # Run the pip3 install command as a subprocess
    subprocess.run(["pip3", "install", module])

# Print title
print(
"\033[1;34m -"*10+"\n|Starting FlashCalc|\n"+" -"*10+"\n\033[0;37m")

# Check if Django is installed
print("\033[1;34mChecking if all prerequisites are installed...\033[0;37m")
install_module("django")
print()

# Change default PORT, if not PORT=8000
PORT = 6900

# Set webpage URL
url = f"http://127.0.0.1:{PORT}/" 

# Open the webbrowser
print("\033[1;34mOpening your webbrowser...\033[0;37m")

# Open URL in a new tab, if a browser window is already open.
#webbrowser.open_new_tab(url)

# Open URL in new window, raising the window if possible.
webbrowser.open_new(url)

# Run migration in Django
print()
try:
    os.system("python manage.py migrate") # Windows
    print("\033[1;34mMigrations checked\033[0;37m")
except: 
    os.system("python3 manage.py migrate") # Linux
    print("\033[1;34mMigrations checked\033[0;37m")

# Run Django server
print(f"\033[1;34mStarting server at http://127.0.0.1:{PORT}/\033[0;37m")
#print("\033[1;33m\nIf you want to quit FlashCalc, press Ctrl+C\n\033[0;37m")
try:
    os.system(f"python manage.py runserver {PORT}") # Windows
except:
    os.system(f"python3 manage.py runserver {PORT}") # Linux


#for external https://www.twilio.com/blog/expose-local-django-server-to-internet-using-ngrok
# ERR_NGROK_6022
#Before you can serve HTML content, you must sign up for a free ngrok account and install your authtoken.

#Get help with this error
    



