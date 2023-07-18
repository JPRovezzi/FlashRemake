import os
import webbrowser

PORT = 6900
url = f"http://127.0.0.1:{PORT}/"

# Open URL in a new tab, if a browser window is already open.
webbrowser.open_new_tab(url)

# Open URL in new window, raising the window if possible.
webbrowser.open_new(url)

os.system("python manage.py migrate")



os.system(f"python manage.py runserver {PORT}")




